#include <llvm/IR/Verifier.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/Triple.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <stdexcept>
#include <cstdlib>
#include <optional>
#include <fstream>
#include <iostream>
#include <unordered_map>
#include <functional>
#include <iomanip>
#include <filesystem>

#include "codegen/codegen.h"

namespace Summit {

// stdlib function registry
// maps summit function names like std.io.println to their internal C names
struct StdlibFunctionSpec {
    std::string mangled_name;
    llvm::FunctionType* (*type_builder)(llvm::LLVMContext&);
};

// type builders for print function overloads
// these help us figure out which variant to call based on argument types
static llvm::FunctionType* buildVoidStringType(llvm::LLVMContext& ctx) {
    return llvm::FunctionType::get(
        llvm::Type::getVoidTy(ctx),
        {llvm::PointerType::getUnqual(ctx)},
        false
    );
}

static llvm::FunctionType* buildVoidInt64Type(llvm::LLVMContext& ctx) {
    return llvm::FunctionType::get(
        llvm::Type::getVoidTy(ctx),
        {llvm::Type::getInt64Ty(ctx)},
        false
    );
}

static llvm::FunctionType* buildVoidFloatType(llvm::LLVMContext& ctx) {
    return llvm::FunctionType::get(
        llvm::Type::getVoidTy(ctx),
        {llvm::Type::getFloatTy(ctx)},
        false
    );
}

static llvm::FunctionType* buildVoidDoubleType(llvm::LLVMContext& ctx) {
    return llvm::FunctionType::get(
        llvm::Type::getVoidTy(ctx),
        {llvm::Type::getDoubleTy(ctx)},
        false
    );
}

static llvm::FunctionType* buildStringVoidType(llvm::LLVMContext& ctx) {
    return llvm::FunctionType::get(
        llvm::PointerType::getUnqual(ctx),
        {},
        false
    );
}

static llvm::FunctionType* buildIntVoidType(llvm::LLVMContext& ctx) {
    return llvm::FunctionType::get(
        llvm::Type::getInt64Ty(ctx),
        {},
        false
    );
}

static llvm::FunctionType* buildVoidBoolType(llvm::LLVMContext& ctx) {
    return llvm::FunctionType::get(
        llvm::Type::getVoidTy(ctx),
        {llvm::Type::getInt1Ty(ctx)},
        false
    );
}

static llvm::FunctionType* buildVoidUint64Type(llvm::LLVMContext& ctx) {
    return llvm::FunctionType::get(
        llvm::Type::getVoidTy(ctx),
        {llvm::Type::getInt64Ty(ctx)},
        false
    );
}

// the big lookup table for all builtin functions
static const std::unordered_map<std::string, StdlibFunctionSpec> STDLIB_REGISTRY = {
    {"std.io.println", {"summit_io_println", buildVoidStringType}},
    {"std.io.print",   {"summit_io_print",   buildVoidStringType}},
    
    {"std.io.println_int", {"summit_io_println_int", buildVoidInt64Type}},
    {"std.io.print_int",   {"summit_io_print_int",   buildVoidInt64Type}},
    
    {"std.io.println_uint", {"summit_io_println_uint", buildVoidUint64Type}},
    {"std.io.print_uint",   {"summit_io_print_uint",   buildVoidUint64Type}},
    
    {"std.io.println_f32", {"summit_io_println_f32", buildVoidFloatType}},
    {"std.io.print_f32",   {"summit_io_print_f32",   buildVoidFloatType}},
    {"std.io.println_f64", {"summit_io_println_f64", buildVoidDoubleType}},
    {"std.io.print_f64",   {"summit_io_print_f64",   buildVoidDoubleType}},
    
    {"std.io.println_bool", {"summit_io_println_bool", buildVoidBoolType}},
    {"std.io.print_bool",   {"summit_io_print_bool",   buildVoidBoolType}},
};

CodeGenerator::CodeGenerator() {
    context_ = std::make_unique<llvm::LLVMContext>();
    module_ = std::make_unique<llvm::Module>("summit_module", *context_);
    builder_ = std::make_unique<llvm::IRBuilder<>>(*context_);
    
    // initialize all the llvm target machinery
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();
}

CodeGenerator::~CodeGenerator() = default;

// type conversion helpers
// converting between summit's type system and llvm's
llvm::Type* CodeGenerator::getLLVMType(const Type& type) {
    switch (type.kind) {
        case Type::Kind::I8:
        case Type::Kind::U8:
            return llvm::Type::getInt8Ty(*context_);
        case Type::Kind::I16:
        case Type::Kind::U16:
            return llvm::Type::getInt16Ty(*context_);
        case Type::Kind::I32:
        case Type::Kind::U32:
            return llvm::Type::getInt32Ty(*context_);
        case Type::Kind::I64:
        case Type::Kind::U64:
            return llvm::Type::getInt64Ty(*context_);
        case Type::Kind::F32:
            return llvm::Type::getFloatTy(*context_);
        case Type::Kind::F64:
            return llvm::Type::getDoubleTy(*context_);
        case Type::Kind::BOOL:
            return llvm::Type::getInt1Ty(*context_);
        case Type::Kind::STRING:
            return llvm::PointerType::getUnqual(*context_);
        case Type::Kind::VOID:
            return llvm::Type::getVoidTy(*context_);
        case Type::Kind::INFERRED:
            return getInt64Type();
        default:
            // this should never happen, but you know how it goes...
            throw std::runtime_error("Unknown type");
    }
}

bool CodeGenerator::isIntegerType(const Type& type) {
    switch (type.kind) {
        case Type::Kind::I8:
        case Type::Kind::I16:
        case Type::Kind::I32:
        case Type::Kind::I64:
        case Type::Kind::U8:
        case Type::Kind::U16:
        case Type::Kind::U32:
        case Type::Kind::U64:
        case Type::Kind::BOOL:
            return true;
        default:
            return false;
    }
}

bool CodeGenerator::isSignedType(const Type& type) {
    // need to know if we're dealing with signed or unsigned for div/comparison ops
    switch (type.kind) {
        case Type::Kind::I8:
        case Type::Kind::I16:
        case Type::Kind::I32:
        case Type::Kind::I64:
            return true;
        case Type::Kind::U8:
        case Type::Kind::U16:
        case Type::Kind::U32:
        case Type::Kind::U64:
        case Type::Kind::BOOL:
            return false;
        case Type::Kind::F32:
        case Type::Kind::F64:
        case Type::Kind::STRING:
        case Type::Kind::VOID:
        case Type::Kind::INFERRED:
        default:
            return true;
    }
}

Type CodeGenerator::getSummitTypeFromLLVMType(llvm::Type* type) {
    // reverse lookup from llvm types back to summit types
    if (type->isIntegerTy(8)) return Type::i8();
    if (type->isIntegerTy(16)) return Type::i16();
    if (type->isIntegerTy(32)) return Type::i32();
    if (type->isIntegerTy(64)) return Type::i64();
    if (type->isFloatTy()) return Type::f32();
    if (type->isDoubleTy()) return Type::f64();
    if (type->isIntegerTy(1)) return Type::boolean();
    if (type->isPointerTy()) return Type::string();
    if (type->isVoidTy()) return Type::void_type();
    return Type::inferred();
}

llvm::Value* CodeGenerator::castValue(llvm::Value* value, llvm::Type* target_type, const Type& target_summit_type) {
    llvm::Type* source_type = value->getType();
    
    if (source_type == target_type) {
        return value;
    }

    // integer to integer casts
    if (target_type->isIntegerTy() && source_type->isIntegerTy()) {
        unsigned source_bits = source_type->getIntegerBitWidth();
        unsigned target_bits = target_type->getIntegerBitWidth();
        
        bool is_signed = isSignedType(target_summit_type);
        
        if (target_bits > source_bits) {
            // widening cast, need sign or zero extension
            return is_signed ? 
                builder_->CreateSExt(value, target_type) :
                builder_->CreateZExt(value, target_type);
        } else if (target_bits < source_bits) {
            // narrowing, just truncate
            return builder_->CreateTrunc(value, target_type);
        } else {
            return builder_->CreateIntCast(value, target_type, is_signed);
        }
    }

    // float to float casts
    if (target_type->isFloatingPointTy() && source_type->isFloatingPointTy()) {
        if (target_type->isDoubleTy() && source_type->isFloatTy()) {
            return builder_->CreateFPExt(value, target_type);
        } else if (target_type->isFloatTy() && source_type->isDoubleTy()) {
            return builder_->CreateFPTrunc(value, target_type);
        }
        return value;
    }
    
    // int to float
    if (target_type->isFloatingPointTy() && source_type->isIntegerTy()) {
        bool is_signed = isSignedType(target_summit_type);
        return is_signed ?
            builder_->CreateSIToFP(value, target_type) :
            builder_->CreateUIToFP(value, target_type);
    }
    
    // float to int
    if (target_type->isIntegerTy() && source_type->isFloatingPointTy()) {
        bool is_signed = isSignedType(target_summit_type);
        return is_signed ?
            builder_->CreateFPToSI(value, target_type) :
            builder_->CreateFPToUI(value, target_type);
    }
    
    // pointer conversions
    if (target_type->isIntegerTy() && source_type->isPointerTy()) {
        return builder_->CreatePtrToInt(value, target_type);
    }
    
    if (target_type->isPointerTy() && source_type->isIntegerTy()) {
        return builder_->CreateIntToPtr(value, target_type);
    }
    
    throw std::runtime_error("Unsupported type cast from " + 
                           getTypeString(source_type) + " to " + 
                           getTypeString(target_type));
}

std::string CodeGenerator::getTypeString(llvm::Type* type) {
    // get a readable name for error messages
    if (type->isIntegerTy(1)) return "bool";
    if (type->isIntegerTy(8)) return "i8";
    if (type->isIntegerTy(16)) return "i16";
    if (type->isIntegerTy(32)) return "i32";
    if (type->isIntegerTy(64)) return "i64";
    if (type->isFloatTy()) return "f32";
    if (type->isDoubleTy()) return "f64";
    if (type->isPointerTy()) return "pointer";
    if (type->isVoidTy()) return "void";
    return "unknown";
}

// module and import system stuff

llvm::Function* CodeGenerator::getOrDeclareStdlibFunction(const std::string& full_path) {
    // check if we already have this function
    auto it = stdlib_functions_.find(full_path);
    if (it != stdlib_functions_.end()) {
        return it->second;
    }

    // look it up in the registry
    auto spec_it = STDLIB_REGISTRY.find(full_path);
    if (spec_it == STDLIB_REGISTRY.end()) {
        return nullptr;
    }
    
    const auto& spec = spec_it->second;
    
    // create the function declaration
    llvm::FunctionType* func_type = spec.type_builder(*context_);
    llvm::Function* func = llvm::Function::Create(
        func_type,
        llvm::Function::ExternalLinkage,
        spec.mangled_name,
        module_.get()
    );
    
    stdlib_functions_[full_path] = func;
    
    // also register it in the module hierarchy
    size_t last_dot = full_path.find_last_of('.');
    if (last_dot != std::string::npos) {
        std::string module_path = full_path.substr(0, last_dot);
        std::string func_name = full_path.substr(last_dot + 1);
        
        ensureModuleExists(module_path);
        modules_[module_path].functions[func_name] = func;
    }
    
    return func;
}

void CodeGenerator::ensureModuleExists(const std::string& path) {
    // make sure this module path is registered, create parent paths too
    if (modules_.find(path) != modules_.end()) {
        return;
    }
    
    modules_[path] = ModuleInfo();
    
    size_t last_dot = path.find_last_of('.');
    if (last_dot != std::string::npos) {
        std::string parent = path.substr(0, last_dot);
        ensureModuleExists(parent);
    }
}

std::string CodeGenerator::extractFunctionPath(const Expression* expr) {
    // parse something like std.io.println from the AST
    if (auto* member = dynamic_cast<const MemberAccess*>(expr)) {
        std::vector<std::string> path;
        const Expression* curr = member;

        // walk up the chain of member accesses
        while (auto* m = dynamic_cast<const MemberAccess*>(curr)) {
            path.insert(path.begin(), m->member);
            curr = m->object.get();
        }

        if (auto* import_expr = dynamic_cast<const ImportExpr*>(curr)) {
            path.insert(path.begin(), import_expr->module);
        } else if (auto* id = dynamic_cast<const Identifier*>(curr)) {
            auto alias_it = module_aliases_.find(id->name);
            if (alias_it != module_aliases_.end()) {
                path.insert(path.begin(), alias_it->second);
            } else {
                path.insert(path.begin(), id->name);
            }
        }
        
        // build the full dotted path string
        if (!path.empty()) {
            std::string full_path = path[0];
            for (size_t i = 1; i < path.size(); i++) {
                full_path += "." + path[i];
            }
            return full_path;
        }
    }
    
    return "";
}

// main code generation entry point

void CodeGenerator::generate(const Program& program) {
    for (const auto& stmt : program.statements) {
        codegen(*stmt);
    }
    
    // verify we got a main function
    llvm::Function* main_func = module_->getFunction("main");
    if (!main_func) {
        throw std::runtime_error("No main function found");
    }
    
    // make sure the generated IR is valid
    std::string error;
    llvm::raw_string_ostream error_stream(error);
    if (llvm::verifyModule(*module_, &error_stream)) {
        throw std::runtime_error("Module verification failed: " + error);
    }

    module_->print(llvm::errs(), nullptr);
}

// object file and executable generation

void CodeGenerator::emitObjectFile(const std::string& filename) {
    auto target_triple = llvm::sys::getDefaultTargetTriple();
    module_->setTargetTriple(target_triple);
    
    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);
    if (!target) {
        throw std::runtime_error("Failed to lookup target: " + error);
    }
    
    llvm::TargetOptions opt;
    std::optional<llvm::Reloc::Model> RM;
    auto target_machine = target->createTargetMachine(
        target_triple, "generic", "", opt, RM
    );
    
    module_->setDataLayout(target_machine->createDataLayout());
    
    std::error_code EC;
    llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);
    if (EC) {
        throw std::runtime_error("Could not open file: " + EC.message());
    }
    
    llvm::legacy::PassManager pass;
    auto FileType = llvm::CodeGenFileType::ObjectFile;
    
    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        throw std::runtime_error("Target machine can't emit object file");
    }
    
    pass.run(*module_);
    dest.flush();
}

void CodeGenerator::emitExecutable(const std::string& filename, 
                                   const std::vector<std::string>& libs,
                                   bool no_stdlib) {
    std::string obj_file = filename + ".o";
    emitObjectFile(obj_file);

    std::string output_filename = filename;
#ifdef _WIN32
    if (output_filename.find(".exe") == std::string::npos) {
        output_filename += ".exe";
    }
#endif
    
    std::string link_cmd;
    
#ifdef _WIN32
    // windows linking is kinda messy with the stdlib
    std::string stdlibPath, dllPath;
    bool useStdlib = !no_stdlib;

    if (useStdlib) {
        const char* envLib = std::getenv("SUMMIT_LIB");
        if (envLib) {
            std::filesystem::path libPath(envLib);
            if (std::filesystem::is_directory(libPath)) {
                std::vector<std::string> winLibNames = {"libsummit_std.lib", "libsummit_std.a"};
                for (const auto& libName : winLibNames) {
                    std::filesystem::path fullPath = libPath / libName;
                    if (std::filesystem::exists(fullPath)) { 
                        stdlibPath = fullPath.string(); 
                        break; 
                    }
                }
                std::vector<std::string> winDllNames = {"libsummit_std.dll", "summit_std.dll"};
                for (const auto& dllName : winDllNames) {
                    std::filesystem::path fullDllPath = libPath / dllName;
                    if (std::filesystem::exists(fullDllPath)) { 
                        dllPath = fullDllPath.string(); 
                        break; 
                    }
                }
            } else if (std::filesystem::exists(libPath)) {
                stdlibPath = envLib;
                std::string ext = libPath.extension().string();
                if (ext == ".dll") dllPath = envLib;
            }
        }

        if (stdlibPath.empty()) {
            std::vector<std::string> searchPaths = {"./lib"};
            std::vector<std::string> libNames = {"libsummit_std.lib", "libsummit_std.a"};
            
            for (const auto& searchPath : searchPaths) {
                for (const auto& libName : libNames) {
                    std::filesystem::path fullPath = std::filesystem::path(searchPath) / libName;
                    if (std::filesystem::exists(fullPath)) {
                        stdlibPath = fullPath.string();
                        break;
                    }
                }
                if (!stdlibPath.empty()) break;
            }

            if (dllPath.empty()) {
                std::vector<std::string> dllNames = {"libsummit_std.dll", "summit_std.dll"};
                for (const auto& dllName : dllNames) {
                    std::filesystem::path fullPath = std::filesystem::path("./lib") / dllName;
                    if (std::filesystem::exists(fullPath)) {
                        dllPath = fullPath.string();
                        break;
                    }
                }
            }
        }
        
        if (stdlibPath.empty() && dllPath.empty()) {
            std::cerr << "Warning: Standard library not found.\n";
            std::cerr << "Set SUMMIT_LIB to point to the library directory.\n";
            std::cerr << "Searched in: ./lib/\n";
        } else {
            std::cout << "Using standard library: " << (stdlibPath.empty() ? dllPath : stdlibPath) << std::endl;
            if (!dllPath.empty()) std::cout << "Shared library: " << dllPath << std::endl;
        }
    }

    link_cmd = "g++ -mconsole -o \"" + output_filename + "\" \"" + obj_file + "\"";

    if (useStdlib) {
        if (!dllPath.empty()) {
            std::filesystem::path dllDir = std::filesystem::path(dllPath).parent_path();
            link_cmd += " -L\"" + dllDir.string() + "\" -lsummit_std";
        } else if (!stdlibPath.empty()) {
            link_cmd += " \"" + stdlibPath + "\"";
        }
    }

    link_cmd += " -lstdc++";
    link_cmd += " -luser32 -lkernel32 -lgdi32 -ladvapi32";
    
#else
    // unix linking is way cleaner
    link_cmd = "g++ -no-pie -o " + output_filename + " " + obj_file;
    
    if (!no_stdlib) {
        std::string stdlibPath;
        
        const char* envLib = std::getenv("SUMMIT_LIB");
        if (envLib) {
            std::filesystem::path libPath(envLib);
            if (std::filesystem::is_directory(libPath)) {
                std::vector<std::string> libNames = {"libsummit_std.so", "libsummit_std.dylib", "libsummit_std.a"};
                for (const auto& libName : libNames) {
                    std::filesystem::path fullPath = libPath / libName;
                    if (std::filesystem::exists(fullPath)) {
                        stdlibPath = fullPath.string();
                        break;
                    }
                }
            } else if (std::filesystem::exists(libPath)) {
                stdlibPath = envLib;
            }
        }
        
        if (stdlibPath.empty()) {
            std::vector<std::string> lib_paths = {
                "lib/libsummit_std.so",
                "lib/libsummit_std.dylib",
                "lib/libsummit_std.a"
            };
            
            for (const auto& path : lib_paths) {
                std::ifstream test(path);
                if (test.good()) {
                    stdlibPath = path;
                    break;
                }
            }
        }
        
        if (!stdlibPath.empty()) {
            std::cout << "Found standard library: " << stdlibPath << std::endl;
            if (stdlibPath.find(".so") != std::string::npos || stdlibPath.find(".dylib") != std::string::npos) {
                size_t last_slash = stdlibPath.find_last_of("/");
                std::string lib_dir = (last_slash != std::string::npos) ? stdlibPath.substr(0, last_slash) : ".";

                std::filesystem::path abs_lib_dir = std::filesystem::absolute(lib_dir);
                std::string lib_dir_str = abs_lib_dir.string();
                
                link_cmd += " -L\"" + lib_dir_str + "\" -lsummit_std";
                link_cmd += " -Wl,-rpath,\"" + lib_dir_str + "\"";
            } else {
                link_cmd += " \"" + stdlibPath + "\"";
            }
        } else {
            std::cerr << "Warning: Could not find libsummit_std, trying with -lsummit_std" << std::endl;
            std::filesystem::path abs_lib_dir = std::filesystem::absolute("./lib");
            std::string lib_dir_str = abs_lib_dir.string();
            link_cmd += " -L\"" + lib_dir_str + "\" -lsummit_std";
            link_cmd += " -Wl,-rpath,\"" + lib_dir_str + "\"";
        }
        
        link_cmd += " -lstdc++ -lm -lpthread -ldl";
    }
#endif
    
    // add any extra libs the user wants
    for (const auto& lib : libs) {
#ifdef _WIN32
        if (lib.find("build/linux") != std::string::npos || 
            lib.find("linux/x86_64") != std::string::npos) {
            std::cout << "Skipping incompatible library: " << lib << std::endl;
            continue;
        }
#endif
        if (lib.find(".a") != std::string::npos || lib.find(".so") != std::string::npos || 
            lib.find(".dylib") != std::string::npos || lib.find(".lib") != std::string::npos ||
            lib.find(".dll") != std::string::npos) {
            link_cmd += " \"" + lib + "\"";
        } else {
            link_cmd += " -l" + lib;
        }
    }
    
    std::cout << "Linking command: " << link_cmd << std::endl;
    
    int result = std::system(link_cmd.c_str());
    
#ifdef _WIN32
    // copy dll to output dir if needed
    if (result == 0 && !dllPath.empty()) {
        std::filesystem::path exeDir = std::filesystem::path(output_filename).parent_path();
        if (exeDir.empty()) exeDir = ".";
        std::filesystem::path targetDll = exeDir / std::filesystem::path(dllPath).filename();
        try { 
            if (!std::filesystem::exists(targetDll)) {
                std::filesystem::copy_file(dllPath, targetDll, std::filesystem::copy_options::overwrite_existing);
                std::cout << "Copied DLL to output directory" << std::endl;
            }
        } catch (...) { 
            std::cerr << "Warning: Failed to copy DLL" << std::endl;
        }
    }
#endif
    
    if (result != 0) {
        throw std::runtime_error("Linking failed with exit code: " + std::to_string(result));
    }
    
    std::remove(obj_file.c_str());
    
#ifndef _WIN32
    std::string chmod_cmd = "chmod +x " + output_filename;
    std::system(chmod_cmd.c_str());
#endif
    
    std::cout << "Successfully created executable: " + output_filename << std::endl;
    
    // print file size for fun
    std::ifstream file(output_filename, std::ios::binary | std::ios::ate);
    if (file) {
        size_t size = file.tellg();
        double size_kb = size / 1024.0;
        double size_mb = size_kb / 1024.0;
        
        std::cout << "Executable size: ";
        if (size_mb >= 1.0) {
            std::cout << std::fixed << std::setprecision(2) << size_mb << " MB";
        } else {
            std::cout << std::fixed << std::setprecision(2) << size_kb << " KB";
        }
        std::cout << std::endl;
    }
}

// expression codegen dispatch

llvm::Value* CodeGenerator::codegen(const Expression& expr) {
    if (auto* e = dynamic_cast<const NumberLiteral*>(&expr)) {
        return codegenNumberLiteral(*e);
    } else if (auto* e = dynamic_cast<const StringLiteral*>(&expr)) {
        return codegenStringLiteral(*e);
    } else if (auto* e = dynamic_cast<const Identifier*>(&expr)) {
        return codegenIdentifier(*e);
    } else if (auto* e = dynamic_cast<const MemberAccess*>(&expr)) {
        return codegenMemberAccess(*e);
    } else if (auto* e = dynamic_cast<const FunctionCall*>(&expr)) {
        return codegenFunctionCall(*e);
    } else if (auto* e = dynamic_cast<const BinaryOp*>(&expr)) {
        return codegenBinaryOp(*e);
    } else if (auto* e = dynamic_cast<const ImportExpr*>(&expr)) {
        return codegenImport(*e);
    } else if (auto* e = dynamic_cast<const NamedImportExpr*>(&expr)) {
        return codegenNamedImport(*e);
    } else if (auto* e = dynamic_cast<const CastExpr*>(&expr)) {
        return codegenCast(*e);
    } else if (auto* e = dynamic_cast<const UnaryOp*>(&expr)) {
        return codegenUnaryOp(*e);
    } else if (auto* e = dynamic_cast<const AssignmentExpr*>(&expr)) {
        return codegenAssignment(*e);
    } else if (auto* e = dynamic_cast<const BooleanLiteral*>(&expr)) {
        return codegenBooleanLiteral(*e);
    }
    throw std::runtime_error("Unknown expression type");
}

// literal codegen

llvm::Value* CodeGenerator::codegenBooleanLiteral(const BooleanLiteral& expr) {
    return llvm::ConstantInt::get(getLLVMType(Type::boolean()), expr.value ? 1 : 0);
}

// llvm's type system gets confused with large integers sometimes so this solution had to be implemented
// not super convenient but it works, might change this later
// everything has to be stored as an i64 whether unsigned or signed for 64 bit integers
llvm::Value* CodeGenerator::codegenNumberLiteral(const NumberLiteral& expr) {
    if (expr.isLargeInteger()) {
        const std::string& largeInt = expr.getLargeIntValue();
        try {
            uint64_t unsigned_value = std::stoull(largeInt);
            if (unsigned_value > INT64_MAX) {
                // this caused issues in early testing, hope it doesn't come back to bite when adding 128 bit support
                return llvm::ConstantInt::get(*context_, 
                    llvm::APInt(64, unsigned_value, false));
            } else {
                int64_t signed_value = std::stoll(largeInt);
                return llvm::ConstantInt::get(*context_, 
                    llvm::APInt(64, signed_value, true));
            }
        } catch (const std::out_of_range&) {
            throw std::runtime_error("Integer too large: " + largeInt);
        }
    } else if (expr.isRegularInteger()) {
        int64_t value = expr.getIntValue();

        // pick the smallest type that fits
        if (value >= -128 && value <= 127) {
            return llvm::ConstantInt::get(*context_, llvm::APInt(8, value, true));
        } else if (value >= -32768 && value <= 32767) {
            return llvm::ConstantInt::get(*context_, llvm::APInt(16, value, true));
        } else if (value >= -2147483648 && value <= 2147483647) {
            return llvm::ConstantInt::get(*context_, llvm::APInt(32, value, true));
        } else {
            return llvm::ConstantInt::get(*context_, llvm::APInt(64, value, true));
        }
    } else {
        return llvm::ConstantFP::get(getDoubleType(), 
            llvm::APFloat(expr.getFloatValue()));
    }
}

llvm::Value* CodeGenerator::codegenStringLiteral(const StringLiteral& expr) {
    return builder_->CreateGlobalStringPtr(expr.value);
}

llvm::Value* CodeGenerator::codegenIdentifier(const Identifier& expr) {
    // check if this is a module alias
    if (module_aliases_.find(expr.name) != module_aliases_.end()) {
        return llvm::ConstantInt::get(*context_, llvm::APInt(64, 0, true));
    }

    // check if it's a function reference
    auto func_it = function_references_.find(expr.name);
    if (func_it != function_references_.end()) {
        return func_it->second;
    }
    
    // must be a variable
    llvm::Value* val = named_values_[expr.name];
    if (!val) {
        throw std::runtime_error("Unknown variable: " + expr.name);
    }
    
    auto* alloca = llvm::dyn_cast<llvm::AllocaInst>(val);
    if (!alloca) {
        throw std::runtime_error("Variable " + expr.name + " is not an alloca");
    }
    llvm::Type* var_type = alloca->getAllocatedType();
    return builder_->CreateLoad(var_type, val, expr.name);
}

llvm::Value* CodeGenerator::codegenMemberAccess(const MemberAccess& expr) {
    return llvm::ConstantInt::get(*context_, llvm::APInt(64, 0, true));
}

llvm::Value* CodeGenerator::codegenUnaryOp(const UnaryOp& expr) {
    llvm::Value* operand = codegen(*expr.operand);
    
    if (expr.op == "-") {
        if (operand->getType()->isFloatingPointTy()) {
            return builder_->CreateFNeg(operand, "negtmp");
        } else if (operand->getType()->isIntegerTy()) {
            return builder_->CreateNeg(operand, "negtmp");
        } else {
            throw std::runtime_error("Unary minus not supported for type");
        }
    }
    
    throw std::runtime_error("Unknown unary operator: " + expr.op);
}

llvm::Value* CodeGenerator::codegenCast(const CastExpr& expr) {
    llvm::Value* value = codegen(*expr.expression);
    llvm::Type* target_type = getLLVMType(expr.target_type);
    
    return castValue(value, target_type, expr.target_type);
}

// binary operation codegen

llvm::Value* CodeGenerator::codegenBinaryOp(const BinaryOp& expr) {
    llvm::Value* left = codegen(*expr.left);
    llvm::Value* right = codegen(*expr.right);
    
    // need to check if we're dealing with unsigned values
    bool left_is_signed = true;
    bool right_is_signed = true;
    
    if (auto* left_num = dynamic_cast<const NumberLiteral*>(expr.left.get())) {
        if (left_num->isLargeInteger()) {
            const std::string& largeInt = left_num->getLargeIntValue();
            try {
                uint64_t unsigned_value = std::stoull(largeInt);
                if (unsigned_value > INT64_MAX) {
                    left_is_signed = false;
                }
            } catch (const std::out_of_range&) {
                left_is_signed = false;
            }
        }
    }
    
    if (auto* right_num = dynamic_cast<const NumberLiteral*>(expr.right.get())) {
        if (right_num->isLargeInteger()) {
            const std::string& largeInt = right_num->getLargeIntValue();
            try {
                uint64_t unsigned_value = std::stoull(largeInt);
                if (unsigned_value > INT64_MAX) {
                    right_is_signed = false;
                }
            } catch (const std::out_of_range&) {
                right_is_signed = false;
            }
        }
    }
    
    bool use_unsigned = !left_is_signed || !right_is_signed;
    
    // type matching for operands
    if (left->getType() != right->getType()) {
        bool left_is_float = left->getType()->isFloatingPointTy();
        bool right_is_float = right->getType()->isFloatingPointTy();
        
        if (left_is_float != right_is_float) {
            throw std::runtime_error("Type error: Cannot perform binary operation between integer and floating-point types without explicit cast");
        }

        llvm::Type* target_type = nullptr;
        Type target_summit_type(Type::Kind::I64);
        
        if (left->getType()->isFloatingPointTy() || right->getType()->isFloatingPointTy()) {
            target_type = getDoubleType();
            target_summit_type.kind = Type::Kind::F64;
        } else if (left->getType()->isIntegerTy() && right->getType()->isIntegerTy()) {
            unsigned left_bits = left->getType()->getIntegerBitWidth();
            unsigned right_bits = right->getType()->getIntegerBitWidth();
            
            // pick the wider type
            if (left_bits >= right_bits) {
                target_type = left->getType();
                if (left_bits == 64) target_summit_type.kind = use_unsigned ? Type::Kind::U64 : Type::Kind::I64;
                else if (left_bits == 32) target_summit_type.kind = use_unsigned ? Type::Kind::U32 : Type::Kind::I32;
                else if (left_bits == 16) target_summit_type.kind = use_unsigned ? Type::Kind::U16 : Type::Kind::I16;
                else if (left_bits == 8) target_summit_type.kind = use_unsigned ? Type::Kind::U8 : Type::Kind::I8;
                else target_summit_type.kind = use_unsigned ? Type::Kind::U64 : Type::Kind::I64;
            } else {
                target_type = right->getType();
                if (right_bits == 64) target_summit_type.kind = use_unsigned ? Type::Kind::U64 : Type::Kind::I64;
                else if (right_bits == 32) target_summit_type.kind = use_unsigned ? Type::Kind::U32 : Type::Kind::I32;
                else if (right_bits == 16) target_summit_type.kind = use_unsigned ? Type::Kind::U16 : Type::Kind::I16;
                else if (right_bits == 8) target_summit_type.kind = use_unsigned ? Type::Kind::U8 : Type::Kind::I8;
                else target_summit_type.kind = use_unsigned ? Type::Kind::U64 : Type::Kind::I64;
            }
        } else {
            target_type = getInt64Type();
            target_summit_type.kind = use_unsigned ? Type::Kind::U64 : Type::Kind::I64;
        }

        if (left->getType() != target_type) {
            left = castValue(left, target_type, target_summit_type);
        }

        if (right->getType() != target_type) {
            right = castValue(right, target_type, target_summit_type);
        }
    }
    
    // floating point ops
    if (left->getType()->isFloatingPointTy()) {
        if (expr.op == "+") {
            return builder_->CreateFAdd(left, right, "addtmp");
        } else if (expr.op == "-") {
            return builder_->CreateFSub(left, right, "subtmp");
        } else if (expr.op == "*") {
            return builder_->CreateFMul(left, right, "multmp");
        } else if (expr.op == "/") {
            return builder_->CreateFDiv(left, right, "divtmp");
        }
    } else {
        // integer ops
        if (expr.op == "+") {
            return builder_->CreateAdd(left, right, "addtmp");
        } else if (expr.op == "-") {
            return builder_->CreateSub(left, right, "subtmp");
        } else if (expr.op == "*") {
            return builder_->CreateMul(left, right, "multmp");
        } else if (expr.op == "/") {
            if (use_unsigned) {
                return builder_->CreateUDiv(left, right, "divtmp");
            } else {
                return builder_->CreateSDiv(left, right, "divtmp");
            }
        }
    }
    
    throw std::runtime_error("Unknown binary operator: " + expr.op);
}

// function call codegen

llvm::Value* CodeGenerator::codegenFunctionCall(const FunctionCall& expr) {
    llvm::Function* callee = nullptr;
    std::string func_name;

    std::string full_path = extractFunctionPath(expr.callee.get());
    
    if (!full_path.empty()) {
        func_name = full_path;
        callee = getOrDeclareStdlibFunction(full_path);

        // try resolving aliases if that didn't work
        if (!callee) {
            size_t first_dot = full_path.find('.');
            if (first_dot != std::string::npos) {
                std::string first_part = full_path.substr(0, first_dot);
                auto alias_it = module_aliases_.find(first_part);
                if (alias_it != module_aliases_.end()) {
                    std::string resolved_path = alias_it->second + full_path.substr(first_dot);
                    callee = getOrDeclareStdlibFunction(resolved_path);
                    if (callee) {
                        func_name = resolved_path;
                    }
                }
            }
        }
    } 
    else if (auto* id = dynamic_cast<const Identifier*>(expr.callee.get())) {
        func_name = id->name;
        
        auto func_ref_it = function_references_.find(func_name);
        if (func_ref_it != function_references_.end()) {
            callee = func_ref_it->second;
            if (callee) {
                func_name = callee->getName().str();
            }
        } else {
            callee = getFunction(func_name);
        }
    }

    if (!callee) {
        throw std::runtime_error("Unknown function: " + func_name);
    }

    std::vector<llvm::Value*> args;
    
    for (size_t i = 0; i < expr.arguments.size(); i++) {
        llvm::Value* arg_val = codegen(*expr.arguments[i]);
        args.push_back(arg_val);
    }

    std::string actual_func_name = callee->getName().str();
    
    Type return_summit_type = Type::i64();
    auto return_type_it = function_return_types_.find(func_name);
    if (return_type_it != function_return_types_.end()) {
        return_summit_type = return_type_it->second;
    } else {
        return_summit_type = getSummitTypeFromLLVMType(callee->getReturnType());
    }
    
    // handle print function overloading
    // this is kinda messy but it works for figuring out which print variant to call
    if (!args.empty()) {
        bool is_print_func = (actual_func_name == "summit_io_println" || 
                             actual_func_name == "summit_io_print" ||
                             actual_func_name.find("std.io.println") != std::string::npos || 
                             actual_func_name.find("std.io.print") != std::string::npos);
        
        if (is_print_func) {
            llvm::Type* arg_type = args[0]->getType();
            Type summit_type = Type::i64();
            
            // try to figure out what type we're actually printing
            if (auto* func_call = dynamic_cast<const FunctionCall*>(expr.arguments[0].get())) {
                std::string called_func_name;
                if (auto* id = dynamic_cast<const Identifier*>(func_call->callee.get())) {
                    called_func_name = id->name;
                    auto ret_type_it = function_return_types_.find(called_func_name);
                    if (ret_type_it != function_return_types_.end()) {
                        summit_type = ret_type_it->second;
                    }
                }
            } else if (auto* id = dynamic_cast<const Identifier*>(expr.arguments[0].get())) {
                auto type_it = variable_types_.find(id->name);
                if (type_it != variable_types_.end()) {
                    summit_type = type_it->second;
                }
            } else if (auto* num_lit = dynamic_cast<const NumberLiteral*>(expr.arguments[0].get())) {
                if (num_lit->isLargeInteger()) {
                    summit_type = Type::u64();
                } else if (num_lit->isRegularInteger()) {
                    summit_type = Type::i64();
                } else {
                    summit_type = Type::f64();
                }
            } else if (auto* bool_lit = dynamic_cast<const BooleanLiteral*>(expr.arguments[0].get())) {
                summit_type = Type::boolean();
            } else if (auto* str_lit = dynamic_cast<const StringLiteral*>(expr.arguments[0].get())) {
                summit_type = Type::string();
            } else if (auto* cast_expr = dynamic_cast<const CastExpr*>(expr.arguments[0].get())) {
                summit_type = cast_expr->target_type;
            }
            
            // pick the right print variant
            if (arg_type->isIntegerTy(1) || summit_type.kind == Type::Kind::BOOL) {
                std::string base_name = (actual_func_name.find("println") != std::string::npos) ? 
                    "std.io.println" : "std.io.print";
                std::string bool_variant = base_name + "_bool";
                llvm::Function* bool_func = getOrDeclareStdlibFunction(bool_variant);
                if (bool_func) {
                    callee = bool_func;
                    if (!arg_type->isIntegerTy(1)) {
                        args[0] = castValue(args[0], getLLVMType(Type::boolean()), Type::boolean());
                    }
                }
            }
            else if ((arg_type->isIntegerTy() && !arg_type->isIntegerTy(1)) || 
                     summit_type.kind == Type::Kind::I8 || summit_type.kind == Type::Kind::I16 || 
                     summit_type.kind == Type::Kind::I32 || summit_type.kind == Type::Kind::I64 ||
                     summit_type.kind == Type::Kind::U8 || summit_type.kind == Type::Kind::U16 || 
                     summit_type.kind == Type::Kind::U32 || summit_type.kind == Type::Kind::U64) {
                
                std::string base_name = (actual_func_name.find("println") != std::string::npos) ? 
                    "std.io.println" : "std.io.print";
                
                bool is_unsigned = (summit_type.kind == Type::Kind::U8 ||
                                summit_type.kind == Type::Kind::U16 ||
                                summit_type.kind == Type::Kind::U32 ||
                                summit_type.kind == Type::Kind::U64);
                
                std::string int_variant = is_unsigned ? base_name + "_uint" : base_name + "_int";
                llvm::Function* int_func = getOrDeclareStdlibFunction(int_variant);
                
                if (int_func) {
                    callee = int_func;
                    if (!arg_type->isIntegerTy()) {
                        if (arg_type->isPointerTy()) {
                            throw std::runtime_error("Cannot convert pointer to integer for print function");
                        }
                        args[0] = castValue(args[0], getInt64Type(), summit_type);
                    } else if (!arg_type->isIntegerTy(64)) {
                        if (is_unsigned) {
                            args[0] = builder_->CreateZExt(args[0], getInt64Type(), "print_zext");
                        } else {
                            args[0] = builder_->CreateSExt(args[0], getInt64Type(), "print_sext");
                        }
                    }
                }
            }
            else if (arg_type->isFloatingPointTy() || 
                     summit_type.kind == Type::Kind::F32 || summit_type.kind == Type::Kind::F64) {
                
                std::string base_name = (actual_func_name.find("println") != std::string::npos) ? 
                    "std.io.println" : "std.io.print";
                
                std::string float_variant;
                if (summit_type.kind == Type::Kind::F32) {
                    float_variant = base_name + "_f32";
                } else {
                    float_variant = base_name + "_f64";
                }
                
                llvm::Function* float_func = getOrDeclareStdlibFunction(float_variant);
                if (float_func) {
                    callee = float_func;
                    if (summit_type.kind == Type::Kind::F32 && arg_type->isDoubleTy()) {
                        args[0] = builder_->CreateFPTrunc(args[0], getLLVMType(Type::f32()), "print_fptrunc");
                    } else if (summit_type.kind == Type::Kind::F64 && arg_type->isFloatTy()) {
                        args[0] = builder_->CreateFPExt(args[0], getLLVMType(Type::f64()), "print_fpext");
                    } else if (!arg_type->isFloatingPointTy()) {
                        args[0] = castValue(args[0], getLLVMType(summit_type), summit_type);
                    }
                }
            }
            else if (arg_type->isPointerTy() || summit_type.kind == Type::Kind::STRING) {
                std::string base_name = (actual_func_name.find("println") != std::string::npos) ? 
                    "std.io.println" : "std.io.print";
                llvm::Function* string_func = getOrDeclareStdlibFunction(base_name);
                if (string_func) {
                    callee = string_func;
                    if (!arg_type->isPointerTy()) {
                        throw std::runtime_error("Cannot convert non-pointer type to string for print function");
                    }
                }
            }
        }
    }

    llvm::FunctionType* final_func_type = callee->getFunctionType();
    
    // cast args to match function signature
    for (size_t i = 0; i < args.size() && i < final_func_type->getNumParams(); i++) {
        if (args[i]->getType() != final_func_type->getParamType(i)) {
            Type expected_summit_type = getSummitTypeFromLLVMType(final_func_type->getParamType(i));
            args[i] = castValue(args[i], final_func_type->getParamType(i), expected_summit_type);
        }
    }

    if (args.size() != final_func_type->getNumParams()) {
        if (!final_func_type->isVarArg()) {
            throw std::runtime_error("Function " + callee->getName().str() + " expects " + 
                                   std::to_string(final_func_type->getNumParams()) + 
                                   " arguments but got " + std::to_string(args.size()));
        }
    }
    
    return builder_->CreateCall(callee, args);
}

// import statement codegen

llvm::Value* CodeGenerator::codegenImport(const ImportExpr& expr) {
    ensureModuleExists(expr.module);
    return llvm::ConstantInt::get(*context_, llvm::APInt(64, 0, true));
}

llvm::Value* CodeGenerator::codegenNamedImport(const NamedImportExpr& expr) {
    ensureModuleExists(expr.module);

    for (const auto& import_name : expr.imports) {
        std::string full_path = expr.module + "." + import_name;
        llvm::Function* func = getOrDeclareStdlibFunction(full_path);
        
        if (func) {
            function_references_[import_name] = func;
        } else {
            std::cerr << "Warning: Function '" << import_name << "' not found in module '" 
                      << expr.module << "'" << std::endl;
        }
    }
    
    return llvm::ConstantInt::get(*context_, llvm::APInt(64, 0, true));
}

// variable assignment and type promotion

void CodeGenerator::promoteVariableType(const std::string& name, llvm::Value* new_value, const Type& new_summit_type) {
    // promote a variable to a wider type if needed
    llvm::Value* var = named_values_[name];
    if (!var) {
        throw std::runtime_error("Unknown variable: " + name);
    }
    
    auto* alloca = llvm::dyn_cast<llvm::AllocaInst>(var);
    if (!alloca) {
        throw std::runtime_error("Variable " + name + " is not an alloca");
    }
    
    llvm::Type* current_type = alloca->getAllocatedType();
    llvm::Type* new_type = new_value->getType();

    if (current_type == new_type) {
        builder_->CreateStore(new_value, var);
        return;
    }

    if (current_type->isIntegerTy() && new_type->isIntegerTy()) {
        unsigned current_bits = current_type->getIntegerBitWidth();
        unsigned new_bits = new_type->getIntegerBitWidth();

        if (new_bits > current_bits) {
            // need to widen the storage
            llvm::AllocaInst* new_alloca = builder_->CreateAlloca(new_type, nullptr, name + "_promoted");
            
            llvm::Value* current_val = builder_->CreateLoad(current_type, var);
            llvm::Value* extended_val;
            
            if (isSignedType(variable_types_[name])) {
                extended_val = builder_->CreateSExt(current_val, new_type);
            } else {
                extended_val = builder_->CreateZExt(current_val, new_type);
            }

            builder_->CreateStore(extended_val, new_alloca);
            
            named_values_[name] = new_alloca;
            variable_types_[name] = new_summit_type;

            builder_->CreateStore(new_value, new_alloca);
            return;
        }
    }
    
    if (new_value->getType() != current_type) {
        new_value = castValue(new_value, current_type, variable_types_[name]);
    }
    
    builder_->CreateStore(new_value, var);
}

llvm::Value* CodeGenerator::codegenAssignment(const AssignmentExpr& expr) {
    llvm::Value* value = codegen(*expr.value);
    
    Type new_summit_type = getSummitTypeFromLLVMType(value->getType());
    
    Type current_summit_type = variable_types_[expr.name];
    llvm::Value* var = named_values_[expr.name];
    
    if (!var) {
        throw std::runtime_error("Unknown variable in assignment: " + expr.name);
    }
    
    auto* alloca = llvm::dyn_cast<llvm::AllocaInst>(var);
    if (!alloca) {
        throw std::runtime_error("Variable " + expr.name + " is not an alloca");
    }
    
    llvm::Type* current_type = alloca->getAllocatedType();
    llvm::Type* new_type = value->getType();
    
    // special handling for u64 assignments
    if (current_summit_type.kind == Type::Kind::U64) {
        if (auto* num_lit = dynamic_cast<const NumberLiteral*>(expr.value.get())) {
            if (num_lit->isLargeInteger()) {
                const std::string& largeInt = num_lit->getLargeIntValue();
                try {
                    uint64_t unsigned_value = std::stoull(largeInt);
                    value = llvm::ConstantInt::get(*context_, 
                        llvm::APInt(64, unsigned_value, false));
                } catch (const std::out_of_range&) {
                    throw std::runtime_error("Integer too large for u64: " + largeInt);
                }
            } else if (num_lit->isRegularInteger()) {
                int64_t signed_value = num_lit->getIntValue();
                uint64_t unsigned_value = static_cast<uint64_t>(signed_value);
                value = llvm::ConstantInt::get(*context_, 
                    llvm::APInt(64, unsigned_value, false));
            }
        }
    }
    
    // range checking for assignments
    if (isIntegerType(current_summit_type)) {
        if (auto* num_lit = dynamic_cast<const NumberLiteral*>(expr.value.get())) {
            if (num_lit->isLargeInteger()) {
                if (!isLargeValueInRange(num_lit->getLargeIntValue(), current_summit_type)) {
                    throw std::runtime_error("Value " + num_lit->getLargeIntValue() + 
                                           " is out of range for type " + 
                                           typeToString(current_summit_type));
                }
            } else if (num_lit->isRegularInteger()) {
                int64_t value = num_lit->getIntValue();
                if (current_summit_type.kind == Type::Kind::U8 ||
                    current_summit_type.kind == Type::Kind::U16 ||
                    current_summit_type.kind == Type::Kind::U32 ||
                    current_summit_type.kind == Type::Kind::U64) {
                    if (value < 0) {
                        throw std::runtime_error("Value " + std::to_string(value) + 
                                               " is out of range for type " + 
                                               typeToString(current_summit_type));
                    }
                }
                if (!isValueInRange(value, current_summit_type)) {
                    throw std::runtime_error("Value " + std::to_string(value) + 
                                           " is out of range for type " + 
                                           typeToString(current_summit_type));
                }
            }
        }
    }
    
    // check if we need type promotion
    if (current_type->isIntegerTy() && new_type->isIntegerTy()) {
        unsigned current_bits = current_type->getIntegerBitWidth();
        unsigned new_bits = new_type->getIntegerBitWidth();
        
        if (new_bits > current_bits) {
            promoteVariableType(expr.name, value, new_summit_type);
            return value;
        }
    }
    
    if (value->getType() != current_type) {
        value = castValue(value, current_type, current_summit_type);
    }
    
    builder_->CreateStore(value, var);
    return value;
}

bool CodeGenerator::isValueInRange(int64_t value, const Type& type) {
    // check if a value fits in the target type
    switch (type.kind) {
        case Type::Kind::I8:
            return value >= -128 && value <= 127;
        case Type::Kind::U8:
            return value >= 0 && value <= 255;
        case Type::Kind::I16:
            return value >= -32768 && value <= 32767;
        case Type::Kind::U16:
            return value >= 0 && value <= 65535;
        case Type::Kind::I32:
            return value >= -2147483648 && value <= 2147483647;
        case Type::Kind::U32:
            return value >= 0 && value <= 4294967295;
        case Type::Kind::I64:
        case Type::Kind::U64:
            return true;
        case Type::Kind::BOOL:
            return value == 0 || value == 1;
        default:
            return true;
    }
}

bool CodeGenerator::isLargeValueInRange(const std::string& largeValue, const Type& type) {
    // for really big numbers that don't fit in int64_t
    if (type.kind != Type::Kind::U64) {
        return false;
    }
    
    try {
        uint64_t value = std::stoull(largeValue);
        return true;
    } catch (const std::out_of_range&) {
        return false;
    }
}

std::string CodeGenerator::typeToString(const Type& type) {
    switch (type.kind) {
        case Type::Kind::I8: return "i8";
        case Type::Kind::I16: return "i16";
        case Type::Kind::I32: return "i32";
        case Type::Kind::I64: return "i64";
        case Type::Kind::U8: return "u8";
        case Type::Kind::U16: return "u16";
        case Type::Kind::U32: return "u32";
        case Type::Kind::U64: return "u64";
        case Type::Kind::F32: return "f32";
        case Type::Kind::F64: return "f64";
        case Type::Kind::BOOL: return "bool";
        case Type::Kind::STRING: return "string";
        case Type::Kind::VOID: return "void";
        case Type::Kind::INFERRED: return "inferred";
        default: return "unknown";
    }
}

// statement codegen dispatch

void CodeGenerator::codegen(const Statement& stmt) {
    if (auto* s = dynamic_cast<const VarDecl*>(&stmt)) {
        codegenVarDecl(*s);
    } else if (auto* s = dynamic_cast<const FunctionDecl*>(&stmt)) {
        codegenFunctionDecl(*s);
    } else if (auto* s = dynamic_cast<const ReturnStmt*>(&stmt)) {
        codegenReturn(*s);
    } else if (auto* s = dynamic_cast<const ExpressionStmt*>(&stmt)) {
        codegenExprStmt(*s);
    } else if (auto* s = dynamic_cast<const UsingStmt*>(&stmt)) {
        codegenUsingStmt(*s);
    } else if (auto* s = dynamic_cast<const UsingImportStmt*>(&stmt)) {
        codegenUsingImportStmt(*s);
    } else {
        throw std::runtime_error("Unknown statement type");
    }
}

void CodeGenerator::importAllFunctionsFromModule(const std::string& module_path) {
    // import everything from a module, used for "using" statements
    for (const auto& [full_path, spec] : STDLIB_REGISTRY) {
        if (full_path.find(module_path + ".") == 0) {
            std::string func_name = full_path.substr(module_path.length() + 1);

            llvm::Function* func = getOrDeclareStdlibFunction(full_path);
            if (func) {
                function_references_[func_name] = func;
            }
        }
    }
}

void CodeGenerator::codegenUsingImportStmt(const UsingImportStmt& stmt) {
    if (auto* import_expr = dynamic_cast<const ImportExpr*>(stmt.import_expr.get())) {
        std::string module_path = import_expr->module;
        ensureModuleExists(module_path);

        importAllFunctionsFromModule(module_path);
        
    } else if (auto* named_import = dynamic_cast<const NamedImportExpr*>(stmt.import_expr.get())) {
        ensureModuleExists(named_import->module);
        
        for (const auto& import_name : named_import->imports) {
            std::string full_path = named_import->module + "." + import_name;
            llvm::Function* func = getOrDeclareStdlibFunction(full_path);
            
            if (func) {
                function_references_[import_name] = func;
            } else {
                std::cerr << "Warning: Function '" << import_name << "' not found in module '" 
                          << named_import->module << "'" << std::endl;
            }
        }
    } else if (auto* member_access = dynamic_cast<const MemberAccess*>(stmt.import_expr.get())) {
        std::string full_path = extractFunctionPath(member_access);
        
        if (!full_path.empty()) {
            importAllFunctionsFromModule(full_path);
        } else {
            throw std::runtime_error("Could not resolve module path for using statement");
        }
    } else {
        throw std::runtime_error("Unsupported import expression in using statement");
    }
}

void CodeGenerator::codegenUsingStmt(const UsingStmt& stmt) {
    auto alias_it = module_aliases_.find(stmt.module_alias);
    if (alias_it == module_aliases_.end()) {
        throw std::runtime_error("Unknown module alias: " + stmt.module_alias);
    }
    
    std::string module_path = alias_it->second;

    for (const auto& [full_path, spec] : STDLIB_REGISTRY) {
        if (full_path.find(module_path + ".") == 0) {
            std::string func_name = full_path.substr(module_path.length() + 1);

            llvm::Function* func = getOrDeclareStdlibFunction(full_path);
            if (func) {
                function_references_[func_name] = func;
            }
        }
    }
}

void CodeGenerator::codegenVarDecl(const VarDecl& stmt) {
    // handle import statements disguised as var decls
    if (auto* import_expr = dynamic_cast<const ImportExpr*>(stmt.initializer.get())) {
        ensureModuleExists(import_expr->module);
        module_aliases_[stmt.name] = import_expr->module;
        return;
    }

    if (auto* named_import = dynamic_cast<const NamedImportExpr*>(stmt.initializer.get())) {
        ensureModuleExists(named_import->module);

        for (const auto& import_name : named_import->imports) {
            std::string full_path = named_import->module + "." + import_name;
            llvm::Function* func = getOrDeclareStdlibFunction(full_path);
            
            if (func) {
                function_references_[import_name] = func;
            } else {
                std::cerr << "Warning: Function '" << import_name << "' not found in module '" 
                          << named_import->module << "'" << std::endl;
            }
        }
        return;
    }

    // handle function references
    if (auto* id = dynamic_cast<const Identifier*>(stmt.initializer.get())) {
        auto func_it = function_references_.find(id->name);
        if (func_it != function_references_.end()) {
            function_references_[stmt.name] = func_it->second;
            return;
        }
    }

    if (auto* member = dynamic_cast<const MemberAccess*>(stmt.initializer.get())) {
        std::string full_path = extractFunctionPath(member);
        
        if (!full_path.empty()) {
            ensureModuleExists(full_path);

            llvm::Function* func = getOrDeclareStdlibFunction(full_path);
            if (func) {
                function_references_[stmt.name] = func;
                return;
            }

            auto mod_it = modules_.find(full_path);
            if (mod_it != modules_.end()) {
                module_aliases_[stmt.name] = full_path;
                return;
            }

            size_t last_dot = full_path.find_last_of('.');
            if (last_dot != std::string::npos) {
                std::string module_path = full_path.substr(0, last_dot);
                ensureModuleExists(module_path);
                auto parent_mod_it = modules_.find(module_path);
                if (parent_mod_it != modules_.end()) {
                    module_aliases_[stmt.name] = full_path;
                    return;
                }
            }

            module_aliases_[stmt.name] = full_path;
            return;
        }
    }

    // regular variable declaration
    llvm::Value* init_val = codegen(*stmt.initializer);
    
    llvm::Type* var_type;
    Type summit_type;
    
    if (stmt.type.has_value()) {
        summit_type = stmt.type.value();
        var_type = getLLVMType(summit_type);
        
        // special handling for u64 type
        if (summit_type.kind == Type::Kind::U64 && init_val->getType()->isIntegerTy(64)) {
            if (llvm::ConstantInt* const_int = llvm::dyn_cast<llvm::ConstantInt>(init_val)) {
                uint64_t unsigned_value = const_int->getZExtValue();
                init_val = llvm::ConstantInt::get(*context_, llvm::APInt(64, unsigned_value, false));
            }
        }
        
        // range checking
        if (isIntegerType(summit_type)) {
            if (auto* num_lit = dynamic_cast<const NumberLiteral*>(stmt.initializer.get())) {
                if (num_lit->isLargeInteger()) {
                    if (!isLargeValueInRange(num_lit->getLargeIntValue(), summit_type)) {
                        throw std::runtime_error("Value " + num_lit->getLargeIntValue() + 
                                               " is out of range for type " + 
                                               typeToString(summit_type));
                    }
                } else if (num_lit->isRegularInteger()) {
                    int64_t value = num_lit->getIntValue();
                    if (!isValueInRange(value, summit_type)) {
                        throw std::runtime_error("Value " + std::to_string(value) + 
                                               " is out of range for type " + 
                                               typeToString(summit_type));
                    }
                }
            }
        }
        
        if (init_val->getType() != var_type) {
            init_val = castValue(init_val, var_type, summit_type);
        }
    } else {
        // infer type from initializer
        if (auto* num_lit = dynamic_cast<const NumberLiteral*>(stmt.initializer.get())) {
            if (num_lit->isLargeInteger()) {
                summit_type = Type::u64();
                var_type = getLLVMType(summit_type);
                
                const std::string& largeInt = num_lit->getLargeIntValue();
                try {
                    uint64_t unsigned_value = std::stoull(largeInt);
                    init_val = llvm::ConstantInt::get(*context_, 
                        llvm::APInt(64, unsigned_value, false));
                } catch (const std::out_of_range&) {
                    throw std::runtime_error("Integer too large for u64: " + largeInt);
                }
            } else {
                var_type = init_val->getType();
                summit_type = getSummitTypeFromLLVMType(var_type);
            }
        } else {
            var_type = init_val->getType();
            summit_type = getSummitTypeFromLLVMType(var_type);
        }
    }
    
    variable_types_[stmt.name] = summit_type;
    
    llvm::AllocaInst* alloca = builder_->CreateAlloca(var_type, nullptr, stmt.name);
    builder_->CreateStore(init_val, alloca);
    named_values_[stmt.name] = alloca;
}

void CodeGenerator::codegenFunctionDecl(const FunctionDecl& stmt) {
    // figure out return type
    llvm::Type* return_type;
    Type return_summit_type;
    if (stmt.return_type.has_value() && stmt.return_type.value().kind != Type::Kind::INFERRED) {
        return_type = getLLVMType(stmt.return_type.value());
        return_summit_type = stmt.return_type.value();
    } else {
        return_type = getInt64Type();
        return_summit_type = Type::i64();
    }
    
    function_return_types_[stmt.name] = return_summit_type;
    
    // build parameter types
    std::vector<llvm::Type*> param_types;
    for (const auto& param_type : stmt.parameter_types) {
        if (param_type.kind == Type::Kind::INFERRED) {
            param_types.push_back(getInt64Type());
        } else {
            param_types.push_back(getLLVMType(param_type));
        }
    }
    
    llvm::FunctionType* func_type = llvm::FunctionType::get(
        return_type, param_types, false
    );
    
    llvm::Function* function = llvm::Function::Create(
        func_type,
        llvm::Function::ExternalLinkage,
        stmt.name,
        module_.get()
    );
    
    functions_[stmt.name] = function;
    function_return_summit_types_[function] = return_summit_type;

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context_, "entry", function);
    builder_->SetInsertPoint(entry);
    
    // save current state
    llvm::Function* prev_func = current_function_;
    current_function_ = function;
    
    auto prev_named_values = named_values_;
    auto prev_variable_types = variable_types_;

    // set up parameters
    size_t idx = 0;
    for (auto& arg : function->args()) {
        arg.setName(stmt.parameters[idx]);
        
        llvm::Type* param_llvm_type = param_types[idx];
        llvm::AllocaInst* alloca = builder_->CreateAlloca(
            param_llvm_type, nullptr, stmt.parameters[idx]
        );
        
        llvm::Value* stored_arg = &arg;
        
        if (return_summit_type.kind == Type::Kind::U64 && 
            stmt.parameter_types[idx].kind == Type::Kind::I64) {
            variable_types_[stmt.parameters[idx]] = Type::u64();
        } else if (stmt.parameter_types[idx].kind == Type::Kind::INFERRED) {
            variable_types_[stmt.parameters[idx]] = Type::i64();
        } else {
            variable_types_[stmt.parameters[idx]] = stmt.parameter_types[idx];
        }
        
        builder_->CreateStore(stored_arg, alloca);
        named_values_[stmt.parameters[idx]] = alloca;
        idx++;
    }

    // generate function body
    for (const auto& s : stmt.body) {
        codegen(*s);
    }

    // add default return if needed
    if (!builder_->GetInsertBlock()->getTerminator()) {
        if (return_type->isVoidTy()) {
            builder_->CreateRetVoid();
        } else {
            llvm::Value* default_val;
            if (return_type->isIntegerTy()) {
                if (stmt.return_type.has_value() && 
                    (stmt.return_type.value().kind == Type::Kind::U8 ||
                     stmt.return_type.value().kind == Type::Kind::U16 ||
                     stmt.return_type.value().kind == Type::Kind::U32 ||
                     stmt.return_type.value().kind == Type::Kind::U64)) {
                    default_val = llvm::ConstantInt::get(return_type, 0, false);
                } else {
                    default_val = llvm::ConstantInt::get(return_type, 0, true);
                }
            } else if (return_type->isFloatingPointTy()) {
                default_val = llvm::ConstantFP::get(return_type, 0.0);
            } else if (return_type->isPointerTy()) {
                default_val = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(return_type));
            } else {
                default_val = llvm::ConstantInt::get(getInt64Type(), 0);
            }
            builder_->CreateRet(default_val);
        }
    }
    
    // verify the function is valid
    std::string error;
    llvm::raw_string_ostream error_stream(error);
    if (llvm::verifyFunction(*function, &error_stream)) {
        function->eraseFromParent();
        throw std::runtime_error("Function verification failed: " + error);
    }
    
    // restore state
    named_values_ = prev_named_values;
    variable_types_ = prev_variable_types;
    current_function_ = prev_func;
}

void CodeGenerator::codegenReturn(const ReturnStmt& stmt) {
    llvm::Value* ret_val = codegen(*stmt.value);

    llvm::Function* current_func = builder_->GetInsertBlock()->getParent();
    llvm::Type* expected_return_type = current_func->getReturnType();

    if (ret_val->getType() != expected_return_type) {
        Type expected_summit_type = getSummitTypeFromLLVMType(expected_return_type);
        ret_val = castValue(ret_val, expected_return_type, expected_summit_type);
    }
    
    builder_->CreateRet(ret_val);
}

void CodeGenerator::codegenExprStmt(const ExpressionStmt& stmt) {
    if (auto* named_import = dynamic_cast<const NamedImportExpr*>(stmt.expression.get())) {
        codegenNamedImport(*named_import);
        return;
    }
    
    codegen(*stmt.expression);
}

// helper getters for common llvm types
llvm::Type* CodeGenerator::getInt64Type() {
    return llvm::Type::getInt64Ty(*context_);
}

llvm::Type* CodeGenerator::getDoubleType() {
    return llvm::Type::getDoubleTy(*context_);
}

llvm::Type* CodeGenerator::getVoidType() {
    return llvm::Type::getVoidTy(*context_);
}

llvm::Type* CodeGenerator::getInt8PtrType() {
    return llvm::PointerType::getUnqual(*context_);
}

llvm::Function* CodeGenerator::getFunction(const std::string& name) {
    auto it = functions_.find(name);
    if (it != functions_.end()) {
        return it->second;
    }
    return module_->getFunction(name);
}

}