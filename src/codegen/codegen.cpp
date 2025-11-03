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
#include "lexer/lexer.h"
#include "parser/parser.h"

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
    if (type.kind == Type::Kind::MAYBE) {
        if (!type.inner_type) {
            throw std::runtime_error("Maybe type must have an inner type");
        }
        
        llvm::Type* inner_llvm = getLLVMType(*type.inner_type);
        std::vector<llvm::Type*> fields = {
            llvm::Type::getInt1Ty(*context_),
            inner_llvm
        };
        
        return llvm::StructType::get(*context_, fields);
    }
    
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
    //handle struct types (maybe types for now)
    if (type->isStructTy()) {
        llvm::StructType* struct_ty = llvm::cast<llvm::StructType>(type);

        if (struct_ty->getNumElements() == 2) {
            llvm::Type* first_elem = struct_ty->getElementType(0);
            llvm::Type* second_elem = struct_ty->getElementType(1);
            
            if (first_elem->isIntegerTy(1)) {
                Type inner_type = getSummitTypeFromLLVMType(second_elem);
                return Type::maybe(inner_type);
            }
        }
        
        throw std::runtime_error("Unknown struct type in getSummitTypeFromLLVMType");
    }
    
    // original type handling
    if (type->isIntegerTy(1)) return Type::boolean();
    if (type->isIntegerTy(8)) return Type::i8();
    if (type->isIntegerTy(16)) return Type::i16();
    if (type->isIntegerTy(32)) return Type::i32();
    if (type->isIntegerTy(64)) return Type::i64();
    if (type->isFloatTy()) return Type::f32();
    if (type->isDoubleTy()) return Type::f64();
    if (type->isPointerTy()) return Type::string();
    if (type->isVoidTy()) return Type::void_type();
    
    return Type::inferred();
}

llvm::Value* CodeGenerator::castValue(llvm::Value* value, llvm::Type* target_type, const Type& target_summit_type) {
    llvm::Type* source_type = value->getType();
    
    if (source_type == target_type) {
        return value;
    }

    // handle casting to maybe type, wrap the value in a maybe struct
    if (target_summit_type.kind == Type::Kind::MAYBE && target_type->isStructTy()) {
        if (!target_summit_type.inner_type) {
            throw std::runtime_error("Maybe type must have an inner type");
        }

        llvm::Type* inner_llvm_type = getLLVMType(*target_summit_type.inner_type);

        llvm::Value* casted_value = value;
        if (source_type != inner_llvm_type) {
            casted_value = castValue(value, inner_llvm_type, *target_summit_type.inner_type);
        }

        llvm::Value* maybe_val = llvm::UndefValue::get(target_type);

        maybe_val = builder_->CreateInsertValue(
            maybe_val,
            llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), 1),
            {0},
            "maybe_has_value"
        );

        maybe_val = builder_->CreateInsertValue(
            maybe_val,
            casted_value,
            {1},
            "maybe_value"
        );
        
        return maybe_val;
    }

    // integer to integer casts
    if (target_type->isIntegerTy() && source_type->isIntegerTy()) {
        unsigned source_bits = source_type->getIntegerBitWidth();
        unsigned target_bits = target_type->getIntegerBitWidth();
        
        bool is_signed = isSignedType(target_summit_type);
        
        if (target_bits > source_bits) {
            return is_signed ? 
                builder_->CreateSExt(value, target_type) :
                builder_->CreateZExt(value, target_type);
        } else if (target_bits < source_bits) {
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
    //get a readable name for error messages
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
    //check if we already have this function
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
    // seed random number generator for chance statements
    seedRandomGenerator();
    
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
                                   bool no_stdlib,
                                   bool nowindow) {
    std::string obj_file = filename + ".o";
    emitObjectFile(obj_file);

    std::string output_filename = filename;
    #ifdef _WIN32
        if (output_filename.find(".exe") == std::string::npos) {
            output_filename += ".exe";
        }
    #endif
        
        std::string link_cmd;
        bool needsWrapper = false;
        
    #ifdef _WIN32
    // windows linking, prefer static libraries to embed stdlib
    std::string stdlibPath;
    bool useStdlib = !no_stdlib;

    if (useStdlib) {
        const char* envLib = std::getenv("SUMMIT_LIB");
        if (envLib) {
            std::filesystem::path libPath(envLib);
            if (std::filesystem::is_directory(libPath)) {
                // prioritize static libraries (.a, .lib) over dynamic (.dll)
                std::vector<std::string> winLibNames = {"libsummit_std.a", "libsummit_std.lib"};
                for (const auto& libName : winLibNames) {
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
            std::vector<std::string> searchPaths = {"./lib"};
            // prioritize static libraries
            std::vector<std::string> libNames = {"libsummit_std.a", "libsummit_std.lib"};
            
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
        }
        
        if (stdlibPath.empty()) {
            std::cerr << "Warning: Standard library not found.\n";
            std::cerr << "Set SUMMIT_LIB to point to the library directory.\n";
            std::cerr << "Searched in: ./lib/\n";
        } else {
            std::cout << "Using standard library: " << stdlibPath << std::endl;
        }
    }

    needsWrapper = !nowindow;

    link_cmd = "g++ -mconsole -static-libgcc -static-libstdc++ -o \"" + output_filename + "\" \"" + obj_file + "\"";

    if (useStdlib && !stdlibPath.empty()) {
        link_cmd += " \"" + stdlibPath + "\"";
    }

    // optimization flags to reduce size
    link_cmd += " -Wl,--gc-sections";
    link_cmd += " -Wl,--strip-all";
    link_cmd += " -Os";
    link_cmd += " -s";
    
    link_cmd += " -luser32 -lkernel32 -lgdi32 -ladvapi32";
    
#else
    // unix linking, prefer static libraries
    link_cmd = "g++ -no-pie -o " + output_filename + " " + obj_file;
    
    if (!no_stdlib) {
        std::string stdlibPath;
        
        const char* envLib = std::getenv("SUMMIT_LIB");
        if (envLib) {
            std::filesystem::path libPath(envLib);
            if (std::filesystem::is_directory(libPath)) {
                // prioritize static library (.a) over shared (.so, .dylib)
                std::vector<std::string> libNames = {"libsummit_std.a", "libsummit_std.so", "libsummit_std.dylib"};
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
            // prioritize static library, use filesystem::exists instead of ifstream
            std::vector<std::string> lib_paths = {
                "lib/libsummit_std.a",
                "lib/libsummit_std.so",
                "lib/libsummit_std.dylib"
            };
            
            for (const auto& path : lib_paths) {
                if (std::filesystem::exists(path)) {
                    stdlibPath = path;
                    break;
                }
            }
        }
        
        if (!stdlibPath.empty()) {
            std::cout << "Found standard library: " << stdlibPath << std::endl;
            if (stdlibPath.find(".a") != std::string::npos) {
                link_cmd += " \"" + stdlibPath + "\"";
            } else if (stdlibPath.find(".so") != std::string::npos || stdlibPath.find(".dylib") != std::string::npos) {
                size_t last_slash = stdlibPath.find_last_of("/");
                std::string lib_dir = (last_slash != std::string::npos) ? stdlibPath.substr(0, last_slash) : ".";

                std::filesystem::path abs_lib_dir = std::filesystem::absolute(lib_dir);
                std::string lib_dir_str = abs_lib_dir.string();
                
                link_cmd += " -L\"" + lib_dir_str + "\" -lsummit_std";
                link_cmd += " -Wl,-rpath,\"" + lib_dir_str + "\"";
            }
        } else {
            std::cerr << "Warning: Could not find libsummit_std" << std::endl;
            std::cerr << "Searched in: ./lib/" << std::endl;
            std::cerr << "Please compile the standard library for Linux or use --no-stdlib" << std::endl;
        }
        
        link_cmd += " -lstdc++ -lm -lpthread -ldl";
    }
    
    // optimization flags to reduce size
    link_cmd += " -Wl,--gc-sections";
    link_cmd += " -Wl,--strip-all";
    link_cmd += " -Os";
    link_cmd += " -s";
#endif

    // add any extra libs the user wants
    for (const auto& lib : libs) {
        // skip the standard library since we already handled it
        if (lib.find("libsummit_std") != std::string::npos) {
            continue;
        }
        
        if (lib.find(".a") != std::string::npos || lib.find(".so") != std::string::npos || 
            lib.find(".dylib") != std::string::npos || lib.find(".lib") != std::string::npos ||
            lib.find(".dll") != std::string::npos) {
            link_cmd += " \"" + lib + "\"";
        } else {
            link_cmd += " -l" + lib;
        }
    }
    
    if (!needsWrapper) {
        std::cout << "Linking command: " << link_cmd << std::endl;
        
        int result = std::system(link_cmd.c_str());
        
        if (result != 0) {
            throw std::runtime_error("Linking failed with exit code: " + std::to_string(result));
        }
        
        std::remove(obj_file.c_str());
    }
    
#ifdef _WIN32
    // if it needs a wrapper, create and compile it
    if (needsWrapper) {
        std::cout << "Creating console wrapper..." << std::endl;
        std::string wrapperSource = 
            "#include <windows.h>\n"
            "#include <cstdio>\n"
            "extern \"C\" int ProgramMain();\n"
            "int main() {\n"
            "    AllocConsole();\n"
            "    FILE* f;\n"
            "    freopen_s(&f,\"CONOUT$\",\"w\",stdout);\n"
            "    freopen_s(&f,\"CONOUT$\",\"w\",stderr);\n"
            "    freopen_s(&f,\"CONIN$\",\"r\",stdin);\n"
            "    int r=ProgramMain();\n"
            "    printf(\"\\nPress Enter...\");\n"
            "    getchar();\n"
            "    return r;\n"
            "}\n";
        
        std::string wrapperCppFile = output_filename + "_w.cpp";
        std::ofstream wrapperFile(wrapperCppFile);
        if (!wrapperFile.is_open()) {
            throw std::runtime_error("Failed to create wrapper source file");
        }
        wrapperFile << wrapperSource;
        wrapperFile.close();
        
        std::string renamedObjFile = obj_file + ".r";
        std::string objcopyCmd = "llvm-objcopy --redefine-sym main=ProgramMain \"" + obj_file + "\" \"" + renamedObjFile + "\"";
        std::cout << "Renaming main symbol: " << objcopyCmd << std::endl;
        
        int renameResult = std::system(objcopyCmd.c_str());
        
        std::string objFileToUse;
        if (renameResult == 0 && std::filesystem::exists(renamedObjFile)) {
            objFileToUse = renamedObjFile;
            std::cout << "Successfully renamed main to ProgramMain" << std::endl;
        } else {
            std::cerr << "Warning: Symbol renaming failed" << std::endl;
            objFileToUse = obj_file;
        }
        
        std::string wrapperCmd = "g++ -mwindows -Os -s -ffunction-sections -fdata-sections -Wl,--gc-sections -o \"" + 
                                output_filename + "\" \"" + wrapperCppFile + "\" \"" + objFileToUse + "\"";
        
        if (useStdlib && !stdlibPath.empty()) {
            wrapperCmd += " \"" + stdlibPath + "\"";
        }
        
        wrapperCmd += " -luser32 -lkernel32";
        
        std::cout << "Compiling wrapper: " << wrapperCmd << std::endl;
        int wrapperResult = std::system(wrapperCmd.c_str());
        
        std::remove(wrapperCppFile.c_str());
        std::remove(obj_file.c_str());
        if (std::filesystem::exists(renamedObjFile)) {
            std::remove(renamedObjFile.c_str());
        }
        
        if (wrapperResult != 0) {
            throw std::runtime_error("Wrapper compilation failed");
        }
        
        std::cout << "Successfully created windowed executable: " << output_filename << std::endl;
    } else {
        std::cout << "Successfully created executable: " << output_filename << std::endl;
    }
#else
    std::string chmod_cmd = "chmod +x " + output_filename;
    std::system(chmod_cmd.c_str());
    std::cout << "Successfully created executable: " << output_filename << std::endl;
#endif
    
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
    } else if (auto* e = dynamic_cast<const NilLiteral*>(&expr)) {
        return codegenNilLiteral(*e);
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
    } else if (auto* e = dynamic_cast<const ChanceExpr*>(&expr)) {
        return codegenChanceExpr(*e);
    } else if (auto* e = dynamic_cast<const MaybeExpr*>(&expr)) {
        return codegenMaybeExpr(*e);
    }
    throw std::runtime_error("Unknown expression type");
}

llvm::Value* CodeGenerator::codegenChanceExpr(const ChanceExpr& expr) {
    llvm::Function* function = builder_->GetInsertBlock()->getParent();

    // get or declare rand() function
    llvm::FunctionType* rand_type = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*context_), 
        false
    );
    llvm::Function* rand_func = module_->getFunction("rand");
    if (!rand_func) {
        rand_func = llvm::Function::Create(
            rand_type,
            llvm::Function::ExternalLinkage,
            "rand",
            module_.get()
        );
    }
    
    // generate random integer between 0-99
    llvm::Value* rand_val = builder_->CreateCall(rand_func);
    llvm::Value* hundred = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 100);
    llvm::Value* random_int = builder_->CreateSRem(rand_val, hundred, "rand_mod");
    
    // save the current block
    llvm::BasicBlock* entry_block = builder_->GetInsertBlock();
    
    // create blocks
    std::vector<llvm::BasicBlock*> branch_blocks;
    llvm::BasicBlock* else_block = nullptr;
    llvm::BasicBlock* merge_block = llvm::BasicBlock::Create(*context_, "chance_merge", function);
    
    for (size_t i = 0; i < expr.branches.size(); i++) {
        branch_blocks.push_back(
            llvm::BasicBlock::Create(*context_, "chance_expr_branch_" + std::to_string(i), function)
        );
    }
    
    if (expr.else_result) {
        else_block = llvm::BasicBlock::Create(*context_, "chance_expr_else", function);
    }
    
    // evaluate first branch to determine type
    builder_->SetInsertPoint(branch_blocks[0]);
    llvm::Value* branch_0_result = codegen(*expr.branches[0].result);
    llvm::Type* result_type = branch_0_result->getType();
    llvm::BasicBlock* branch_0_end = builder_->GetInsertBlock();
    
    // go back to entry block and create the alloca BEFORE any branching
    builder_->SetInsertPoint(entry_block);
    llvm::AllocaInst* result_var = builder_->CreateAlloca(result_type, nullptr, "chance_result");
    
    // build cascading checks
    int cumulative = static_cast<int>(expr.branches[0].percentage);
    llvm::Value* threshold = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), cumulative);
    llvm::Value* cond = builder_->CreateICmpSLT(random_int, threshold, "chance_cmp_0");
    
    llvm::BasicBlock* next_check;
    if (expr.branches.size() > 1) {
        next_check = llvm::BasicBlock::Create(*context_, "chance_expr_check_1", function);
    } else if (else_block) {
        next_check = else_block;
    } else {
        next_check = merge_block;
    }
    
    builder_->CreateCondBr(cond, branch_blocks[0], next_check);
    
    // complete first branch
    builder_->SetInsertPoint(branch_0_end);
    builder_->CreateStore(branch_0_result, result_var);
    builder_->CreateBr(merge_block);
    
    // handle remaining branches
    for (size_t i = 1; i < expr.branches.size(); i++) {
        builder_->SetInsertPoint(next_check);
        
        cumulative += static_cast<int>(expr.branches[i].percentage);
        threshold = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), cumulative);
        cond = builder_->CreateICmpSLT(random_int, threshold, "chance_cmp_" + std::to_string(i));
        
        if (i + 1 < expr.branches.size()) {
            next_check = llvm::BasicBlock::Create(*context_, "chance_expr_check_" + std::to_string(i + 1), function);
        } else if (else_block) {
            next_check = else_block;
        } else {
            next_check = merge_block;
        }
        
        builder_->CreateCondBr(cond, branch_blocks[i], next_check);
        
        builder_->SetInsertPoint(branch_blocks[i]);
        llvm::Value* branch_result = codegen(*expr.branches[i].result);
        if (branch_result->getType() != result_type) {
            Type target_summit_type = getSummitTypeFromLLVMType(result_type);
            branch_result = castValue(branch_result, result_type, target_summit_type);
        }
        builder_->CreateStore(branch_result, result_var);
        builder_->CreateBr(merge_block);
    }
    
    // handle else block
    if (else_block) {
        builder_->SetInsertPoint(else_block);
        llvm::Value* else_val = codegen(*expr.else_result);
        if (else_val->getType() != result_type) {
            Type target_summit_type = getSummitTypeFromLLVMType(result_type);
            else_val = castValue(else_val, result_type, target_summit_type);
        }
        builder_->CreateStore(else_val, result_var);
        builder_->CreateBr(merge_block);
    }
    
    builder_->SetInsertPoint(merge_block);
    return builder_->CreateLoad(result_type, result_var, "chance_value");
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
        
        // For for loops and general numeric operations, use i64 by default
        // This prevents overflow in cases like Fibonacci sequences
        return llvm::ConstantInt::get(*context_, llvm::APInt(64, value, true));
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

llvm::Value* CodeGenerator::codegenNilLiteral(const NilLiteral& expr) {
    return llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(*context_));
}

llvm::Value* CodeGenerator::codegenMemberAccess(const MemberAccess& expr) {
    // check if the object is a file import
    if (auto* import_expr = dynamic_cast<const ImportExpr*>(expr.object.get())) {
        if (import_expr->is_file_import) {
            // this is accessing a member of an imported file
            std::string module_name = std::filesystem::path(import_expr->module).stem().string();
            
            // look up the function with mangled name
            std::string mangled_name = module_name + "_" + expr.member;
            llvm::Function* func = module_->getFunction(mangled_name);
            
            if (!func) {
                throw std::runtime_error("Function '" + expr.member + 
                                       "' not found in imported file '" + 
                                       import_expr->module + "'");
            }
            
            // return the function pointer
            return func;
        }
    }
    
    if (auto* id = dynamic_cast<const Identifier*>(expr.object.get())) {
        auto file_alias_it = file_import_aliases_.find(id->name);
        if (file_alias_it != file_import_aliases_.end()) {
            std::string module_name = file_alias_it->second;
            std::string mangled_name = module_name + "_" + expr.member;
            llvm::Function* func = module_->getFunction(mangled_name);
            
            if (!func) {
                throw std::runtime_error("Function '" + expr.member + 
                                       "' not found in imported file module '" + 
                                       module_name + "'");
            }
            
            return func;
        }
    }
    
    return llvm::ConstantInt::get(*context_, llvm::APInt(64, 0, true));
}

llvm::Value* CodeGenerator::codegenMaybeExpr(const MaybeExpr& expr) {
    llvm::Function* function = builder_->GetInsertBlock()->getParent();
    
    // evaluate the expression, this should give us an identifier that we can look up
    if (auto* id = dynamic_cast<const Identifier*>(expr.value.get())) {
        llvm::Value* maybe_alloca = named_values_[id->name];
        if (!maybe_alloca) {
            throw std::runtime_error("Variable not found: " + id->name);
        }
        
        auto* alloca_inst = llvm::dyn_cast<llvm::AllocaInst>(maybe_alloca);
        if (!alloca_inst) {
            throw std::runtime_error("Maybe expression requires a variable");
        }
        
        llvm::Type* maybe_type = alloca_inst->getAllocatedType();
        
        if (!maybe_type->isStructTy()) {
            throw std::runtime_error("Maybe expression requires a maybe type value");
        }
 
        llvm::Value* has_value_ptr = builder_->CreateStructGEP(
            maybe_type,
            maybe_alloca,
            0,
            "has_value_ptr"
        );
        llvm::Value* has_value = builder_->CreateLoad(
            llvm::Type::getInt1Ty(*context_),
            has_value_ptr,
            "has_value"
        );
        
        // create blocks
        llvm::BasicBlock* then_block = llvm::BasicBlock::Create(*context_, "maybe_then", function);
        llvm::BasicBlock* else_block = llvm::BasicBlock::Create(*context_, "maybe_else", function);
        llvm::BasicBlock* merge_block = llvm::BasicBlock::Create(*context_, "maybe_merge", function);
        
        builder_->CreateCondBr(has_value, then_block, else_block);
        
        // generate then branch
        builder_->SetInsertPoint(then_block);
        for (const auto& stmt : expr.then_branch) {
            codegen(*stmt);
        }
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
        
        // generate else branch
        builder_->SetInsertPoint(else_block);
        for (const auto& stmt : expr.else_branch) {
            codegen(*stmt);
        }
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
        
        builder_->SetInsertPoint(merge_block);
        
        // return the loaded maybe value
        return builder_->CreateLoad(maybe_type, maybe_alloca, "maybe_val");
    } else if (auto* func_call = dynamic_cast<const FunctionCall*>(expr.value.get())) {
        // handle function call that returns a maybe type
        llvm::Value* maybe_val = codegen(*func_call);
        
        if (!maybe_val->getType()->isStructTy()) {
            throw std::runtime_error("Maybe expression requires a maybe type value");
        }
        
        // store the value in an alloca so we can use GEP on it
        llvm::AllocaInst* temp_alloca = builder_->CreateAlloca(maybe_val->getType(), nullptr, "maybe_temp");
        builder_->CreateStore(maybe_val, temp_alloca);
        
        // extract the has_value flag using GEP on the alloca
        llvm::Value* has_value_ptr = builder_->CreateStructGEP(
            maybe_val->getType(),
            temp_alloca,
            0,
            "has_value_ptr"
        );
        llvm::Value* has_value = builder_->CreateLoad(
            llvm::Type::getInt1Ty(*context_),
            has_value_ptr,
            "has_value"
        );
        
        // create blocks
        llvm::BasicBlock* then_block = llvm::BasicBlock::Create(*context_, "maybe_then", function);
        llvm::BasicBlock* else_block = llvm::BasicBlock::Create(*context_, "maybe_else", function);
        llvm::BasicBlock* merge_block = llvm::BasicBlock::Create(*context_, "maybe_merge", function);
        
        builder_->CreateCondBr(has_value, then_block, else_block);
        
        // generate then branch
        builder_->SetInsertPoint(then_block);
        for (const auto& stmt : expr.then_branch) {
            codegen(*stmt);
        }
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
        
        // generate else branch
        builder_->SetInsertPoint(else_block);
        for (const auto& stmt : expr.else_branch) {
            codegen(*stmt);
        }
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
        
        builder_->SetInsertPoint(merge_block);
        
        return maybe_val;
    } else {
        throw std::runtime_error("Maybe expression requires an identifier or function call");
    }
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

void CodeGenerator::codegenForStmt(const ForStmt& stmt) {
    llvm::Function* function = builder_->GetInsertBlock()->getParent();
    
    // save th current variable scope
    auto saved_named_values = named_values_;
    auto saved_variable_types = variable_types_;
    
    // create the basic blocks
    llvm::BasicBlock* init_block = llvm::BasicBlock::Create(*context_, "for_init", function);
    llvm::BasicBlock* cond_block = llvm::BasicBlock::Create(*context_, "for_cond", function);
    llvm::BasicBlock* body_block = llvm::BasicBlock::Create(*context_, "for_body", function);
    llvm::BasicBlock* update_block = llvm::BasicBlock::Create(*context_, "for_update", function);
    llvm::BasicBlock* end_block = llvm::BasicBlock::Create(*context_, "for_end", function);
    
    builder_->CreateBr(init_block);
    
    builder_->SetInsertPoint(init_block);
    
    std::vector<std::string> loop_var_names;

    if (stmt.init_exprs.size() == 1) {
        if (auto* assign = dynamic_cast<const AssignmentExpr*>(stmt.init_exprs[0].get())) {
            // variable declaration
            std::string var_name = assign->name;
            llvm::Value* init_val = codegen(*assign->value);
            
            // use i64 for loop variables
            Type summit_type = Type::i64();
            llvm::Type* var_type = getLLVMType(summit_type);
            
            if (init_val->getType() != var_type) {
                init_val = castValue(init_val, var_type, summit_type);
            }
            
            llvm::AllocaInst* alloca = builder_->CreateAlloca(var_type, nullptr, var_name);
            builder_->CreateStore(init_val, alloca);
            named_values_[var_name] = alloca;
            variable_types_[var_name] = summit_type;
            loop_var_names.push_back(var_name);
        } else {
            // any other single expression
            codegen(*stmt.init_exprs[0]);
        }
    } else {
        for (const auto& init_expr : stmt.init_exprs) {
            if (auto* assign = dynamic_cast<const AssignmentExpr*>(init_expr.get())) {
                std::string var_name = assign->name;
                llvm::Value* init_val = codegen(*assign->value);
                
                // force the i64 for for loop variables
                Type summit_type = Type::i64();
                llvm::Type* var_type = getLLVMType(summit_type);
                
                if (init_val->getType() != var_type) {
                    init_val = castValue(init_val, var_type, summit_type);
                }
                
                // check if variable already exists
                auto existing_var = named_values_.find(var_name);
                if (existing_var != named_values_.end()) {
                    llvm::Value* var = existing_var->second;
                    auto* alloca = llvm::dyn_cast<llvm::AllocaInst>(var);
                    if (!alloca) {
                        throw std::runtime_error("Variable " + var_name + " is not an alloca");
                    }
                    
                    llvm::Type* current_type = alloca->getAllocatedType();
                    if (current_type != var_type) {
                        llvm::AllocaInst* new_alloca = builder_->CreateAlloca(var_type, nullptr, var_name);
                        llvm::Value* current_val = builder_->CreateLoad(current_type, var);
                        llvm::Value* extended_val = builder_->CreateSExt(current_val, var_type);
                        builder_->CreateStore(extended_val, new_alloca);
                        named_values_[var_name] = new_alloca;
                        variable_types_[var_name] = summit_type;
                    }
                    
                    builder_->CreateStore(init_val, var);
                } else {
                    llvm::AllocaInst* alloca = builder_->CreateAlloca(var_type, nullptr, var_name);
                    builder_->CreateStore(init_val, alloca);
                    named_values_[var_name] = alloca;
                    variable_types_[var_name] = summit_type;
                }
                loop_var_names.push_back(var_name);
            } else {
                codegen(*init_expr);
            }
        }
    }
    
    builder_->CreateBr(cond_block);
    
    // generate condition block
    builder_->SetInsertPoint(cond_block);
    llvm::Value* cond_value = codegen(*stmt.condition);
    
    // convert condition to boolean if needed
    if (!cond_value->getType()->isIntegerTy(1)) {
        if (cond_value->getType()->isIntegerTy()) {
            cond_value = builder_->CreateICmpNE(
                cond_value,
                llvm::ConstantInt::get(cond_value->getType(), 0),
                "for_cond"
            );
        } else if (cond_value->getType()->isFloatingPointTy()) {
            cond_value = builder_->CreateFCmpONE(
                cond_value,
                llvm::ConstantFP::get(cond_value->getType(), 0.0),
                "for_cond"
            );
        }
    }
    
    builder_->CreateCondBr(cond_value, body_block, end_block);

    builder_->SetInsertPoint(body_block);
    for (const auto& s : stmt.body) {
        codegen(*s);
    }

    if (!builder_->GetInsertBlock()->getTerminator()) {
        builder_->CreateBr(update_block);
    }
    
    // generate update block, execute all update expressions
    builder_->SetInsertPoint(update_block);
    
    bool using_simplified_step = false;
    
    if (stmt.update_exprs.size() == loop_var_names.size() && !loop_var_names.empty()) {
        using_simplified_step = true;
        for (const auto& update_expr : stmt.update_exprs) {
            if (!dynamic_cast<const NumberLiteral*>(update_expr.get())) {
                using_simplified_step = false;
                break;
            }
        }
    }
    
    if (using_simplified_step) {
        // handle simplified step syntax
        for (size_t i = 0; i < stmt.update_exprs.size() && i < loop_var_names.size(); i++) {
            const std::string& loop_var = loop_var_names[i];
            auto& update_expr = stmt.update_exprs[i];
            
            llvm::Value* var = named_values_[loop_var];
            if (var) {
                // load current value
                llvm::Value* current_val = builder_->CreateLoad(getLLVMType(Type::i64()), var);
                // create step value
                llvm::Value* step_val = codegen(*update_expr);
                
                // check if step is negative and use subtraction instead of addition
                if (auto* num_lit = dynamic_cast<const NumberLiteral*>(update_expr.get())) {
                    if (num_lit->isRegularInteger() && num_lit->getIntValue() < 0) {
                        llvm::Value* new_val = builder_->CreateAdd(current_val, step_val);
                        builder_->CreateStore(new_val, var);
                    } else if (num_lit->isFloat() && num_lit->getFloatValue() < 0) {
                        llvm::Value* new_val = builder_->CreateFAdd(current_val, step_val);
                        builder_->CreateStore(new_val, var);
                    } else {
                        // For positive steps, use addition as before
                        llvm::Value* new_val = builder_->CreateAdd(current_val, step_val);
                        builder_->CreateStore(new_val, var);
                    }
                } else {
                    llvm::Value* new_val = builder_->CreateAdd(current_val, step_val);
                    builder_->CreateStore(new_val, var);
                }
            }
        }
    } else {
        if (stmt.update_exprs.size() == 1) {
            auto& update_expr = stmt.update_exprs[0];
            
            if (auto* num_lit = dynamic_cast<const NumberLiteral*>(update_expr.get())) {
                if (stmt.init_exprs.size() == 1) {
                    if (auto* assign = dynamic_cast<const AssignmentExpr*>(stmt.init_exprs[0].get())) {
                        std::string loop_var = assign->name;
                        llvm::Value* var = named_values_[loop_var];
                        if (var) {
                            // load current value
                            llvm::Value* current_val = builder_->CreateLoad(getLLVMType(Type::i64()), var);
                            // create step value
                            llvm::Value* step_val = codegen(*update_expr);
                            
                            // handle negative steps for single variable case too
                            if (num_lit->isRegularInteger() && num_lit->getIntValue() < 0) {
                                llvm::Value* abs_step_val = builder_->CreateNeg(step_val, "neg_step");
                                llvm::Value* new_val = builder_->CreateSub(current_val, abs_step_val);
                                builder_->CreateStore(new_val, var);
                            } else if (num_lit->isFloat() && num_lit->getFloatValue() < 0) {
                                llvm::Value* abs_step_val = builder_->CreateFNeg(step_val, "fneg_step");
                                llvm::Value* new_val = builder_->CreateFSub(current_val, abs_step_val);
                                builder_->CreateStore(new_val, var);
                            } else {
                                llvm::Value* new_val = builder_->CreateAdd(current_val, step_val);
                                builder_->CreateStore(new_val, var);
                            }
                        }
                    }
                }
            } else if (auto* binop = dynamic_cast<const BinaryOp*>(update_expr.get())) {
                codegen(*update_expr);
            } else if (auto* assign = dynamic_cast<const AssignmentExpr*>(update_expr.get())) {
                codegen(*update_expr);
            } else {
                codegen(*update_expr);
            }
        } else {
            for (const auto& update_expr : stmt.update_exprs) {
                if (auto* assign = dynamic_cast<const AssignmentExpr*>(update_expr.get())) {
                    std::string var_name = assign->name;
                    llvm::Value* update_val = codegen(*assign->value);
                    
                    // i64 for for loop variables
                    Type summit_type = Type::i64();
                    llvm::Type* var_type = getLLVMType(summit_type);
                    
                    if (update_val->getType() != var_type) {
                        update_val = castValue(update_val, var_type, summit_type);
                    }
                    
                    auto existing_var = named_values_.find(var_name);
                    if (existing_var != named_values_.end()) {
                        llvm::Value* var = existing_var->second;
                        auto* alloca = llvm::dyn_cast<llvm::AllocaInst>(var);
                        if (!alloca) {
                            throw std::runtime_error("Variable " + var_name + " is not an alloca");
                        }
                        
                        llvm::Type* current_type = alloca->getAllocatedType();
                        if (current_type != var_type) {
                            llvm::AllocaInst* new_alloca = builder_->CreateAlloca(var_type, nullptr, var_name);
                            llvm::Value* current_val = builder_->CreateLoad(current_type, var);
                            llvm::Value* extended_val = builder_->CreateSExt(current_val, var_type);
                            builder_->CreateStore(extended_val, new_alloca);
                            named_values_[var_name] = new_alloca;
                            variable_types_[var_name] = summit_type;
                        }
                        
                        builder_->CreateStore(update_val, var);
                    } else {
                        llvm::AllocaInst* alloca = builder_->CreateAlloca(var_type, nullptr, var_name);
                        builder_->CreateStore(update_val, alloca);
                        named_values_[var_name] = alloca;
                        variable_types_[var_name] = summit_type;
                    }
                } else if (auto* var_decl = dynamic_cast<const VarDecl*>(update_expr.get())) {
                    codegen(*var_decl);
                } else {
                    codegen(*update_expr);
                }
            }
        }
    }
    
    builder_->CreateBr(cond_block);
    
    // continue from end block
    builder_->SetInsertPoint(end_block);
    
    // restore variable scope
    named_values_ = saved_named_values;
    variable_types_ = saved_variable_types;
}

llvm::Value* CodeGenerator::codegenCast(const CastExpr& expr) {
    llvm::Value* value = codegen(*expr.expression);
    llvm::Type* target_type = getLLVMType(expr.target_type);
    
    if (expr.target_type.kind == Type::Kind::STRING && !value->getType()->isPointerTy()) {
        return convertToString(value, expr.expression.get());
    }
    
    return castValue(value, target_type, expr.target_type);
}

// binary operation codegen
llvm::Value* CodeGenerator::codegenBinaryOp(const BinaryOp& expr) {
    llvm::Value* left = codegen(*expr.left);
    llvm::Value* right = codegen(*expr.right);
    
    // handle string concat
    if ((expr.op == "+" || expr.op == ",")) {
        bool left_is_string = left->getType()->isPointerTy();
        bool right_is_string = right->getType()->isPointerTy();
        
        if (left_is_string || right_is_string) {
            if (!left_is_string) {
                left = convertToString(left, expr.left.get());
            }
            if (!right_is_string) {
                right = convertToString(right, expr.right.get());
            }

            bool add_space = (expr.op == ",");
            return concatenateStrings(left, right, add_space);
        }

        if (expr.op == ",") {
            throw std::runtime_error(
                "Comma operator can only be used for string concatenation"
            );
        }
    }
    
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
    
    // handle comparison operators
    if (expr.op == "<" || expr.op == "<=" || expr.op == ">" || expr.op == ">=" || 
        expr.op == "==" || expr.op == "!=") {
        
        // special handling for string equality/inequality
        if ((expr.op == "==" || expr.op == "!=") && left->getType()->isPointerTy() && right->getType()->isPointerTy()) {
            llvm::FunctionType* strcmp_type = llvm::FunctionType::get(
                llvm::Type::getInt32Ty(*context_),
                {llvm::PointerType::getUnqual(*context_), llvm::PointerType::getUnqual(*context_)},
                false
            );
            llvm::Function* strcmp_func = module_->getFunction("strcmp");
            if (!strcmp_func) {
                strcmp_func = llvm::Function::Create(
                    strcmp_type,
                    llvm::Function::ExternalLinkage,
                    "strcmp",
                    module_.get()
                );
            }

            llvm::Value* cmp_result = builder_->CreateCall(strcmp_func, {left, right});
            
            // strcmp returns 0 if strings are equal
            if (expr.op == "==") {
                return builder_->CreateICmpEQ(cmp_result, llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0), "streq");
            } else { // !=
                return builder_->CreateICmpNE(cmp_result, llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0), "strne");
            }
        }
        
        if (left->getType()->isFloatingPointTy()) {
            // floating point comparisons
            if (expr.op == "<") {
                return builder_->CreateFCmpOLT(left, right, "cmptmp");
            } else if (expr.op == "<=") {
                return builder_->CreateFCmpOLE(left, right, "cmptmp");
            } else if (expr.op == ">") {
                return builder_->CreateFCmpOGT(left, right, "cmptmp");
            } else if (expr.op == ">=") {
                return builder_->CreateFCmpOGE(left, right, "cmptmp");
            } else if (expr.op == "==") {
                return builder_->CreateFCmpOEQ(left, right, "cmptmp");
            } else if (expr.op == "!=") {
                return builder_->CreateFCmpONE(left, right, "cmptmp");
            }
        } else {
            // integer comparisons
            if (expr.op == "<") {
                return use_unsigned ? 
                    builder_->CreateICmpULT(left, right, "cmptmp") :
                    builder_->CreateICmpSLT(left, right, "cmptmp");
            } else if (expr.op == "<=") {
                return use_unsigned ? 
                    builder_->CreateICmpULE(left, right, "cmptmp") :
                    builder_->CreateICmpSLE(left, right, "cmptmp");
            } else if (expr.op == ">") {
                return use_unsigned ? 
                    builder_->CreateICmpUGT(left, right, "cmptmp") :
                    builder_->CreateICmpSGT(left, right, "cmptmp");
            } else if (expr.op == ">=") {
                return use_unsigned ? 
                    builder_->CreateICmpUGE(left, right, "cmptmp") :
                    builder_->CreateICmpSGE(left, right, "cmptmp");
            } else if (expr.op == "==") {
                return builder_->CreateICmpEQ(left, right, "cmptmp");
            } else if (expr.op == "!=") {
                return builder_->CreateICmpNE(left, right, "cmptmp");
            }
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

void CodeGenerator::codegenChanceStmt(const ChanceStmt& stmt) {
    llvm::Function* function = builder_->GetInsertBlock()->getParent();
    
    // seed the random number generator on first use
    if (!random_seeded_) {
        // get or declare time() function
        llvm::FunctionType* time_type = llvm::FunctionType::get(
            llvm::Type::getInt64Ty(*context_),
            {llvm::PointerType::getUnqual(*context_)},
            false
        );
        llvm::Function* time_func = module_->getFunction("time");
        if (!time_func) {
            time_func = llvm::Function::Create(
                time_type,
                llvm::Function::ExternalLinkage,
                "time",
                module_.get()
            );
        }
        
        // get or declare srand() function
        llvm::FunctionType* srand_type = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*context_),
            {llvm::Type::getInt32Ty(*context_)},
            false
        );
        llvm::Function* srand_func = module_->getFunction("srand");
        if (!srand_func) {
            srand_func = llvm::Function::Create(
                srand_type,
                llvm::Function::ExternalLinkage,
                "srand",
                module_.get()
            );
        }
        
        // call time(NULL) to get current time
        llvm::Value* null_ptr = llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(*context_));
        llvm::Value* time_val = builder_->CreateCall(time_func, {null_ptr});
        
        // truncate to i32 for srand
        llvm::Value* seed = builder_->CreateTrunc(time_val, llvm::Type::getInt32Ty(*context_));
        
        builder_->CreateCall(srand_func, {seed});
        
        random_seeded_ = true;
    }
    
    // get or declare rand() function from C stdlib
    llvm::FunctionType* rand_type = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*context_), 
        false
    );
    llvm::Function* rand_func = module_->getFunction("rand");
    if (!rand_func) {
        rand_func = llvm::Function::Create(
            rand_type,
            llvm::Function::ExternalLinkage,
            "rand",
            module_.get()
        );
    }
    
    // call rand() and convert to 0-100 range
    llvm::Value* rand_val = builder_->CreateCall(rand_func);
    llvm::Value* rand_f64 = builder_->CreateSIToFP(rand_val, llvm::Type::getDoubleTy(*context_));
    
    // RAND_MAX is typically 2147483647
    llvm::Value* rand_max = llvm::ConstantFP::get(llvm::Type::getDoubleTy(*context_), 2147483647.0);
    llvm::Value* normalized = builder_->CreateFDiv(rand_f64, rand_max);
    llvm::Value* percentage = builder_->CreateFMul(
        normalized, 
        llvm::ConstantFP::get(llvm::Type::getDoubleTy(*context_), 100.0)
    );
    
    // create the blocks for each branch
    std::vector<llvm::BasicBlock*> branch_blocks;
    llvm::BasicBlock* else_block = nullptr;
    llvm::BasicBlock* merge_block = llvm::BasicBlock::Create(*context_, "chance_end", function);
    
    for (size_t i = 0; i < stmt.branches.size(); i++) {
        branch_blocks.push_back(
            llvm::BasicBlock::Create(*context_, "chance_branch_" + std::to_string(i), function)
        );
    }
    
    if (stmt.else_result) {
        else_block = llvm::BasicBlock::Create(*context_, "chance_else", function);
    }
    
    // build the cascading if else chain
    double cumulative = 0.0;
    
    for (size_t i = 0; i < stmt.branches.size(); i++) {
        cumulative += stmt.branches[i].percentage;
        
        llvm::Value* threshold = llvm::ConstantFP::get(
            llvm::Type::getDoubleTy(*context_), 
            cumulative
        );
        llvm::Value* cond = builder_->CreateFCmpOLT(percentage, threshold, "chance_cmp");
        
        llvm::BasicBlock* next_check;
        if (i + 1 < stmt.branches.size()) {
            next_check = llvm::BasicBlock::Create(*context_, "chance_check_" + std::to_string(i + 1), function);
        } else if (else_block) {
            next_check = else_block;
        } else {
            next_check = merge_block;
        }
        
        builder_->CreateCondBr(cond, branch_blocks[i], next_check);
        
        // fill in the branch block
        builder_->SetInsertPoint(branch_blocks[i]);
        codegen(*stmt.branches[i].result);
        builder_->CreateBr(merge_block);
        
        // move to next check block
        if (i + 1 < stmt.branches.size()) {
            builder_->SetInsertPoint(next_check);
        }
    }
    
    // handle else block if present
    if (else_block) {
        builder_->SetInsertPoint(else_block);
        codegen(*stmt.else_result);
        builder_->CreateBr(merge_block);
    }
    
    builder_->SetInsertPoint(merge_block);
}

void CodeGenerator::codegenIfStmt(const IfStmt& stmt) {
    llvm::Function* function = builder_->GetInsertBlock()->getParent();
    
    // create basic blocks for the if statement
    llvm::BasicBlock* then_block = llvm::BasicBlock::Create(*context_, "then", function);
    llvm::BasicBlock* else_block = nullptr;
    llvm::BasicBlock* merge_block = llvm::BasicBlock::Create(*context_, "ifcont", function);
    
    // generate the main condition
    llvm::Value* cond_value = codegen(*stmt.condition);
    
    // convert condition to boolean if needed
    if (!cond_value->getType()->isIntegerTy(1)) {
        if (cond_value->getType()->isIntegerTy()) {
            cond_value = builder_->CreateICmpNE(
                cond_value, 
                llvm::ConstantInt::get(cond_value->getType(), 0),
                "ifcond"
            );
        } else if (cond_value->getType()->isFloatingPointTy()) {
            cond_value = builder_->CreateFCmpONE(
                cond_value,
                llvm::ConstantFP::get(cond_value->getType(), 0.0),
                "ifcond"
            );
        } else {
            throw std::runtime_error("If condition must be a boolean, integer, or floating-point value");
        }
    }
    
    // create initial branch
    if (!stmt.elif_branches.empty() || !stmt.else_branch.empty()) {
        else_block = llvm::BasicBlock::Create(*context_, "else", function);
        builder_->CreateCondBr(cond_value, then_block, else_block);
    } else {
        builder_->CreateCondBr(cond_value, then_block, merge_block);
    }
    
    // generate then block
    builder_->SetInsertPoint(then_block);
    for (const auto& s : stmt.then_branch) {
        codegen(*s);
    }
    // Only add branch if block doesn't already have a terminator
    if (!builder_->GetInsertBlock()->getTerminator()) {
        builder_->CreateBr(merge_block);
    }
    
    // generate elif branches if they exist
    llvm::BasicBlock* current_else_block = else_block;
    for (size_t i = 0; i < stmt.elif_branches.size(); i++) {
        const auto& elif_stmt = stmt.elif_branches[i];
        
        llvm::BasicBlock* elif_then_block = llvm::BasicBlock::Create(*context_, "elifthen", function);
        llvm::BasicBlock* next_elif_block = nullptr;
        
        bool is_last_elif = (i == stmt.elif_branches.size() - 1) && stmt.else_branch.empty();
        if (!is_last_elif) {
            next_elif_block = llvm::BasicBlock::Create(*context_, "elifelse", function);
        }
        
        // generate elif condition in the current else block
        builder_->SetInsertPoint(current_else_block);
        llvm::Value* elif_cond_value = codegen(*elif_stmt->condition);
        if (!elif_cond_value->getType()->isIntegerTy(1)) {
            if (elif_cond_value->getType()->isIntegerTy()) {
                elif_cond_value = builder_->CreateICmpNE(
                    elif_cond_value, 
                    llvm::ConstantInt::get(elif_cond_value->getType(), 0),
                    "elifcond"
                );
            } else if (elif_cond_value->getType()->isFloatingPointTy()) {
                elif_cond_value = builder_->CreateFCmpONE(
                    elif_cond_value,
                    llvm::ConstantFP::get(elif_cond_value->getType(), 0.0),
                    "elifcond"
                );
            }
        }
        
        if (next_elif_block) {
            builder_->CreateCondBr(elif_cond_value, elif_then_block, next_elif_block);
        } else {
            builder_->CreateCondBr(elif_cond_value, elif_then_block, merge_block);
        }
        
        // generate elif then block
        builder_->SetInsertPoint(elif_then_block);
        for (const auto& s : elif_stmt->then_branch) {
            codegen(*s);
        }

        // only add branch if block doesn't already have a terminator
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
        
        current_else_block = next_elif_block;
    }
    
    // generate else block if it exists
    if (!stmt.else_branch.empty()) {
        builder_->SetInsertPoint(current_else_block ? current_else_block : else_block);
        for (const auto& s : stmt.else_branch) {
            codegen(*s);
        }
        // only add branch if block doesn't already have a terminator
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
    } else if (current_else_block) {
        // if there's an else block but no else branch, just branch to merge
        builder_->SetInsertPoint(current_else_block);
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
    } else if (else_block) {
        builder_->SetInsertPoint(else_block);
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
    }
    
    // continue from merge block
    builder_->SetInsertPoint(merge_block);
}

llvm::Value* CodeGenerator::codegenFunctionCall(const FunctionCall& expr) {
    llvm::Function* callee = nullptr;
    std::string func_name;

    // check if this is a call to an inline import
    if (auto* member = dynamic_cast<const MemberAccess*>(expr.callee.get())) {
        if (auto* import_expr = dynamic_cast<const ImportExpr*>(member->object.get())) {
            if (import_expr->is_file_import) {
                // compile the file if not already done
                compileImportedFile(import_expr->module);
                
                // get the mangled function name
                std::string module_name = std::filesystem::path(import_expr->module).stem().string();
                std::string mangled_name = module_name + "_" + member->member;
                
                callee = module_->getFunction(mangled_name);
                
                if (!callee) {
                    throw std::runtime_error("Function '" + member->member + 
                                           "' not found in imported file '" + 
                                           import_expr->module + "'");
                }
                
                func_name = mangled_name;
            }
        }
    }

    // if not found through inline import, try standard resolution
    if (!callee) {
        if (auto* member = dynamic_cast<const MemberAccess*>(expr.callee.get())) {
            if (auto* id = dynamic_cast<const Identifier*>(member->object.get())) {
                auto file_alias_it = file_import_aliases_.find(id->name);
                if (file_alias_it != file_import_aliases_.end()) {
                    // this is a call to a function from an imported file
                    std::string mangled_name = file_alias_it->second + "_" + member->member;
                    callee = module_->getFunction(mangled_name);
                    
                    if (callee) {
                        func_name = mangled_name;
                    }
                }
            }
        }
    }
    
    if (!callee) {
        std::string full_path = extractFunctionPath(expr.callee.get());
        
        if (!full_path.empty()) {
            func_name = full_path;
            callee = getOrDeclareStdlibFunction(full_path);

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
    }

    if (!callee) {
        throw std::runtime_error("Unknown function: " + func_name);
    }
    
    std::string actual_func_name = callee->getName().str();

    bool is_print_func = (actual_func_name == "summit_io_println" || 
                         actual_func_name == "summit_io_print" ||
                         actual_func_name.find("std.io.println") != std::string::npos || 
                         actual_func_name.find("std.io.print") != std::string::npos);
    
    if (is_print_func && !expr.arguments.empty()) {
        if (auto* id = dynamic_cast<const Identifier*>(expr.arguments[0].get())) {
            auto type_it = variable_types_.find(id->name);
            if (type_it != variable_types_.end() && type_it->second.kind == Type::Kind::MAYBE) {
                llvm::Value* maybe_alloca = named_values_[id->name];
                llvm::Type* maybe_type = llvm::cast<llvm::AllocaInst>(maybe_alloca)->getAllocatedType();
                return extractMaybeValueForPrint(maybe_alloca, maybe_type, type_it->second);
            }
        }
    }
    
    std::vector<llvm::Value*> args;
    
    for (size_t i = 0; i < expr.arguments.size(); i++) {
        llvm::Value* arg_val = codegen(*expr.arguments[i]);
        args.push_back(arg_val);
    }

    Type return_summit_type = Type::i64();
    auto return_type_it = function_return_types_.find(func_name);
    if (return_type_it != function_return_types_.end()) {
        return_summit_type = return_type_it->second;
    } else {
        return_summit_type = getSummitTypeFromLLVMType(callee->getReturnType());
    }
    
    // handle print function overloading
    if (!args.empty() && is_print_func) {
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
        } else if (auto* binop = dynamic_cast<const BinaryOp*>(expr.arguments[0].get())) {
            // Check if this is string concatenation (result will be a string)
            if ((binop->op == "+" || binop->op == ",") && arg_type->isPointerTy()) {
                summit_type = Type::string();
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

// simple random seed generation
void CodeGenerator::generateRandomSeed() {
    llvm::Function* srand_func = module_->getFunction("srand");
    llvm::Function* time_func = module_->getFunction("time");
    
    if (!srand_func || !time_func) return;
    
    // call time(NULL) to get current time
    llvm::Value* null_ptr = llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(*context_));
    llvm::Value* time_val = builder_->CreateCall(time_func, {null_ptr});
    
    // truncate to i32 for srand
    llvm::Value* seed = builder_->CreateTrunc(time_val, llvm::Type::getInt32Ty(*context_));
    
    // call srand with the seed
    builder_->CreateCall(srand_func, {seed});
}

// import statement codegen
llvm::Value* CodeGenerator::codegenImport(const ImportExpr& expr) {
    if (expr.is_file_import) {
        // compile the imported file
        compileImportedFile(expr.module);
        
        // get the module name
        std::string module_name = std::filesystem::path(expr.module).stem().string();
        file_import_aliases_[module_name] = module_name;
        
        // create a global constant that represents this module
        std::string module_marker_name = "__module_marker_" + module_name;
        llvm::GlobalVariable* module_marker = module_->getGlobalVariable(module_marker_name);
        
        if (!module_marker) {
            module_marker = new llvm::GlobalVariable(
                *module_,
                llvm::Type::getInt8Ty(*context_),
                true,  // is constant
                llvm::GlobalValue::PrivateLinkage,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(*context_), 0),
                module_marker_name
            );
        }
        
        return module_marker;
    } else {
        // handle standard library imports
        ensureModuleExists(expr.module);
        return llvm::ConstantInt::get(*context_, llvm::APInt(64, 0, true));
    }
}

llvm::Value* CodeGenerator::codegenNamedImport(const NamedImportExpr& expr) {
    if (expr.is_file_import) {
        // compile the imported file
        compileImportedFile(expr.module);
        
        // get the module name
        std::string module_name = std::filesystem::path(expr.module).stem().string();
        
        // import specific functions
        for (const auto& import_name : expr.imports) {
            std::string mangled_name = module_name + "_" + import_name;
            llvm::Function* func = module_->getFunction(mangled_name);
            
            if (func) {
                function_references_[import_name] = func;
                functions_[import_name] = func;
            } else {
                std::cerr << "Warning: Function '" << import_name 
                         << "' not found in file '" << expr.module << "'" << std::endl;
            }
        }
        
        return llvm::ConstantInt::get(*context_, llvm::APInt(64, 0, true));
    } else {
        // handle standard library imports
        ensureModuleExists(expr.module);
        
        for (const auto& import_name : expr.imports) {
            std::string full_path = expr.module + "." + import_name;
            llvm::Function* func = getOrDeclareStdlibFunction(full_path);
            
            if (func) {
                function_references_[import_name] = func;
            } else {
                std::cerr << "Warning: Function '" << import_name 
                         << "' not found in module '" << expr.module << "'" << std::endl;
            }
        }
        
        return llvm::ConstantInt::get(*context_, llvm::APInt(64, 0, true));
    }
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
    
    // handle assigning nil to maybe types
    if (auto* nil_lit = dynamic_cast<const NilLiteral*>(expr.value.get())) {
        if (current_summit_type.kind == Type::Kind::MAYBE) {
            // create a nil maybe value
            llvm::Value* maybe_val = llvm::UndefValue::get(current_type);
            
            maybe_val = builder_->CreateInsertValue(
                maybe_val,
                llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), 0),
                {0},
                "assign_nil_has_value"
            );
            
            llvm::Type* inner_llvm_type = getLLVMType(*current_summit_type.inner_type);
            llvm::Value* zero_val;
            if (inner_llvm_type->isIntegerTy()) {
                zero_val = llvm::ConstantInt::get(inner_llvm_type, 0);
            } else if (inner_llvm_type->isFloatingPointTy()) {
                zero_val = llvm::ConstantFP::get(inner_llvm_type, 0.0);
            } else if (inner_llvm_type->isPointerTy()) {
                zero_val = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(inner_llvm_type));
            } else {
                zero_val = llvm::UndefValue::get(inner_llvm_type);
            }
            
            maybe_val = builder_->CreateInsertValue(
                maybe_val,
                zero_val,
                {1},
                "assign_nil_value"
            );
            
            builder_->CreateStore(maybe_val, var);
            return maybe_val;
        } else {
            throw std::runtime_error("Cannot assign nil to non-maybe type '" + 
                                   typeToString(current_summit_type) + "'");
        }
    }
    
    llvm::Value* value = codegen(*expr.value);
    Type new_summit_type = getSummitTypeFromLLVMType(value->getType());
    
    // type checking for maybe types
    if (current_summit_type.kind == Type::Kind::MAYBE) {
        if (!current_summit_type.inner_type) {
            throw std::runtime_error("Maybe type must have an inner type");
        }
        
        if (new_summit_type.kind != Type::Kind::MAYBE) {
            Type inner_type = *current_summit_type.inner_type;
            
            // allow assignment if types are compatible
            bool types_compatible = (new_summit_type == inner_type);
            
            // also allow numeric type compatibility
            if (!types_compatible && isIntegerType(inner_type) && isIntegerType(new_summit_type)) {
                types_compatible = true;
            }
            if (!types_compatible && (inner_type.kind == Type::Kind::F32 || inner_type.kind == Type::Kind::F64) &&
                (new_summit_type.kind == Type::Kind::F32 || new_summit_type.kind == Type::Kind::F64)) {
                types_compatible = true;
            }
            
            if (!types_compatible) {
                throw std::runtime_error("Cannot assign type '" + typeToString(new_summit_type) + 
                                       "' to maybe<" + typeToString(inner_type) + ">");
            }
            
            // cast the value to the inner type if needed
            llvm::Type* inner_llvm_type = getLLVMType(inner_type);
            if (value->getType() != inner_llvm_type) {
                value = castValue(value, inner_llvm_type, inner_type);
            }
            
            // wrap in maybe
            llvm::Value* maybe_val = llvm::UndefValue::get(current_type);
            
            maybe_val = builder_->CreateInsertValue(
                maybe_val,
                llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), 1),
                {0},
                "assign_maybe_has_value"
            );
            
            maybe_val = builder_->CreateInsertValue(
                maybe_val,
                value,
                {1},
                "assign_maybe_value"
            );
            
            builder_->CreateStore(maybe_val, var);
            return maybe_val;
        } else {
            // assigning maybe to maybe, must be same inner type
            if (!new_summit_type.inner_type || 
                *new_summit_type.inner_type != *current_summit_type.inner_type) {
                throw std::runtime_error("Cannot assign maybe<" + 
                    typeToString(new_summit_type.inner_type ? *new_summit_type.inner_type : Type::void_type()) + 
                    "> to maybe<" + typeToString(*current_summit_type.inner_type) + ">");
            }
            
            builder_->CreateStore(value, var);
            return value;
        }
    }
    
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
    } else if (auto* s = dynamic_cast<const IfStmt*>(&stmt)) {
        codegenIfStmt(*s);
    } else if (auto* s = dynamic_cast<const ChanceStmt*>(&stmt)) {
        codegenChanceStmt(*s);
    } else if (auto* s = dynamic_cast<const DoStmt*>(&stmt)) {
        codegenDoStmt(*s);
    } else if (auto* s = dynamic_cast<const ForStmt*>(&stmt)) {
        codegenForStmt(*s);
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
        if (import_expr->is_file_import) {
            // compile the imported file
            compileImportedFile(import_expr->module);
            
            // get the module name
            std::string module_name = std::filesystem::path(import_expr->module).stem().string();
            
            // import all functions from this file into the global scope
            for (auto& func : *module_) {
                std::string func_name = func.getName().str();
                
                std::string prefix = module_name + "_";
                if (func_name.find(prefix) == 0) {
                    std::string original_name = func_name.substr(prefix.length());
                    function_references_[original_name] = &func;
                }
            }
            
            return;
        }
        
        // standard library module import
        std::string module_path = import_expr->module;
        ensureModuleExists(module_path);
        importAllFunctionsFromModule(module_path);
        
    } else if (auto* named_import = dynamic_cast<const NamedImportExpr*>(stmt.import_expr.get())) {
        if (named_import->is_file_import) {
            // compile the imported file
            compileImportedFile(named_import->module);
            
            std::string module_name = std::filesystem::path(named_import->module).stem().string();
            
            // Import only the specified functions
            for (const auto& import_name : named_import->imports) {
                std::string mangled_name = module_name + "_" + import_name;
                llvm::Function* func = module_->getFunction(mangled_name);
                
                if (func) {
                    function_references_[import_name] = func;
                } else {
                    std::cerr << "Warning: Function '" << import_name 
                             << "' not found in file '" << named_import->module << "'" << std::endl;
                }
            }
            
            return;
        }
        
        // standard library imports
        ensureModuleExists(named_import->module);
        
        for (const auto& import_name : named_import->imports) {
            std::string full_path = named_import->module + "." + import_name;
            llvm::Function* func = getOrDeclareStdlibFunction(full_path);
            
            if (func) {
                function_references_[import_name] = func;
            } else {
                std::cerr << "Warning: Function '" << import_name 
                         << "' not found in module '" << named_import->module << "'" << std::endl;
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
        if (import_expr->is_file_import) {
            // Compile the imported file
            compileImportedFile(import_expr->module);
            
            // Get the module name
            std::string module_name = std::filesystem::path(import_expr->module).stem().string();
            
            // Create alias
            file_import_aliases_[stmt.name] = module_name;
            return;
        } else {
            ensureModuleExists(import_expr->module);
            module_aliases_[stmt.name] = import_expr->module;
            return;
        }
    }

    if (auto* named_import = dynamic_cast<const NamedImportExpr*>(stmt.initializer.get())) {
        ensureModuleExists(named_import->module);
        for (const auto& import_name : named_import->imports) {
            std::string full_path = named_import->module + "." + import_name;
            llvm::Function* func = getOrDeclareStdlibFunction(full_path);
            if (func) {
                function_references_[import_name] = func;
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

    // handle maybe types with null initialization
    if (stmt.type.has_value() && stmt.type.value().kind == Type::Kind::MAYBE) {
        Type maybe_type = stmt.type.value();
        llvm::Type* llvm_maybe_type = getLLVMType(maybe_type);
        
        llvm::AllocaInst* alloca = builder_->CreateAlloca(llvm_maybe_type, nullptr, stmt.name);

        if (dynamic_cast<const NilLiteral*>(stmt.initializer.get())) {
            llvm::Value* maybe_val = llvm::UndefValue::get(llvm_maybe_type);

            maybe_val = builder_->CreateInsertValue(
                maybe_val,
                llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), 0),
                {0},
                "null_has_value"
            );

            llvm::Type* inner_llvm_type = getLLVMType(*maybe_type.inner_type);
            llvm::Value* zero_val;
            if (inner_llvm_type->isIntegerTy()) {
                zero_val = llvm::ConstantInt::get(inner_llvm_type, 0);
            } else if (inner_llvm_type->isFloatingPointTy()) {
                zero_val = llvm::ConstantFP::get(inner_llvm_type, 0.0);
            } else if (inner_llvm_type->isPointerTy()) {
                zero_val = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(inner_llvm_type));
            } else {
                zero_val = llvm::UndefValue::get(inner_llvm_type);
            }
            
            maybe_val = builder_->CreateInsertValue(
                maybe_val,
                zero_val,
                {1},
                "null_value"
            );
            
            builder_->CreateStore(maybe_val, alloca);
        } else if (auto* maybe_expr = dynamic_cast<const MaybeExpr*>(stmt.initializer.get())) {
            llvm::Value* result = codegenMaybeExpr(*maybe_expr);
            builder_->CreateStore(result, alloca);
        } else {
            llvm::Value* init_val = codegen(*stmt.initializer);
            
            if (init_val->getType() == llvm_maybe_type) {
                builder_->CreateStore(init_val, alloca);
            } else {
                llvm::Value* maybe_val = llvm::UndefValue::get(llvm_maybe_type);

                maybe_val = builder_->CreateInsertValue(
                    maybe_val,
                    llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), 1),
                    {0},
                    "has_value"
                );

                llvm::Type* inner_llvm_type = getLLVMType(*maybe_type.inner_type);
                if (init_val->getType() != inner_llvm_type) {
                    init_val = castValue(init_val, inner_llvm_type, *maybe_type.inner_type);
                }
                
                maybe_val = builder_->CreateInsertValue(
                    maybe_val,
                    init_val,
                    {1},
                    "value"
                );
                
                builder_->CreateStore(maybe_val, alloca);
            }
        }
        
        named_values_[stmt.name] = alloca;
        variable_types_[stmt.name] = maybe_type;
        return;
    }

    llvm::Value* init_val = codegen(*stmt.initializer);
    
    llvm::Type* var_type;
    Type summit_type;
    
    if (stmt.type.has_value()) {
        summit_type = stmt.type.value();
        var_type = getLLVMType(summit_type);
        
        if (summit_type.kind == Type::Kind::U64 && init_val->getType()->isIntegerTy(64)) {
            if (llvm::ConstantInt* const_int = llvm::dyn_cast<llvm::ConstantInt>(init_val)) {
                uint64_t unsigned_value = const_int->getZExtValue();
                init_val = llvm::ConstantInt::get(*context_, 
                    llvm::APInt(64, unsigned_value, false));
            }
        }
        
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
                // FORCE i64 for integer variables to prevent overflow
                summit_type = Type::i64();
                var_type = getLLVMType(summit_type);
                
                // cast the initial value to i64
                if (init_val->getType() != var_type) {
                    init_val = castValue(init_val, var_type, summit_type);
                }
            }
        } else {
            // for non number literals, use the inferred type but promote integers to i64
            var_type = init_val->getType();
            summit_type = getSummitTypeFromLLVMType(var_type);
            
            // if it's a small integer type, promote to i64
            if (summit_type.kind == Type::Kind::I8 || 
                summit_type.kind == Type::Kind::I16 || 
                summit_type.kind == Type::Kind::I32 ||
                summit_type.kind == Type::Kind::U8 ||
                summit_type.kind == Type::Kind::U16 ||
                summit_type.kind == Type::Kind::U32) {
                summit_type = Type::i64();
                var_type = getLLVMType(summit_type);
                init_val = castValue(init_val, var_type, summit_type);
            }
        }
    }
    
    variable_types_[stmt.name] = summit_type;
    
    llvm::AllocaInst* alloca = builder_->CreateAlloca(var_type, nullptr, stmt.name);
    builder_->CreateStore(init_val, alloca);
    named_values_[stmt.name] = alloca;
}

bool CodeGenerator::isMaybeValue(llvm::Value* val, const Expression* expr) {
    if (auto* id = dynamic_cast<const Identifier*>(expr)) {
        auto type_it = variable_types_.find(id->name);
        if (type_it != variable_types_.end() && type_it->second.kind == Type::Kind::MAYBE) {
            return true;
        }
    }
    return false;
}

llvm::Value* CodeGenerator::extractMaybeValueForPrint(llvm::Value* maybe_alloca, 
                                                       llvm::Type* maybe_type,
                                                       const Type& summit_type) {
    if (summit_type.kind != Type::Kind::MAYBE || !summit_type.inner_type) {
        throw std::runtime_error("Expected maybe type");
    }
    
    llvm::Function* function = builder_->GetInsertBlock()->getParent();
    
    // extract has_value using GEP on the alloca
    llvm::Value* has_value_ptr = builder_->CreateStructGEP(maybe_type, maybe_alloca, 0, "has_value_ptr");
    llvm::Value* has_value = builder_->CreateLoad(llvm::Type::getInt1Ty(*context_), has_value_ptr, "has_value");
    
    // create blocks
    llvm::BasicBlock* has_value_block = llvm::BasicBlock::Create(*context_, "has_value", function);
    llvm::BasicBlock* null_block = llvm::BasicBlock::Create(*context_, "is_null", function);
    llvm::BasicBlock* merge_block = llvm::BasicBlock::Create(*context_, "print_merge", function);
    
    builder_->CreateCondBr(has_value, has_value_block, null_block);
    builder_->SetInsertPoint(has_value_block);

    Type inner_type = *summit_type.inner_type;
    llvm::Type* inner_llvm_type = getLLVMType(inner_type);
    llvm::Value* value_ptr = builder_->CreateStructGEP(maybe_type, maybe_alloca, 1, "value_ptr");
    llvm::Value* actual_value = builder_->CreateLoad(inner_llvm_type, value_ptr, "actual_value");

    std::string print_func_name;
    
    switch (inner_type.kind) {
        case Type::Kind::STRING:
            print_func_name = "std.io.println";
            break;
        case Type::Kind::I8:
        case Type::Kind::I16:
        case Type::Kind::I32:
        case Type::Kind::I64:
            print_func_name = "std.io.println_int";
            break;
        case Type::Kind::U8:
        case Type::Kind::U16:
        case Type::Kind::U32:
        case Type::Kind::U64:
            print_func_name = "std.io.println_uint";
            break;
        case Type::Kind::F32:
            print_func_name = "std.io.println_f32";
            break;
        case Type::Kind::F64:
            print_func_name = "std.io.println_f64";
            break;
        case Type::Kind::BOOL:
            print_func_name = "std.io.println_bool";
            break;
        default:
            print_func_name = "std.io.println_int";
            break;
    }
    
    llvm::Function* print_func = getOrDeclareStdlibFunction(print_func_name);
    if (!print_func) {
        throw std::runtime_error("Could not find print function: " + print_func_name);
    }

    llvm::Type* expected_type = print_func->getFunctionType()->getParamType(0);

    if (actual_value->getType() != expected_type) {
        if (inner_type.kind == Type::Kind::BOOL && actual_value->getType()->isIntegerTy(1)) {
            
        } else {
            try {
                actual_value = castValue(actual_value, expected_type, inner_type);
            } catch (const std::runtime_error& e) {
                throw std::runtime_error(
                    "Failed to cast maybe<" + typeToString(inner_type) + 
                    "> value for printing: " + std::string(e.what())
                );
            }
        }
    }
    
    builder_->CreateCall(print_func, {actual_value});
    builder_->CreateBr(merge_block);
    
    // nil block, print "nil"
    builder_->SetInsertPoint(null_block);
    llvm::Function* null_print_func = getOrDeclareStdlibFunction("std.io.println");
    if (null_print_func) {
        llvm::Value* null_str = builder_->CreateGlobalStringPtr("nil");
        builder_->CreateCall(null_print_func, {null_str});
    }
    builder_->CreateBr(merge_block);
    builder_->SetInsertPoint(merge_block);
    
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0);
}

void CodeGenerator::codegenDoStmt(const DoStmt& stmt) {
    llvm::Function* function = builder_->GetInsertBlock()->getParent();
    
    if (dynamic_cast<const NilLiteral*>(stmt.value.get())) {
        auto saved_named_values = named_values_;
        auto saved_variable_types = variable_types_;

        llvm::BasicBlock* scope_block = llvm::BasicBlock::Create(*context_, "scope_block", function);
        llvm::BasicBlock* after_scope = llvm::BasicBlock::Create(*context_, "after_scope", function);
        
        builder_->CreateBr(scope_block);
        builder_->SetInsertPoint(scope_block);
        
        // generate the body statements
        for (const auto& s : stmt.then_branch) {
            codegen(*s);
        }
        
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(after_scope);
        }
        
        // restore variable scope (variable defined in block go out of scope)
        named_values_ = saved_named_values;
        variable_types_ = saved_variable_types;
        
        builder_->SetInsertPoint(after_scope);
        return;
    }

    if (auto* id = dynamic_cast<const Identifier*>(stmt.value.get())) {
        llvm::Value* maybe_alloca = named_values_[id->name];
        if (!maybe_alloca) {
            throw std::runtime_error("Variable not found: " + id->name);
        }
        
        auto* alloca_inst = llvm::dyn_cast<llvm::AllocaInst>(maybe_alloca);
        if (!alloca_inst) {
            throw std::runtime_error("Do statement requires a variable");
        }
        
        llvm::Type* maybe_type = alloca_inst->getAllocatedType();
        
        if (!maybe_type->isStructTy()) {
            throw std::runtime_error("Do statement requires a maybe type value");
        }
        
        llvm::Value* has_value_ptr = builder_->CreateStructGEP(
            maybe_type,
            maybe_alloca,
            0,
            "has_value_ptr"
        );
        llvm::Value* has_value = builder_->CreateLoad(
            llvm::Type::getInt1Ty(*context_),
            has_value_ptr,
            "has_value"
        );
        
        // save variable scope for maybe branches
        auto saved_then_named_values = named_values_;
        auto saved_then_variable_types = variable_types_;
        
        llvm::BasicBlock* then_block = llvm::BasicBlock::Create(*context_, "do_then", function);
        llvm::BasicBlock* else_block = llvm::BasicBlock::Create(*context_, "do_else", function);
        llvm::BasicBlock* merge_block = llvm::BasicBlock::Create(*context_, "do_merge", function);
        
        builder_->CreateCondBr(has_value, then_block, else_block);
        
        // generate then branch with scoped variables
        builder_->SetInsertPoint(then_block);
        for (const auto& s : stmt.then_branch) {
            codegen(*s);
        }
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
        
        // restore scope and generate else branch
        named_values_ = saved_then_named_values;
        variable_types_ = saved_then_variable_types;
        
        builder_->SetInsertPoint(else_block);
        for (const auto& s : stmt.else_branch) {
            codegen(*s);
        }
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
        
        // restore original scope after both branches
        named_values_ = saved_then_named_values;
        variable_types_ = saved_then_variable_types;
        
        builder_->SetInsertPoint(merge_block);
        
    } else if (auto* func_call = dynamic_cast<const FunctionCall*>(stmt.value.get())) {
        // handle function call that returns a maybe type
        llvm::Value* maybe_val = codegen(*func_call);
        
        if (!maybe_val->getType()->isStructTy()) {
            throw std::runtime_error("Do statement requires a maybe type value");
        }
        
        // store the value in an alloca so can use GEP on it
        llvm::AllocaInst* temp_alloca = builder_->CreateAlloca(maybe_val->getType(), nullptr, "do_temp");
        builder_->CreateStore(maybe_val, temp_alloca);
        
        // extract the has_value flag using GEP on the alloca
        llvm::Value* has_value_ptr = builder_->CreateStructGEP(
            maybe_val->getType(),
            temp_alloca,
            0,
            "has_value_ptr"
        );
        llvm::Value* has_value = builder_->CreateLoad(
            llvm::Type::getInt1Ty(*context_),
            has_value_ptr,
            "has_value"
        );
        
        // save variable scope
        auto saved_then_named_values = named_values_;
        auto saved_then_variable_types = variable_types_;
        
        // create blocks
        llvm::BasicBlock* then_block = llvm::BasicBlock::Create(*context_, "do_then", function);
        llvm::BasicBlock* else_block = llvm::BasicBlock::Create(*context_, "do_else", function);
        llvm::BasicBlock* merge_block = llvm::BasicBlock::Create(*context_, "do_merge", function);
        
        builder_->CreateCondBr(has_value, then_block, else_block);
        
        // generate then branch
        builder_->SetInsertPoint(then_block);
        for (const auto& s : stmt.then_branch) {
            codegen(*s);
        }
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
        
        // restore scope and generate else branch
        named_values_ = saved_then_named_values;
        variable_types_ = saved_then_variable_types;
        
        builder_->SetInsertPoint(else_block);
        for (const auto& s : stmt.else_branch) {
            codegen(*s);
        }
        if (!builder_->GetInsertBlock()->getTerminator()) {
            builder_->CreateBr(merge_block);
        }
        
        // restore original scope
        named_values_ = saved_then_named_values;
        variable_types_ = saved_then_variable_types;
        
        builder_->SetInsertPoint(merge_block);
        
    } else {
        throw std::runtime_error("Do statement requires an identifier, function call, or scoped block");
    }
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

    if (stmt.name == "main") {
        // seed random number generator once at program start
        llvm::FunctionType* time_type = llvm::FunctionType::get(
            llvm::Type::getInt64Ty(*context_),
            {llvm::PointerType::getUnqual(*context_)},
            false
        );
        llvm::Function* time_func = module_->getFunction("time");
        if (!time_func) {
            time_func = llvm::Function::Create(
                time_type,
                llvm::Function::ExternalLinkage,
                "time",
                module_.get()
            );
        }
        
        llvm::FunctionType* srand_type = llvm::FunctionType::get(
            llvm::Type::getVoidTy(*context_),
            {llvm::Type::getInt32Ty(*context_)},
            false
        );
        llvm::Function* srand_func = module_->getFunction("srand");
        if (!srand_func) {
            srand_func = llvm::Function::Create(
                srand_type,
                llvm::Function::ExternalLinkage,
                "srand",
                module_.get()
            );
        }
        
        // call time(NULL) and srand() at the start of main
        llvm::Value* null_ptr = llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(*context_));
        llvm::Value* time_val = builder_->CreateCall(time_func, {null_ptr});
        llvm::Value* seed = builder_->CreateTrunc(time_val, llvm::Type::getInt32Ty(*context_));
        builder_->CreateCall(srand_func, {seed});
    }
    
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
        } else if (return_type->isStructTy() && return_summit_type.kind == Type::Kind::MAYBE) {
            // default return for maybe types is null
            llvm::Value* maybe_val = llvm::UndefValue::get(return_type);
            
            // set has_value to false
            maybe_val = builder_->CreateInsertValue(
                maybe_val,
                llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), 0),
                {0},
                "default_null_maybe"
            );
            
            // set value to zero
            llvm::Type* inner_type = return_type->getStructElementType(1);
            llvm::Value* zero_val;
            if (inner_type->isIntegerTy()) {
                zero_val = llvm::ConstantInt::get(inner_type, 0);
            } else if (inner_type->isFloatingPointTy()) {
                zero_val = llvm::ConstantFP::get(inner_type, 0.0);
            } else if (inner_type->isPointerTy()) {
                zero_val = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(inner_type));
            } else {
                zero_val = llvm::UndefValue::get(inner_type);
            }
            
            maybe_val = builder_->CreateInsertValue(
                maybe_val,
                zero_val,
                {1},
                "default_null_maybe_with_val"
            );
            
            builder_->CreateRet(maybe_val);
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
    llvm::Function* current_func = builder_->GetInsertBlock()->getParent();
    llvm::Type* expected_return_type = current_func->getReturnType();
    
    // check if we're returning null and the return type is maybe
    if (auto* null_lit = dynamic_cast<const NilLiteral*>(stmt.value.get())) {
        auto func_it = function_return_summit_types_.find(current_func);
        if (func_it != function_return_summit_types_.end()) {
            Type return_summit_type = func_it->second;
            
            if (return_summit_type.kind == Type::Kind::MAYBE) {
                llvm::Value* maybe_val = llvm::UndefValue::get(expected_return_type);

                maybe_val = builder_->CreateInsertValue(
                    maybe_val,
                    llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), 0),
                    {0},
                    "null_maybe"
                );
                
                llvm::Type* inner_type = expected_return_type->getStructElementType(1);
                llvm::Value* zero_val;
                if (inner_type->isIntegerTy()) {
                    zero_val = llvm::ConstantInt::get(inner_type, 0);
                } else if (inner_type->isFloatingPointTy()) {
                    zero_val = llvm::ConstantFP::get(inner_type, 0.0);
                } else if (inner_type->isPointerTy()) {
                    zero_val = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(inner_type));
                } else {
                    zero_val = llvm::UndefValue::get(inner_type);
                }
                
                maybe_val = builder_->CreateInsertValue(
                    maybe_val,
                    zero_val,
                    {1},
                    "null_maybe_with_val"
                );
                
                builder_->CreateRet(maybe_val);
                return;
            }
        }
    }
    
    llvm::Value* ret_val = codegen(*stmt.value);
    
    auto func_it = function_return_summit_types_.find(current_func);
    if (func_it != function_return_summit_types_.end()) {
        Type return_summit_type = func_it->second;
        
        if (return_summit_type.kind == Type::Kind::MAYBE) {
            llvm::Value* maybe_val = llvm::UndefValue::get(expected_return_type);
            
            maybe_val = builder_->CreateInsertValue(
                maybe_val,
                llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), 1),
                {0},
                "maybe_has_value"
            );

            llvm::Type* inner_type = expected_return_type->getStructElementType(1);
            if (ret_val->getType() != inner_type) {
                ret_val = castValue(ret_val, inner_type, *return_summit_type.inner_type);
            }

            maybe_val = builder_->CreateInsertValue(
                maybe_val,
                ret_val,
                {1},
                "maybe_with_value"
            );
            
            builder_->CreateRet(maybe_val);
            return;
        }
    }

    // normal return handling
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

void CodeGenerator::seedRandomGenerator() {
    // declare srand(unsigned int seed)
    llvm::FunctionType* srand_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*context_),
        {llvm::Type::getInt32Ty(*context_)},
        false
    );
    llvm::Function::Create(
        srand_type,
        llvm::Function::ExternalLinkage,
        "srand",
        module_.get()
    );
    
    // declare time(time_t* t) 
    llvm::FunctionType* time_type = llvm::FunctionType::get(
        llvm::Type::getInt64Ty(*context_),
        {llvm::PointerType::getUnqual(*context_)},
        false
    );
    llvm::Function::Create(
        time_type,
        llvm::Function::ExternalLinkage,
        "time",
        module_.get()
    );
}

std::string CodeGenerator::resolveImportPath(const std::string& import_path) {
    // if it's an absolute path, use it directly
    if (std::filesystem::path(import_path).is_absolute()) {
        if (std::filesystem::exists(import_path)) {
            return std::filesystem::canonical(import_path).string();
        }
        throw std::runtime_error("Import file not found: " + import_path);
    }
    
    // try relative to current source file
    if (!current_source_file_.empty()) {
        std::filesystem::path source_dir = std::filesystem::path(current_source_file_).parent_path();
        std::filesystem::path relative_path = source_dir / import_path;
        
        if (std::filesystem::exists(relative_path)) {
            return std::filesystem::canonical(relative_path).string();
        }
    }
    
    // try relative to current directory
    if (std::filesystem::exists(import_path)) {
        return std::filesystem::canonical(import_path).string();
    }
    
    throw std::runtime_error("Import file not found: " + import_path);
}

void CodeGenerator::compileImportedFile(const std::string& file_path) {
    std::string resolved_path = resolveImportPath(file_path);
    
    // check if already processed to avoid circular imports
    if (processed_files_.find(resolved_path) != processed_files_.end()) {
        return;
    }
    
    processed_files_.insert(resolved_path);
    
    // read the file
    std::ifstream file(resolved_path);
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open import file: " + resolved_path);
    }
    
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    
    // lex and parse the file
    Summit::Lexer lexer(source);
    auto tokens = lexer.tokenize();
    
    Summit::Parser parser(tokens);
    auto program = parser.parse();
    
    // store the program
    imported_programs_[resolved_path] = std::move(program);
    
    // extract the module name (filename without .sm extension)
    std::string module_name = std::filesystem::path(resolved_path).stem().string();
    
    // generate code for global functions in this file
    exportGlobalFunctions(*imported_programs_[resolved_path], module_name);
}

void CodeGenerator::exportGlobalFunctions(const Program& program, const std::string& module_prefix) {
    llvm::BasicBlock* saved_insert_block = builder_->GetInsertBlock();
    
    // process all statements in the imported file
    for (const auto& stmt : program.statements) {
        if (auto* func_decl = dynamic_cast<const FunctionDecl*>(stmt.get())) {
            if (func_decl->is_global) {
                // generate the function with prefixed name to avoid conflicts
                std::string mangled_name = module_prefix + "_" + func_decl->name;
                
                // build function type
                llvm::Type* return_type;
                Type return_summit_type;
                if (func_decl->return_type.has_value() && 
                    func_decl->return_type.value().kind != Type::Kind::INFERRED) {
                    return_type = getLLVMType(func_decl->return_type.value());
                    return_summit_type = func_decl->return_type.value();
                } else {
                    return_type = getInt64Type();
                    return_summit_type = Type::i64();
                }
                
                std::vector<llvm::Type*> param_types;
                for (const auto& param_type : func_decl->parameter_types) {
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
                    mangled_name,
                    module_.get()
                );
                
                // store in functions map with both names
                functions_[mangled_name] = function;
                functions_[func_decl->name] = function;
                function_return_types_[mangled_name] = return_summit_type;
                function_return_summit_types_[function] = return_summit_type;
                
                // generate function body
                llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context_, "entry", function);
                builder_->SetInsertPoint(entry);
                
                // save variable scope
                auto prev_named_values = named_values_;
                auto prev_variable_types = variable_types_;
                
                // set up parameters
                size_t idx = 0;
                for (auto& arg : function->args()) {
                    arg.setName(func_decl->parameters[idx]);
                    
                    llvm::Type* param_llvm_type = param_types[idx];
                    llvm::AllocaInst* alloca = builder_->CreateAlloca(
                        param_llvm_type, nullptr, func_decl->parameters[idx]
                    );
                    
                    if (func_decl->parameter_types[idx].kind == Type::Kind::INFERRED) {
                        variable_types_[func_decl->parameters[idx]] = Type::i64();
                    } else {
                        variable_types_[func_decl->parameters[idx]] = func_decl->parameter_types[idx];
                    }
                    
                    builder_->CreateStore(&arg, alloca);
                    named_values_[func_decl->parameters[idx]] = alloca;
                    idx++;
                }
                
                // generate function body
                for (const auto& s : func_decl->body) {
                    codegen(*s);
                }
                
                // add default return if needed
                if (!builder_->GetInsertBlock()->getTerminator()) {
                    if (return_type->isVoidTy()) {
                        builder_->CreateRetVoid();
                    } else {
                        llvm::Value* default_val;
                        if (return_type->isIntegerTy()) {
                            default_val = llvm::ConstantInt::get(return_type, 0);
                        } else if (return_type->isFloatingPointTy()) {
                            default_val = llvm::ConstantFP::get(return_type, 0.0);
                        } else if (return_type->isPointerTy()) {
                            default_val = llvm::ConstantPointerNull::get(
                                llvm::cast<llvm::PointerType>(return_type)
                            );
                        } else {
                            default_val = llvm::ConstantInt::get(getInt64Type(), 0);
                        }
                        builder_->CreateRet(default_val);
                    }
                }
                
                // restore variable scope
                named_values_ = prev_named_values;
                variable_types_ = prev_variable_types;
            }
        }
    }
    
    // restore builder state
    if (saved_insert_block) {
        builder_->SetInsertPoint(saved_insert_block);
    }
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

llvm::Value* CodeGenerator::concatenateStrings(llvm::Value* left, llvm::Value* right, bool add_space) {
    // get or declare strlen function
    llvm::FunctionType* strlen_type = llvm::FunctionType::get(
        llvm::Type::getInt64Ty(*context_),
        {llvm::PointerType::getUnqual(*context_)},
        false
    );
    llvm::Function* strlen_func = module_->getFunction("strlen");
    if (!strlen_func) {
        strlen_func = llvm::Function::Create(
            strlen_type,
            llvm::Function::ExternalLinkage,
            "strlen",
            module_.get()
        );
    }
    
    // get or declare malloc function
    llvm::FunctionType* malloc_type = llvm::FunctionType::get(
        llvm::PointerType::getUnqual(*context_),
        {llvm::Type::getInt64Ty(*context_)},
        false
    );
    llvm::Function* malloc_func = module_->getFunction("malloc");
    if (!malloc_func) {
        malloc_func = llvm::Function::Create(
            malloc_type,
            llvm::Function::ExternalLinkage,
            "malloc",
            module_.get()
        );
    }
    
    // get or declare strcpy function
    llvm::FunctionType* strcpy_type = llvm::FunctionType::get(
        llvm::PointerType::getUnqual(*context_),
        {llvm::PointerType::getUnqual(*context_), llvm::PointerType::getUnqual(*context_)},
        false
    );
    llvm::Function* strcpy_func = module_->getFunction("strcpy");
    if (!strcpy_func) {
        strcpy_func = llvm::Function::Create(
            strcpy_type,
            llvm::Function::ExternalLinkage,
            "strcpy",
            module_.get()
        );
    }
    
    // get or declare strcat function
    llvm::FunctionType* strcat_type = llvm::FunctionType::get(
        llvm::PointerType::getUnqual(*context_),
        {llvm::PointerType::getUnqual(*context_), llvm::PointerType::getUnqual(*context_)},
        false
    );
    llvm::Function* strcat_func = module_->getFunction("strcat");
    if (!strcat_func) {
        strcat_func = llvm::Function::Create(
            strcat_type,
            llvm::Function::ExternalLinkage,
            "strcat",
            module_.get()
        );
    }
    
    // calculate lengths
    llvm::Value* left_len = builder_->CreateCall(strlen_func, {left}, "left_len");
    llvm::Value* right_len = builder_->CreateCall(strlen_func, {right}, "right_len");
    
    llvm::Value* total_len = builder_->CreateAdd(left_len, right_len, "total_len");
    if (add_space) {
        total_len = builder_->CreateAdd(
            total_len, 
            llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 2),
            "total_len_with_space_and_null"
        );
    } else {
        total_len = builder_->CreateAdd(
            total_len, 
            llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 1),
            "total_len_plus_null"
        );
    }
    
    llvm::Value* result = builder_->CreateCall(malloc_func, {total_len}, "concat_result");

    builder_->CreateCall(strcpy_func, {result, left});

    if (add_space) {
        llvm::Value* space_str = builder_->CreateGlobalStringPtr(" ");
        builder_->CreateCall(strcat_func, {result, space_str});
    }

    builder_->CreateCall(strcat_func, {result, right});
    
    return result;
}

llvm::Value* CodeGenerator::convertToString(llvm::Value* value, const Expression* expr) {
    // if already a pointer (string), return as-is
    if (value->getType()->isPointerTy()) {
        return value;
    }
    
    // get or declare sprintf function
    llvm::FunctionType* sprintf_type = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*context_),
        {llvm::PointerType::getUnqual(*context_), llvm::PointerType::getUnqual(*context_)},
        true  // variadic
    );
    llvm::Function* sprintf_func = module_->getFunction("sprintf");
    if (!sprintf_func) {
        sprintf_func = llvm::Function::Create(
            sprintf_type,
            llvm::Function::ExternalLinkage,
            "sprintf",
            module_.get()
        );
    }
    
    // get or declare malloc
    llvm::FunctionType* malloc_type = llvm::FunctionType::get(
        llvm::PointerType::getUnqual(*context_),
        {llvm::Type::getInt64Ty(*context_)},
        false
    );
    llvm::Function* malloc_func = module_->getFunction("malloc");
    if (!malloc_func) {
        malloc_func = llvm::Function::Create(
            malloc_type,
            llvm::Function::ExternalLinkage,
            "malloc",
            module_.get()
        );
    }
    
    llvm::Value* buffer_size = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 64);
    llvm::Value* buffer = builder_->CreateCall(malloc_func, {buffer_size}, "str_buffer");
    
    // determine format string based on type
    llvm::Type* value_type = value->getType();
    llvm::Value* format_str;
    
    if (value_type->isIntegerTy(1)) {
        llvm::Value* true_str = builder_->CreateGlobalStringPtr("true");
        llvm::Value* false_str = builder_->CreateGlobalStringPtr("false");
        return builder_->CreateSelect(value, true_str, false_str, "bool_str");
    } else if (value_type->isIntegerTy()) {
        bool is_unsigned = false;
        if (auto* id = dynamic_cast<const Identifier*>(expr)) {
            auto type_it = variable_types_.find(id->name);
            if (type_it != variable_types_.end()) {
                is_unsigned = (type_it->second.kind == Type::Kind::U8 ||
                             type_it->second.kind == Type::Kind::U16 ||
                             type_it->second.kind == Type::Kind::U32 ||
                             type_it->second.kind == Type::Kind::U64);
            }
        }
        
        if (!value_type->isIntegerTy(64)) {
            if (is_unsigned) {
                value = builder_->CreateZExt(value, llvm::Type::getInt64Ty(*context_));
            } else {
                value = builder_->CreateSExt(value, llvm::Type::getInt64Ty(*context_));
            }
        }
        
        format_str = is_unsigned ? 
            builder_->CreateGlobalStringPtr("%llu") :
            builder_->CreateGlobalStringPtr("%lld");
            
    } else if (value_type->isFloatTy()) {
        value = builder_->CreateFPExt(value, llvm::Type::getDoubleTy(*context_));
        format_str = builder_->CreateGlobalStringPtr("%g");
    } else if (value_type->isDoubleTy()) {
        format_str = builder_->CreateGlobalStringPtr("%g");
    } else {
        throw std::runtime_error("Cannot convert type to string for concatenation");
    }
    
    // call sprintf
    builder_->CreateCall(sprintf_func, {buffer, format_str, value});
    
    return buffer;
}

llvm::Function* CodeGenerator::getFunction(const std::string& name) {
    auto it = functions_.find(name);
    if (it != functions_.end()) {
        return it->second;
    }
    return module_->getFunction(name);
}

}