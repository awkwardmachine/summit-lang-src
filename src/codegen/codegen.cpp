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

#include "codegen/codegen.h"

namespace Summit {

struct StdlibFunctionSpec {
    std::string mangled_name;
    llvm::FunctionType* (*type_builder)(llvm::LLVMContext&);
};

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

static const std::unordered_map<std::string, StdlibFunctionSpec> STDLIB_REGISTRY = {
    {"std.io.println", {"summit_io_println", buildVoidStringType}},
    {"std.io.print",   {"summit_io_print",   buildVoidStringType}},
    
    {"std.io.println_int", {"summit_io_println_int", buildVoidInt64Type}},
    {"std.io.print_int",   {"summit_io_print_int",   buildVoidInt64Type}},
    
    {"std.io.println_float", {"summit_io_println_float", buildVoidDoubleType}},
    {"std.io.print_float",   {"summit_io_print_float",   buildVoidDoubleType}},
};

CodeGenerator::CodeGenerator() {
    context_ = std::make_unique<llvm::LLVMContext>();
    module_ = std::make_unique<llvm::Module>("summit_module", *context_);
    builder_ = std::make_unique<llvm::IRBuilder<>>(*context_);
    
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();
}

CodeGenerator::~CodeGenerator() = default;

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
    switch (type.kind) {
        case Type::Kind::I8:
        case Type::Kind::I16:
        case Type::Kind::I32:
        case Type::Kind::I64:
            return true;
        default:
            return false;
    }
}

llvm::Value* CodeGenerator::castValue(llvm::Value* value, llvm::Type* target_type, const Type& target_summit_type) {
    llvm::Type* source_type = value->getType();
    
    if (source_type == target_type) {
        return value;
    }
    
    if (target_type->isIntegerTy() && source_type->isIntegerTy()) {
        bool is_signed = isSignedType(target_summit_type);
        if (target_type->getIntegerBitWidth() > source_type->getIntegerBitWidth()) {
            return is_signed ? 
                builder_->CreateSExt(value, target_type) :
                builder_->CreateZExt(value, target_type);
        } else if (target_type->getIntegerBitWidth() < source_type->getIntegerBitWidth()) {
            return builder_->CreateTrunc(value, target_type);
        }
        return value;
    }
    
    if (target_type->isFloatingPointTy() && source_type->isFloatingPointTy()) {
        if (target_type->isDoubleTy() && source_type->isFloatTy()) {
            return builder_->CreateFPExt(value, target_type);
        } else if (target_type->isFloatTy() && source_type->isDoubleTy()) {
            return builder_->CreateFPTrunc(value, target_type);
        }
        return value;
    }
    
    if (target_type->isFloatingPointTy() && source_type->isIntegerTy()) {
        bool is_signed = isSignedType(target_summit_type);
        return is_signed ?
            builder_->CreateSIToFP(value, target_type) :
            builder_->CreateUIToFP(value, target_type);
    }
    
    if (target_type->isIntegerTy() && source_type->isFloatingPointTy()) {
        bool is_signed = isSignedType(target_summit_type);
        return is_signed ?
            builder_->CreateFPToSI(value, target_type) :
            builder_->CreateFPToUI(value, target_type);
    }
    
    throw std::runtime_error("Unsupported type cast");
}

llvm::Function* CodeGenerator::getOrDeclareStdlibFunction(const std::string& full_path) {
    auto it = stdlib_functions_.find(full_path);
    if (it != stdlib_functions_.end()) {
        return it->second;
    }

    auto spec_it = STDLIB_REGISTRY.find(full_path);
    if (spec_it == STDLIB_REGISTRY.end()) {
        return nullptr;
    }
    
    const auto& spec = spec_it->second;
    
    llvm::FunctionType* func_type = spec.type_builder(*context_);
    llvm::Function* func = llvm::Function::Create(
        func_type,
        llvm::Function::ExternalLinkage,
        spec.mangled_name,
        module_.get()
    );
    
    stdlib_functions_[full_path] = func;
    
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
    if (auto* member = dynamic_cast<const MemberAccess*>(expr)) {
        std::vector<std::string> path;
        const Expression* curr = member;

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

void CodeGenerator::generate(const Program& program) {
    for (const auto& stmt : program.statements) {
        codegen(*stmt);
    }
    
    llvm::Function* main_func = module_->getFunction("main");
    if (!main_func) {
        throw std::runtime_error("No main function found");
    }
    
    std::string error;
    llvm::raw_string_ostream error_stream(error);
    if (llvm::verifyModule(*module_, &error_stream)) {
        throw std::runtime_error("Module verification failed: " + error);
    }

    module_->print(llvm::errs(), nullptr);
}

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
    
    std::string link_cmd = "g++ -no-pie -o " + filename + " " + obj_file;
    
    if (!no_stdlib) {
        std::vector<std::string> lib_paths = {
            "libsummit_std.a",
            "build/linux/x86_64/release/libsummit_std.a",
            "./build/linux/x86_64/release/libsummit_std.a",
            "../build/linux/x86_64/release/libsummit_std.a",
            "../../build/linux/x86_64/release/libsummit_std.a"
        };
        
        std::string std_lib_path;
        for (const auto& path : lib_paths) {
            std::ifstream test(path);
            if (test.good()) {
                std_lib_path = path;
                break;
            }
        }
        
        if (!std_lib_path.empty()) {
            link_cmd += " " + std_lib_path;
        } else {
            std::cerr << "Warning: Could not find libsummit_std.a, trying with -lsummit_std" << std::endl;
            link_cmd += " -L. -Lbuild/linux/x86_64/release -L./build/linux/x86_64/release -lsummit_std";
        }
    }
    
    for (const auto& lib : libs) {
        link_cmd += " " + lib;
    }
    
    link_cmd += " -lstdc++";
    
    #ifdef _WIN32
    link_cmd += " -lucrt";
    #endif
    
    std::cout << "Linking command: " << link_cmd << std::endl;
    
    int result = std::system(link_cmd.c_str());
    if (result != 0) {
        throw std::runtime_error("Linking failed");
    }
    
    std::remove(obj_file.c_str());
}

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
    }
    throw std::runtime_error("Unknown expression type");
}

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

llvm::Value* CodeGenerator::codegenNumberLiteral(const NumberLiteral& expr) {
    if (std::holds_alternative<int64_t>(expr.value)) {
        return llvm::ConstantInt::get(*context_, 
            llvm::APInt(64, std::get<int64_t>(expr.value), true));
    } else {
        return llvm::ConstantFP::get(getDoubleType(), 
            llvm::APFloat(std::get<double>(expr.value)));
    }
}

llvm::Value* CodeGenerator::codegenStringLiteral(const StringLiteral& expr) {
    return builder_->CreateGlobalStringPtr(expr.value);
}

llvm::Value* CodeGenerator::codegenIdentifier(const Identifier& expr) {
    if (module_aliases_.find(expr.name) != module_aliases_.end()) {
        return llvm::ConstantInt::get(*context_, llvm::APInt(64, 0, true));
    }

    auto func_it = function_references_.find(expr.name);
    if (func_it != function_references_.end()) {
        return func_it->second;
    }
    
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

llvm::Value* CodeGenerator::codegenFunctionCall(const FunctionCall& expr) {
    llvm::Function* callee = nullptr;
    std::string func_name;

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

    if (!callee) {
        throw std::runtime_error("Unknown function: " + func_name);
    }

    std::vector<llvm::Value*> args;
    for (const auto& arg : expr.arguments) {
        args.push_back(codegen(*arg));
    }
    
    std::string actual_func_name = callee->getName().str();
    
    if (!args.empty()) {
        bool is_print_func = (actual_func_name == "summit_io_println" || 
                             actual_func_name == "summit_io_print" ||
                             actual_func_name.find("std.io.println") != std::string::npos || 
                             actual_func_name.find("std.io.print") != std::string::npos);
        
        if (is_print_func) {
            llvm::Type* arg_type = args[0]->getType();
            
            if (arg_type->isIntegerTy() && !arg_type->isIntegerTy(1)) {
                std::string base_name = (actual_func_name.find("print") != std::string::npos) ? "std.io.print" : "std.io.println";
                if (actual_func_name.find("println") != std::string::npos) {
                    base_name = "std.io.println";
                }
                
                std::string int_variant = base_name + "_int";
                llvm::Function* int_func = getOrDeclareStdlibFunction(int_variant);
                if (int_func) {
                    callee = int_func;
                    if (arg_type->getIntegerBitWidth() < 64) {
                        args[0] = builder_->CreateSExt(args[0], getInt64Type());
                    } else if (arg_type->getIntegerBitWidth() > 64) {
                        args[0] = builder_->CreateTrunc(args[0], getInt64Type());
                    }
                }
            }
            else if (arg_type->isFloatingPointTy()) {
                std::string base_name = (actual_func_name.find("print") != std::string::npos) ? "std.io.print" : "std.io.println";
                if (actual_func_name.find("println") != std::string::npos) {
                    base_name = "std.io.println";
                }
                
                std::string float_variant = base_name + "_float";
                llvm::Function* float_func = getOrDeclareStdlibFunction(float_variant);
                if (float_func) {
                    callee = float_func;
                    if (arg_type->isFloatTy()) {
                        args[0] = builder_->CreateFPExt(args[0], getDoubleType());
                    }
                }
            }
            else if (arg_type->isPointerTy()) {
                
            }
        }
    }

    return builder_->CreateCall(callee, args);
}

llvm::Value* CodeGenerator::codegenBinaryOp(const BinaryOp& expr) {
    llvm::Value* left = codegen(*expr.left);
    llvm::Value* right = codegen(*expr.right);
    
    if (expr.op == "+") {
        return builder_->CreateAdd(left, right, "addtmp");
    } else if (expr.op == "-") {
        return builder_->CreateSub(left, right, "subtmp");
    } else if (expr.op == "*") {
        return builder_->CreateMul(left, right, "multmp");
    } else if (expr.op == "/") {
        return builder_->CreateSDiv(left, right, "divtmp");
    }
    
    throw std::runtime_error("Unknown binary operator: " + expr.op);
}

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

void CodeGenerator::codegenVarDecl(const VarDecl& stmt) {
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

    if (auto* member = dynamic_cast<const MemberAccess*>(stmt.initializer.get())) {
        std::string full_path = extractFunctionPath(member);
        
        if (!full_path.empty()) {
            auto mod_it = modules_.find(full_path);
            if (mod_it != modules_.end()) {
                module_aliases_[stmt.name] = full_path;
                return;
            }
            
            llvm::Function* func = getOrDeclareStdlibFunction(full_path);
            if (func) {
                function_references_[stmt.name] = func;
                return;
            }
            
            size_t last_dot = full_path.find_last_of('.');
            if (last_dot != std::string::npos) {
                std::string module_path = full_path.substr(0, last_dot);
                auto mod_it = modules_.find(module_path);
                if (mod_it != modules_.end()) {
                    module_aliases_[stmt.name] = full_path;
                    return;
                }
            }
        }
    }
    
    llvm::Value* init_val = codegen(*stmt.initializer);
    
    llvm::Type* var_type;
    if (stmt.type.has_value()) {
        var_type = getLLVMType(stmt.type.value());

        if (init_val->getType() != var_type) {
            init_val = castValue(init_val, var_type, stmt.type.value());
        }
    } else {
        var_type = init_val->getType();
    }
    
    llvm::AllocaInst* alloca = builder_->CreateAlloca(var_type, nullptr, stmt.name);
    builder_->CreateStore(init_val, alloca);
    named_values_[stmt.name] = alloca;
}


void CodeGenerator::codegenFunctionDecl(const FunctionDecl& stmt) {
    std::vector<llvm::Type*> param_types(stmt.parameters.size(), getInt64Type());
    llvm::FunctionType* func_type = llvm::FunctionType::get(
        getInt64Type(), param_types, false
    );
    
    llvm::Function* function = llvm::Function::Create(
        func_type,
        llvm::Function::ExternalLinkage,
        stmt.name,
        module_.get()
    );
    
    functions_[stmt.name] = function;

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context_, "entry", function);
    builder_->SetInsertPoint(entry);
    
    llvm::Function* prev_func = current_function_;
    current_function_ = function;
    
    auto prev_named_values = named_values_;

    size_t idx = 0;
    for (auto& arg : function->args()) {
        arg.setName(stmt.parameters[idx]);
        
        llvm::AllocaInst* alloca = builder_->CreateAlloca(
            getInt64Type(), nullptr, stmt.parameters[idx]
        );
        builder_->CreateStore(&arg, alloca);
        named_values_[stmt.parameters[idx]] = alloca;
        idx++;
    }

    for (const auto& s : stmt.body) {
        codegen(*s);
    }

    if (!builder_->GetInsertBlock()->getTerminator()) {
        builder_->CreateRet(llvm::ConstantInt::get(*context_, llvm::APInt(64, 0, true)));
    }
    
    std::string error;
    llvm::raw_string_ostream error_stream(error);
    if (llvm::verifyFunction(*function, &error_stream)) {
        function->eraseFromParent();
        throw std::runtime_error("Function verification failed: " + error);
    }
    
    named_values_ = prev_named_values;
    current_function_ = prev_func;
}

void CodeGenerator::codegenReturn(const ReturnStmt& stmt) {
    llvm::Value* ret_val = codegen(*stmt.value);
    builder_->CreateRet(ret_val);
}

void CodeGenerator::codegenExprStmt(const ExpressionStmt& stmt) {
    if (auto* named_import = dynamic_cast<const NamedImportExpr*>(stmt.expression.get())) {
        codegenNamedImport(*named_import);
        return;
    }
    
    codegen(*stmt.expression);
}

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