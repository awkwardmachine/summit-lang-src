#pragma once
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <memory>
#include <unordered_map>
#include <string>

#include "parser/ast.h"

namespace Summit {

class CodeGenerator {
public:
    CodeGenerator();
    ~CodeGenerator();
    
    void generate(const Program& program);
    void emitObjectFile(const std::string& filename);
    void emitExecutable(const std::string& filename, 
                   const std::vector<std::string>& libs,
                   bool no_stdlib = false);
    
private:
    std::unique_ptr<llvm::LLVMContext> context_;
    std::unique_ptr<llvm::Module> module_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    
    std::unordered_map<std::string, llvm::Value*> named_values_;
    std::unordered_map<std::string, llvm::Function*> functions_;
    std::unordered_map<std::string, llvm::GlobalVariable*> globals_;
    
    struct ModuleInfo {
        std::unordered_map<std::string, llvm::Value*> members;
        std::unordered_map<std::string, llvm::Function*> functions;
    };
    std::unordered_map<std::string, ModuleInfo> modules_;
    
    std::unordered_map<std::string, llvm::Function*> stdlib_functions_;
    
    std::unordered_map<std::string, std::string> module_aliases_;
    
    std::unordered_map<std::string, llvm::Function*> function_references_;

    llvm::Function* current_function_ = nullptr;

    llvm::Function* getOrDeclareStdlibFunction(const std::string& full_path);
    void ensureModuleExists(const std::string& path);
    std::string extractFunctionPath(const Expression* expr);

    llvm::Value* codegen(const Expression& expr);
    void codegen(const Statement& stmt);

    llvm::Value* codegenNumberLiteral(const NumberLiteral& expr);
    llvm::Value* codegenStringLiteral(const StringLiteral& expr);
    llvm::Value* codegenIdentifier(const Identifier& expr);
    llvm::Value* codegenMemberAccess(const MemberAccess& expr);
    llvm::Value* codegenFunctionCall(const FunctionCall& expr);
    llvm::Value* codegenBinaryOp(const BinaryOp& expr);
    llvm::Value* codegenImport(const ImportExpr& expr);
    llvm::Value* codegenNamedImport(const NamedImportExpr& expr);
    
    void codegenVarDecl(const VarDecl& stmt);
    void codegenFunctionDecl(const FunctionDecl& stmt);
    void codegenReturn(const ReturnStmt& stmt);
    void codegenExprStmt(const ExpressionStmt& stmt);
    
    llvm::Type* getInt64Type();
    llvm::Type* getDoubleType();
    llvm::Type* getVoidType();
    llvm::Type* getInt8PtrType();
    llvm::Function* getFunction(const std::string& name);

    llvm::Type* getLLVMType(const Type& type);
    bool isIntegerType(const Type& type);
    bool isSignedType(const Type& type);
    llvm::Value* castValue(llvm::Value* value, llvm::Type* target_type, const Type& target_summit_type);
};

}