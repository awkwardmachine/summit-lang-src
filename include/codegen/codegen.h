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
#include <unordered_set>
#include <set>

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
                   bool no_stdlib = false,
                   bool nowindow = true);

    void setSourceFile(const std::string& file_path) { 
        current_source_file_ = file_path; 
    }
private:
    // llvm infrastructure
    std::unique_ptr<llvm::LLVMContext> context_;
    std::unique_ptr<llvm::Module> module_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    
    // symbol tables and tracking
    std::unordered_map<std::string, llvm::Value*> named_values_;
    std::unordered_map<std::string, llvm::Function*> functions_;
    std::unordered_map<std::string, llvm::GlobalVariable*> globals_;
    std::unordered_map<std::string, Type> variable_types_;
    
    // module and import system
    struct ModuleInfo {
        std::unordered_map<std::string, llvm::Value*> members;
        std::unordered_map<std::string, llvm::Function*> functions;
    };
    std::unordered_map<std::string, ModuleInfo> modules_;
    std::unordered_map<std::string, std::string> module_aliases_;
    std::unordered_map<std::string, llvm::Function*> function_references_;
    std::unordered_map<std::string, llvm::Function*> stdlib_functions_;
    
    // type tracking for functions
    std::unordered_map<std::string, Type> function_return_types_;
    std::unordered_map<llvm::Function*, Type> function_return_summit_types_;
    std::unordered_set<std::string> inferred_unsigned_functions_;

    std::unordered_map<std::string, std::unique_ptr<Program>> imported_programs_;
    std::unordered_map<std::string, std::string> file_import_aliases_;
    std::set<std::string> processed_files_;
    std::string current_source_file_;

    // current compilation context
    llvm::Function* current_function_ = nullptr;

    // module and stdlib helpers
    llvm::Function* getOrDeclareStdlibFunction(const std::string& full_path);
    void ensureModuleExists(const std::string& path);
    std::string extractFunctionPath(const Expression* expr);
    void importAllFunctionsFromModule(const std::string& module_path);

    // analysis passes
    void analyzeFunctionCalls(const Program& program);
    void analyzeFunctionCallsInExpr(const Expression& expr);
    Type inferReturnType(const FunctionDecl& stmt);

    // expression codegen
    llvm::Value* codegen(const Expression& expr);
    llvm::Value* codegenNumberLiteral(const NumberLiteral& expr);
    llvm::Value* codegenStringLiteral(const StringLiteral& expr);
    llvm::Value* codegenBooleanLiteral(const BooleanLiteral& expr);
    llvm::Value* codegenIdentifier(const Identifier& expr);
    llvm::Value* codegenMemberAccess(const MemberAccess& expr);
    llvm::Value* codegenFunctionCall(const FunctionCall& expr);
    llvm::Value* codegenBinaryOp(const BinaryOp& expr);
    llvm::Value* codegenUnaryOp(const UnaryOp& expr);
    llvm::Value* codegenCast(const CastExpr& expr);
    llvm::Value* codegenAssignment(const AssignmentExpr& expr);
    llvm::Value* codegenImport(const ImportExpr& expr);
    llvm::Value* codegenNamedImport(const NamedImportExpr& expr);
    llvm::Value* codegenChanceExpr(const ChanceExpr& expr);
    llvm::Value* codegenNilLiteral(const NilLiteral& expr);
    llvm::Value* codegenMaybeExpr(const MaybeExpr& expr);
    
    // statement codegen
    void codegen(const Statement& stmt);
    void codegenVarDecl(const VarDecl& stmt);
    void codegenFunctionDecl(const FunctionDecl& stmt);
    void codegenReturn(const ReturnStmt& stmt);
    void codegenExprStmt(const ExpressionStmt& stmt);
    void codegenUsingStmt(const UsingStmt& stmt);
    void codegenUsingImportStmt(const UsingImportStmt& stmt);
    void codegenIfStmt(const IfStmt& stmt);
    void codegenChanceStmt(const ChanceStmt& stmt);
    void codegenDoStmt(const DoStmt& stmt);
    void codegenForStmt(const ForStmt& stmt);
    
    // type system utilities
    llvm::Type* getLLVMType(const Type& type);
    Type getSummitTypeFromLLVMType(llvm::Type* type);
    bool isIntegerType(const Type& type);
    bool isSignedType(const Type& type);
    std::string typeToString(const Type& type);
    std::string getTypeString(llvm::Type* type);
    
    // type casting and conversion
    llvm::Value* castValue(llvm::Value* value, llvm::Type* target_type, const Type& target_summit_type);
    void promoteVariableType(const std::string& name, llvm::Value* new_value, const Type& new_summit_type);
    bool isValueInRange(int64_t value, const Type& type);
    bool isLargeValueInRange(const std::string& largeValue, const Type& type);
    
    // common type helpers
    llvm::Type* getInt64Type();
    llvm::Type* getDoubleType();
    llvm::Type* getVoidType();
    llvm::Type* getInt8PtrType();
    llvm::Function* getFunction(const std::string& name);

    // seed helpers
    void seedRandomGenerator();
    void generateRandomSeed();
    bool random_seeded_ = true;

    // maybe function helpers
    bool isMaybeValue(llvm::Value* val, const Expression* expr);
    llvm::Value* extractMaybeValueForPrint(llvm::Value* maybe_val, 
                                           llvm::Type* maybe_type,
                                           const Type& summit_type);

    // string concat helpers
    llvm::Value* concatenateStrings(llvm::Value* left, llvm::Value* right, bool add_space);
    llvm::Value* convertToString(llvm::Value* value, const Expression* expr);

    std::string resolveImportPath(const std::string& import_path);
    void compileImportedFile(const std::string& file_path);
    void exportGlobalFunctions(const Program& program, const std::string& module_prefix);
};

}