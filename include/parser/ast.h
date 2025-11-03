#pragma once
#include <memory>
#include <vector>
#include <string>
#include <variant>
#include <optional>
#include <cstdint>

namespace Summit {

// base ast nodes
struct ASTNode {
    virtual ~ASTNode() = default;
    virtual std::string toString(int indent = 0) const = 0;
    
protected:
    std::string getIndent(int indent) const {
        return std::string(indent * 2, ' ');
    }
};

struct Expression : ASTNode {};
struct Statement : ASTNode {};

// type system
struct Type {
    enum class Kind {
        I8, I16, I32, I64,
        U8, U16, U32, U64,
        F32, F64,
        BOOL,
        STRING,
        VOID,
        INFERRED,
        MAYBE
    };
    
    Kind kind;
    std::unique_ptr<Type> inner_type;
    
    Type() : kind(Kind::INFERRED), inner_type(nullptr) {}
    
    explicit Type(Kind k) : kind(k), inner_type(nullptr) {}
 
    Type(const Type& other) : kind(other.kind) {
        if (other.inner_type) {
            inner_type = std::make_unique<Type>(*other.inner_type);
        }
    }
    
    Type(Type&& other) noexcept = default;
    
    Type& operator=(const Type& other) {
        if (this != &other) {
            kind = other.kind;
            if (other.inner_type) {
                inner_type = std::make_unique<Type>(*other.inner_type);
            } else {
                inner_type.reset();
            }
        }
        return *this;
    }
    
    Type& operator=(Type&& other) noexcept = default;
    
    // factory methods for creating types
    static Type i8() { return Type(Kind::I8); }
    static Type i16() { return Type(Kind::I16); }
    static Type i32() { return Type(Kind::I32); }
    static Type i64() { return Type(Kind::I64); }
    static Type u8() { return Type(Kind::U8); }
    static Type u16() { return Type(Kind::U16); }
    static Type u32() { return Type(Kind::U32); }
    static Type u64() { return Type(Kind::U64); }
    static Type f32() { return Type(Kind::F32); }
    static Type f64() { return Type(Kind::F64); }
    static Type boolean() { return Type(Kind::BOOL); }
    static Type string() { return Type(Kind::STRING); }
    static Type void_type() { return Type(Kind::VOID); }
    static Type inferred() { return Type(Kind::INFERRED); }
    
    static Type maybe(Type inner) {
        Type t(Kind::MAYBE);
        t.inner_type = std::make_unique<Type>(std::move(inner));
        return t;
    }

    bool operator==(const Type& other) const {
        if (kind != other.kind) return false;
        if (kind == Kind::MAYBE && other.kind == Kind::MAYBE) {
            if (inner_type && other.inner_type) {
                return *inner_type == *other.inner_type;
            }
            return !inner_type && !other.inner_type;
        }
        return true;
    }
    
    bool operator!=(const Type& other) const {
        return !(*this == other);
    }
    
    std::string toString() const {
        switch (kind) {
            case Kind::I8: return "i8";
            case Kind::I16: return "i16";
            case Kind::I32: return "i32";
            case Kind::I64: return "i64";
            case Kind::U8: return "u8";
            case Kind::U16: return "u16";
            case Kind::U32: return "u32";
            case Kind::U64: return "u64";
            case Kind::F32: return "f32";
            case Kind::F64: return "f64";
            case Kind::BOOL: return "bool";
            case Kind::STRING: return "string";
            case Kind::VOID: return "void";
            case Kind::INFERRED: return "<inferred>";
            case Kind::MAYBE:
                if (inner_type) {
                    return "maybe " + inner_type->toString();
                }
                return "maybe";
            default: return "<unknown>";
        }
    }
};


// literal expressions
struct NumberLiteral : Expression {
    std::variant<int64_t, double, std::string> value;
    
    explicit NumberLiteral(int64_t v) : value(v) {}
    explicit NumberLiteral(double v) : value(v) {}
    explicit NumberLiteral(const std::string& largeInt) : value(largeInt) {}
    
    bool isLargeInteger() const {
        return std::holds_alternative<std::string>(value);
    }
    
    bool isRegularInteger() const {
        return std::holds_alternative<int64_t>(value);
    }
    
    bool isFloat() const {
        return std::holds_alternative<double>(value);
    }
    
    int64_t getIntValue() const {
        return std::get<int64_t>(value);
    }
    
    double getFloatValue() const {
        return std::get<double>(value);
    }
    
    const std::string& getLargeIntValue() const {
        return std::get<std::string>(value);
    }
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "NumberLiteral(";
        if (isRegularInteger()) {
            result += std::to_string(getIntValue());
        } else if (isFloat()) {
            result += std::to_string(getFloatValue());
        } else {
            result += getLargeIntValue();
        }
        result += ")";
        return result;
    }
};

struct StringLiteral : Expression {
    std::string value;
    explicit StringLiteral(std::string v) : value(std::move(v)) {}
    
    std::string toString(int indent = 0) const override {
        return getIndent(indent) + "StringLiteral(\"" + value + "\")";
    }
};

struct BooleanLiteral : Expression {
    bool value;
    explicit BooleanLiteral(bool v) : value(v) {}
    
    std::string toString(int indent = 0) const override {
        return getIndent(indent) + "BooleanLiteral(" + (value ? "true" : "false") + ")";
    }
};

// identifier and access expressions
struct Identifier : Expression {
    std::string name;
    explicit Identifier(std::string n) : name(std::move(n)) {}
    
    std::string toString(int indent = 0) const override {
        return getIndent(indent) + "Identifier(" + name + ")";
    }
};

struct MemberAccess : Expression {
    std::unique_ptr<Expression> object;
    std::string member;
    MemberAccess(std::unique_ptr<Expression> obj, std::string mem)
        : object(std::move(obj)), member(std::move(mem)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "MemberAccess\n";
        result += getIndent(indent + 1) + "object:\n";
        result += object->toString(indent + 2) + "\n";
        result += getIndent(indent + 1) + "member: " + member;
        return result;
    }
};

// function call expression
struct FunctionCall : Expression {
    std::unique_ptr<Expression> callee;
    std::vector<std::unique_ptr<Expression>> arguments;
    FunctionCall(std::unique_ptr<Expression> c, std::vector<std::unique_ptr<Expression>> args)
        : callee(std::move(c)), arguments(std::move(args)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "FunctionCall\n";
        result += getIndent(indent + 1) + "callee:\n";
        result += callee->toString(indent + 2);
        if (!arguments.empty()) {
            result += "\n" + getIndent(indent + 1) + "arguments:";
            for (const auto& arg : arguments) {
                result += "\n" + arg->toString(indent + 2);
            }
        }
        return result;
    }
};

// operator expressions
struct BinaryOp : Expression {
    std::unique_ptr<Expression> left;
    std::string op;
    std::unique_ptr<Expression> right;
    BinaryOp(std::unique_ptr<Expression> l, std::string o, std::unique_ptr<Expression> r)
        : left(std::move(l)), op(std::move(o)), right(std::move(r)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "BinaryOp(" + op + ")\n";
        result += getIndent(indent + 1) + "left:\n";
        result += left->toString(indent + 2) + "\n";
        result += getIndent(indent + 1) + "right:\n";
        result += right->toString(indent + 2);
        return result;
    }
};

struct UnaryOp : Expression {
    std::string op;
    std::unique_ptr<Expression> operand;
    UnaryOp(std::string o, std::unique_ptr<Expression> expr)
        : op(std::move(o)), operand(std::move(expr)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "UnaryOp(" + op + ")\n";
        result += getIndent(indent + 1) + "operand:\n";
        result += operand->toString(indent + 2);
        return result;
    }
};

struct AssignmentExpr : Expression {
    std::string name;
    std::unique_ptr<Expression> value;
    AssignmentExpr(std::string n, std::unique_ptr<Expression> v)
        : name(std::move(n)), value(std::move(v)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "AssignmentExpr\n";
        result += getIndent(indent + 1) + "name: " + name + "\n";
        result += getIndent(indent + 1) + "value:\n";
        result += value->toString(indent + 2);
        return result;
    }
};

// type casting
struct CastExpr : Expression {
    std::unique_ptr<Expression> expression;
    Type target_type;
    
    CastExpr(std::unique_ptr<Expression> expr, Type type) 
        : expression(std::move(expr)), target_type(type) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "CastExpr\n";
        result += getIndent(indent + 1) + "target_type: " + target_type.toString() + "\n";
        result += getIndent(indent + 1) + "expression:\n";
        result += expression->toString(indent + 2);
        return result;
    }
};

// import expressions
struct ImportExpr : Expression {
    std::string module;
    bool is_file_import;

    explicit ImportExpr(std::string mod, bool file_import = false) 
        : module(std::move(mod)), is_file_import(file_import) {}

    std::string toString(int indent = 0) const override {
        return getIndent(indent) + "ImportExpr(\"" + module + "\")";
    }
};


struct NamedImportExpr : public Expression {
    std::string module;
    std::vector<std::string> imports;
    bool is_file_import;

    NamedImportExpr(std::string mod, std::vector<std::string> imp, bool file_import = false)
        : module(std::move(mod)), imports(std::move(imp)), is_file_import(file_import) {}

    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "NamedImportExpr\n";
        result += getIndent(indent + 1) + "module: \"" + module + "\"\n";
        result += getIndent(indent + 1) + "imports: [";
        for (size_t i = 0; i < imports.size(); i++) {
            if (i > 0) result += ", ";
            result += imports[i];
        }
        result += "]\n";
        result += getIndent(indent + 1) + "is_file_import: " + (is_file_import ? "true" : "false");
        return result;
    }
};


struct NilLiteral : Expression {
    NilLiteral() = default;
    
    std::string toString(int indent = 0) const override {
        return getIndent(indent) + "NilLiteral";
    }
};

struct MaybeExpr : Expression {
    std::unique_ptr<Expression> value;
    std::vector<std::unique_ptr<Statement>> then_branch;
    std::vector<std::unique_ptr<Statement>> else_branch;
    
    MaybeExpr(std::unique_ptr<Expression> val,
              std::vector<std::unique_ptr<Statement>> then_body,
              std::vector<std::unique_ptr<Statement>> else_body)
        : value(std::move(val)), 
          then_branch(std::move(then_body)),
          else_branch(std::move(else_body)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "MaybeExpr\n";
        result += getIndent(indent + 1) + "value:\n";
        result += value->toString(indent + 2);
        
        if (!then_branch.empty()) {
            result += "\n" + getIndent(indent + 1) + "then_branch:";
            for (const auto& stmt : then_branch) {
                result += "\n" + stmt->toString(indent + 2);
            }
        }
        
        if (!else_branch.empty()) {
            result += "\n" + getIndent(indent + 1) + "else_branch:";
            for (const auto& stmt : else_branch) {
                result += "\n" + stmt->toString(indent + 2);
            }
        }
        
        return result;
    }
};

// statements
struct ExpressionStmt : Statement {
    std::unique_ptr<Expression> expression;
    explicit ExpressionStmt(std::unique_ptr<Expression> expr)
        : expression(std::move(expr)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "ExpressionStmt\n";
        result += expression->toString(indent + 1);
        return result;
    }
};

struct VarDecl : Statement {
    std::string name;
    std::optional<Type> type;
    std::unique_ptr<Expression> initializer;
    bool is_const;
    
    VarDecl(std::string n, std::optional<Type> t, std::unique_ptr<Expression> init, bool c)
        : name(std::move(n)), type(t), initializer(std::move(init)), is_const(c) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "VarDecl\n";
        result += getIndent(indent + 1) + "name: " + name + "\n";
        result += getIndent(indent + 1) + "const: " + (is_const ? "true" : "false");
        
        if (type.has_value()) {
            result += "\n" + getIndent(indent + 1) + "type: " + type->toString();
        }
        
        if (initializer) {
            result += "\n" + getIndent(indent + 1) + "initializer:\n";
            result += initializer->toString(indent + 2);
        }
        
        return result;
    }
};

struct FunctionDecl : Statement {
    std::string name;
    std::vector<std::string> parameters;
    std::vector<std::unique_ptr<Statement>> body;
    std::optional<Type> return_type;
    std::vector<Type> parameter_types;
    bool is_global;

    FunctionDecl(std::string name, 
                 std::vector<std::string> parameters, 
                 std::vector<std::unique_ptr<Statement>> body,
                 std::optional<Type> return_type = std::nullopt,
                 std::vector<Type> param_types = {},
                 bool global = false)
        : name(std::move(name)), 
          parameters(std::move(parameters)), 
          body(std::move(body)),
          return_type(return_type),
          parameter_types(std::move(param_types)),
          is_global(global) {}

    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "FunctionDecl\n";
        result += getIndent(indent + 1) + "name: " + name;
        result += "\n" + getIndent(indent + 1) + "is_global: " + (is_global ? "true" : "false");

        if (!parameters.empty()) {
            result += "\n" + getIndent(indent + 1) + "parameters: [";
            for (size_t i = 0; i < parameters.size(); i++) {
                if (i > 0) result += ", ";
                result += parameters[i];
                if (i < parameter_types.size()) {
                    result += ": " + parameter_types[i].toString();
                }
            }
            result += "]";
        }

        if (return_type.has_value()) {
            result += "\n" + getIndent(indent + 1) + "return_type: " + return_type->toString();
        }

        if (!body.empty()) {
            result += "\n" + getIndent(indent + 1) + "body:";
            for (const auto& stmt : body) {
                result += "\n" + stmt->toString(indent + 2);
            }
        }

        return result;
    }
};


struct ReturnStmt : Statement {
    std::unique_ptr<Expression> value;
    explicit ReturnStmt(std::unique_ptr<Expression> v) : value(std::move(v)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "ReturnStmt";
        if (value) {
            result += "\n" + value->toString(indent + 1);
        }
        return result;
    }
};

struct IfStmt : Statement {
    std::unique_ptr<Expression> condition;
    std::vector<std::unique_ptr<Statement>> then_branch;
    std::vector<std::unique_ptr<IfStmt>> elif_branches;
    std::vector<std::unique_ptr<Statement>> else_branch;
    
    IfStmt(std::unique_ptr<Expression> cond,
           std::vector<std::unique_ptr<Statement>> then_body)
        : condition(std::move(cond)), then_branch(std::move(then_body)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "IfStmt\n";
        result += getIndent(indent + 1) + "condition:\n";
        result += condition->toString(indent + 2);
        
        if (!then_branch.empty()) {
            result += "\n" + getIndent(indent + 1) + "then_branch:";
            for (const auto& stmt : then_branch) {
                result += "\n" + stmt->toString(indent + 2);
            }
        }
        
        if (!elif_branches.empty()) {
            result += "\n" + getIndent(indent + 1) + "elif_branches:";
            for (const auto& elif : elif_branches) {
                result += "\n" + elif->toString(indent + 2);
            }
        }
        
        if (!else_branch.empty()) {
            result += "\n" + getIndent(indent + 1) + "else_branch:";
            for (const auto& stmt : else_branch) {
                result += "\n" + stmt->toString(indent + 2);
            }
        }
        
        return result;
    }
};

struct DoStmt : Statement {
    std::unique_ptr<Expression> value;
    std::vector<std::unique_ptr<Statement>> then_branch;
    std::vector<std::unique_ptr<Statement>> else_branch;
    
    DoStmt(std::unique_ptr<Expression> val,
           std::vector<std::unique_ptr<Statement>> then_body,
           std::vector<std::unique_ptr<Statement>> else_body)
        : value(std::move(val)), 
          then_branch(std::move(then_body)),
          else_branch(std::move(else_body)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "DoStmt\n";
        result += getIndent(indent + 1) + "value:\n";
        result += value->toString(indent + 2);
        
        if (!then_branch.empty()) {
            result += "\n" + getIndent(indent + 1) + "then_branch:";
            for (const auto& stmt : then_branch) {
                result += "\n" + stmt->toString(indent + 2);
            }
        }
        
        if (!else_branch.empty()) {
            result += "\n" + getIndent(indent + 1) + "else_branch:";
            for (const auto& stmt : else_branch) {
                result += "\n" + stmt->toString(indent + 2);
            }
        }
        
        return result;
    }
};

struct ForStmt : Statement {
    std::vector<std::unique_ptr<Expression>> init_exprs;
    std::unique_ptr<Expression> condition;
    std::vector<std::unique_ptr<Expression>> update_exprs;
    std::vector<std::unique_ptr<Statement>> body;
    bool has_explicit_condition;
    
    ForStmt(std::vector<std::unique_ptr<Expression>> init_exprs,
            std::unique_ptr<Expression> condition,
            std::vector<std::unique_ptr<Expression>> update_exprs,
            std::vector<std::unique_ptr<Statement>> body,
            bool has_explicit_condition = true)
        : init_exprs(std::move(init_exprs))
        , condition(std::move(condition))
        , update_exprs(std::move(update_exprs))
        , body(std::move(body))
        , has_explicit_condition(has_explicit_condition) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "ForStmt\n";
        
        if (!init_exprs.empty()) {
            result += getIndent(indent + 1) + "init:";
            for (const auto& expr : init_exprs) {
                result += "\n" + expr->toString(indent + 2);
            }
        }
        
        if (condition) {
            result += "\n" + getIndent(indent + 1) + "condition:\n";
            result += condition->toString(indent + 2);
        }
        
        if (!update_exprs.empty()) {
            result += "\n" + getIndent(indent + 1) + "update:";
            for (const auto& expr : update_exprs) {
                result += "\n" + expr->toString(indent + 2);
            }
        }
        
        if (!body.empty()) {
            result += "\n" + getIndent(indent + 1) + "body:";
            for (const auto& stmt : body) {
                result += "\n" + stmt->toString(indent + 2);
            }
        }
        
        return result;
    }
};

// chance statements
struct ChanceBranch {
    double percentage;
    std::unique_ptr<Expression> result;
    
    ChanceBranch(double pct, std::unique_ptr<Expression> res)
        : percentage(pct), result(std::move(res)) {}
    
    std::string toString(int indent = 0) const {
        std::string ind(indent * 2, ' ');
        std::string res = ind + "ChanceBranch(" + std::to_string(percentage) + "%)\n";
        res += result->toString(indent + 1);
        return res;
    }
}; 

struct ChanceStmt : Statement {
    std::vector<ChanceBranch> branches;
    std::unique_ptr<Expression> else_result;
    
    ChanceStmt(std::vector<ChanceBranch> br, std::unique_ptr<Expression> else_res = nullptr)
        : branches(std::move(br)), else_result(std::move(else_res)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "ChanceStmt\n";
        result += getIndent(indent + 1) + "branches:";
        for (const auto& branch : branches) {
            result += "\n" + branch.toString(indent + 2);
        }
        
        if (else_result) {
            result += "\n" + getIndent(indent + 1) + "else:\n";
            result += else_result->toString(indent + 2);
        }
        
        return result;
    }
};

struct ChanceExpr : Expression {
    std::vector<ChanceBranch> branches;
    std::unique_ptr<Expression> else_result;
    
    ChanceExpr(std::vector<ChanceBranch> br, std::unique_ptr<Expression> else_res = nullptr)
        : branches(std::move(br)), else_result(std::move(else_res)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "ChanceExpr\n";
        result += getIndent(indent + 1) + "branches:";
        for (const auto& branch : branches) {
            result += "\n" + branch.toString(indent + 2);
        }
        
        if (else_result) {
            result += "\n" + getIndent(indent + 1) + "else:\n";
            result += else_result->toString(indent + 2);
        }
        
        return result;
    }
};

// import statements
struct UsingStmt : public Statement {
    std::string module_alias;
    
    UsingStmt(std::string alias) : module_alias(std::move(alias)) {}
    
    std::string toString(int indent = 0) const override {
        return getIndent(indent) + "UsingStmt(\"" + module_alias + "\")";
    }
};

struct UsingImportStmt : public Statement {
    std::unique_ptr<Expression> import_expr;
    
    UsingImportStmt(std::unique_ptr<Expression> expr) : import_expr(std::move(expr)) {}
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "UsingImportStmt\n";
        result += import_expr->toString(indent + 1);
        return result;
    }
};

// top level program node
struct Program : ASTNode {
    std::vector<std::unique_ptr<Statement>> statements;
    
    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "Program\n";
        for (const auto& stmt : statements) {
            result += stmt->toString(indent + 1) + "\n";
        }
        return result;
    }
};

}