#pragma once
#include <memory>
#include <vector>
#include <string>
#include <variant>
#include <optional>

namespace Summit {

struct ASTNode {
    virtual ~ASTNode() = default;
};

struct Expression : ASTNode {};
struct Statement : ASTNode {};

struct Type {
    enum class Kind {
        I8, I16, I32, I64,
        U8, U16, U32, U64,
        F32, F64,
        BOOL,
        STRING,
        VOID,
        INFERRED
    };
    
    Kind kind;
    
    explicit Type(Kind k) : kind(k) {}
    
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
            default: return "<unknown>";
        }
    }
};

// Expressions
struct NumberLiteral : Expression {
    std::variant<int64_t, double> value;
    explicit NumberLiteral(int64_t v) : value(v) {}
    explicit NumberLiteral(double v) : value(v) {}
};

struct StringLiteral : Expression {
    std::string value;
    explicit StringLiteral(std::string v) : value(std::move(v)) {}
};

struct Identifier : Expression {
    std::string name;
    explicit Identifier(std::string n) : name(std::move(n)) {}
};

struct MemberAccess : Expression {
    std::unique_ptr<Expression> object;
    std::string member;
    MemberAccess(std::unique_ptr<Expression> obj, std::string mem)
        : object(std::move(obj)), member(std::move(mem)) {}
};

struct FunctionCall : Expression {
    std::unique_ptr<Expression> callee;
    std::vector<std::unique_ptr<Expression>> arguments;
    FunctionCall(std::unique_ptr<Expression> c, std::vector<std::unique_ptr<Expression>> args)
        : callee(std::move(c)), arguments(std::move(args)) {}
};

struct BinaryOp : Expression {
    std::unique_ptr<Expression> left;
    std::string op;
    std::unique_ptr<Expression> right;
    BinaryOp(std::unique_ptr<Expression> l, std::string o, std::unique_ptr<Expression> r)
        : left(std::move(l)), op(std::move(o)), right(std::move(r)) {}
};

struct ImportExpr : Expression {
    std::string module;
    explicit ImportExpr(std::string mod) : module(std::move(mod)) {}
};

struct NamedImportExpr : public Expression {
    std::string module;
    std::vector<std::string> imports;
    
    NamedImportExpr(std::string mod, std::vector<std::string> imp)
        : module(std::move(mod)), imports(std::move(imp)) {}
};

// Statements
struct ExpressionStmt : Statement {
    std::unique_ptr<Expression> expression;
    explicit ExpressionStmt(std::unique_ptr<Expression> expr)
        : expression(std::move(expr)) {}
};

struct UsingStmt : public Statement {
    std::string module_alias;
    
    UsingStmt(std::string alias) : module_alias(std::move(alias)) {}
};

struct UsingImportStmt : public Statement {
    std::unique_ptr<Expression> import_expr;
    
    UsingImportStmt(std::unique_ptr<Expression> expr) : import_expr(std::move(expr)) {}
};

struct VarDecl : Statement {
    std::string name;
    std::optional<Type> type;
    std::unique_ptr<Expression> initializer;
    bool is_const;
    
    VarDecl(std::string n, std::optional<Type> t, std::unique_ptr<Expression> init, bool c)
        : name(std::move(n)), type(t), initializer(std::move(init)), is_const(c) {}
};

struct ReturnStmt : Statement {
    std::unique_ptr<Expression> value;
    explicit ReturnStmt(std::unique_ptr<Expression> v) : value(std::move(v)) {}
};

struct FunctionDecl : Statement {
    std::string name;
    std::vector<std::string> parameters;
    std::vector<std::unique_ptr<Statement>> body;
    FunctionDecl(std::string n, std::vector<std::string> params,
                 std::vector<std::unique_ptr<Statement>> b)
        : name(std::move(n)), parameters(std::move(params)), body(std::move(b)) {}
};

struct Program : ASTNode {
    std::vector<std::unique_ptr<Statement>> statements;
};

}