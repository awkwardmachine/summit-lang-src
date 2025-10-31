#pragma once
#include <vector>
#include <memory>
#include <map>
#include "lexer/token.h"
#include "parser/ast.h"

namespace Summit {

class Parser {
public:
    explicit Parser(std::vector<Token> tokens);
    std::unique_ptr<Program> parse();
    
private:
    // parser data
    std::vector<Token> tokens_;
    size_t current_ = 0;
    std::map<std::string, Type> variable_types_;
    std::optional<Type> current_function_return_type_;
    
    // type inference and analysis
    std::optional<Type> inferExpressionType(const Expression* expr);
    bool isFloatingPoint(const Type& type);
    bool isInteger(const Type& type);
    
    // token navigation
    bool isAtEnd() const;
    Token peek() const;
    Token previous() const;
    Token advance();
    bool check(TokenType type) const;
    bool match(TokenType type);
    void consume(TokenType type, const std::string& message);
    void skipNewlines();
    
    // type and expression helpers
    std::optional<Type> parseType();
    bool isReferenceExpression(const Expression* expr);
    
    // declaration parsing
    std::unique_ptr<Statement> declaration();
    std::unique_ptr<Statement> constDeclaration();
    std::unique_ptr<Statement> varDeclaration();
    std::unique_ptr<Statement> functionDeclaration();
    
    // statement parsing
    std::unique_ptr<Statement> statement();
    std::unique_ptr<Statement> returnStatement();
    std::unique_ptr<Statement> usingDeclaration();
    std::unique_ptr<Statement> expressionStatement();
    std::unique_ptr<Statement> ifStatement();
    
    // expression parsing (precedence climbing)
    std::unique_ptr<Expression> expression();
    std::unique_ptr<Expression> assignment();
    std::unique_ptr<Expression> comparison();
    std::unique_ptr<Expression> addition();
    std::unique_ptr<Expression> multiplication();
    std::unique_ptr<Expression> call();
    std::unique_ptr<Expression> memberAccess();
    std::unique_ptr<Expression> unary();
    std::unique_ptr<Expression> primary();
    
    // import parsing
    std::unique_ptr<Expression> import();
    std::unique_ptr<Expression> simpleImport();
    std::unique_ptr<Expression> namedImport();
    std::unique_ptr<Expression> singleNamedImport();
    
    // type casting
    std::unique_ptr<Expression> cast();
};

}