#pragma once
#include <vector>
#include <memory>
#include "lexer/token.h"
#include "parser/ast.h"

namespace Summit {

class Parser {
public:
    explicit Parser(std::vector<Token> tokens);
    std::unique_ptr<Program> parse();
    
private:
    std::vector<Token> tokens_;
    size_t current_ = 0;
    
    bool isAtEnd() const;
    Token peek() const;
    Token previous() const;
    Token advance();
    bool check(TokenType type) const;
    bool match(TokenType type);
    void consume(TokenType type, const std::string& message);
    void skipNewlines();
    
    std::optional<Type> parseType();
    bool isReferenceExpression(const Expression* expr);

    std::unique_ptr<Statement> declaration();
    std::unique_ptr<Statement> constDeclaration();
    std::unique_ptr<Statement> varDeclaration();
    std::unique_ptr<Statement> functionDeclaration();
    std::unique_ptr<Statement> statement();
    std::unique_ptr<Statement> returnStatement();
    std::unique_ptr<Statement> expressionStatement();

    std::unique_ptr<Expression> expression();
    std::unique_ptr<Expression> assignment();
    std::unique_ptr<Expression> addition();
    std::unique_ptr<Expression> multiplication();
    std::unique_ptr<Expression> call();
    std::unique_ptr<Expression> memberAccess();
    std::unique_ptr<Expression> primary();
    std::unique_ptr<Expression> import();
    std::unique_ptr<Expression> simpleImport();
    std::unique_ptr<Expression> namedImport();
    std::unique_ptr<Expression> singleNamedImport();
};

}