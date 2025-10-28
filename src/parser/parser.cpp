#include <stdexcept>

#include "parser/parser.h"

namespace Summit {

Parser::Parser(std::vector<Token> tokens) : tokens_(std::move(tokens)) {}

std::unique_ptr<Program> Parser::parse() {
    auto program = std::make_unique<Program>();
    
    while (!isAtEnd()) {
        skipNewlines();
        if (!isAtEnd()) {
            program->statements.push_back(declaration());
        }
    }
    
    return program;
}

bool Parser::isAtEnd() const {
    return peek().type == TokenType::END_OF_FILE;
}

Token Parser::peek() const {
    return tokens_[current_];
}

Token Parser::previous() const {
    return tokens_[current_ - 1];
}

Token Parser::advance() {
    if (!isAtEnd()) current_++;
    return previous();
}

bool Parser::check(TokenType type) const {
    if (isAtEnd()) return false;
    return peek().type == type;
}

bool Parser::match(TokenType type) {
    if (check(type)) {
        advance();
        return true;
    }
    return false;
}

void Parser::skipNewlines() {
    while (match(TokenType::NEWLINE));
}

void Parser::consume(TokenType type, const std::string& message) {
    if (check(type)) {
        advance();
        return;
    }
    throw std::runtime_error(message + " at line " + std::to_string(peek().line));
}

std::optional<Type> Parser::parseType() {
    TokenType tt = peek().type;
    
    switch (tt) {
        case TokenType::I8: advance(); return Type::i8();
        case TokenType::I16: advance(); return Type::i16();
        case TokenType::I32: advance(); return Type::i32();
        case TokenType::I64: advance(); return Type::i64();
        case TokenType::U8: advance(); return Type::u8();
        case TokenType::U16: advance(); return Type::u16();
        case TokenType::U32: advance(); return Type::u32();
        case TokenType::U64: advance(); return Type::u64();
        case TokenType::F32: advance(); return Type::f32();
        case TokenType::F64: advance(); return Type::f64();
        case TokenType::BOOL: advance(); return Type::boolean();
        case TokenType::STRING_TYPE: advance(); return Type::string();
        default: return std::nullopt;
    }
}

std::unique_ptr<Statement> Parser::declaration() {
    skipNewlines();
    if (match(TokenType::CONST)) return constDeclaration();
    if (match(TokenType::VAR)) return varDeclaration();
    if (match(TokenType::FUNC)) return functionDeclaration();
    return statement();
}

std::unique_ptr<Statement> Parser::constDeclaration() {
    Token name = advance();
    if (name.type != TokenType::IDENTIFIER) {
        throw std::runtime_error("Expected constant name at line " + std::to_string(name.line));
    }
    
    std::optional<Type> type = std::nullopt;
    
    if (match(TokenType::COLON)) {
        type = parseType();
        if (!type) {
            throw std::runtime_error("Expected type after ':' at line " + std::to_string(peek().line));
        }
    }
    
    consume(TokenType::ASSIGN, "Expected '=' after constant name");
    auto initializer = expression();
    
    if (!type && !isReferenceExpression(initializer.get())) {
        throw std::runtime_error(
            "Constant '" + name.lexeme + "' requires type annotation at line " + 
            std::to_string(name.line)
        );
    }
    
    return std::make_unique<VarDecl>(name.lexeme, type, std::move(initializer), true);
}

std::unique_ptr<Statement> Parser::varDeclaration() {
    Token name = advance();
    if (name.type != TokenType::IDENTIFIER) {
        throw std::runtime_error("Expected variable name at line " + std::to_string(name.line));
    }
    
    std::optional<Type> type = std::nullopt;
    
    if (match(TokenType::COLON)) {
        type = parseType();
        if (!type) {
            throw std::runtime_error("Expected type after ':' at line " + std::to_string(peek().line));
        }
    }
    
    consume(TokenType::ASSIGN, "Expected '=' after variable name");
    auto initializer = expression();
    
    return std::make_unique<VarDecl>(name.lexeme, type, std::move(initializer), false);
}

std::unique_ptr<Statement> Parser::functionDeclaration() {
    Token name = advance();
    if (name.type != TokenType::IDENTIFIER) {
        throw std::runtime_error("Expected function name at line " + std::to_string(name.line));
    }
    
    consume(TokenType::LPAREN, "Expected '(' after function name");
    
    std::vector<std::string> parameters;
    if (!check(TokenType::RPAREN)) {
        do {
            skipNewlines();
            Token param = advance();
            if (param.type != TokenType::IDENTIFIER) {
                throw std::runtime_error("Expected parameter name at line " + std::to_string(param.line));
            }
            parameters.push_back(param.lexeme);
            skipNewlines();
        } while (match(TokenType::COMMA));
    }
    
    consume(TokenType::RPAREN, "Expected ')' after parameters");
    consume(TokenType::COLON, "Expected ':' after function signature");
    skipNewlines();
    
    std::vector<std::unique_ptr<Statement>> body;
    while (!check(TokenType::END) && !isAtEnd()) {
        skipNewlines();
        if (!check(TokenType::END)) {
            body.push_back(declaration());
            skipNewlines();
        }
    }
    
    consume(TokenType::END, "Expected 'end' after function body");
    
    return std::make_unique<FunctionDecl>(name.lexeme, std::move(parameters), std::move(body));
}

std::unique_ptr<Statement> Parser::statement() {
    if (match(TokenType::RET)) return returnStatement();
    return expressionStatement();
}

std::unique_ptr<Statement> Parser::returnStatement() {
    auto value = expression();
    return std::make_unique<ReturnStmt>(std::move(value));
}

std::unique_ptr<Statement> Parser::expressionStatement() {
    auto expr = expression();
    return std::make_unique<ExpressionStmt>(std::move(expr));
}

std::unique_ptr<Expression> Parser::expression() {
    return assignment();
}

std::unique_ptr<Expression> Parser::assignment() {
    return addition();
}

std::unique_ptr<Expression> Parser::addition() {
    auto expr = multiplication();
    
    while (match(TokenType::PLUS) || match(TokenType::MINUS)) {
        std::string op = previous().lexeme;
        auto right = multiplication();
        expr = std::make_unique<BinaryOp>(std::move(expr), op, std::move(right));
    }
    
    return expr;
}

std::unique_ptr<Expression> Parser::multiplication() {
    auto expr = call();
    
    while (match(TokenType::STAR) || match(TokenType::SLASH)) {
        std::string op = previous().lexeme;
        auto right = call();
        expr = std::make_unique<BinaryOp>(std::move(expr), op, std::move(right));
    }
    
    return expr;
}

std::unique_ptr<Expression> Parser::call() {
    auto expr = memberAccess();
    
    while (match(TokenType::LPAREN)) {
        std::vector<std::unique_ptr<Expression>> arguments;
        
        if (!check(TokenType::RPAREN)) {
            do {
                skipNewlines();
                arguments.push_back(expression());
                skipNewlines();
            } while (match(TokenType::COMMA));
        }
        
        consume(TokenType::RPAREN, "Expected ')' after arguments");
        expr = std::make_unique<FunctionCall>(std::move(expr), std::move(arguments));
    }
    
    return expr;
}

std::unique_ptr<Expression> Parser::memberAccess() {
    auto expr = primary();
    
    while (match(TokenType::DOT)) {
        Token member = advance();
        if (member.type != TokenType::IDENTIFIER) {
            throw std::runtime_error("Expected member name after '.' at line " + std::to_string(member.line));
        }
        expr = std::make_unique<MemberAccess>(std::move(expr), member.lexeme);
    }
    
    return expr;
}

std::unique_ptr<Expression> Parser::primary() {
    if (match(TokenType::AT)) {
        return import();
    }
    
    if (match(TokenType::NUMBER)) {
        Token num = previous();
        if (std::holds_alternative<int64_t>(num.literal)) {
            return std::make_unique<NumberLiteral>(std::get<int64_t>(num.literal));
        } else {
            return std::make_unique<NumberLiteral>(std::get<double>(num.literal));
        }
    }
    
    if (match(TokenType::STRING)) {
        return std::make_unique<StringLiteral>(std::get<std::string>(previous().literal));
    }
    
    if (match(TokenType::IDENTIFIER)) {
        return std::make_unique<Identifier>(previous().lexeme);
    }
    
    if (match(TokenType::LPAREN)) {
        auto expr = expression();
        consume(TokenType::RPAREN, "Expected ')' after expression");
        return expr;
    }
    
    throw std::runtime_error("Unexpected token '" + peek().lexeme + "' at line " + std::to_string(peek().line));
}

std::unique_ptr<Expression> Parser::import() {
    if (!match(TokenType::IDENTIFIER) || previous().lexeme != "import") {
        throw std::runtime_error("Expected 'import' after '@' at line " + std::to_string(peek().line));
    }
    
    if (check(TokenType::IDENTIFIER) && !check(TokenType::LBRACE)) {
        return singleNamedImport();
    } else if (match(TokenType::LBRACE)) {
        return namedImport();
    } else {
        return simpleImport();
    }
}

std::unique_ptr<Expression> Parser::singleNamedImport() {
    Token importName = advance();
    if (importName.type != TokenType::IDENTIFIER) {
        throw std::runtime_error("Expected identifier in import at line " + std::to_string(importName.line));
    }
    
    consume(TokenType::FROM, "Expected 'from' after import name");

    if (!match(TokenType::STRING)) {
        throw std::runtime_error("Expected module name string at line " + std::to_string(peek().line));
    }
    
    std::string module = std::get<std::string>(previous().literal);
    
    std::vector<std::string> imports;
    imports.push_back(importName.lexeme);
    
    return std::make_unique<NamedImportExpr>(module, std::move(imports));
}

std::unique_ptr<Expression> Parser::namedImport() {
    std::vector<std::string> imports;
    
    if (!check(TokenType::RBRACE)) {
        do {
            skipNewlines();
            Token importName = advance();
            if (importName.type != TokenType::IDENTIFIER) {
                throw std::runtime_error("Expected identifier in import list at line " + std::to_string(importName.line));
            }
            imports.push_back(importName.lexeme);
            skipNewlines();
        } while (match(TokenType::COMMA));
    }
    
    consume(TokenType::RBRACE, "Expected '}' after import list");
    consume(TokenType::FROM, "Expected 'from' after import list");
    
    if (!match(TokenType::STRING)) {
        throw std::runtime_error("Expected module name string at line " + std::to_string(peek().line));
    }
    
    std::string module = std::get<std::string>(previous().literal);
    
    return std::make_unique<NamedImportExpr>(module, std::move(imports));
}

std::unique_ptr<Expression> Parser::simpleImport() {
    consume(TokenType::LPAREN, "Expected '(' after '@import'");
    
    if (!match(TokenType::STRING)) {
        throw std::runtime_error("Expected module name string at line " + std::to_string(peek().line));
    }
    
    std::string module = std::get<std::string>(previous().literal);
    consume(TokenType::RPAREN, "Expected ')' after module name");
    
    return std::make_unique<ImportExpr>(module);
}
bool Parser::isReferenceExpression(const Expression* expr) {
    if (dynamic_cast<const ImportExpr*>(expr)) {
        return true;
    }
    
    if (dynamic_cast<const NamedImportExpr*>(expr)) {
        return true;
    }
    
    if (auto* member = dynamic_cast<const MemberAccess*>(expr)) {
        return true;
    }
    
    return false;
}

}