#pragma once
#include <string>
#include <variant>
#include <cstdint>

namespace Summit {

enum class TokenType {
    // Keywords
    FIXED, VAR, FUNC, RET, END, IF, THEN, ELIF,
    ELSE, WHILE, FOR, FROM, USING, AS, DO, CHANCE, MATCH,
    MAYBE,
    
    // Type keywords
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    BOOL, STRING_TYPE, TRUE, FALSE,
    NIL_LITERAL,
    
    // Operators
    ASSIGN, PLUS, MINUS, STAR, SLASH, LPAREN, RPAREN,
    LBRACE, RBRACE, COLON, DOT, COMMA, AT, GREATER, LESS, 
    LESS_EQUAL, GREATER_EQUAL, NOT_EQUAL, EQUAL, ARROW, 
    RIGHT_SHIFT, PERCENT,
    
    // Literals
    IDENTIFIER, STRING, NUMBER,
    
    // Special
    NEWLINE, END_OF_FILE, INVALID
};

struct Token {
    TokenType type;
    std::string lexeme;
    std::variant<std::monostate, int64_t, double, std::string> literal;
    size_t line;
    size_t column;
    
    // constructors for different token types
    Token(TokenType t, std::string lex, size_t ln, size_t col)
        : type(t), lexeme(std::move(lex)), line(ln), column(col) {}
    
    Token(TokenType t, std::string lex, int64_t val, size_t ln, size_t col)
        : type(t), lexeme(std::move(lex)), literal(val), line(ln), column(col) {}
    
    Token(TokenType t, std::string lex, double val, size_t ln, size_t col)
        : type(t), lexeme(std::move(lex)), literal(val), line(ln), column(col) {}
    
    Token(TokenType t, std::string lex, std::string val, size_t ln, size_t col)
        : type(t), lexeme(std::move(lex)), literal(std::move(val)), line(ln), column(col) {}
    
    // helpers for checking number types
    bool isLargeInteger() const {
        return type == TokenType::NUMBER && 
               std::holds_alternative<std::string>(literal);
    }
    
    bool isRegularInteger() const {
        return type == TokenType::NUMBER && 
               std::holds_alternative<int64_t>(literal);
    }
    
    bool isFloat() const {
        return type == TokenType::NUMBER && 
               std::holds_alternative<double>(literal);
    }
    
    // getters for extracting values
    int64_t getIntValue() const {
        return std::get<int64_t>(literal);
    }
    
    double getFloatValue() const {
        return std::get<double>(literal);
    }
    
    const std::string& getLargeIntValue() const {
        return std::get<std::string>(literal);
    }
};

}