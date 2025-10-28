#pragma once
#include <string>
#include <variant>

namespace Summit {

enum class TokenType {
    // Keywords
    CONST, VAR, FUNC, RET, END, IF, ELSE, WHILE, FOR, FROM, USING,
    
    // Type keywords
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    BOOL, STRING_TYPE,
    
    // Operators
    ASSIGN, PLUS, MINUS, STAR, SLASH, LPAREN, RPAREN,
    LBRACE, RBRACE, COLON, DOT, COMMA, AT,
    
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
    
    Token(TokenType t, std::string lex, size_t ln, size_t col)
        : type(t), lexeme(std::move(lex)), line(ln), column(col) {}
    
    Token(TokenType t, std::string lex, int64_t val, size_t ln, size_t col)
        : type(t), lexeme(std::move(lex)), literal(val), line(ln), column(col) {}
    
    Token(TokenType t, std::string lex, double val, size_t ln, size_t col)
        : type(t), lexeme(std::move(lex)), literal(val), line(ln), column(col) {}
    
    Token(TokenType t, std::string lex, std::string val, size_t ln, size_t col)
        : type(t), lexeme(std::move(lex)), literal(std::move(val)), line(ln), column(col) {}
};

}