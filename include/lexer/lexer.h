#pragma once
#include <vector>
#include <unordered_map>
#include <string_view>
#include "token.h"

namespace Summit {

class Lexer {
public:
    explicit Lexer(std::string source);

    std::vector<Token> tokenize();
    
private:
    // source tracking
    std::string source_;
    size_t current_ = 0;
    size_t line_ = 1;
    size_t column_ = 1;
    std::vector<Token> tokens_;
    
    // keyword lookup table
    static const std::unordered_map<std::string, TokenType> keywords_;
    
    // basic utilities
    bool isAtEnd() const { return current_ >= source_.size(); }
    char advance();
    char peek() const;
    char peekNext() const;
    bool match(char expected);
    
    // token scanning
    void scanToken();
    void skipWhitespace();
    void skipHashComment();
    void skipDoubleSlashComment();
    
    // literal parsing
    Token identifier();
    Token number();
    Token string();
};

}