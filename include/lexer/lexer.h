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
    std::string source_;
    size_t current_ = 0;
    size_t line_ = 1;
    size_t column_ = 1;
    std::vector<Token> tokens_;
    
    static const std::unordered_map<std::string, TokenType> keywords_;
    
    bool isAtEnd() const { return current_ >= source_.size(); }
    char advance();
    char peek() const;
    char peekNext() const;
    bool match(char expected);
    
    void scanToken();
    void skipWhitespace();
    void skipHashComment();
    void skipDoubleSlashComment();
    Token identifier();
    Token number();
    Token string();
};

}