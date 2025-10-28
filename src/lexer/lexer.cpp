#include <cctype>
#include <stdexcept>

#include "lexer/lexer.h"

namespace Summit {

const std::unordered_map<std::string, TokenType> Lexer::keywords_ = {
    {"const", TokenType::CONST},
    {"var", TokenType::VAR},
    {"func", TokenType::FUNC},
    {"ret", TokenType::RET},
    {"end", TokenType::END},
    {"if", TokenType::IF},
    {"else", TokenType::ELSE},
    {"while", TokenType::WHILE},
    {"for", TokenType::FOR},
    {"from", TokenType::FROM},
    
    {"i8", TokenType::I8},
    {"i16", TokenType::I16},
    {"i32", TokenType::I32},
    {"i64", TokenType::I64},
    {"u8", TokenType::U8},
    {"u16", TokenType::U16},
    {"u32", TokenType::U32},
    {"u64", TokenType::U64},
    
    {"f32", TokenType::F32},
    {"f64", TokenType::F64},
    
    {"bool", TokenType::BOOL},
    {"string", TokenType::STRING_TYPE}
};

Lexer::Lexer(std::string source) : source_(std::move(source)) {}

std::vector<Token> Lexer::tokenize() {
    while (!isAtEnd()) {
        scanToken();
    }
    tokens_.emplace_back(TokenType::END_OF_FILE, "", line_, column_);
    return std::move(tokens_);
}

char Lexer::advance() {
    column_++;
    return source_[current_++];
}

char Lexer::peek() const {
    return isAtEnd() ? '\0' : source_[current_];
}

char Lexer::peekNext() const {
    return (current_ + 1 >= source_.size()) ? '\0' : source_[current_ + 1];
}

bool Lexer::match(char expected) {
    if (isAtEnd() || source_[current_] != expected) return false;
    advance();
    return true;
}

void Lexer::scanToken() {
    skipWhitespace();
    if (isAtEnd()) return;
    
    size_t start_col = column_;
    char c = advance();
    
    switch (c) {
        case '(': tokens_.emplace_back(TokenType::LPAREN, "(", line_, start_col); break;
        case ')': tokens_.emplace_back(TokenType::RPAREN, ")", line_, start_col); break;
        case ':': tokens_.emplace_back(TokenType::COLON, ":", line_, start_col); break;
        case '.': tokens_.emplace_back(TokenType::DOT, ".", line_, start_col); break;
        case ',': tokens_.emplace_back(TokenType::COMMA, ",", line_, start_col); break;
        case '@': tokens_.emplace_back(TokenType::AT, "@", line_, start_col); break;
        case '+': tokens_.emplace_back(TokenType::PLUS, "+", line_, start_col); break;
        case '-': tokens_.emplace_back(TokenType::MINUS, "-", line_, start_col); break;
        case '*': tokens_.emplace_back(TokenType::STAR, "*", line_, start_col); break;
        case '/': 
            tokens_.emplace_back(TokenType::SLASH, "/", line_, start_col); 
            break;
        case '{': tokens_.emplace_back(TokenType::LBRACE, "{", line_, start_col); break;
        case '}': tokens_.emplace_back(TokenType::RBRACE, "}", line_, start_col); break;
        case '=': tokens_.emplace_back(TokenType::ASSIGN, "=", line_, start_col); break;
        case '"': tokens_.push_back(string()); break;
        case '\n':
            tokens_.emplace_back(TokenType::NEWLINE, "\\n", line_, start_col);
            line_++;
            column_ = 1;
            break;
        default:
            if (std::isdigit(c)) {
                current_--; column_--;
                tokens_.push_back(number());
            } else if (std::isalpha(c) || c == '_') {
                current_--; column_--;
                tokens_.push_back(identifier());
            } else {
                tokens_.emplace_back(TokenType::INVALID, std::string(1, c), line_, start_col);
            }
            break;
    }
}

void Lexer::skipWhitespace() {
    while (!isAtEnd()) {
        char c = peek();
        if (c == ' ' || c == '\t' || c == '\r') {
            advance();
        } else if (c == '#') {
            skipHashComment();
        } else if (c == '/' && peekNext() == '/') {
            skipDoubleSlashComment();
        } else {
            break;
        }
    }
}

void Lexer::skipHashComment() {
    while (!isAtEnd() && peek() != '\n') {
        advance();
    }
}

void Lexer::skipDoubleSlashComment() {
    advance();
    advance();
    
    while (!isAtEnd() && peek() != '\n') {
        advance();
    }
}

Token Lexer::identifier() {
    size_t start = current_;
    size_t start_col = column_;
    
    while (!isAtEnd() && (std::isalnum(peek()) || peek() == '_')) {
        advance();
    }
    
    std::string text = source_.substr(start, current_ - start);
    auto it = keywords_.find(text);
    TokenType type = (it != keywords_.end()) ? it->second : TokenType::IDENTIFIER;
    
    return Token(type, text, line_, start_col);
}

Token Lexer::number() {
    size_t start = current_;
    size_t start_col = column_;
    bool is_float = false;
    
    while (!isAtEnd() && std::isdigit(peek())) {
        advance();
    }
    
    if (!isAtEnd() && peek() == '.' && std::isdigit(peekNext())) {
        is_float = true;
        advance();
        while (!isAtEnd() && std::isdigit(peek())) {
            advance();
        }
    }
    
    std::string text = source_.substr(start, current_ - start);
    
    if (is_float) {
        return Token(TokenType::NUMBER, text, std::stod(text), line_, start_col);
    } else {
        return Token(TokenType::NUMBER, text, static_cast<int64_t>(std::stoll(text)), line_, start_col);
    }
}

Token Lexer::string() {
    size_t start = current_;
    size_t start_col = column_ - 1;
    std::string value;
    
    while (!isAtEnd() && peek() != '"') {
        if (peek() == '\n') {
            line_++;
            column_ = 0;
        }
        if (peek() == '\\' && !isAtEnd()) {
            advance();
            if (!isAtEnd()) {
                char c = advance();
                switch (c) {
                    case 'n': value += '\n'; break;
                    case 't': value += '\t'; break;
                    case 'r': value += '\r'; break;
                    case '\\': value += '\\'; break;
                    case '"': value += '"'; break;
                    default: value += c; break;
                }
            }
        } else {
            value += advance();
        }
    }
    
    if (isAtEnd()) {
        throw std::runtime_error("Unterminated string at line " + std::to_string(line_));
    }
    
    advance();
    std::string lexeme = source_.substr(start - 1, current_ - start + 1);
    return Token(TokenType::STRING, lexeme, value, line_, start_col);
}

}