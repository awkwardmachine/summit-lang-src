#include <cctype>
#include <stdexcept>
#include <string>
#include <limits>
#include <cstdint>

#include "lexer/lexer.h"

namespace Summit {

// keyword lookup table
// maps reserved words to their token types
const std::unordered_map<std::string, TokenType> Lexer::keywords_ = {
    {"fixed", TokenType::FIXED},
    {"var", TokenType::VAR},
    {"func", TokenType::FUNC},
    {"ret", TokenType::RET},
    {"end", TokenType::END},
    {"if", TokenType::IF},
    {"then", TokenType::THEN},
    {"elif", TokenType::ELIF},
    {"else", TokenType::ELSE},
    {"while", TokenType::WHILE},
    {"for", TokenType::FOR},
    {"from", TokenType::FROM},
    {"using", TokenType::USING},
    {"as", TokenType::AS},
    {"true", TokenType::TRUE},
    {"false", TokenType::FALSE},
    
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

// basic character reading utilities
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

// token scanning
void Lexer::scanToken() {
    skipWhitespace();
    if (isAtEnd()) return;
    
    size_t start_col = column_;
    char c = advance();
    
    // big switch for all the single char tokens
    switch (c) {
        case '(': tokens_.emplace_back(TokenType::LPAREN, "(", line_, start_col); break;
        case ')': tokens_.emplace_back(TokenType::RPAREN, ")", line_, start_col); break;
        case ':': tokens_.emplace_back(TokenType::COLON, ":", line_, start_col); break;
        case '.': tokens_.emplace_back(TokenType::DOT, ".", line_, start_col); break;
        case ',': tokens_.emplace_back(TokenType::COMMA, ",", line_, start_col); break;
        case '@': tokens_.emplace_back(TokenType::AT, "@", line_, start_col); break;
        case '+': tokens_.emplace_back(TokenType::PLUS, "+", line_, start_col); break;
        case '-': 
            // check for arrow operator
            if (peek() == '>') {
                advance();
                tokens_.emplace_back(TokenType::ARROW, "->", line_, start_col);
            } else {
                tokens_.emplace_back(TokenType::MINUS, "-", line_, start_col);
            }
            break;
        case '*': tokens_.emplace_back(TokenType::STAR, "*", line_, start_col); break;
        case '/': 
            tokens_.emplace_back(TokenType::SLASH, "/", line_, start_col); 
            break;
        case '{': tokens_.emplace_back(TokenType::LBRACE, "{", line_, start_col); break;
        case '}': tokens_.emplace_back(TokenType::RBRACE, "}", line_, start_col); break;
        case '=': 
            if (match('=')) {
                tokens_.emplace_back(TokenType::EQUAL, "==", line_, start_col);
            } else {
                tokens_.emplace_back(TokenType::ASSIGN, "=", line_, start_col);
            }
            break;
        case '!':
            if (match('=')) {
                tokens_.emplace_back(TokenType::NOT_EQUAL, "!=", line_, start_col);
            } else {
                tokens_.emplace_back(TokenType::INVALID, "!", line_, start_col);
            }
            break;
        case '<':
            if (match('=')) {
                tokens_.emplace_back(TokenType::LESS_EQUAL, "<=", line_, start_col);
            } else {
                tokens_.emplace_back(TokenType::LESS, "<", line_, start_col);
            }
            break;
        case '>': 
            if (match('=')) {
                tokens_.emplace_back(TokenType::GREATER_EQUAL, ">=", line_, start_col);
            } else if (match('>')) {
                tokens_.emplace_back(TokenType::RIGHT_SHIFT, ">>", line_, start_col);
            } else {
                tokens_.emplace_back(TokenType::GREATER, ">", line_, start_col);
            }
            break;
        case '"': tokens_.push_back(string()); break;
        case '\n':
            tokens_.emplace_back(TokenType::NEWLINE, "\\n", line_, start_col);
            line_++;
            column_ = 1;
            break;
        default:
            // check for numbers, identifiers, or invalid chars
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

// whitespace and comment handling
void Lexer::skipWhitespace() {
    while (!isAtEnd()) {
        char c = peek();
        if (c == ' ' || c == '\t' || c == '\r') {
            advance();
        } else if (c == '/' && peekNext() == '/') {
            // make sure its actually a comment and not division
            if (current_ == 0 || std::isspace(source_[current_ - 1])) {
                skipDoubleSlashComment();
            } else {
                break;
            }
        } else {
            break;
        }
    }
}

void Lexer::skipDoubleSlashComment() {
    advance();
    advance();
    
    while (!isAtEnd() && peek() != '\n') {
        advance();
    }
}

// identifier and keyword parsing
Token Lexer::identifier() {
    size_t start = current_;
    size_t start_col = column_;
    
    // read until we hit something that cant be part of an identifier
    while (!isAtEnd() && (std::isalnum(peek()) || peek() == '_')) {
        advance();
    }
    
    std::string text = source_.substr(start, current_ - start);
    auto it = keywords_.find(text);
    TokenType type = (it != keywords_.end()) ? it->second : TokenType::IDENTIFIER;
    
    return Token(type, text, line_, start_col);
}

// number parsing
// handles integers, floats, and underscores for readability
Token Lexer::number() {
    size_t start = current_;
    size_t start_col = column_;
    bool is_float = false;
    bool has_underscores = false;
    
    std::string number_str;
    while (!isAtEnd()) {
        char c = peek();
        if (std::isdigit(c)) {
            number_str += advance();
        } else if (c == '_') {
            // underscores for readability like 1_000_000
            has_underscores = true;
            advance();
        } else if (c == '.' && std::isdigit(peekNext())) {
            is_float = true;
            number_str += advance();
        } else {
            break;
        }
    }
    
    // strip out underscores before parsing
    std::string clean_number_str;
    if (has_underscores) {
        for (char c : number_str) {
            if (c != '_') clean_number_str += c;
        }
    } else {
        clean_number_str = number_str;
    }
    
    if (is_float) {
        try {
            double value = std::stod(clean_number_str);
            return Token(TokenType::NUMBER, number_str, value, line_, start_col);
        } catch (const std::out_of_range&) {
            throw std::runtime_error("Floating point number too large at line " + std::to_string(line_));
        }
    } else {
        // try to parse as signed int first
        try {
            int64_t int_value = std::stoll(clean_number_str);
            return Token(TokenType::NUMBER, number_str, int_value, line_, start_col);
        } catch (const std::out_of_range&) {
            // if that fails, try unsigned
            try {
                uint64_t uint_value = std::stoull(clean_number_str);

                // double check its actually within u64 range
                if (clean_number_str.size() > 20 || 
                    (clean_number_str.size() == 20 && clean_number_str > "18446744073709551615")) {
                    throw std::out_of_range("Number exceeds uint64_t range");
                }

                return Token(TokenType::NUMBER, number_str, clean_number_str, line_, start_col);
            } catch (const std::out_of_range&) {
                throw std::runtime_error("Integer too large at line " + std::to_string(line_));
            }
        }
    }
}

// string literal parsing
// handles escape sequences and multiline strings
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
                // handle escape sequences
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