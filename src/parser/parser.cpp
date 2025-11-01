#include <stdexcept>
#include <iostream>
#include <ostream>

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

// basic token navigation utilities
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

// type parsing
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

// type inference helpers
std::optional<Type> Parser::inferExpressionType(const Expression* expr) {
    // imports and references dont have types we can infer
    if (auto* import_expr = dynamic_cast<const ImportExpr*>(expr)) {
        return std::nullopt;
    }
    
    if (auto* named_import = dynamic_cast<const NamedImportExpr*>(expr)) {
        return std::nullopt;
    }
    
    if (auto* member = dynamic_cast<const MemberAccess*>(expr)) {
        return std::nullopt;
    }
    
    // literals are easy
    if (auto* num = dynamic_cast<const NumberLiteral*>(expr)) {
        if (std::holds_alternative<int64_t>(num->value)) {
            return Type::i64();
        } else {
            return Type::f64();
        }
    }
    
    if (auto* str = dynamic_cast<const StringLiteral*>(expr)) {
        return Type::string();
    }
    
    if (auto* boolean = dynamic_cast<const BooleanLiteral*>(expr)) {
        return Type::boolean();
    }
    
    // identifiers need lookup
    if (auto* id = dynamic_cast<const Identifier*>(expr)) {
        auto it = variable_types_.find(id->name);
        if (it != variable_types_.end()) {
            return it->second;
        }
        return std::nullopt;
    }

    // casts tell us explicitly
    if (auto* cast_expr = dynamic_cast<const CastExpr*>(expr)) {
        return cast_expr->target_type;
    }
    
    // binary ops need to check both sides
    if (auto* binop = dynamic_cast<const BinaryOp*>(expr)) {
        auto left_type = inferExpressionType(binop->left.get());
        auto right_type = inferExpressionType(binop->right.get());
        
        if (left_type.has_value() && right_type.has_value()) {
            // if either side is float, result is float
            if (isFloatingPoint(left_type.value()) || isFloatingPoint(right_type.value())) {
                return Type::f64();
            }
            return Type::i64();
        }
    }
    
    // unary ops preserve type
    if (auto* unary = dynamic_cast<const UnaryOp*>(expr)) {
        auto operand_type = inferExpressionType(unary->operand.get());
        if (operand_type.has_value()) {
            return operand_type.value();
        }
    }
    
    return std::nullopt;
}

bool Parser::isFloatingPoint(const Type& type) {
    return type.kind == Type::Kind::F32 || type.kind == Type::Kind::F64;
}

bool Parser::isInteger(const Type& type) {
    return type.kind == Type::Kind::I8 || type.kind == Type::Kind::I16 ||
           type.kind == Type::Kind::I32 || type.kind == Type::Kind::I64 ||
           type.kind == Type::Kind::U8 || type.kind == Type::Kind::U16 ||
           type.kind == Type::Kind::U32 || type.kind == Type::Kind::U64;
}

bool Parser::isReferenceExpression(const Expression* expr) {
    // check if this expression is a reference to something (import, module, function)
    if (dynamic_cast<const ImportExpr*>(expr)) {
        return true;
    }
    
    if (dynamic_cast<const NamedImportExpr*>(expr)) {
        return true;
    }
    
    if (auto* member = dynamic_cast<const MemberAccess*>(expr)) {
        return true;
    }
    
    if (auto* id = dynamic_cast<const Identifier*>(expr)) {
        return true;
    }
    
    return false;
}

std::unique_ptr<Statement> Parser::ifStatement() {
    auto condition = expression();
    
    // check for then token
    if (check(TokenType::THEN)) {
        advance();
    } else {
        throw std::runtime_error("Expected 'then' after if condition at line " + std::to_string(peek().line));
    }
    
    skipNewlines();
    
    // parse then branch
    std::vector<std::unique_ptr<Statement>> then_branch;
    while (!check(TokenType::END) && !check(TokenType::ELIF) && 
           !check(TokenType::ELSE) && !isAtEnd()) {
        skipNewlines();
        if (check(TokenType::END) || check(TokenType::ELIF) || check(TokenType::ELSE)) break;
        then_branch.push_back(declaration());
        skipNewlines();
    }
    
    auto if_stmt = std::make_unique<IfStmt>(std::move(condition), std::move(then_branch));
    
    // parse elif branches
    while (match(TokenType::ELIF)) {
        auto elif_condition = expression();
        consume(TokenType::THEN, "Expected 'then' after elif condition");
        skipNewlines();
        
        std::vector<std::unique_ptr<Statement>> elif_body;
        while (!check(TokenType::END) && !check(TokenType::ELIF) && 
               !check(TokenType::ELSE) && !isAtEnd()) {
            skipNewlines();
            if (check(TokenType::END) || check(TokenType::ELIF) || check(TokenType::ELSE)) break;
            elif_body.push_back(declaration());
            skipNewlines();
        }
        
        auto elif_stmt = std::make_unique<IfStmt>(std::move(elif_condition), std::move(elif_body));
        if_stmt->elif_branches.push_back(std::move(elif_stmt));
    }
    
    // parse else branch
    if (match(TokenType::ELSE)) {
        skipNewlines();
        while (!check(TokenType::END) && !isAtEnd()) {
            skipNewlines();
            if (check(TokenType::END)) break;
            if_stmt->else_branch.push_back(declaration());
            skipNewlines();
        }
    }
    
    consume(TokenType::END, "Expected 'end' after if statement");
    return if_stmt;
}

// declaration parsing
std::unique_ptr<Statement> Parser::declaration() {
    skipNewlines();
    if (match(TokenType::FIXED)) return constDeclaration();
    if (match(TokenType::VAR)) return varDeclaration();
    if (match(TokenType::FUNC)) return functionDeclaration();
    if (match(TokenType::USING)) return usingDeclaration();
    return statement();
}

std::unique_ptr<Statement> Parser::usingDeclaration() {
    // using statements can import whole modules or specific functions
    if (match(TokenType::AT)) {
        auto import_expr = import();
        
        if (match(TokenType::DOT)) {
            Token member = advance();
            if (member.type != TokenType::IDENTIFIER) {
                throw std::runtime_error("Expected member name after '.' at line " + std::to_string(member.line));
            }
            
            auto member_access = std::make_unique<MemberAccess>(std::move(import_expr), member.lexeme);
            return std::make_unique<UsingImportStmt>(std::move(member_access));
        }

        return std::make_unique<UsingImportStmt>(std::move(import_expr));
    }

    Token alias = advance();
    if (alias.type != TokenType::IDENTIFIER) {
        throw std::runtime_error("Expected module alias after 'using' at line " + std::to_string(alias.line));
    }
    
    // handle chained member access like using std.io
    if (match(TokenType::DOT)) {
        std::unique_ptr<Expression> id_expr = std::make_unique<Identifier>(alias.lexeme);
        
        while (true) {
            Token member = advance();
            if (member.type != TokenType::IDENTIFIER) {
                throw std::runtime_error("Expected member name after '.' at line " + std::to_string(member.line));
            }
            
            id_expr = std::make_unique<MemberAccess>(std::move(id_expr), member.lexeme);
            
            if (!match(TokenType::DOT)) {
                break;
            }
        }
        
        return std::make_unique<UsingImportStmt>(std::move(id_expr));
    }
    
    return std::make_unique<UsingStmt>(alias.lexeme);
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
    
    bool is_reference = isReferenceExpression(initializer.get());
    
    // try to infer type if not provided and its not a reference
    if (!type && !is_reference) {
        type = inferExpressionType(initializer.get());
        
        if (!type) {
            throw std::runtime_error(
                "Cannot infer type for constant '" + name.lexeme + "' at line " + 
                std::to_string(name.line) + ". Please provide explicit type annotation."
            );
        }
    }

    if (type.has_value()) {
        variable_types_.insert({name.lexeme, type.value()});
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

    if (type.has_value()) {
        variable_types_.insert({name.lexeme, type.value()});
    }
    
    return std::make_unique<VarDecl>(name.lexeme, type, std::move(initializer), false);
}

std::unique_ptr<Statement> Parser::functionDeclaration() {
    Token name = advance();
    if (name.type != TokenType::IDENTIFIER) {
        throw std::runtime_error("Expected function name at line " + std::to_string(name.line));
    }
    
    consume(TokenType::LPAREN, "Expected '(' after function name");
    
    std::vector<std::string> parameters;
    std::vector<Type> parameter_types;
    
    // parse parameter list
    if (!check(TokenType::RPAREN)) {
        do {
            skipNewlines();
            Token param = advance();
            if (param.type != TokenType::IDENTIFIER) {
                throw std::runtime_error("Expected parameter name at line " + std::to_string(param.line));
            }
            parameters.push_back(param.lexeme);
            
            Type param_type = Type::inferred();
            if (match(TokenType::COLON)) {
                auto parsed_type = parseType();
                if (!parsed_type) {
                    throw std::runtime_error("Expected parameter type after ':' at line " + std::to_string(peek().line));
                }
                param_type = parsed_type.value();
            }
            parameter_types.push_back(param_type);
            
            skipNewlines();
        } while (match(TokenType::COMMA));
    }
    
    consume(TokenType::RPAREN, "Expected ')' after parameters");
    
    // check for return type annotation
    std::optional<Type> return_type = std::nullopt;
    if (match(TokenType::ARROW)) {
        return_type = parseType();
        if (!return_type) {
            throw std::runtime_error("Expected return type after '->' at line " + std::to_string(peek().line));
        }
    } else {
        return_type = Type::inferred();
    }
    
    skipNewlines();
    consume(TokenType::DO, "Expected 'do' after function signature");
    skipNewlines();
    
    auto previous_return_type = current_function_return_type_;
    current_function_return_type_ = return_type;
    
    // parse function body
    std::vector<std::unique_ptr<Statement>> body;
    bool has_return_statement = false;
    
    while (!check(TokenType::END) && !isAtEnd()) {
        skipNewlines();
        if (check(TokenType::END)) break;
        
        auto stmt = declaration();
        
        if (auto* return_stmt = dynamic_cast<ReturnStmt*>(stmt.get())) {
            has_return_statement = true;
        }
        
        body.push_back(std::move(stmt));
        skipNewlines();
    }
    
    consume(TokenType::END, "Expected 'end' after function body");

    // make sure functions with return types actually return something
    if (return_type.has_value() && return_type.value().kind != Type::Kind::INFERRED && !has_return_statement) {
        throw std::runtime_error(
            "Function '" + name.lexeme + "' with return type '" + 
            return_type.value().toString() + "' must have at least one return statement at line " + 
            std::to_string(name.line)
        );
    }

    current_function_return_type_ = previous_return_type;
    
    return std::make_unique<FunctionDecl>(name.lexeme, std::move(parameters), std::move(body), return_type, std::move(parameter_types));
}

// statement parsing
std::unique_ptr<Statement> Parser::statement() {
    if (match(TokenType::IF)) return ifStatement();
    if (match(TokenType::RET)) return returnStatement();
    if (match(TokenType::CHANCE)) return chanceStatement();
    return expressionStatement();
}

std::unique_ptr<Statement> Parser::chanceStatement() {
    consume(TokenType::DO, "Expected 'do' after 'chance'");
    skipNewlines();
    
    std::vector<ChanceBranch> branches;
    double total_percentage = 0.0;
    
    // parse chance branches
    while (!check(TokenType::ELSE) && !check(TokenType::END) && !isAtEnd()) {
        skipNewlines();
        if (check(TokenType::ELSE) || check(TokenType::END)) break;

        Token percent_token = advance();
        if (percent_token.type != TokenType::NUMBER) {
            throw std::runtime_error("Expected percentage number at line " + std::to_string(percent_token.line));
        }
        
        double percentage;
        if (percent_token.isRegularInteger()) {
            percentage = static_cast<double>(percent_token.getIntValue());
        } else if (percent_token.isFloat()) {
            percentage = percent_token.getFloatValue();
        } else {
            throw std::runtime_error("Invalid percentage format at line " + std::to_string(percent_token.line));
        }
        
        consume(TokenType::PERCENT, "Expected '%' after percentage");
        
        total_percentage += percentage;
        
        consume(TokenType::ARROW, "Expected '->' after percentage");

        auto result = expression();
        
        branches.push_back(ChanceBranch(percentage, std::move(result)));
        skipNewlines();
    }
    
    // parse optional else branch
    std::unique_ptr<Expression> else_result = nullptr;
    if (match(TokenType::ELSE)) {
        skipNewlines();
        
        if (match(TokenType::RET)) {
            else_result = expression();
        } else {
            else_result = expression();
        }
        
        skipNewlines();
    }
    
    consume(TokenType::END, "Expected 'end' after chance statement");
    
    if (else_result == nullptr) {
        if (std::abs(total_percentage - 100.0) > 0.001) {
            throw std::runtime_error(
                "Chance percentages must add up to 100% (got " + 
                std::to_string(total_percentage) + "%) or provide an else branch"
            );
        }
    } else {
        if (total_percentage > 100.0) {
            throw std::runtime_error(
                "Chance percentages cannot exceed 100% (got " + 
                std::to_string(total_percentage) + "%)"
            );
        }
    }
    
    return std::make_unique<ChanceStmt>(std::move(branches), std::move(else_result));
}

std::unique_ptr<Statement> Parser::returnStatement() {
    auto value = expression();

    // auto cast return values to match function signature if needed
    if (current_function_return_type_.has_value()) {
        auto return_type = current_function_return_type_.value();
        auto value_type = inferExpressionType(value.get());
        
        if (value_type.has_value() && value_type.value() != return_type) {
            value = std::make_unique<CastExpr>(std::move(value), return_type);
        }
    }
    
    return std::make_unique<ReturnStmt>(std::move(value));
}

std::unique_ptr<Statement> Parser::expressionStatement() {
    auto expr = expression();
    return std::make_unique<ExpressionStmt>(std::move(expr));
}


std::unique_ptr<Expression> Parser::comparison() {
    auto expr = addition();
    
    while (match(TokenType::LESS) || match(TokenType::LESS_EQUAL) || 
           match(TokenType::GREATER) || match(TokenType::GREATER_EQUAL) ||
           match(TokenType::EQUAL) || match(TokenType::NOT_EQUAL)) {
        std::string op = previous().lexeme;
        auto right = addition();
        expr = std::make_unique<BinaryOp>(std::move(expr), op, std::move(right));
    }
    
    return expr;
}

std::unique_ptr<Expression> Parser::expression() {
    return comparison();
}

std::unique_ptr<Expression> Parser::assignment() {
    auto expr = comparison();
    
    if (match(TokenType::ASSIGN)) {
        if (auto* id = dynamic_cast<Identifier*>(expr.get())) {
            auto value = assignment();
            return std::make_unique<AssignmentExpr>(id->name, std::move(value));
        } else {
            throw std::runtime_error("Invalid assignment target at line " + std::to_string(previous().line));
        }
    }
    
    return expr;
}

std::unique_ptr<Expression> Parser::addition() {
    auto expr = multiplication();
    
    while (match(TokenType::PLUS) || match(TokenType::MINUS)) {
        std::string op = previous().lexeme;
        int op_line = previous().line;
        auto right = multiplication();
        
        // type check to prevent mixing floats and ints without explicit cast
        auto left_type = inferExpressionType(expr.get());
        auto right_type = inferExpressionType(right.get());
        
        if (left_type.has_value() && right_type.has_value()) {
            bool left_is_float = isFloatingPoint(left_type.value());
            bool right_is_float = isFloatingPoint(right_type.value());
            bool left_is_int = isInteger(left_type.value());
            bool right_is_int = isInteger(right_type.value());
            
            if ((left_is_float && right_is_int) || (left_is_int && right_is_float)) {
                throw std::runtime_error(
                    "Type error at line " + std::to_string(op_line) + 
                    ": Cannot perform binary operation between integer and floating-point types without explicit cast"
                );
            }
        }
        
        expr = std::make_unique<BinaryOp>(std::move(expr), op, std::move(right));
    }
    
    return expr;
}

std::unique_ptr<Expression> Parser::multiplication() {
    auto expr = unary();
    
    while (match(TokenType::STAR) || match(TokenType::SLASH)) {
        std::string op = previous().lexeme;
        int op_line = previous().line;
        auto right = unary();
        
        // same type checking as addition
        auto left_type = inferExpressionType(expr.get());
        auto right_type = inferExpressionType(right.get());
        
        if (left_type.has_value() && right_type.has_value()) {
            bool left_is_float = isFloatingPoint(left_type.value());
            bool right_is_float = isFloatingPoint(right_type.value());
            bool left_is_int = isInteger(left_type.value());
            bool right_is_int = isInteger(right_type.value());

            if ((left_is_float && right_is_int) || (left_is_int && right_is_float)) {
                throw std::runtime_error(
                    "Type error at line " + std::to_string(op_line) + 
                    ": Cannot perform binary operation between integer and floating-point types without explicit cast"
                );
            }
        }
        
        expr = std::make_unique<BinaryOp>(std::move(expr), op, std::move(right));
    }
    
    return expr;
}

std::unique_ptr<Expression> Parser::unary() {
    if (match(TokenType::MINUS)) {
        auto operand = unary();
        return std::make_unique<UnaryOp>("-", std::move(operand));
    }
    return cast();
}

std::unique_ptr<Expression> Parser::cast() {
    auto expr = call();
    
    // handle explicit type casts with "as"
    while (match(TokenType::AS)) {
        auto target_type = parseType();
        if (!target_type) {
            throw std::runtime_error("Expected type after 'as' at line " + std::to_string(peek().line));
        }
        expr = std::make_unique<CastExpr>(std::move(expr), target_type.value());
    }
    
    return expr;
}

std::unique_ptr<Expression> Parser::call() {
    auto expr = memberAccess();
    
    // handle function calls
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
    
    // handle chained member access like std.io.println
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
    // handle imports
    if (match(TokenType::AT)) {
        return import();
    }
    
    // literals
    if (match(TokenType::NUMBER)) {
        Token num = previous();
        if (num.isLargeInteger()) {
            return std::make_unique<NumberLiteral>(num.getLargeIntValue());
        } else if (num.isRegularInteger()) {
            return std::make_unique<NumberLiteral>(num.getIntValue());
        } else {
            return std::make_unique<NumberLiteral>(num.getFloatValue());
        }
    }
    
    if (match(TokenType::STRING)) {
        return std::make_unique<StringLiteral>(std::get<std::string>(previous().literal));
    }
    
    if (match(TokenType::TRUE)) {
        return std::make_unique<BooleanLiteral>(true);
    }
    
    if (match(TokenType::FALSE)) {
        return std::make_unique<BooleanLiteral>(false);
    }
    
    if (match(TokenType::IDENTIFIER)) {
        return std::make_unique<Identifier>(previous().lexeme);
    }

    if (match(TokenType::CHANCE)) {
        consume(TokenType::DO, "Expected 'do' after 'chance'");
        skipNewlines();
        
        std::vector<ChanceBranch> branches;
        double total_percentage = 0.0;
        
        while (!check(TokenType::ELSE) && !check(TokenType::END) && !isAtEnd()) {
            skipNewlines();
            if (check(TokenType::ELSE) || check(TokenType::END)) break;
            
            Token percent_token = advance();
            if (percent_token.type != TokenType::NUMBER) {
                throw std::runtime_error("Expected percentage number at line " + std::to_string(percent_token.line));
            }
            
            double percentage;
            if (percent_token.isRegularInteger()) {
                percentage = static_cast<double>(percent_token.getIntValue());
            } else if (percent_token.isFloat()) {
                percentage = percent_token.getFloatValue();
            } else {
                throw std::runtime_error("Invalid percentage format at line " + std::to_string(percent_token.line));
            }
            
            consume(TokenType::PERCENT, "Expected '%' after percentage");
            total_percentage += percentage;
            consume(TokenType::ARROW, "Expected '->' after percentage");
            
            auto result = expression();
            branches.push_back(ChanceBranch(percentage, std::move(result)));
            skipNewlines();
        }
        
        std::unique_ptr<Expression> else_result = nullptr;
        if (match(TokenType::ELSE)) {
            skipNewlines();
            if (match(TokenType::RET)) {
                else_result = expression();
            } else {
                else_result = expression();
            }
            skipNewlines();
        }
        
        consume(TokenType::END, "Expected 'end' after chance expression");
        
        if (else_result == nullptr) {
            if (std::abs(total_percentage - 100.0) > 0.001) {
                throw std::runtime_error(
                    "Chance percentages must add up to 100% (got " + 
                    std::to_string(total_percentage) + "%) or provide an else branch"
                );
            }
        } else {
            if (total_percentage > 100.0) {
                throw std::runtime_error(
                    "Chance percentages cannot exceed 100% (got " + 
                    std::to_string(total_percentage) + "%)"
                );
            }
        }
        
        return std::make_unique<ChanceExpr>(std::move(branches), std::move(else_result));
    }
    
    // grouped expressions
    if (match(TokenType::LPAREN)) {
        auto expr = expression();
        consume(TokenType::RPAREN, "Expected ')' after expression");
        return expr;
    }
    
    throw std::runtime_error("Unexpected token '" + peek().lexeme + "' at line " + std::to_string(peek().line));
}

// import statement parsing
// handles different import styles like @import("module"), @import name from "module", etc
std::unique_ptr<Expression> Parser::import() {
    if (!match(TokenType::IDENTIFIER) || previous().lexeme != "import") {
        throw std::runtime_error("Expected 'import' after '@' at line " + std::to_string(peek().line));
    }
    
    // figure out which import style we're dealing with
    if (check(TokenType::IDENTIFIER) && !check(TokenType::LBRACE)) {
        return singleNamedImport();
    } else if (match(TokenType::LBRACE)) {
        return namedImport();
    } else {
        return simpleImport();
    }
}

std::unique_ptr<Expression> Parser::singleNamedImport() {
    // this corresponds to @import name from "module"
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
    // this corresponds to @import { name1, name2 } from "module"
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
    // this corresponds to @import("module")
    consume(TokenType::LPAREN, "Expected '(' after '@import'");
    
    if (!match(TokenType::STRING)) {
        throw std::runtime_error("Expected module name string at line " + std::to_string(peek().line));
    }
    
    std::string module = std::get<std::string>(previous().literal);
    consume(TokenType::RPAREN, "Expected ')' after module name");
    
    return std::make_unique<ImportExpr>(module);
}

}