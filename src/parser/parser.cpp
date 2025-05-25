#include "parser.h"

#include <stdexcept>

#include "ast.h"
#include "fmt/base.h"
#include "fmt/format.h"

namespace parser {

using namespace lexer;

std::unordered_map<std::string, Parser::Precedence>
    Parser::operator_precedence = {
        {"||", 1}, {"&&", 2}, {"==", 3}, {"!=", 3}, {"<", 3},
        {"<=", 3}, {">", 3},  {">=", 3}, {"+", 4},  {"-", 4},
        {"*", 5},  {"/", 5},  {"%", 5},
};

void Parser::InitStatementHandlers() {
  statement_handlers_ = {{"if", [this] { return ParseIfStatement(); }},
                         {"while", [this] { return ParseWhileStatement(); }},
                         {"def", [this] { return ParseFunctionDeclaration(); }},
                         {"return", [this] { return ParseReturnStatement(); }},
                         {"begin", [this] {
                            ExpectAndConsumeSymbol("begin");
                            return ParseBlock("end");
                          }}};
}

Parser::Parser(Tokenizer* tokenizer) : tokenizer_(tokenizer) {
  InitStatementHandlers();
}

AstNode Parser::Parse() {
  std::vector<std::shared_ptr<Statement>> statements;
  while (!tokenizer_->IsEnd()) {
    ConsumeSemicolons();
    if (tokenizer_->IsEnd()) {
      break;
    }

    try {
      auto statement_node = ParseStatement();
      if (statement_node) {
        auto statement = std::dynamic_pointer_cast<Statement>(statement_node);
        if (statement) {
          statements.push_back(statement);
        } else if (statement_node) {
          throw std::runtime_error(
              "ParseStatement returned a non-statement AST node");
        }
      }
      ConsumeSemicolons();
    } catch (const std::exception& e) {
      throw std::runtime_error(fmt::format("Parsing error: {}", e.what()));
    }
  }
  return std::make_shared<Program>(statements);
}

void Parser::ConsumeSemicolons() {
  while (!tokenizer_->IsEnd() &&
         std::holds_alternative<SymbolToken>(tokenizer_->GetToken()) &&
         std::get<SymbolToken>(tokenizer_->GetToken()).symbol == ";") {
    tokenizer_->Next();
  }
}

AstNode Parser::ParseIdentifierStatement(const std::string& identifier_name) {
  tokenizer_->Next();

  if (!tokenizer_->IsEnd() &&
      std::holds_alternative<SymbolToken>(tokenizer_->GetToken())) {
    auto token = tokenizer_->GetToken();
    const auto& next_symbol = std::get<SymbolToken>(token);
    if (next_symbol.symbol == "=") {
      tokenizer_->Next();
      auto value_expr = ParseExpression();
      return std::make_shared<VariableDeclaration>(
          identifier_name, std::dynamic_pointer_cast<Expression>(value_expr));
    }
  }

  auto callee_expr = std::make_shared<Identifier>(identifier_name);
  auto call_or_ident_expr = AttemptParseFunctionCall(callee_expr);
  return std::make_shared<ExpressionStatement>(
      std::dynamic_pointer_cast<Expression>(call_or_ident_expr));
}

bool Parser::IsBlockTerminator(const std::string& expected_terminator) {
  if (tokenizer_->IsEnd()) {
    return false;
  }

  auto current_token = tokenizer_->GetToken();
  if (std::holds_alternative<SymbolToken>(current_token)) {
    return std::get<SymbolToken>(current_token).symbol == expected_terminator;
  }

  if (expected_terminator == "}" &&
      std::holds_alternative<BracketToken>(current_token)) {
    const auto& bracket = std::get<BracketToken>(current_token);
    return bracket.bracket_type == BracketToken::BracketType::kBrace &&
           bracket.direction == BracketToken::Direction::kRight;
  }

  return false;
}

bool Parser::IsElseOrElsif() {
  if (tokenizer_->IsEnd() ||
      !std::holds_alternative<SymbolToken>(tokenizer_->GetToken())) {
    return false;
  }

  auto token = tokenizer_->GetToken();
  const auto& sym_val = std::get<SymbolToken>(token).symbol;

  return sym_val == "else" || sym_val == "elsif";
}

bool Parser::ConsumeBlockStarter() {
  if (tokenizer_->IsEnd()) {
    return false;
  }

  if (std::holds_alternative<SymbolToken>(tokenizer_->GetToken())) {
    auto token = tokenizer_->GetToken();
    const auto& symbol = std::get<SymbolToken>(token);
    if (symbol.symbol == "do" || symbol.symbol == "begin") {
      tokenizer_->Next();
      return true;
    }
  } else if (std::holds_alternative<BracketToken>(tokenizer_->GetToken())) {
    auto token = tokenizer_->GetToken();
    const auto& bracket = std::get<BracketToken>(token);
    if (bracket.bracket_type == BracketToken::BracketType::kBrace &&
        bracket.direction == BracketToken::Direction::kLeft) {
      tokenizer_->Next();
      return true;
    }
  }

  return false;
}

AstNode Parser::ParseBlock(const std::string& expected_terminator) {
  ConsumeBlockStarter();

  std::vector<std::shared_ptr<Statement>> statements;
  while (!tokenizer_->IsEnd()) {
    ConsumeSemicolons();
    if (tokenizer_->IsEnd()) {
      break;
    }

    if (IsBlockTerminator(expected_terminator)) {
      tokenizer_->Next();
      break;
    }

    if (expected_terminator == "end" && IsElseOrElsif()) {
      break;
    }

    auto stmt_node = ParseStatement();
    if (stmt_node) {
      auto stmt_ptr = std::dynamic_pointer_cast<Statement>(stmt_node);
      if (stmt_ptr) {
        statements.push_back(stmt_ptr);
      }
    } else if (!tokenizer_->IsEnd()) {
      break;
    }

    ConsumeSemicolons();
  }
  return std::make_shared<BlockStatement>(statements);
}

bool Parser::IsMethodCallOperation(const TokenVariant& token) {
  if (std::holds_alternative<DotToken>(token)) {
    return true;
  }

  if (std::holds_alternative<BracketToken>(token)) {
    const auto& bracket = std::get<BracketToken>(token);
    return bracket.bracket_type == BracketToken::BracketType::kParenthesis &&
           bracket.direction == BracketToken::Direction::kLeft;
  }

  return false;
}

bool Parser::IsBinaryOperator(const TokenVariant& token) {
  if (std::holds_alternative<SymbolToken>(token)) {
    return GetPrecedence(token) > 0;
  }
  return false;
}

AstNode Parser::ParseMethodCall(AstNode& left) {
  auto current_op_token = tokenizer_->GetToken();

  if (std::holds_alternative<BracketToken>(current_op_token)) {
    return AttemptParseFunctionCall(left);
  }

  tokenizer_->Next();
  if (tokenizer_->IsEnd() ||
      !std::holds_alternative<SymbolToken>(tokenizer_->GetToken())) {
    throw std::runtime_error("Expected method name after '.'");
  }

  std::string method_name =
      std::get<SymbolToken>(tokenizer_->GetToken()).symbol;
  tokenizer_->Next();

  auto method_ident = std::make_shared<Identifier>(method_name);
  return std::make_shared<BinaryExpression>(
      std::dynamic_pointer_cast<Expression>(left), ".", method_ident);
}

AstNode Parser::ParseBinaryExpression(AstNode& left) {
  auto current_op_token = tokenizer_->GetToken();
  const auto& symbol = std::get<SymbolToken>(current_op_token);
  Precedence current = GetPrecedence(current_op_token);

  tokenizer_->Next();
  auto rhs = ParseExpression(current);
  return std::make_shared<BinaryExpression>(
      std::dynamic_pointer_cast<Expression>(left), symbol.symbol,
      std::dynamic_pointer_cast<Expression>(rhs));
}

AstNode Parser::ParseExpression(Precedence precedence) {
  if (tokenizer_->IsEnd()) {
    throw std::runtime_error(
        "Unexpected end of input while parsing expression.");
  }

  AstNode left = ParsePrimaryExpression();

  while (!tokenizer_->IsEnd()) {
    auto current_op_token = tokenizer_->GetToken();
    Precedence current = GetPrecedence(current_op_token);

    if (IsMethodCallOperation(current_op_token)) {
      if (CallOperatorPrecedence() > precedence) {
        left = ParseMethodCall(left);
        continue;
      }
      break;
    }

    if (IsBinaryOperator(current_op_token)) {
      if (current <= precedence) {
        break;
      }
      left = ParseBinaryExpression(left);
      continue;
    }

    break;
  }

  return left;
}

AstNode Parser::ParseIfStatement() {
  ExpectAndConsumeSymbol("if");
  auto condition = ParseExpression();

  if (!tokenizer_->IsEnd() &&
      std::holds_alternative<SymbolToken>(tokenizer_->GetToken())) {
    if (std::get<SymbolToken>(tokenizer_->GetToken()).symbol == "then") {
      tokenizer_->Next();
    }
  }

  auto consequence = ParseBlock("end");
  std::shared_ptr<Statement> alternative = nullptr;
  if (!tokenizer_->IsEnd() &&
      std::holds_alternative<SymbolToken>(tokenizer_->GetToken())) {
    auto token = tokenizer_->GetToken();
    const auto& symbol = std::get<SymbolToken>(token);
    if (symbol.symbol == "elsif") {
      alternative = std::dynamic_pointer_cast<Statement>(ParseIfStatement());
    } else if (symbol.symbol == "else") {
      tokenizer_->Next();
      alternative = std::dynamic_pointer_cast<Statement>(ParseBlock("end"));
    }
  }

  return std::make_shared<IfStatement>(
      std::dynamic_pointer_cast<Expression>(condition),
      std::dynamic_pointer_cast<Statement>(consequence), alternative);
}

AstNode Parser::ParseReturnStatement() {
  ExpectAndConsumeSymbol("return");
  std::shared_ptr<Expression> value = nullptr;
  bool can_parse_expression = true;
  if (tokenizer_->IsEnd()) {
    can_parse_expression = false;
  } else {
    auto token = tokenizer_->GetToken();
    if (std::holds_alternative<SymbolToken>(token)) {
      const auto& symbol = std::get<SymbolToken>(token);
      if (symbol.symbol == "end" || symbol.symbol == "else" ||
          symbol.symbol == "elsif" || symbol.symbol == "if" ||
          symbol.symbol == "unless") {
        can_parse_expression = false;
      }
    }
  }

  if (can_parse_expression) {
    value = std::dynamic_pointer_cast<Expression>(ParseExpression());
  }
  return std::make_shared<ReturnStatement>(value);
}

AstNode Parser::ParseWhileStatement() {
  ExpectAndConsumeSymbol("while");
  auto condition = ParseExpression();
  if (!tokenizer_->IsEnd() &&
      std::holds_alternative<SymbolToken>(tokenizer_->GetToken())) {
    if (std::get<SymbolToken>(tokenizer_->GetToken()).symbol == "do") {
      tokenizer_->Next();
    }
  }
  auto body = ParseBlock("end");
  return std::make_shared<WhileStatement>(
      std::dynamic_pointer_cast<Expression>(condition),
      std::dynamic_pointer_cast<Statement>(body));
}

AstNode Parser::ParseTimesStatement() {
  auto count_expr = ParseExpression();
  ConsumeBlockStarter();
  auto body = ParseBlock("end");

  return std::make_shared<TimesStatement>(
      std::dynamic_pointer_cast<Expression>(count_expr),
      std::dynamic_pointer_cast<Statement>(body));
}

AstNode Parser::ParseFunctionDeclaration() {
  ExpectAndConsumeSymbol("def");

  if (tokenizer_->IsEnd() ||
      !std::holds_alternative<SymbolToken>(tokenizer_->GetToken())) {
    throw std::runtime_error("Expected function name after 'def'");
  }
  std::string func_name = std::get<SymbolToken>(tokenizer_->GetToken()).symbol;
  tokenizer_->Next();

  auto params = ParseFunctionParameters();
  auto body = std::dynamic_pointer_cast<BlockStatement>(ParseBlock("end"));

  return std::make_shared<FunctionDeclaration>(func_name, params, body);
}

std::vector<std::string> Parser::ParseFunctionParameters() {
  std::vector<std::string> params;

  bool has_parens = false;
  if (!tokenizer_->IsEnd() &&
      std::holds_alternative<BracketToken>(tokenizer_->GetToken())) {
    auto bracket = std::get<BracketToken>(tokenizer_->GetToken());
    if (bracket.bracket_type == BracketToken::BracketType::kParenthesis &&
        bracket.direction == BracketToken::Direction::kLeft) {
      has_parens = true;
      tokenizer_->Next();
    }
  }

  if (has_parens && IsClosingParenthesis()) {
    tokenizer_->Next();
    return params;
  }

  while (true) {
    if (tokenizer_->IsEnd() ||
        !std::holds_alternative<SymbolToken>(tokenizer_->GetToken())) {
      break;
    }

    params.push_back(std::get<SymbolToken>(tokenizer_->GetToken()).symbol);
    tokenizer_->Next();

    if (has_parens && IsClosingParenthesis()) {
      tokenizer_->Next();
      break;
    }
    if (IsCommaToken()) {
      tokenizer_->Next();
    } else {
      break;
    }
  }

  return params;
}

Parser::Precedence Parser::GetPrecedence(const lexer::TokenVariant& token) {
  if (std::holds_alternative<SymbolToken>(token)) {
    const std::string& op = std::get<SymbolToken>(token).symbol;
    return operator_precedence[op];
  }
  if (std::holds_alternative<BracketToken>(token)) {
    const auto& bracket = std::get<BracketToken>(token);
    if (bracket.bracket_type == BracketToken::BracketType::kParenthesis &&
        bracket.direction == BracketToken::Direction::kLeft) {
      return CallOperatorPrecedence();
    }
  } else if (std::holds_alternative<DotToken>(token)) {
    return CallOperatorPrecedence();
  }
  return 0;
}

bool Parser::IsUnaryOperator(const std::string& op_value) {
  return op_value == "-" || op_value == "!" || op_value == "~" ||
         op_value == "+";
}

Parser::Precedence Parser::UnaryOperatorPrecedence() {
  return kUnaryOperatorPrecedence;
}

Parser::Precedence Parser::CallOperatorPrecedence() {
  return kCallOperatorPrecedence;
}

bool Parser::IsStatementStartingSymbol(const std::string& symbol) {
  return symbol == "," || symbol == "." || symbol == ")" || symbol == "}" ||
         symbol == "]" ||
         ((GetPrecedence(SymbolToken{symbol, 0, 0}) > 0 &&
           GetPrecedence(SymbolToken{symbol, 0, 0}) < kCallOperatorPrecedence &&
           symbol != "+" && symbol != "-") &&
          !(IsUnaryOperator(symbol)));
}

bool Parser::IsTimesMethodCall(
    const std::shared_ptr<BinaryExpression>& binary_expr) {
  return binary_expr->GetOperator() == "." &&
         std::dynamic_pointer_cast<Identifier>(binary_expr->GetRight()) &&
         std::dynamic_pointer_cast<Identifier>(binary_expr->GetRight())
                 ->GetName() == "times";
}

bool Parser::IsDoBlockStart() {
  if (tokenizer_->IsEnd()) {
    return false;
  }

  auto token = tokenizer_->GetToken();
  return std::holds_alternative<SymbolToken>(token) &&
         std::get<SymbolToken>(token).symbol == "do";
}

bool Parser::IsBraceBlockStart() {
  if (tokenizer_->IsEnd()) {
    return false;
  }

  auto token = tokenizer_->GetToken();
  return std::holds_alternative<BracketToken>(token) &&
         std::get<BracketToken>(token).bracket_type ==
             BracketToken::BracketType::kBrace &&
         std::get<BracketToken>(token).direction ==
             BracketToken::Direction::kLeft;
}

bool Parser::CanBeExpressionStart(const TokenVariant& token) {
  return std::holds_alternative<ConstantToken>(token) ||
         (std::holds_alternative<BracketToken>(token) &&
          std::get<BracketToken>(token).bracket_type ==
              BracketToken::BracketType::kParenthesis &&
          std::get<BracketToken>(token).direction ==
              BracketToken::Direction::kLeft) ||
         (std::holds_alternative<SymbolToken>(token) &&
          IsUnaryOperator(std::get<SymbolToken>(token).symbol));
}

AstNode Parser::ParseTimesMethodBlock() {
  auto expr_node = ParseExpression();
  auto binary_expr = std::dynamic_pointer_cast<BinaryExpression>(expr_node);

  if (binary_expr && IsTimesMethodCall(binary_expr)) {
    if (IsDoBlockStart()) {
      tokenizer_->Next();
      auto body = ParseBlock("end");
      return std::make_shared<TimesStatement>(
          binary_expr->GetLeft(), std::dynamic_pointer_cast<Statement>(body));
    }

    if (IsBraceBlockStart()) {
      auto body = ParseBlock("}");
      return std::make_shared<TimesStatement>(
          binary_expr->GetLeft(), std::dynamic_pointer_cast<Statement>(body));
    }
  }

  return std::make_shared<ExpressionStatement>(
      std::dynamic_pointer_cast<Expression>(expr_node));
}

AstNode Parser::ParseStatement() {
  if (tokenizer_->IsEnd()) {
    return nullptr;
  }

  auto token = tokenizer_->GetToken();

  if (std::holds_alternative<SymbolToken>(token)) {
    const auto& symbol_token = std::get<SymbolToken>(token);
    const std::string& symbol_value = symbol_token.symbol;

    if (IsStatementStartingSymbol(symbol_value) &&
        !IsUnaryOperator(symbol_value)) {
      throw std::runtime_error(fmt::format(
          "Unexpected token '{}' at start of statement at line {}, col {}",
          symbol_value, symbol_token.line, symbol_token.column));
    }

    auto handler_it = statement_handlers_.find(symbol_value);
    if (handler_it != statement_handlers_.end()) {
      return handler_it->second();
    }

    if (!IsUnaryOperator(symbol_value) && GetPrecedence(token) == 0 &&
        symbol_value != "(" && symbol_value != "{") {
      return ParseIdentifierStatement(symbol_value);
    }
  }

  if (CanBeExpressionStart(token)) {
    return ParseTimesMethodBlock();
  }

  if (std::holds_alternative<BracketToken>(token)) {
    const auto& bracket = std::get<BracketToken>(token);
    if (bracket.bracket_type == BracketToken::BracketType::kBrace &&
        bracket.direction == BracketToken::Direction::kLeft) {
      return ParseBlock("}");
    }
  }

  std::string current_token = "unknown";
  if (std::holds_alternative<SymbolToken>(token)) {
    current_token = std::get<SymbolToken>(token).symbol;
  } else if (std::holds_alternative<ConstantToken>(token)) {
    current_token = "constant";
  } else if (std::holds_alternative<BracketToken>(token)) {
    current_token = "bracket";
  } else if (std::holds_alternative<DotToken>(token)) {
    current_token = "dot";
  }

  throw CreateUnexpectedTokenError("statement", token);
}

AstNode Parser::ParsePrimaryExpression() {
  auto token = tokenizer_->GetToken();

  if (IsUnaryOperatorToken(token)) {
    return ParseUnaryExpression();
  }

  if (IsConstantToken(token)) {
    return ParseConstantLiteral();
  }

  if (IsIdentifierToken(token)) {
    return ParseIdentifier();
  }

  if (IsGroupingExpression(token)) {
    return ParseGroupedExpression();
  }

  throw CreateUnexpectedTokenError("primary expression", token);
}

bool Parser::IsUnaryOperatorToken(const TokenVariant& token) {
  if (std::holds_alternative<SymbolToken>(token)) {
    const auto& symbol = std::get<SymbolToken>(token);
    return IsUnaryOperator(symbol.symbol);
  }
  return false;
}

AstNode Parser::ParseUnaryExpression() {
  auto token = tokenizer_->GetToken();
  const auto& symbol = std::get<SymbolToken>(token);
  std::string op = symbol.symbol;
  tokenizer_->Next();
  auto operand = ParseExpression(UnaryOperatorPrecedence());
  return std::make_shared<UnaryExpression>(
      op, std::dynamic_pointer_cast<Expression>(operand));
}

bool Parser::IsConstantToken(const TokenVariant& token) {
  return std::holds_alternative<ConstantToken>(token);
}

AstNode Parser::ParseConstantLiteral() {
  auto token = tokenizer_->GetToken();
  const auto& const_token = std::get<ConstantToken>(token);
  tokenizer_->Next();

  if (std::holds_alternative<int>(const_token.value)) {
    return std::make_shared<IntegerLiteral>(std::get<int>(const_token.value));
  }
  if (std::holds_alternative<double>(const_token.value)) {
    return std::make_shared<FloatLiteral>(std::get<double>(const_token.value));
  }
  if (std::holds_alternative<std::string>(const_token.value)) {
    return std::make_shared<StringLiteral>(
        std::get<std::string>(const_token.value));
  }

  throw std::runtime_error("Unknown constant type");
}

bool Parser::IsIdentifierToken(const TokenVariant& token) {
  return std::holds_alternative<SymbolToken>(token);
}

AstNode Parser::ParseIdentifier() {
  auto token = tokenizer_->GetToken();
  const auto& symbol = std::get<SymbolToken>(token);
  tokenizer_->Next();
  return std::make_shared<Identifier>(symbol.symbol);
}

bool Parser::IsGroupingExpression(const TokenVariant& token) {
  if (std::holds_alternative<BracketToken>(token)) {
    const auto& bracket = std::get<BracketToken>(token);
    return bracket.bracket_type == BracketToken::BracketType::kParenthesis &&
           bracket.direction == BracketToken::Direction::kLeft;
  }
  return false;
}

AstNode Parser::ParseGroupedExpression() {
  tokenizer_->Next();
  auto expr = ParseExpression();
  ExpectAndConsumeRightParenthesis();
  return expr;
}

std::runtime_error Parser::CreateUnexpectedTokenError(
    const std::string& context, const TokenVariant& token) {
  std::string token_str = "unknown";
  if (std::holds_alternative<SymbolToken>(token)) {
    token_str = std::get<SymbolToken>(token).symbol;
  }

  return std::runtime_error(fmt::format(
      "Unexpected token '{}' in {} at line {}, col {}", token_str, context,
      std::visit([](auto&& arg) { return arg.line; }, token),
      std::visit([](auto&& arg) { return arg.column; }, token)));
}

std::vector<std::shared_ptr<Expression>> Parser::ParseFunctionArguments() {
  std::vector<std::shared_ptr<Expression>> args;

  if (IsClosingParenthesis()) {
    return args;
  }

  while (true) {
    auto expr = ParseExpression();
    if (!expr) {
      throw std::runtime_error("Expected expression in function arguments");
    }
    args.push_back(std::dynamic_pointer_cast<Expression>(expr));

    if (IsClosingParenthesis()) {
      break;
    }

    ExpectAndConsumeToken(TokenType::kComma);
  }
  return args;
}

AstNode Parser::AttemptParseFunctionCall(AstNode callee_ast_node) {
  auto callee_expr = std::dynamic_pointer_cast<Expression>(callee_ast_node);
  if (!callee_expr) {
    return callee_ast_node;
  }

  if (HasParenthesizedArguments()) {
    return ParseParenthesizedFunctionCall(callee_expr);
  }

  if (HasImplicitArguments()) {
    return ParseImplicitFunctionCall(callee_expr);
  }

  return callee_expr;
}

bool Parser::HasParenthesizedArguments() {
  if (tokenizer_->IsEnd()) {
    return false;
  }

  auto token = tokenizer_->GetToken();
  if (std::holds_alternative<BracketToken>(token)) {
    const auto& bracket = std::get<BracketToken>(token);
    return bracket.bracket_type == BracketToken::BracketType::kParenthesis &&
           bracket.direction == BracketToken::Direction::kLeft;
  }
  return false;
}

AstNode Parser::ParseParenthesizedFunctionCall(
    std::shared_ptr<Expression> callee) {
  tokenizer_->Next();
  auto args = ParseFunctionArguments();
  ExpectAndConsumeRightParenthesis();
  return std::make_shared<CallExpression>(callee, args);
}

bool Parser::HasImplicitArguments() {
  if (tokenizer_->IsEnd()) {
    return false;
  }

  auto current_token = tokenizer_->GetToken();

  if (std::holds_alternative<ConstantToken>(current_token)) {
    return true;
  }

  if (std::holds_alternative<SymbolToken>(current_token)) {
    const std::string& s_val = std::get<SymbolToken>(current_token).symbol;
    return IsUnaryOperator(s_val) ||
           (GetPrecedence(current_token) == 0 && s_val != "end" &&
            s_val != "else" && s_val != "elsif" && s_val != "do" &&
            s_val != "then");
  }

  return false;
}

AstNode Parser::ParseImplicitFunctionCall(std::shared_ptr<Expression> callee) {
  std::vector<std::shared_ptr<Expression>> args;
  args.push_back(std::dynamic_pointer_cast<Expression>(ParseExpression()));

  while (IsCommaToken()) {
    tokenizer_->Next();
    args.push_back(std::dynamic_pointer_cast<Expression>(ParseExpression()));
  }

  return std::make_shared<CallExpression>(callee, args);
}

bool Parser::IsClosingParenthesis() {
  if (tokenizer_->IsEnd()) {
    return false;
  }
  auto token = tokenizer_->GetToken();
  if (std::holds_alternative<BracketToken>(token)) {
    const auto& bracket = std::get<BracketToken>(token);
    return bracket.bracket_type == BracketToken::BracketType::kParenthesis &&
           bracket.direction == BracketToken::Direction::kRight;
  }
  return false;
}

bool Parser::IsCommaToken() {
  if (tokenizer_->IsEnd()) {
    return false;
  }
  auto token = tokenizer_->GetToken();
  if (std::holds_alternative<SymbolToken>(token)) {
    return std::get<SymbolToken>(token).symbol == ",";
  }
  return false;
}

bool Parser::CheckForOpeningParenthesisAndConsume() {
  if (!tokenizer_->IsEnd() &&
      std::holds_alternative<BracketToken>(tokenizer_->GetToken())) {
    auto token = tokenizer_->GetToken();
    const auto& bracket = std::get<BracketToken>(token);
    if (bracket.bracket_type == BracketToken::BracketType::kParenthesis &&
        bracket.direction == BracketToken::Direction::kLeft) {
      tokenizer_->Next();
      return true;
    }
  }
  return false;
}

void Parser::ExpectAndConsumeToken(lexer::TokenType /*expected_type*/,
                                   const std::string& /*expected_value*/) {
  if (tokenizer_->IsEnd()) {
    throw std::runtime_error("Expected token but found end of input.");
  }

  if (!IsCommaToken()) {
    throw std::runtime_error("Expected comma but found other token or EOF.");
  }
  tokenizer_->Next();
}

void Parser::ExpectAndConsumeSymbol(const std::string& symbol_value) {
  if (tokenizer_->IsEnd() ||
      !std::holds_alternative<SymbolToken>(tokenizer_->GetToken()) ||
      std::get<SymbolToken>(tokenizer_->GetToken()).symbol != symbol_value) {
    std::string found = tokenizer_->IsEnd() ? "EOF" : "other token";
    if (!tokenizer_->IsEnd() &&
        std::holds_alternative<SymbolToken>(tokenizer_->GetToken())) {
      found = std::get<SymbolToken>(tokenizer_->GetToken()).symbol;
    }
    throw std::runtime_error(fmt::format("Expected symbol '{}' but found '{}'",
                                         symbol_value, found));
  }

  tokenizer_->Next();
}

void Parser::ExpectAndConsumeLeftParenthesis() {
  if (tokenizer_->IsEnd() ||
      !std::holds_alternative<BracketToken>(tokenizer_->GetToken())) {
    throw std::runtime_error("Expected '(' but found other or EOF.");
  }
  auto token = tokenizer_->GetToken();
  const auto& bracket = std::get<BracketToken>(token);
  if (bracket.bracket_type != BracketToken::BracketType::kParenthesis ||
      bracket.direction != BracketToken::Direction::kLeft) {
    throw std::runtime_error("Expected '(' but found other bracket.");
  }
  tokenizer_->Next();
}

void Parser::ExpectAndConsumeRightParenthesis() {
  if (!IsClosingParenthesis()) {
    throw std::runtime_error("Expected ')' but found other token or EOF.");
  }
  tokenizer_->Next();
}

}  // namespace parser