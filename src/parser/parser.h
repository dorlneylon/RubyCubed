#pragma once

#include <functional>

#include "../lexer/tokenizer.h"
#include "ast.h"

namespace parser {

class Parser {
 public:
  explicit Parser(lexer::Tokenizer*);

  AstNode Parse();

 private:
  using Precedence = int;
  using Keyword = std::string;
  using Dispatch = std::function<AstNode()>;

  enum _ : Precedence {
    kUnaryOperatorPrecedence = 6,
    kCallOperatorPrecedence = 7,
  };

  lexer::Tokenizer* tokenizer_;
  std::unordered_map<Keyword, Dispatch> statement_handlers_;
  static std::unordered_map<std::string, Precedence> operator_precedence;

  void InitStatementHandlers();

  AstNode ParseStatement();
  AstNode ParseIdentifierStatement(const std::string&);
  AstNode ParseBlock(const std::string& = "end");
  AstNode ParseIfStatement();
  AstNode ParseWhileStatement();
  AstNode ParseTimesStatement();
  AstNode ParseFunctionDeclaration();
  std::vector<std::string> ParseFunctionParameters();
  AstNode ParseReturnStatement();

  AstNode ParseExpression(Precedence = 0);
  AstNode ParsePrimaryExpression();
  AstNode ParseConstantExpression(const lexer::ConstantToken&);
  AstNode ParseSymbolAsExpression(const lexer::SymbolToken&);
  AstNode ParseGroupedExpression();

  AstNode AttemptParseFunctionCall(AstNode);
  std::vector<std::shared_ptr<Expression>> ParseFunctionArguments();

  bool IsClosingParenthesis();
  bool IsCommaToken();
  bool CheckForOpeningParenthesisAndConsume();
  void ExpectAndConsumeToken(lexer::TokenType, const std::string& = "");
  void ExpectAndConsumeSymbol(const std::string&);
  void ExpectAndConsumeLeftParenthesis();
  void ExpectAndConsumeRightParenthesis();
  void ConsumeSemicolons();

  AstNode ParseMethodCall(AstNode&);
  AstNode ParseBinaryExpression(AstNode&);
  AstNode ParseTimesMethodBlock();
  static bool IsTimesMethodCall(const std::shared_ptr<BinaryExpression>&);
  bool IsDoBlockStart();
  bool IsBraceBlockStart();
  static bool CanBeExpressionStart(const lexer::TokenVariant&);
  static bool IsStatementStartingSymbol(const std::string&);
  static bool IsMethodCallOperation(const lexer::TokenVariant&);
  static bool IsBinaryOperator(const lexer::TokenVariant&);
  bool IsBlockTerminator(const std::string&);
  bool IsElseOrElsif();
  bool ConsumeBlockStarter();

  static bool IsUnaryOperatorToken(const lexer::TokenVariant& token);
  AstNode ParseUnaryExpression();
  static bool IsConstantToken(const lexer::TokenVariant& token);
  AstNode ParseConstantLiteral();
  static bool IsIdentifierToken(const lexer::TokenVariant& token);
  AstNode ParseIdentifier();
  static bool IsGroupingExpression(const lexer::TokenVariant& token);
  static std::runtime_error CreateUnexpectedTokenError(
      const std::string& context, const lexer::TokenVariant& token);

  bool HasParenthesizedArguments();
  AstNode ParseParenthesizedFunctionCall(std::shared_ptr<Expression> callee);
  bool HasImplicitArguments();
  AstNode ParseImplicitFunctionCall(std::shared_ptr<Expression> callee);

  static Precedence GetPrecedence(const lexer::TokenVariant&);
  static bool IsUnaryOperator(const std::string&);
  static Precedence UnaryOperatorPrecedence();
  static Precedence CallOperatorPrecedence();
};

auto Read(lexer::Tokenizer*) -> AstNode;

}  // namespace parser