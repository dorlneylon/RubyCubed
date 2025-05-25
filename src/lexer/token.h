#pragma once

#include <string>
#include <variant>

namespace lexer {

enum class TokenType {
  kIdentifier,
  kInteger,
  kFloat,
  kString,
  kKeyword,
  kOperator,
  kSymbol,
  kDot,
  kLeftParenthesis,
  kRightParenthesis,
  kLeftBrace,
  kRightBrace,
  kLeftBracket,
  kRightBracket,
  kComma,
  kSemicolon,
  kColon,
  kNewline,
  kEndOfFile,
};

struct Token {
  TokenType type;
  std::string value;
  int line;
  int column;

  Token(TokenType type, const std::string& value, int line, int column)
      : type(type), value(value), line(line), column(column) {
  }
};

struct ConstantToken {
  std::variant<int, double, std::string> value;
  int line;
  int column;
};

struct BracketToken {
  enum class BracketType { kParenthesis, kBrace, kBracket };
  enum class Direction { kLeft, kRight };

  BracketType bracket_type;
  Direction direction;
  int line;
  int column;
};

struct SymbolToken {
  std::string symbol;
  int line;
  int column;
};

struct DotToken {
  int line;
  int column;
};

using TokenVariant =
    std::variant<ConstantToken, BracketToken, SymbolToken, DotToken>;

}  // namespace lexer