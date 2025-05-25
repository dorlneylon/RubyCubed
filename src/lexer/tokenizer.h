#pragma once

#include <istream>
#include <string>
#include <unordered_set>
#include <vector>

#include "token.h"

namespace lexer {

class Tokenizer {
 public:
  explicit Tokenizer(std::istream* input);

  std::vector<Token> Tokenize();
  std::vector<TokenVariant> TokenizeVariant();

  bool IsEnd() const;
  TokenVariant GetToken() const;
  void Next();

 private:
  std::istream* input_;
  int line_ = 1;
  int column_ = 0;
  char current_char_ = ' ';

  std::vector<TokenVariant> tokens_;
  size_t current_token_ = 0;

  static const std::unordered_set<std::string> kKeywords;

  void Advance();
  void SkipWhitespace();

  void SkipComment();
  void SkipSingleLineComment();
  void SkipMultiLineComment();
  void SkipCStyleMultiLineComment();

  void ProcessEscapeSequence(std::string& str_value);

  Token ReadIdentifier();
  Token ReadNumber();
  Token ReadString();
  Token ReadOperator();

  static bool IsAlpha(char c);
  static bool IsAlphaNumeric(char c);
  static bool IsDigit(char c);

  static bool IsBracketToken(TokenType type);
  static BracketToken CreateBracketToken(const Token& token);

  void ProcessToken(const Token& token);
  void ProcessNumericToken(const Token& token);
  void ProcessStringToken(const Token& token);
  void ProcessDotToken(const Token& token);
  void ProcessSymbolToken(const Token& token);

  template <typename T>
  T MakeToken(const Token& token);
};

}  // namespace lexer