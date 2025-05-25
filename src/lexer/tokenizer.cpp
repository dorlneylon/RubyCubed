#include "tokenizer.h"

#include <cctype>
#include <stdexcept>
#include <string>
#include <unordered_map>

#include "fmt/format.h"

namespace lexer {

const std::unordered_set<std::string> Tokenizer::kKeywords = {
    "if",  "else",   "elsif", "end",   "while", "do",
    "def", "return", "true",  "false", "nil",   "puts"};

static const std::unordered_map<char, TokenType> kSingleCharOperators = {
    {'+', TokenType::kOperator}, {'-', TokenType::kOperator},
    {'*', TokenType::kOperator}, {'/', TokenType::kOperator},
    {'%', TokenType::kOperator}, {'=', TokenType::kOperator},
    {'<', TokenType::kOperator}, {'>', TokenType::kOperator},
    {'!', TokenType::kOperator}, {'&', TokenType::kOperator},
    {'|', TokenType::kOperator}, {'^', TokenType::kOperator}};

static const std::unordered_map<std::string, TokenType> kDoubleCharOperators = {
    {"==", TokenType::kOperator}, {"!=", TokenType::kOperator},
    {"<=", TokenType::kOperator}, {">=", TokenType::kOperator},
    {"&&", TokenType::kOperator}, {"||", TokenType::kOperator},
    {"+=", TokenType::kOperator}, {"-=", TokenType::kOperator},
    {"*=", TokenType::kOperator}, {"/=", TokenType::kOperator},
    {"%=", TokenType::kOperator}, {"^=", TokenType::kOperator}};

static const std::unordered_map<char, TokenType> kSpecialOperators = {
    {'.', TokenType::kDot},
    {',', TokenType::kComma},
    {':', TokenType::kColon},
    {';', TokenType::kSemicolon},
    {'(', TokenType::kLeftParenthesis},
    {')', TokenType::kRightParenthesis},
    {'{', TokenType::kLeftBrace},
    {'}', TokenType::kRightBrace},
    {'[', TokenType::kLeftBracket},
    {']', TokenType::kRightBracket}};

Tokenizer::Tokenizer(std::istream* input)
    : input_(input), line_(1), column_(0), current_char_(' ') {
  Advance();
}

void Tokenizer::Advance() {
  int ich = input_->get();

  if (ich == EOF) {
    current_char_ = static_cast<char>(EOF);
    return;
  }

  current_char_ = static_cast<char>(ich);

  if (current_char_ == '\n') {
    line_++;
    column_ = 0;
  } else {
    column_++;
  }
}

void Tokenizer::SkipWhitespace() {
  while (!input_->eof() &&
         (std::isspace(static_cast<unsigned char>(current_char_)) != 0)) {
    Advance();
  }
}

void Tokenizer::SkipSingleLineComment() {
  while (!input_->eof() && current_char_ != '\n') {
    Advance();
  }
}

void Tokenizer::SkipMultiLineComment() {
  Advance();
  std::string begin_tag;
  for (int i = 0; i < 5 && !input_->eof(); ++i) {
    begin_tag.push_back(current_char_);
    Advance();
  }

  if (begin_tag == "begin") {
    while (!input_->eof()) {
      if (current_char_ == '=' && !input_->eof() && input_->peek() == 'e') {
        Advance();
        std::string end_tag;
        for (int i = 0; i < 3 && !input_->eof(); ++i) {
          end_tag.push_back(current_char_);
          Advance();
        }
        if (end_tag == "end") {
          break;
        }
      } else {
        Advance();
      }
    }
  }
}

void Tokenizer::SkipCStyleMultiLineComment() {
  Advance();
  Advance();

  while (!input_->eof()) {
    if (current_char_ == '*' && !input_->eof() && input_->peek() == '/') {
      Advance();
      Advance();
      break;
    }
    Advance();
  }
}

void Tokenizer::SkipComment() {
  if (current_char_ == '#') {
    SkipSingleLineComment();
  } else if (current_char_ == '=' && !input_->eof() && input_->peek() == 'b') {
    SkipMultiLineComment();
  } else if (current_char_ == '/' && !input_->eof() && input_->peek() == '*') {
    SkipCStyleMultiLineComment();
  }
}

bool Tokenizer::IsAlpha(char c) {
  return (std::isalpha(static_cast<unsigned char>(c)) != 0) || c == '_';
}

bool Tokenizer::IsAlphaNumeric(char c) {
  return (std::isalnum(static_cast<unsigned char>(c)) != 0) || c == '_';
}

bool Tokenizer::IsDigit(char c) {
  return std::isdigit(static_cast<unsigned char>(c)) != 0;
}

Token Tokenizer::ReadIdentifier() {
  int start_line = line_;
  int start_column = column_;
  std::string identifier_val;

  while (!input_->eof() && IsAlphaNumeric(current_char_)) {
    identifier_val.push_back(current_char_);
    Advance();
  }

  TokenType type = (kKeywords.contains(identifier_val))
                       ? TokenType::kKeyword
                       : TokenType::kIdentifier;
  return Token(type, identifier_val, start_line, start_column);
}

Token Tokenizer::ReadNumber() {
  int start_line = line_;
  int start_column = column_;
  std::string number_str;
  bool is_float = false;

  while (!input_->eof()) {
    if (IsDigit(current_char_)) {
      number_str.push_back(current_char_);
    } else if (current_char_ == '.') {
      if (is_float) {
        break;
      }
      if (!input_->eof() && IsDigit(static_cast<char>(input_->peek()))) {
        is_float = true;
        number_str.push_back(current_char_);  // Add '.' to the number string
      } else {
        break;
      }
    } else {
      break;
    }
    Advance();
  }

  if (number_str.empty() && is_float) {
    throw std::runtime_error("Invalid float starting with just '.' at line " +
                             std::to_string(start_line) + " col " +
                             std::to_string(start_column));
  }
  if (number_str.empty() && !is_float) {
    throw std::runtime_error("Empty number parsed at line " +
                             std::to_string(start_line) + " col " +
                             std::to_string(start_column));
  }

  return Token(is_float ? TokenType::kFloat : TokenType::kInteger, number_str,
               start_line, start_column);
}

Token Tokenizer::ReadString() {
  int start_line = line_;
  int start_column = column_;
  char quote_char = current_char_;
  std::string str_value;

  Advance();

  while (!input_->eof() && current_char_ != quote_char) {
    if (current_char_ == '\\') {
      Advance();
      if (input_->eof()) {
        throw std::runtime_error(
            "Unterminated string: EOF after escape character at line " +
            std::to_string(line_) + " col " + std::to_string(column_));
      }
      ProcessEscapeSequence(str_value);
    } else {
      str_value.push_back(current_char_);
      Advance();
    }
  }

  if (input_->eof() || current_char_ != quote_char) {
    throw std::runtime_error("Unterminated string literal at line " +
                             std::to_string(start_line) + " col " +
                             std::to_string(start_column));
  }

  Advance();
  return Token(TokenType::kString, str_value, start_line, start_column);
}

void Tokenizer::ProcessEscapeSequence(std::string& str_value) {
  switch (current_char_) {
    case 'n':
      str_value.push_back('\n');
      break;
    case 't':
      str_value.push_back('\t');
      break;
    case 'r':
      str_value.push_back('\r');
      break;
    case '\\':
      str_value.push_back('\\');
      break;
    case '\'':
      str_value.push_back('\'');
      break;
    case '"':
      str_value.push_back('\"');
      break;
    default:
      str_value.push_back(current_char_);
      break;
  }
  Advance();
}

Token Tokenizer::ReadOperator() {
  int start_line = line_;
  int start_column = column_;
  std::string op_str;
  op_str.push_back(current_char_);

  if (!input_->eof() && input_->peek() != EOF) {
    char peeked_char = static_cast<char>(input_->peek());
    std::string potential_double_op = op_str + peeked_char;
    auto double_it = kDoubleCharOperators.find(potential_double_op);
    if (double_it != kDoubleCharOperators.end()) {
      Advance();
      Advance();
      return Token(double_it->second, potential_double_op, start_line,
                   start_column);
    }
  }

  Advance();

  auto single_it = kSingleCharOperators.find(op_str[0]);
  if (single_it != kSingleCharOperators.end()) {
    return Token(single_it->second, op_str, start_line, start_column);
  }

  auto special_it = kSpecialOperators.find(op_str[0]);
  if (special_it != kSpecialOperators.end()) {
    return Token(special_it->second, op_str, start_line, start_column);
  }

  throw std::runtime_error("Unknown operator or character: '" + op_str +
                           "' at line " + std::to_string(start_line) + " col " +
                           std::to_string(start_column));
}

std::vector<Token> Tokenizer::Tokenize() {
  std::vector<Token> tokens_list;

  while (current_char_ != static_cast<char>(EOF) && !input_->eof()) {
    SkipWhitespace();

    if (current_char_ == static_cast<char>(EOF) || input_->eof()) {
      break;
    }

    if (current_char_ == '#' ||
        (current_char_ == '=' && !input_->eof() &&
         input_->peek() == 'b')) {  // || (current_char_ == '/' &&
                                    // !input_->eof() && input_->peek() == '*')
      SkipComment();
      continue;
    }

    if (current_char_ == static_cast<char>(EOF) || input_->eof()) {
      break;
    }

    if (IsAlpha(current_char_)) {
      tokens_list.push_back(ReadIdentifier());
    } else if (IsDigit(current_char_)) {
      tokens_list.push_back(ReadNumber());
    } else if (current_char_ == '"' || current_char_ == '\'') {
      tokens_list.push_back(ReadString());
    } else {
      tokens_list.push_back(ReadOperator());
    }
  }

  tokens_list.emplace_back(TokenType::kEndOfFile, "", line_, column_);
  return tokens_list;
}

std::vector<TokenVariant> Tokenizer::TokenizeVariant() {
  std::vector<Token> basic_tokens = Tokenize();

  tokens_.clear();
  current_token_ = 0;

  for (const auto& token : basic_tokens) {
    ProcessToken(token);
  }
  return tokens_;
}

void Tokenizer::ProcessToken(const Token& token) {
  if (token.type == TokenType::kInteger || token.type == TokenType::kFloat) {
    ProcessNumericToken(token);
  } else if (token.type == TokenType::kString) {
    ProcessStringToken(token);
  } else if (IsBracketToken(token.type)) {
    tokens_.push_back(CreateBracketToken(token));
  } else if (token.type == TokenType::kDot) {
    ProcessDotToken(token);
  } else if (token.type != TokenType::kEndOfFile &&
             token.type != TokenType::kNewline) {
    ProcessSymbolToken(token);
  }
}

void Tokenizer::ProcessNumericToken(const Token& token) {
  ConstantToken constant;
  constant.line = token.line;
  constant.column = token.column;

  try {
    if (token.type == TokenType::kInteger) {
      constant.value = std::stoi(token.value);
    } else {  // TokenType::kFloat
      constant.value = std::stod(token.value);
    }
  } catch (const std::out_of_range&) {
    throw std::runtime_error(fmt::format(
        "{} value out of range: {}",
        token.type == TokenType::kInteger ? "Integer" : "Float", token.value));
  } catch (const std::invalid_argument&) {
    throw std::runtime_error(fmt::format(
        "Invalid {} value: {}",
        token.type == TokenType::kInteger ? "Integer" : "Float", token.value));
  }

  tokens_.push_back(constant);
}

void Tokenizer::ProcessStringToken(const Token& token) {
  ConstantToken constant;
  constant.value = token.value;
  constant.line = token.line;
  constant.column = token.column;
  tokens_.push_back(constant);
}

void Tokenizer::ProcessDotToken(const Token& token) {
  DotToken dot;
  dot.line = token.line;
  dot.column = token.column;
  tokens_.push_back(dot);
}

void Tokenizer::ProcessSymbolToken(const Token& token) {
  SymbolToken symbol;
  symbol.symbol = token.value;
  symbol.line = token.line;
  symbol.column = token.column;
  tokens_.push_back(symbol);
}

bool Tokenizer::IsBracketToken(TokenType type) {
  return type == TokenType::kLeftParenthesis ||
         type == TokenType::kRightParenthesis ||
         type == TokenType::kLeftBrace || type == TokenType::kRightBrace ||
         type == TokenType::kLeftBracket || type == TokenType::kRightBracket;
}

BracketToken Tokenizer::CreateBracketToken(const Token& token) {
  BracketToken bracket;
  if (token.type == TokenType::kLeftParenthesis ||
      token.type == TokenType::kRightParenthesis) {
    bracket.bracket_type = BracketToken::BracketType::kParenthesis;
  } else if (token.type == TokenType::kLeftBrace ||
             token.type == TokenType::kRightBrace) {
    bracket.bracket_type = BracketToken::BracketType::kBrace;
  } else {
    bracket.bracket_type = BracketToken::BracketType::kBracket;
  }
  bracket.direction = (token.type == TokenType::kLeftParenthesis ||
                       token.type == TokenType::kLeftBrace ||
                       token.type == TokenType::kLeftBracket)
                          ? BracketToken::Direction::kLeft
                          : BracketToken::Direction::kRight;
  bracket.line = token.line;
  bracket.column = token.column;
  return bracket;
}

bool Tokenizer::IsEnd() const {
  return current_token_ >= tokens_.size();
}

TokenVariant Tokenizer::GetToken() const {
  if (!IsEnd()) {
    return tokens_[current_token_];
  }
  throw std::runtime_error("GetToken called when at end of tokens");
}

void Tokenizer::Next() {
  if (!IsEnd()) {
    current_token_++;
  }
}

}  // namespace lexer