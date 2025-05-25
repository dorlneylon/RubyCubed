#include <fmt/core.h>
#include <gtest/gtest.h>

#include <sstream>

#include "lexer/tokenizer.h"

using namespace lexer;

class LexerTest : public ::testing::Test {
 protected:
  std::stringstream input_stream_;
  Tokenizer* tokenizer_ = nullptr;

  void CreateTokenizerForInput(const std::string& input) {
    input_stream_.clear();
    input_stream_.str(input);
    delete tokenizer_;
    tokenizer_ = new Tokenizer(&input_stream_);
  }

  void TearDown() override {
    delete tokenizer_;
    tokenizer_ = nullptr;
  }
};

TEST_F(LexerTest, TokenizeEmptyInput) {
  CreateTokenizerForInput("");
  auto tokens = tokenizer_->Tokenize();
  ASSERT_EQ(tokens.size(), 1);
  EXPECT_EQ(tokens[0].type, TokenType::kEndOfFile);
}

TEST_F(LexerTest, TokenizeIdentifier) {
  CreateTokenizerForInput("variable");
  auto tokens = tokenizer_->Tokenize();
  ASSERT_EQ(tokens.size(), 2);
  EXPECT_EQ(tokens[0].type, TokenType::kIdentifier);
  EXPECT_EQ(tokens[0].value, "variable");
}

TEST_F(LexerTest, TokenizeInteger) {
  CreateTokenizerForInput("42");
  auto tokens = tokenizer_->Tokenize();
  ASSERT_EQ(tokens.size(), 2);
  EXPECT_EQ(tokens[0].type, TokenType::kInteger);
  EXPECT_EQ(tokens[0].value, "42");
}

TEST_F(LexerTest, TokenizeFloat) {
  CreateTokenizerForInput("3.14");  // Assumes ReadNumber handles this correctly
  auto tokens = tokenizer_->Tokenize();
  ASSERT_EQ(tokens.size(), 2);
  EXPECT_EQ(tokens[0].type, TokenType::kFloat);
  EXPECT_EQ(tokens[0].value, "3.14");
}

TEST_F(LexerTest, TokenizeString) {
  CreateTokenizerForInput("\"hello world\"");
  auto tokens = tokenizer_->Tokenize();
  ASSERT_EQ(tokens.size(), 2);
  EXPECT_EQ(tokens[0].type, TokenType::kString);
  EXPECT_EQ(tokens[0].value, "hello world");
}

TEST_F(LexerTest, TokenizeKeyword) {
  CreateTokenizerForInput("if");
  auto tokens = tokenizer_->Tokenize();
  ASSERT_EQ(tokens.size(), 2);
  EXPECT_EQ(tokens[0].type, TokenType::kKeyword);
  EXPECT_EQ(tokens[0].value, "if");
}

TEST_F(LexerTest, TokenizeOperator) {
  CreateTokenizerForInput("+ - * /");
  auto tokens = tokenizer_->Tokenize();
  ASSERT_EQ(tokens.size(), 5);
  EXPECT_EQ(tokens[0].type, TokenType::kOperator);
  EXPECT_EQ(tokens[0].value, "+");
  EXPECT_EQ(tokens[1].type, TokenType::kOperator);
  EXPECT_EQ(tokens[1].value, "-");
  EXPECT_EQ(tokens[2].type, TokenType::kOperator);
  EXPECT_EQ(tokens[2].value, "*");
  EXPECT_EQ(tokens[3].type, TokenType::kOperator);
  EXPECT_EQ(tokens[3].value, "/");
  EXPECT_EQ(tokens[4].type, TokenType::kEndOfFile);
}

TEST_F(LexerTest, TokenizeComments) {
  CreateTokenizerForInput(
      "# Single line comment\nx = 1 # Another comment\n\ny = 2");
  auto tokens = tokenizer_->Tokenize();
  ASSERT_EQ(tokens.size(), 7);
  EXPECT_EQ(tokens[0].type, TokenType::kIdentifier);
  EXPECT_EQ(tokens[0].value, "x");
  EXPECT_EQ(tokens[1].type, TokenType::kOperator);
  EXPECT_EQ(tokens[1].value, "=");
  EXPECT_EQ(tokens[2].type, TokenType::kInteger);
  EXPECT_EQ(tokens[2].value, "1");
  EXPECT_EQ(tokens[3].type, TokenType::kIdentifier);
  EXPECT_EQ(tokens[3].value, "y");
  EXPECT_EQ(tokens[4].type, TokenType::kOperator);
  EXPECT_EQ(tokens[4].value, "=");
  EXPECT_EQ(tokens[5].type, TokenType::kInteger);
  EXPECT_EQ(tokens[5].value, "2");
  EXPECT_EQ(tokens[6].type, TokenType::kEndOfFile);
}

TEST_F(LexerTest, TokenizeVariantTokens) {
  CreateTokenizerForInput("42 \"hello\" ( ) { } [ ] ;");  // Added semicolon
  // Tokenize() output: 42, "hello", (, ), {, }, [, ], ;, EOF (9 tokens + EOF =
  // 10) TokenizeVariant() filters EOF, so 9 TokenVariant objects.
  auto variant_tokens = tokenizer_->TokenizeVariant();
  ASSERT_EQ(variant_tokens.size(),
            9);  // Corrected: 8 from original + 1 semicolon

  EXPECT_TRUE(std::holds_alternative<ConstantToken>(variant_tokens[0]));
  auto constant_int = std::get<ConstantToken>(variant_tokens[0]);
  EXPECT_TRUE(std::holds_alternative<int>(constant_int.value));
  EXPECT_EQ(std::get<int>(constant_int.value), 42);

  EXPECT_TRUE(std::holds_alternative<ConstantToken>(variant_tokens[1]));
  auto constant_str = std::get<ConstantToken>(variant_tokens[1]);
  EXPECT_TRUE(std::holds_alternative<std::string>(constant_str.value));
  EXPECT_EQ(std::get<std::string>(constant_str.value), "hello");

  // Brackets: ( ) { } [ ] are at indices 2, 3, 4, 5, 6, 7
  const int kIterations = 6;
  for (int i = 0; i < kIterations; ++i) {  // 6 bracket tokens
    ASSERT_TRUE(std::holds_alternative<BracketToken>(variant_tokens[i + 2]));
  }

  auto left_paren = std::get<BracketToken>(variant_tokens[2]);
  EXPECT_EQ(left_paren.bracket_type, BracketToken::BracketType::kParenthesis);
  EXPECT_EQ(left_paren.direction, BracketToken::Direction::kLeft);

  // Semicolon is the last variant token (index 8)
  EXPECT_TRUE(std::holds_alternative<SymbolToken>(variant_tokens[8]));
  EXPECT_EQ(std::get<SymbolToken>(variant_tokens[8]).symbol, ";");
}

TEST_F(LexerTest, TokenizeComplexRubyCode) {
  CreateTokenizerForInput(R"(
def fibonacci(n)
  if n <= 1
    return n
  else
    return fibonacci(n-1) + fibonacci(n-2)
  end
end

puts fibonacci(10) 
)");
  auto tokens = tokenizer_->Tokenize();
  EXPECT_GT(tokens.size(), 10);

  bool found_def = false;
  bool found_if = false;
  bool found_else = false;
  bool found_end = false;
  bool found_puts = false;
  for (const auto& token : tokens) {
    if (token.type == TokenType::kKeyword) {
      if (token.value == "def") {
        found_def = true;
      }
      if (token.value == "if") {
        found_if = true;
      }
      if (token.value == "else") {
        found_else = true;
      }
      if (token.value == "end") {
        found_end = true;
      }
      if (token.value == "puts") {
        found_puts = true;
      }
    }
  }
  EXPECT_TRUE(found_def);
  EXPECT_TRUE(found_if);
  EXPECT_TRUE(found_else);
  EXPECT_TRUE(found_end);
  EXPECT_TRUE(found_puts);
}

TEST_F(LexerTest, TokenizeDotOperator) {
  CreateTokenizerForInput("obj.method");
  auto tokens = tokenizer_->Tokenize();
  ASSERT_EQ(tokens.size(), 4);  // obj, ., method, EOF
  EXPECT_EQ(tokens[0].type, TokenType::kIdentifier);
  EXPECT_EQ(tokens[0].value, "obj");
  EXPECT_EQ(tokens[1].type, TokenType::kDot);
  EXPECT_EQ(tokens[1].value, ".");
  EXPECT_EQ(tokens[2].type, TokenType::kIdentifier);
  EXPECT_EQ(tokens[2].value, "method");
}

TEST_F(LexerTest, TokenizeIntegerMethodCallNoArgs) {
  // This test depends on how "5.times" is tokenized by the lexer.
  // If ReadNumber stops before '.', then it's INT(5), DOT(.), IDENTIFIER(times)
  CreateTokenizerForInput("5.times");
  auto tokens = tokenizer_->Tokenize();
  ASSERT_EQ(tokens.size(), 4);
  EXPECT_EQ(tokens[0].type, TokenType::kInteger);
  EXPECT_EQ(tokens[0].value, "5");
  EXPECT_EQ(tokens[1].type, TokenType::kDot);
  EXPECT_EQ(tokens[1].value, ".");
  EXPECT_EQ(tokens[2].type, TokenType::kIdentifier);
  EXPECT_EQ(tokens[2].value, "times");
}