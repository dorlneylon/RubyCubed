#include "parser/parser.h"

#include <fmt/core.h>
#include <gtest/gtest.h>

#include <sstream>

#include "lexer/tokenizer.h"
#include "parser/ast.h"

using namespace parser;
using namespace lexer;

class ParserTest : public ::testing::Test {
 protected:
  std::stringstream input_stream_;
  Tokenizer* tokenizer_ = nullptr;
  Parser* parser_ = nullptr;

  void SetupParserForInput(const std::string& input) {
    input_stream_.clear();
    input_stream_.str(input);

    delete tokenizer_;
    tokenizer_ = new Tokenizer(&input_stream_);

    tokenizer_->TokenizeVariant();  // Prime the tokenizer

    delete parser_;
    parser_ = new Parser(tokenizer_);
  }

  void TearDown() override {
    delete parser_;
    parser_ = nullptr;
    delete tokenizer_;
    tokenizer_ = nullptr;
  }
};

TEST_F(ParserTest, ParseEmptyProgram) {
  SetupParserForInput("");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  EXPECT_TRUE(program->GetStatements().empty());
}

TEST_F(ParserTest, ParseVariableAssignment) {
  SetupParserForInput("x = 42");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->GetStatements().size(), 1);

  auto var_decl = std::dynamic_pointer_cast<VariableDeclaration>(
      program->GetStatements()[0]);
  ASSERT_NE(var_decl, nullptr);
  EXPECT_EQ(var_decl->GetName(), "x");

  auto value =
      std::dynamic_pointer_cast<IntegerLiteral>(var_decl->GetInitializer());
  ASSERT_NE(value, nullptr);
  EXPECT_EQ(value->GetValue(), 42);
}

TEST_F(ParserTest, ParseStringAssignment) {
  SetupParserForInput("str = \"hello world\"");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->GetStatements().size(), 1);

  auto var_decl = std::dynamic_pointer_cast<VariableDeclaration>(
      program->GetStatements()[0]);
  ASSERT_NE(var_decl, nullptr);
  EXPECT_EQ(var_decl->GetName(), "str");

  auto value =
      std::dynamic_pointer_cast<StringLiteral>(var_decl->GetInitializer());
  ASSERT_NE(value, nullptr);
  EXPECT_EQ(value->GetValue(), "hello world");
}

TEST_F(ParserTest, ParseIfStatement) {
  SetupParserForInput("if x > 10 then puts \"large\" else puts \"small\" end");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->GetStatements().size(), 1);

  auto if_stmt =
      std::dynamic_pointer_cast<IfStatement>(program->GetStatements()[0]);
  ASSERT_NE(if_stmt, nullptr);

  auto condition =
      std::dynamic_pointer_cast<BinaryExpression>(if_stmt->GetCondition());
  ASSERT_NE(condition, nullptr);
  EXPECT_EQ(condition->GetOperator(), ">");

  auto consequence_block =
      std::dynamic_pointer_cast<BlockStatement>(if_stmt->GetConsequence());
  ASSERT_NE(consequence_block, nullptr);
  ASSERT_FALSE(consequence_block->GetStatements().empty());
  // Check consequence content more thoroughly if needed
  auto first_consequence_stmt = std::dynamic_pointer_cast<ExpressionStatement>(
      consequence_block->GetStatements()[0]);
  ASSERT_NE(first_consequence_stmt, nullptr);

  auto alternative_block =
      std::dynamic_pointer_cast<BlockStatement>(if_stmt->GetAlternative());
  ASSERT_NE(alternative_block, nullptr)
      << "Alternative block should not be null";  // Key check
  ASSERT_FALSE(alternative_block->GetStatements().empty());
  auto first_alternative_stmt = std::dynamic_pointer_cast<ExpressionStatement>(
      alternative_block->GetStatements()[0]);
  ASSERT_NE(first_alternative_stmt, nullptr);
}

TEST_F(ParserTest, ParseWhileLoop) {
  SetupParserForInput("while i < 10 do puts i; i = i + 1 end");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->GetStatements().size(), 1);

  auto while_stmt =
      std::dynamic_pointer_cast<WhileStatement>(program->GetStatements()[0]);
  ASSERT_NE(while_stmt, nullptr);

  auto condition =
      std::dynamic_pointer_cast<BinaryExpression>(while_stmt->GetCondition());
  ASSERT_NE(condition, nullptr);
  EXPECT_EQ(condition->GetOperator(), "<");

  auto body = std::dynamic_pointer_cast<BlockStatement>(while_stmt->GetBody());
  ASSERT_NE(body, nullptr);
  EXPECT_EQ(body->GetStatements().size(), 2);
}

TEST_F(ParserTest, ParseTimesStatement) {
  SetupParserForInput("5.times do puts \"Hello\" end");
  auto node = parser_->Parse();

  ASSERT_NO_THROW({
    auto program = std::dynamic_pointer_cast<Program>(node);
    ASSERT_NE(program, nullptr);

    ASSERT_EQ(program->GetStatements().size(), 1);

    auto times_stmt =
        std::dynamic_pointer_cast<TimesStatement>(program->GetStatements()[0]);
    ASSERT_NE(times_stmt, nullptr);

    auto count_expr =
        std::dynamic_pointer_cast<IntegerLiteral>(times_stmt->GetCount());
    ASSERT_NE(count_expr, nullptr);
    EXPECT_EQ(count_expr->GetValue(), 5);

    auto body_block =
        std::dynamic_pointer_cast<BlockStatement>(times_stmt->GetBody());
    ASSERT_NE(body_block, nullptr);
    ASSERT_FALSE(body_block->GetStatements().empty());

    auto expr_stmt = std::dynamic_pointer_cast<ExpressionStatement>(
        body_block->GetStatements()[0]);
    ASSERT_NE(expr_stmt, nullptr);
    auto call_expr =
        std::dynamic_pointer_cast<CallExpression>(expr_stmt->GetExpression());
    ASSERT_NE(call_expr, nullptr);
    auto callee = std::dynamic_pointer_cast<Identifier>(call_expr->GetCallee());
    ASSERT_NE(callee, nullptr);
    EXPECT_EQ(callee->GetName(), "puts");
    ASSERT_EQ(call_expr->GetArguments().size(), 1);
    auto arg =
        std::dynamic_pointer_cast<StringLiteral>(call_expr->GetArguments()[0]);
    ASSERT_NE(arg, nullptr);
    EXPECT_EQ(arg->GetValue(), "Hello");
  });
}

TEST_F(ParserTest, ParseFunctionDeclaration) {
  SetupParserForInput("def add(a, b) return a + b end");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->GetStatements().size(), 1);

  auto func_decl = std::dynamic_pointer_cast<FunctionDeclaration>(
      program->GetStatements()[0]);
  ASSERT_NE(func_decl, nullptr);
  EXPECT_EQ(func_decl->GetName(), "add");
  ASSERT_EQ(func_decl->GetParameters().size(), 2);
  EXPECT_EQ(func_decl->GetParameters()[0], "a");
  EXPECT_EQ(func_decl->GetParameters()[1], "b");

  auto body = func_decl->GetBody();
  ASSERT_NE(body, nullptr);
  ASSERT_GE(body->GetStatements().size(), 1);

  auto return_stmt =
      std::dynamic_pointer_cast<ReturnStatement>(body->GetStatements()[0]);
  ASSERT_NE(return_stmt, nullptr);
}

TEST_F(ParserTest, ParseFunctionCall) {
  ASSERT_NO_THROW({
    SetupParserForInput("result = add(5, 3)");
    auto node = parser_->Parse();
    auto program = std::dynamic_pointer_cast<Program>(node);
    ASSERT_NE(program, nullptr);
    ASSERT_EQ(program->GetStatements().size(), 1);

    auto var_decl = std::dynamic_pointer_cast<VariableDeclaration>(
        program->GetStatements()[0]);
    ASSERT_NE(var_decl, nullptr);
    EXPECT_EQ(var_decl->GetName(), "result");

    auto call_expr =
        std::dynamic_pointer_cast<CallExpression>(var_decl->GetInitializer());
    ASSERT_NE(call_expr, nullptr);

    auto callee_ident =
        std::dynamic_pointer_cast<Identifier>(call_expr->GetCallee());
    ASSERT_NE(callee_ident, nullptr);
    EXPECT_EQ(callee_ident->GetName(), "add");

    auto arg1 =
        std::dynamic_pointer_cast<IntegerLiteral>(call_expr->GetArguments()[0]);
    ASSERT_NE(arg1, nullptr);
    EXPECT_EQ(arg1->GetValue(), 5);
    auto arg2 =
        std::dynamic_pointer_cast<IntegerLiteral>(call_expr->GetArguments()[1]);
    ASSERT_NE(arg2, nullptr);
    EXPECT_EQ(arg2->GetValue(), 3);
  });
}

TEST_F(ParserTest, ParseComplexExpression) {
  SetupParserForInput("result = 5 * (3 + 2) / 4");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->GetStatements().size(), 1);

  auto var_decl = std::dynamic_pointer_cast<VariableDeclaration>(
      program->GetStatements()[0]);
  ASSERT_NE(var_decl, nullptr);
  EXPECT_EQ(var_decl->GetName(), "result");

  auto expr =
      std::dynamic_pointer_cast<BinaryExpression>(var_decl->GetInitializer());
  ASSERT_NE(expr, nullptr);
  EXPECT_EQ(expr->GetOperator(), "/");
  auto left_of_div =
      std::dynamic_pointer_cast<BinaryExpression>(expr->GetLeft());
  ASSERT_NE(left_of_div, nullptr);
  EXPECT_EQ(left_of_div->GetOperator(), "*");

  auto right_of_div =
      std::dynamic_pointer_cast<IntegerLiteral>(expr->GetRight());
  ASSERT_NE(right_of_div, nullptr);
  EXPECT_EQ(right_of_div->GetValue(), 4);
}

TEST_F(ParserTest, ParseBlockWithMultipleStatements) {
  SetupParserForInput("begin x = 1; y = 2; z = x + y end");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->GetStatements().size(), 1);

  auto block =
      std::dynamic_pointer_cast<BlockStatement>(program->GetStatements()[0]);
  ASSERT_NE(block, nullptr);
  // Was 5, expected 3. Due to semicolons.
  ASSERT_EQ(block->GetStatements().size(), 3);

  auto stmt1 =
      std::dynamic_pointer_cast<VariableDeclaration>(block->GetStatements()[0]);
  ASSERT_NE(stmt1, nullptr);
  EXPECT_EQ(stmt1->GetName(), "x");

  auto stmt2 =
      std::dynamic_pointer_cast<VariableDeclaration>(block->GetStatements()[1]);
  ASSERT_NE(stmt2, nullptr)
      << "Statement y=2 should be parsed as VariableDeclaration";  // Key check
  EXPECT_EQ(stmt2->GetName(), "y");

  auto stmt3 =
      std::dynamic_pointer_cast<VariableDeclaration>(block->GetStatements()[2]);
  ASSERT_NE(stmt3, nullptr);
  EXPECT_EQ(stmt3->GetName(), "z");
}

TEST_F(ParserTest, ParseNestedIfStatements) {
  SetupParserForInput(
      "if x > 10 then if y < 5 then puts \"both\" else puts \"just x\" end "
      "else puts \"neither\" end");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->GetStatements().size(), 1);

  auto if_stmt =
      std::dynamic_pointer_cast<IfStatement>(program->GetStatements()[0]);
  ASSERT_NE(if_stmt, nullptr);

  auto consequence_block =
      std::dynamic_pointer_cast<BlockStatement>(if_stmt->GetConsequence());
  ASSERT_NE(consequence_block, nullptr);
  ASSERT_EQ(consequence_block->GetStatements().size(), 1);
  auto nested_if_stmt = std::dynamic_pointer_cast<IfStatement>(
      consequence_block->GetStatements()[0]);
  ASSERT_NE(nested_if_stmt, nullptr);

  auto alternative_block =
      std::dynamic_pointer_cast<BlockStatement>(if_stmt->GetAlternative());
  ASSERT_NE(alternative_block, nullptr)
      << "Outer else block should be parsed";  // Key check
  ASSERT_FALSE(alternative_block->GetStatements().empty());
}

TEST_F(ParserTest, ParseUnaryExpression) {
  SetupParserForInput("x = -5");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->GetStatements().size(), 1);

  auto var_decl = std::dynamic_pointer_cast<VariableDeclaration>(
      program->GetStatements()[0]);
  ASSERT_NE(var_decl, nullptr);

  auto unary_expr =
      std::dynamic_pointer_cast<UnaryExpression>(var_decl->GetInitializer());
  ASSERT_NE(unary_expr, nullptr);
  EXPECT_EQ(unary_expr->GetOperator(), "-");

  auto operand =
      std::dynamic_pointer_cast<IntegerLiteral>(unary_expr->GetExpression());
  ASSERT_NE(operand, nullptr);
  EXPECT_EQ(operand->GetValue(), 5);
}

TEST_F(ParserTest, ParseReturnStatement) {
  SetupParserForInput("return 42");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->GetStatements().size(), 1);

  auto return_stmt =
      std::dynamic_pointer_cast<ReturnStatement>(program->GetStatements()[0]);
  ASSERT_NE(return_stmt, nullptr);

  auto value =
      std::dynamic_pointer_cast<IntegerLiteral>(return_stmt->GetValue());
  ASSERT_NE(value, nullptr);
  EXPECT_EQ(value->GetValue(), 42);
}

TEST_F(ParserTest, ParseReturnStatementWithoutValue) {
  SetupParserForInput("return");
  auto node = parser_->Parse();
  auto program = std::dynamic_pointer_cast<Program>(node);
  ASSERT_NE(program, nullptr);
  ASSERT_EQ(program->GetStatements().size(), 1);

  auto return_stmt =
      std::dynamic_pointer_cast<ReturnStatement>(program->GetStatements()[0]);
  ASSERT_NE(return_stmt, nullptr);
  EXPECT_EQ(return_stmt->GetValue(), nullptr);  // Check for no return value
}