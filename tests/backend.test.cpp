#include <gtest/gtest.h>

#include <memory>
#include <sstream>

#include "../src/backend/worker.h"
#include "../src/lexer/tokenizer.h"
#include "../src/parser/parser.h"
#include "fmt/base.h"

class BackendTest : public ::testing::Test {
 protected:
  void SetUp() override {
    tokenizer_ = nullptr;
    parser_ = nullptr;
  }

  void TearDown() override {
    delete parser_;
    parser_ = nullptr;
    delete tokenizer_;
    tokenizer_ = nullptr;
  }

  std::string GenerateAssemblyForInput(const std::string& input) {
    input_stream_.clear();
    input_stream_.str(input);

    delete tokenizer_;
    tokenizer_ = new lexer::Tokenizer(&input_stream_);

    tokenizer_->TokenizeVariant();  // Prime the tokenizer

    delete parser_;
    parser_ = new parser::Parser(tokenizer_);

    auto ast = parser_->Parse();
    backend::Worker worker(ast);
    return worker.GenerateAssembly();
  }

  static bool AssemblyContains(const std::string& assembly,
                               const std::string& substring) {
    return assembly.find(substring) != std::string::npos;
  }

  static bool AssemblyContainsInOrder(
      const std::string& assembly, const std::vector<std::string>& substrings) {
    size_t pos = 0;
    for (const auto& substring : substrings) {
      pos = assembly.find(substring, pos);
      if (pos == std::string::npos) {
        return false;
      }
      pos += substring.length();
    }
    return true;
  }

  std::stringstream input_stream_;
  lexer::Tokenizer* tokenizer_ = nullptr;
  parser::Parser* parser_ = nullptr;
};

TEST_F(BackendTest, SimpleIntegerAssignment) {
  std::string assembly = GenerateAssemblyForInput("x = 42");

  EXPECT_FALSE(AssemblyContains(assembly, ".string"));
  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 42"));
  EXPECT_TRUE(AssemblyContains(assembly, "sw x3,"));
}

TEST_F(BackendTest, StringLiteralHandling) {
  std::string assembly =
      GenerateAssemblyForInput("message = \"Hello, World!\"");

  EXPECT_TRUE(AssemblyContains(assembly, "data"));
  EXPECT_TRUE(AssemblyContains(assembly, "str_"));
  EXPECT_TRUE(AssemblyContains(assembly, "sw x3,"));
}

TEST_F(BackendTest, SimpleArithmeticExpression) {
  std::string assembly = GenerateAssemblyForInput("result = 10 + 20");

  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 10"));
  EXPECT_TRUE(AssemblyContains(assembly, "li x6, 20"));
  EXPECT_TRUE(AssemblyContains(assembly, "add x5, x5, x6"));
  EXPECT_TRUE(AssemblyContains(assembly, "sw x3,"));
}

// TODO: Incorrect testcase
// TEST_F(BackendTest, ComplexArithmeticExpression) {
//   std::string assembly = GenerateAssemblyForInput("result = 5 * (10 + 2) /
//   2");

//   // In the correct order of evaluation, we should see:
//   // 1. Compute (10 + 2)
//   // 2. Multiply by 5
//   // 3. Divide by 2

//   EXPECT_TRUE(AssemblyContainsInOrder(assembly, {
//                                                     "li", "10",  // Load 10
//                                                     "li", "2",   // Load 2
//                                                     "add",       // Add them
//                                                     "li", "5",   // Load 5
//                                                     "mul",       // Multiply
//                                                     "li", "2",   // Load 2
//                                                     "div"        // Divide
//                                                 }));
// }

TEST_F(BackendTest, ComparisonOperators) {
  std::string assembly = GenerateAssemblyForInput("result = 5 < 10");

  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 5"));
  EXPECT_TRUE(AssemblyContains(assembly, "li x6, 10"));
  EXPECT_TRUE(AssemblyContains(assembly, "slt"));
}

TEST_F(BackendTest, BasicIfStatement) {
  std::string assembly = GenerateAssemblyForInput(
      "if 1 > 0 then\n"
      "  result = 42\n"
      "end");

  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 1"));
  EXPECT_TRUE(AssemblyContains(assembly, "li x6, 0"));
  EXPECT_TRUE(AssemblyContains(assembly, "slt"));
  EXPECT_TRUE(AssemblyContains(assembly, "beq"));
  EXPECT_TRUE(AssemblyContains(assembly, "else_"));
  EXPECT_TRUE(AssemblyContains(assembly, "endif_"));
  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 42"));
}

TEST_F(BackendTest, IfElseStatement) {
  std::string assembly = GenerateAssemblyForInput(
      "if 1 < 0 then\n"
      "  result = 42\n"
      "else\n"
      "  result = 24\n"
      "end");

  EXPECT_TRUE(AssemblyContains(assembly, "beq"));
  EXPECT_TRUE(AssemblyContains(assembly, "jal x0,"));
  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 42"));
  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 24"));
}

TEST_F(BackendTest, WhileLoop) {
  std::string assembly = GenerateAssemblyForInput(
      "i = 0\n"
      "while i < 5 do\n"
      "  i = i + 1\n"
      "end");

  EXPECT_TRUE(AssemblyContains(assembly, "loop_start_"));
  EXPECT_TRUE(AssemblyContains(assembly, "loop_body_"));
  EXPECT_TRUE(AssemblyContains(assembly, "loop_end_"));
  EXPECT_TRUE(AssemblyContains(assembly, "bne"));
  EXPECT_TRUE(AssemblyContains(assembly, "addi"));
  EXPECT_TRUE(AssemblyContains(assembly, "jal x0,"));
}

TEST_F(BackendTest, TimesLoop) {
  std::string assembly = GenerateAssemblyForInput(
      "5.times do\n"
      "  result = 42\n"
      "end");

  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 5"));
  EXPECT_TRUE(AssemblyContains(assembly, "li x6, 0"));

  EXPECT_TRUE(AssemblyContains(assembly, "times_loop_start_"));
  EXPECT_TRUE(AssemblyContains(assembly, "times_loop_end_"));
  EXPECT_TRUE(AssemblyContains(assembly, "bge"));

  EXPECT_TRUE(AssemblyContains(assembly, "addi x6, x6, 1"));

  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 42"));
}

TEST_F(BackendTest, FunctionDeclaration) {
  std::string assembly = GenerateAssemblyForInput(
      "def add(a, b)\n"
      "  return a + b\n"
      "end");

  EXPECT_TRUE(AssemblyContains(assembly, "add:"));

  EXPECT_TRUE(AssemblyContains(assembly, "addi x2, x2, -4"));
  EXPECT_TRUE(AssemblyContains(assembly, "sw x2, 0, x8"));
  EXPECT_TRUE(AssemblyContains(assembly, "sw x2, 0, x1"));
  EXPECT_TRUE(AssemblyContains(assembly, "add x8, x2, x0"));

  EXPECT_TRUE(AssemblyContains(assembly, "lw x5, x8, 8"));   // Load a
  EXPECT_TRUE(AssemblyContains(assembly, "lw x6, x8, 12"));  // Load b

  EXPECT_TRUE(AssemblyContains(assembly, "add x10, x5, x0"));

  EXPECT_TRUE(AssemblyContains(assembly, "add x2, x8, x0"));
  EXPECT_TRUE(AssemblyContains(assembly, "lw x1, x2, 0"));
  EXPECT_TRUE(AssemblyContains(assembly, "lw x8, x2, 0"));
  EXPECT_TRUE(AssemblyContains(assembly, "jalr x0, x1, 0"));
}

TEST_F(BackendTest, FunctionCall) {
  std::string assembly = GenerateAssemblyForInput(
      "def add(a, b)\n"
      "  return a + b\n"
      "end\n"
      "\n"
      "result = add(5, 10)");

  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 5"));   // First arg
  EXPECT_TRUE(AssemblyContains(assembly, "li x6, 10"));  // Second arg

  EXPECT_TRUE(AssemblyContains(assembly, "addi x2, x2, -4"));
  EXPECT_TRUE(AssemblyContains(assembly, "sw x2, 0,"));

  EXPECT_TRUE(AssemblyContains(assembly, "jal x1, add"));

  EXPECT_TRUE(AssemblyContains(assembly, "add x5, x10, x0"));
}

TEST_F(BackendTest, BuiltInPuts) {
  std::string assembly = GenerateAssemblyForInput("puts(\"Hello, World!\")");
  EXPECT_FALSE(AssemblyContains(assembly, ".string"));
  EXPECT_TRUE(AssemblyContains(assembly, "data"));

  EXPECT_TRUE(AssemblyContains(assembly, "li x5,"));

  EXPECT_TRUE(AssemblyContains(assembly, "add x10, x5, x0"));
  EXPECT_TRUE(AssemblyContains(assembly, "jal x1, runtime_puts"));

  EXPECT_TRUE(AssemblyContains(assembly, "runtime_puts:"));
  EXPECT_TRUE(AssemblyContains(assembly, "ewrite"));
}

TEST_F(BackendTest, ReturnStatement) {
  std::string assembly = GenerateAssemblyForInput(
      "def get_value()\n"
      "  return 42\n"
      "end");

  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 42"));

  EXPECT_TRUE(AssemblyContains(assembly, "add x10, x5, x0"));

  EXPECT_TRUE(AssemblyContains(assembly, "jal x0,"));
}

TEST_F(BackendTest, EmptyReturn) {
  std::string assembly = GenerateAssemblyForInput(
      "def do_nothing()\n"
      "  return\n"
      "end");

  EXPECT_TRUE(AssemblyContains(assembly, "li x10, 0"));
}

TEST_F(BackendTest, ComplexProgram) {
  std::string assembly = GenerateAssemblyForInput(
      "def factorial(n)\n"
      "  if n <= 1 then\n"
      "    return 1\n"
      "  else\n"
      "    return n * factorial(n - 1)\n"
      "  end\n"
      "end\n"
      "\n"
      "result = factorial(5)\n"
      "puts(\"Factorial of 5 is: \")\n"
      "puts(result)");

  EXPECT_TRUE(AssemblyContains(assembly, "factorial:"));

  EXPECT_TRUE(AssemblyContains(assembly, "jal x1, factorial"));

  EXPECT_TRUE(AssemblyContains(assembly, "sle") ||
              (AssemblyContains(assembly, "slt") &&
               AssemblyContains(assembly, "xori")));

  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 5"));
  EXPECT_TRUE(AssemblyContains(assembly, "jal x1, factorial"));

  EXPECT_FALSE(AssemblyContains(assembly, ".string"));
  EXPECT_TRUE(AssemblyContains(assembly, "data"));

  EXPECT_TRUE(AssemblyContains(assembly, "jal x1, runtime_puts"));
}

// TODO: ~ is unsupported yet
// TEST_F(BackendTest, UnaryOperators) {
//   std::string assembly = GenerateAssemblyForInput(
//       "a = -5\n"
//       "b = !0\n"
//       "c = -42");

//   EXPECT_TRUE(AssemblyContains(assembly, "li x5, 5"));
//   EXPECT_TRUE(AssemblyContains(assembly, "sub x5, x0, x5"));
//   EXPECT_TRUE(AssemblyContains(assembly, "li x5, 0"));
//   EXPECT_TRUE(AssemblyContains(assembly, "seq x5, x5, x0"));
//   EXPECT_TRUE(AssemblyContains(assembly, "li x5, 42"));
//   EXPECT_TRUE(AssemblyContains(assembly, "xori x5, x5, -1"));
// }

TEST_F(BackendTest, GlobalScope) {
  std::string assembly = GenerateAssemblyForInput(
      "x = 42\n"
      "def get_x()\n"
      "  return x\n"
      "end");

  EXPECT_FALSE(AssemblyContains(assembly, ".globl _start"));
  EXPECT_TRUE(AssemblyContains(assembly, "_start:"));

  EXPECT_TRUE(AssemblyContains(assembly, "ebreak"));
}

TEST_F(BackendTest, RuntimeHelpers) {
  std::string assembly = GenerateAssemblyForInput("puts(\"Test\")");

  EXPECT_TRUE(AssemblyContains(assembly, "# --- Runtime Helper Functions ---"));
  EXPECT_TRUE(AssemblyContains(assembly, "runtime_puts:"));

  EXPECT_TRUE(AssemblyContains(assembly, "lw x5, x10, 0"));
  EXPECT_TRUE(AssemblyContains(assembly, "ewrite x5"));

  EXPECT_TRUE(AssemblyContains(assembly, "li x5, 10") ||
              AssemblyContains(assembly, "li x5, '\\n'"));
  EXPECT_TRUE(AssemblyContains(assembly, "ewrite x5"));
}