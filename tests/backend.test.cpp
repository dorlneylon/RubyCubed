#include <gtest/gtest.h>

#include <memory>
#include <sstream>

#include "../src/backend/worker.h"
#include "../src/lexer/tokenizer.h"
#include "../src/parser/parser.h"

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

  // Check for data section (should be empty for integers)
  EXPECT_FALSE(AssemblyContains(assembly, ".string"));

  // Check for integer loading
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 42"));

  // Check for stack allocation and store
  EXPECT_TRUE(AssemblyContains(assembly, "sw t0,"));
}

TEST_F(BackendTest, StringLiteralHandling) {
  std::string assembly =
      GenerateAssemblyForInput("message = \"Hello, World!\"");

  // Check for string in data section
  EXPECT_TRUE(AssemblyContains(assembly, ".data"));
  EXPECT_TRUE(AssemblyContains(assembly, ".string \"Hello, World!\""));

  // Check for string address loading
  EXPECT_TRUE(AssemblyContains(assembly, "str_"));

  // Check for stack allocation and store
  EXPECT_TRUE(AssemblyContains(assembly, "sw t0,"));
}

TEST_F(BackendTest, SimpleArithmeticExpression) {
  std::string assembly = GenerateAssemblyForInput("result = 10 + 20");

  // Check for loading operands
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 10"));
  EXPECT_TRUE(AssemblyContains(assembly, "li t1, 20"));

  // Check for addition operation
  EXPECT_TRUE(AssemblyContains(assembly, "add t0, t0, t1"));

  // Check for storing result
  EXPECT_TRUE(AssemblyContains(assembly, "sw t0,"));
}

TEST_F(BackendTest, ComplexArithmeticExpression) {
  std::string assembly = GenerateAssemblyForInput("result = 5 * (10 + 2) / 2");

  // In the correct order of evaluation, we should see:
  // 1. Compute (10 + 2)
  // 2. Multiply by 5
  // 3. Divide by 2

  EXPECT_TRUE(AssemblyContainsInOrder(assembly, {
                                                    "li", "10",  // Load 10
                                                    "li", "2",   // Load 2
                                                    "add",       // Add them
                                                    "li", "5",   // Load 5
                                                    "mul",       // Multiply
                                                    "li", "2",   // Load 2
                                                    "div"        // Divide
                                                }));
}

TEST_F(BackendTest, ComparisonOperators) {
  std::string assembly = GenerateAssemblyForInput("result = 5 < 10");

  // Check for loading operands
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 5"));
  EXPECT_TRUE(AssemblyContains(assembly, "li t1, 10"));

  // Check for comparison operation
  EXPECT_TRUE(AssemblyContains(assembly, "slt"));
}

TEST_F(BackendTest, BasicIfStatement) {
  std::string assembly = GenerateAssemblyForInput(
      "if 1 > 0 then\n"
      "  result = 42\n"
      "end");

  // Check for condition evaluation
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 1"));
  EXPECT_TRUE(AssemblyContains(assembly, "li t1, 0"));
  EXPECT_TRUE(AssemblyContains(assembly, "slt"));

  // Check for conditional branch
  EXPECT_TRUE(AssemblyContains(assembly, "beq"));

  // Check for label generation
  EXPECT_TRUE(AssemblyContains(assembly, "else_"));
  EXPECT_TRUE(AssemblyContains(assembly, "endif_"));

  // Check for assignment in consequence block
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 42"));
}

TEST_F(BackendTest, IfElseStatement) {
  std::string assembly = GenerateAssemblyForInput(
      "if 1 < 0 then\n"
      "  result = 42\n"
      "else\n"
      "  result = 24\n"
      "end");

  // Check for conditional branch and jump
  EXPECT_TRUE(AssemblyContains(assembly, "beq"));
  EXPECT_TRUE(AssemblyContains(assembly, "jal zero,"));

  // Check that both assignments are present
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 42"));
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 24"));
}

TEST_F(BackendTest, WhileLoop) {
  std::string assembly = GenerateAssemblyForInput(
      "i = 0\n"
      "while i < 5 do\n"
      "  i = i + 1\n"
      "end");

  // Check for loop labels
  EXPECT_TRUE(AssemblyContains(assembly, "loop_start_"));
  EXPECT_TRUE(AssemblyContains(assembly, "loop_body_"));
  EXPECT_TRUE(AssemblyContains(assembly, "loop_end_"));

  // Check for condition and branch
  EXPECT_TRUE(AssemblyContains(assembly, "bne"));

  // Check for increment in loop body
  EXPECT_TRUE(AssemblyContains(assembly, "addi"));

  // Check for jump back to condition
  EXPECT_TRUE(AssemblyContains(assembly, "jal zero,"));
}

TEST_F(BackendTest, TimesLoop) {
  std::string assembly = GenerateAssemblyForInput(
      "5.times do\n"
      "  result = 42\n"
      "end");

  // Check for counter initialization
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 5"));
  EXPECT_TRUE(AssemblyContains(assembly, "li t1, 0"));

  // Check for loop control
  EXPECT_TRUE(AssemblyContains(assembly, "times_loop_start_"));
  EXPECT_TRUE(AssemblyContains(assembly, "times_loop_end_"));
  EXPECT_TRUE(AssemblyContains(assembly, "bge"));

  // Check for counter increment
  EXPECT_TRUE(AssemblyContains(assembly, "addi t1, t1, 1"));

  // Check for loop body execution
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 42"));
}

TEST_F(BackendTest, FunctionDeclaration) {
  std::string assembly = GenerateAssemblyForInput(
      "def add(a, b)\n"
      "  return a + b\n"
      "end");

  // Check for function label
  EXPECT_TRUE(AssemblyContains(assembly, "add:"));

  // Check for prologue
  EXPECT_TRUE(AssemblyContains(assembly, "addi sp, sp, -4"));
  EXPECT_TRUE(AssemblyContains(assembly, "sw fp, 0(sp)"));
  EXPECT_TRUE(AssemblyContains(assembly, "sw ra, 0(sp)"));
  EXPECT_TRUE(AssemblyContains(assembly, "mv fp, sp"));

  // Check for parameter access
  EXPECT_TRUE(AssemblyContains(assembly, "lw t0, 8(fp)"));   // Load a
  EXPECT_TRUE(AssemblyContains(assembly, "lw t1, 12(fp)"));  // Load b

  // Check for return value
  EXPECT_TRUE(AssemblyContains(assembly, "mv a0, t0"));

  // Check for epilogue
  EXPECT_TRUE(AssemblyContains(assembly, "mv sp, fp"));
  EXPECT_TRUE(AssemblyContains(assembly, "lw ra, 0(sp)"));
  EXPECT_TRUE(AssemblyContains(assembly, "lw fp, 0(sp)"));
  EXPECT_TRUE(AssemblyContains(assembly, "jalr zero, ra, 0"));
}

TEST_F(BackendTest, FunctionCall) {
  std::string assembly = GenerateAssemblyForInput(
      "def add(a, b)\n"
      "  return a + b\n"
      "end\n"
      "\n"
      "result = add(5, 10)");

  // Check for function call setup
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 5"));   // First arg
  EXPECT_TRUE(AssemblyContains(assembly, "li t1, 10"));  // Second arg

  // Check for saving arguments
  EXPECT_TRUE(AssemblyContains(assembly, "addi sp, sp, -4"));
  EXPECT_TRUE(AssemblyContains(assembly, "sw t"));

  // Check for the call
  EXPECT_TRUE(AssemblyContains(assembly, "jal ra, add"));

  // Check for result retrieval
  EXPECT_TRUE(AssemblyContains(assembly, "mv t0, a0"));
}

TEST_F(BackendTest, BuiltInPuts) {
  std::string assembly = GenerateAssemblyForInput("puts(\"Hello, World!\")");

  // Check for string in data section
  EXPECT_TRUE(AssemblyContains(assembly, ".string \"Hello, World!\""));

  // Check for loading string address
  EXPECT_TRUE(AssemblyContains(assembly, "la"));

  // Check for call to runtime puts
  EXPECT_TRUE(AssemblyContains(assembly, "mv a0, t"));
  EXPECT_TRUE(AssemblyContains(assembly, "jal ra, runtime_puts"));

  // Check for runtime_puts implementation
  EXPECT_TRUE(AssemblyContains(assembly, "runtime_puts:"));
  EXPECT_TRUE(AssemblyContains(assembly, "ewrite"));
}

TEST_F(BackendTest, ReturnStatement) {
  std::string assembly = GenerateAssemblyForInput(
      "def get_value()\n"
      "  return 42\n"
      "end");

  // Check for return value loading
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 42"));

  // Check for setting return value
  EXPECT_TRUE(AssemblyContains(assembly, "mv a0, t0"));

  // Check for jumping to epilogue
  EXPECT_TRUE(AssemblyContains(assembly, "jal zero,"));
}

TEST_F(BackendTest, EmptyReturn) {
  std::string assembly = GenerateAssemblyForInput(
      "def do_nothing()\n"
      "  return\n"
      "end");

  // Check for default return value
  EXPECT_TRUE(AssemblyContains(assembly, "li a0, 0"));
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

  // Check for function declaration
  EXPECT_TRUE(AssemblyContains(assembly, "factorial:"));

  // Check for recursive call
  EXPECT_TRUE(AssemblyContains(assembly, "jal ra, factorial"));

  // Check for conditional logic
  EXPECT_TRUE(AssemblyContains(assembly, "sle") ||
              (AssemblyContains(assembly, "slt") &&
               AssemblyContains(assembly, "xori")));

  // Check for main program call to factorial
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 5"));
  EXPECT_TRUE(AssemblyContains(assembly, "jal ra, factorial"));

  // Check for puts calls
  EXPECT_TRUE(AssemblyContains(assembly, ".string \"Factorial of 5 is: \""));
  EXPECT_TRUE(AssemblyContains(assembly, "jal ra, runtime_puts"));
}

TEST_F(BackendTest, UnaryOperators) {
  std::string assembly = GenerateAssemblyForInput(
      "a = -5\n"
      "b = !0\n"
      "c = ~42");

  // Check for negation
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 5"));
  EXPECT_TRUE(AssemblyContains(assembly, "sub t0, zero, t0"));

  // Check for logical NOT
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 0"));
  EXPECT_TRUE(AssemblyContains(assembly, "seq t0, t0, zero"));

  // Check for bitwise NOT
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 42"));
  EXPECT_TRUE(AssemblyContains(assembly, "xori t0, t0, -1"));
}

TEST_F(BackendTest, GlobalScope) {
  std::string assembly = GenerateAssemblyForInput(
      "x = 42\n"
      "def get_x()\n"
      "  return x\n"
      "end");

  // Check for global start section
  EXPECT_TRUE(AssemblyContains(assembly, ".globl _start"));
  EXPECT_TRUE(AssemblyContains(assembly, "_start:"));

  // Check for main entry
  EXPECT_TRUE(AssemblyContains(assembly, "ebreak"));
}

TEST_F(BackendTest, RuntimeHelpers) {
  std::string assembly = GenerateAssemblyForInput("puts(\"Test\")");

  // Check for runtime helpers section
  EXPECT_TRUE(AssemblyContains(assembly, "# --- Runtime Helper Functions ---"));
  EXPECT_TRUE(AssemblyContains(assembly, "runtime_puts:"));

  // Check for character handling
  EXPECT_TRUE(AssemblyContains(assembly, "lb t0, 0(a0)"));
  EXPECT_TRUE(AssemblyContains(assembly, "ewrite t0"));

  // Check for newline output
  EXPECT_TRUE(AssemblyContains(assembly, "li t0, 10") ||
              AssemblyContains(assembly, "li t0, '\\n'"));
  EXPECT_TRUE(AssemblyContains(assembly, "ewrite t0"));
}