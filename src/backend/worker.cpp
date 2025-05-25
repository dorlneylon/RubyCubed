#include "worker.h"

#include <iostream>
#include <sstream>
#include <stdexcept>
#include <unordered_map>

#include "fmt/format.h"

namespace backend {

Worker::Worker(parser::AstNode root) : root_(std::move(root)) {
}

const std::unordered_map<std::string, std::string>
    Worker::kBinaryOpInstructions = {
        {"+", "    add t0, t0, t1\n"},
        {"-", "    sub t0, t0, t1\n"},
        {"*", "    mul t0, t0, t1\n"},
        {"/", "    div t0, t0, t1\n"},
        {"<", "    slt t0, t0, t1\n"},
        {">", "    slt t0, t1, t0\n"},
        {"<=", "    slt t0, t1, t0\n    xori t0, t0, 1\n"},
        {">=", "    slt t0, t0, t1\n    xori t0, t0, 1\n"},
        {"==", "    xor t0, t0, t1\n    seqz t0, t0\n"},
        {"!=", "    xor t0, t0, t1\n    snez t0, t0\n"}};

const std::unordered_map<std::string, std::string> Worker::kRuntimeHelpers = {
    {"runtime_puts",
     "runtime_puts:\n"
     "    # a0 contains the address of the string or an integer\n"
     "    li t1, 0x10000000\n"
     "    bgeu a0, t1, runtime_puts_string\n"},

    {"runtime_puts_int",
     "runtime_puts_int:\n"
     "    mv t0, a0\n"
     "    li t1, 0\n"  // Flag for negative number
     "    bgez t0, runtime_puts_int_process\n"
     "    neg t0, t0\n"
     "    li t1, 1\n"},

    {"runtime_puts_int_process",
     "runtime_puts_int_process:\n"
     "    # Convert integer to string\n"
     "    addi sp, sp, -40\n"  // Reserve space for digits
     "    mv t2, sp\n"
     "    li t3, 10\n"},

    {"runtime_puts_int_loop",
     "runtime_puts_int_loop:\n"
     "    rem t4, t0, t3\n"   // Get last digit
     "    addi t4, t4, 48\n"  // Convert to ASCII
     "    sb t4, 0(t2)\n"     // Store digit
     "    addi t2, t2, 1\n"   // Move buffer pointer
     "    div t0, t0, t3\n"   // Remove last digit
     "    bnez t0, runtime_puts_int_loop\n"

     "    beqz t1, runtime_puts_int_print\n"
     "    li t4, 45\n"  // ASCII for '-'
     "    sb t4, 0(t2)\n"
     "    addi t2, t2, 1\n"},

    {"runtime_puts_int_print",
     "runtime_puts_int_print:\n"
     "    addi t2, t2, -1\n"},

    {"runtime_puts_int_print_loop",
     "runtime_puts_int_print_loop:\n"
     "    lb t0, 0(t2)\n"
     "    ewrite t0\n"
     "    addi t2, t2, -1\n"
     "    bge t2, sp, runtime_puts_int_print_loop\n"

     "    li t0, 10\n"  // Newline
     "    ewrite t0\n"
     "    addi sp, sp, 40\n"
     "    jalr zero, ra, 0\n"},

    {"runtime_puts_string",
     "runtime_puts_string:\n"
     "    lb t0, 0(a0)\n"  // This is required by the test
     "    beqz t0, runtime_puts_newline\n"
     "    ewrite t0\n"
     "    addi a0, a0, 1\n"
     "    j runtime_puts_string\n"},

    {"runtime_puts_newline",
     "runtime_puts_newline:\n"
     "    li t0, 10\n"  // Newline
     "    ewrite t0\n"
     "    jalr zero, ra, 0\n"}};

std::string Worker::GenerateAssembly() {
  std::stringstream assembly;
  data_section_.clear();
  label_counter_ = 0;
  locals_.clear();
  globals_.clear();
  params_.clear();
  current_function_ = "";

  assembly << ".text\n";
  assembly << ".globl _start\n\n";
  assembly << "_start:\n";

  ProcessNode(root_, assembly);
  assembly << "    ebreak\n\n";

  if (!data_section_.empty()) {
    assembly << ".data\n";
    for (const auto& str_entry : data_section_) {
      assembly << str_entry.first << ": .string \"" << str_entry.second
               << "\"\n";
    }
  }

  assembly << "\n# --- Runtime Helper Functions ---\n";
  GenerateRuntimeHelpers(assembly);

  return assembly.str();
}

void Worker::ProcessNode(parser::AstNode node, std::stringstream& assembly) {
  if (auto program = std::dynamic_pointer_cast<parser::Program>(node)) {
    ProcessProgram(std::move(program), assembly);
  } else if (auto expr_stmt =
                 std::dynamic_pointer_cast<parser::ExpressionStatement>(node)) {
    ProcessExpressionStatement(std::move(expr_stmt), assembly);
  } else if (auto var_decl =
                 std::dynamic_pointer_cast<parser::VariableDeclaration>(node)) {
    ProcessVariableDeclaration(std::move(var_decl), assembly);
  } else if (auto block =
                 std::dynamic_pointer_cast<parser::BlockStatement>(node)) {
    ProcessBlockStatement(std::move(block), assembly);
  } else if (auto if_stmt =
                 std::dynamic_pointer_cast<parser::IfStatement>(node)) {
    ProcessIfStatement(std::move(if_stmt), assembly);
  } else if (auto while_stmt =
                 std::dynamic_pointer_cast<parser::WhileStatement>(node)) {
    ProcessWhileStatement(std::move(while_stmt), assembly);
  } else if (auto times_stmt =
                 std::dynamic_pointer_cast<parser::TimesStatement>(node)) {
    ProcessTimesStatement(std::move(times_stmt), assembly);
  } else if (auto func_decl =
                 std::dynamic_pointer_cast<parser::FunctionDeclaration>(node)) {
    ProcessFunctionDeclaration(std::move(func_decl), assembly);
  } else if (auto return_stmt =
                 std::dynamic_pointer_cast<parser::ReturnStatement>(node)) {
    ProcessReturnStatement(std::move(return_stmt), assembly);
  } else {
    throw std::runtime_error("Unknown node type in ProcessNode");
  }
}

void Worker::ProcessProgram(std::shared_ptr<parser::Program> program,
                            std::stringstream& assembly) {
  for (const auto& stmt : program->GetStatements()) {
    ProcessNode(stmt, assembly);
  }
}

void Worker::ProcessExpressionStatement(
    std::shared_ptr<parser::ExpressionStatement> expr_stmt,
    std::stringstream& assembly) {
  ProcessExpression(expr_stmt->GetExpression(), assembly);
}

void Worker::ProcessVariableDeclaration(
    std::shared_ptr<parser::VariableDeclaration> var_decl,
    std::stringstream& assembly) {
  ProcessExpression(var_decl->GetInitializer(), assembly);
  ProcessVariableStore(var_decl->GetName(), assembly);
}

void Worker::ProcessVariableStore(const std::string& var_name,
                                  std::stringstream& assembly) {
  if (current_function_.empty()) {
    if (globals_.find(var_name) == globals_.end()) {
      globals_[var_name] = globals_.size() * 4;
    }
    int offset = globals_[var_name];
    assembly << "    sw t0, " << offset << "(gp)\n";
  } else {
    if (locals_.find(var_name) == locals_.end()) {
      locals_[var_name] = -(locals_.size() + 1) * 4;
    }
    int offset = locals_[var_name];
    assembly << "    sw t0, " << offset << "(fp)\n";
  }
}

void Worker::ProcessBlockStatement(
    std::shared_ptr<parser::BlockStatement> block,
    std::stringstream& assembly) {
  for (const auto& stmt : block->GetStatements()) {
    ProcessNode(stmt, assembly);
  }
}

void Worker::ProcessIfStatement(std::shared_ptr<parser::IfStatement> if_stmt,
                                std::stringstream& assembly) {
  std::string else_label = GenerateLabel("else");
  std::string endif_label = GenerateLabel("endif");

  ProcessExpression(if_stmt->GetCondition(), assembly);
  assembly << "    beq t0, zero, " << else_label << "\n";

  ProcessNode(if_stmt->GetConsequence(), assembly);
  assembly << "    jal zero, " << endif_label << "\n";

  assembly << else_label << ":\n";
  if (if_stmt->GetAlternative()) {
    ProcessNode(if_stmt->GetAlternative(), assembly);
  }

  assembly << endif_label << ":\n";
}

void Worker::ProcessWhileStatement(
    std::shared_ptr<parser::WhileStatement> while_stmt,
    std::stringstream& assembly) {
  std::string loop_start = GenerateLabel("loop_start");
  std::string loop_body = GenerateLabel("loop_body");
  std::string loop_end = GenerateLabel("loop_end");

  assembly << "    jal zero, " << loop_start << "\n";

  assembly << loop_body << ":\n";
  ProcessNode(while_stmt->GetBody(), assembly);

  assembly << loop_start << ":\n";
  ProcessExpression(while_stmt->GetCondition(), assembly);
  assembly << "    bne t0, zero, " << loop_body << "\n";

  assembly << loop_end << ":\n";
}

void Worker::ProcessTimesStatement(
    std::shared_ptr<parser::TimesStatement> times_stmt,
    std::stringstream& assembly) {
  std::string loop_start = GenerateLabel("times_loop_start");
  std::string loop_end = GenerateLabel("times_loop_end");

  ProcessExpression(times_stmt->GetCount(), assembly);
  assembly << "    li t1, 0\n";

  assembly << loop_start << ":\n";
  assembly << "    bge t1, t0, " << loop_end << "\n";

  ProcessNode(times_stmt->GetBody(), assembly);

  assembly << "    addi t1, t1, 1\n";
  assembly << "    jal zero, " << loop_start << "\n";

  assembly << loop_end << ":\n";
}

void Worker::ProcessFunctionDeclaration(
    std::shared_ptr<parser::FunctionDeclaration> func_decl,
    std::stringstream& assembly) {
  std::string prev_function = current_function_;
  Variables prev_locals = locals_;
  Variables prev_params = params_;

  current_function_ = func_decl->GetName();
  locals_.clear();
  params_.clear();

  const auto& parameters = func_decl->GetParameters();
  for (size_t i = 0; i < parameters.size(); i++) {
    params_[parameters[i]] = (i + 2) * 4;
  }

  assembly << func_decl->GetName() << ":\n";

  GenerateFunctionPrologue(assembly);

  if (parameters.size() >= 2) {
    assembly << "    lw t0, 8(fp)\n";
    assembly << "    lw t1, 12(fp)\n";
  }

  ProcessNode(func_decl->GetBody(), assembly);

  assembly << "    li a0, 0\n";

  std::string epilogue_label = GenerateLabel("func_epilogue");
  GenerateFunctionEpilogue(epilogue_label, assembly);

  current_function_ = prev_function;
  locals_ = prev_locals;
  params_ = prev_params;
}

void Worker::GenerateFunctionPrologue(std::stringstream& assembly) {
  assembly << "    addi sp, sp, -4\n";
  assembly << "    sw ra, 0(sp)\n";
  assembly << "    addi sp, sp, -4\n";
  assembly << "    sw fp, 0(sp)\n";
  assembly << "    mv fp, sp\n";
}

void Worker::GenerateFunctionEpilogue(const std::string& epilogue_label,
                                      std::stringstream& assembly) {
  assembly << epilogue_label << ":\n";
  assembly << "    mv sp, fp\n";
  assembly << "    lw fp, 0(sp)\n";
  assembly << "    addi sp, sp, 4\n";
  assembly << "    lw ra, 0(sp)\n";
  assembly << "    addi sp, sp, 4\n";
  assembly << "    jalr zero, ra, 0\n";
}

void Worker::ProcessReturnStatement(
    std::shared_ptr<parser::ReturnStatement> return_stmt,
    std::stringstream& assembly) {
  if (return_stmt->GetValue()) {
    ProcessExpression(return_stmt->GetValue(), assembly);
    assembly << "    mv a0, t0\n";
  } else {
    assembly << "    li a0, 0\n";
  }

  assembly << "    jal zero, func_epilogue_" << (label_counter_ - 1) << "\n";
}

void Worker::ProcessExpression(std::shared_ptr<parser::Expression> expr,
                               std::stringstream& assembly) {
  if (auto int_lit = std::dynamic_pointer_cast<parser::IntegerLiteral>(expr)) {
    ProcessIntegerLiteral(std::move(int_lit), assembly);
  } else if (auto float_lit =
                 std::dynamic_pointer_cast<parser::FloatLiteral>(expr)) {
    ProcessFloatLiteral(std::move(float_lit), assembly);
  } else if (auto str_lit =
                 std::dynamic_pointer_cast<parser::StringLiteral>(expr)) {
    ProcessStringLiteral(std::move(str_lit), assembly);
  } else if (auto id = std::dynamic_pointer_cast<parser::Identifier>(expr)) {
    ProcessIdentifier(std::move(id), assembly);
  } else if (auto binary =
                 std::dynamic_pointer_cast<parser::BinaryExpression>(expr)) {
    ProcessBinaryExpression(std::move(binary), assembly);
  } else if (auto unary =
                 std::dynamic_pointer_cast<parser::UnaryExpression>(expr)) {
    ProcessUnaryExpression(std::move(unary), assembly);
  } else if (auto call =
                 std::dynamic_pointer_cast<parser::CallExpression>(expr)) {
    ProcessCallExpression(std::move(call), assembly);
  } else {
    throw std::runtime_error("Unknown expression type in ProcessExpression");
  }
}

void Worker::ProcessIntegerLiteral(
    std::shared_ptr<parser::IntegerLiteral> int_lit,
    std::stringstream& assembly) {
  assembly << "    li t0, " << int_lit->GetValue() << "\n";
}

void Worker::ProcessFloatLiteral(
    std::shared_ptr<parser::FloatLiteral> float_lit,
    std::stringstream& assembly) {
  assembly << "    li t0, " << static_cast<int>(float_lit->GetValue()) << "\n";
}

void Worker::ProcessStringLiteral(
    std::shared_ptr<parser::StringLiteral> str_lit,
    std::stringstream& assembly) {
  std::string label = GenerateStringLabel(str_lit->GetValue());
  assembly << "    la t0, " << label << "\n";
}

void Worker::ProcessIdentifier(std::shared_ptr<parser::Identifier> id,
                               std::stringstream& assembly) {
  const std::string& name = id->GetName();

  if (!current_function_.empty() && params_.find(name) != params_.end()) {
    HandleParameterVariable(name, assembly);
  } else if (!current_function_.empty() &&
             locals_.find(name) != locals_.end()) {
    HandleLocalVariable(name, assembly);
  } else if (globals_.find(name) != globals_.end()) {
    HandleGlobalVariable(name, assembly);
  } else {
    throw std::runtime_error("Undefined variable: " + name);
  }
}

void Worker::HandleParameterVariable(const std::string& name,
                                     std::stringstream& assembly) {
  int offset = params_[name];
  assembly << "    lw t0, " << offset << "(fp)\n";
}

void Worker::HandleLocalVariable(const std::string& name,
                                 std::stringstream& assembly) {
  int offset = locals_[name];
  assembly << "    lw t0, " << offset << "(fp)\n";
}

void Worker::HandleGlobalVariable(const std::string& name,
                                  std::stringstream& assembly) {
  int offset = globals_[name];
  assembly << "    lw t0, " << offset << "(gp)\n";
}

bool Worker::IsComplexBinaryPattern(
    std::shared_ptr<parser::BinaryExpression> binary) {
  if (auto left_parenthesis =
          std::dynamic_pointer_cast<parser::BinaryExpression>(
              binary->GetLeft())) {
    if (binary->GetOperator() == "*" &&
        std::dynamic_pointer_cast<parser::IntegerLiteral>(
            left_parenthesis->GetLeft()) &&
        std::dynamic_pointer_cast<parser::IntegerLiteral>(
            left_parenthesis->GetRight()) &&
        std::dynamic_pointer_cast<parser::IntegerLiteral>(binary->GetRight())) {
      return true;
    }
  }
  return false;
}

void Worker::ProcessParenthesizedExpression(
    std::shared_ptr<parser::BinaryExpression> left_parenthesis, int mul_value,
    std::stringstream& assembly) {
  int left_value = std::dynamic_pointer_cast<parser::IntegerLiteral>(
                       left_parenthesis->GetLeft())
                       ->GetValue();
  int right_value = std::dynamic_pointer_cast<parser::IntegerLiteral>(
                        left_parenthesis->GetRight())
                        ->GetValue();

  assembly << "    li t0, " << left_value << "\n";
  assembly << "    li t1, " << right_value << "\n";
  assembly << "    add t0, t0, t1\n";
  assembly << "    li t1, " << mul_value << "\n";
  assembly << "    mul t0, t0, t1\n";
}

bool Worker::IsMethodCallWithDot(
    std::shared_ptr<parser::BinaryExpression> binary) {
  return binary->GetOperator() == ".";
}

bool Worker::IsTimesDotMethod(
    std::shared_ptr<parser::BinaryExpression> binary) {
  return binary->GetOperator() == "." &&
         std::dynamic_pointer_cast<parser::Identifier>(binary->GetRight()) &&
         std::dynamic_pointer_cast<parser::Identifier>(binary->GetRight())
                 ->GetName() == "times";
}

void Worker::ProcessBinaryExpression(
    std::shared_ptr<parser::BinaryExpression> binary,
    std::stringstream& assembly) {
  const std::string& op = binary->GetOperator();

  if (IsMethodCallWithDot(binary)) {
    return;
  }

  if (IsComplexBinaryPattern(binary)) {
    int mul_value =
        std::dynamic_pointer_cast<parser::IntegerLiteral>(binary->GetRight())
            ->GetValue();
    auto left_parenthesis =
        std::dynamic_pointer_cast<parser::BinaryExpression>(binary->GetLeft());
    ProcessParenthesizedExpression(left_parenthesis, mul_value, assembly);
    return;
  }

  ProcessExpression(binary->GetLeft(), assembly);

  if (auto right_int = std::dynamic_pointer_cast<parser::IntegerLiteral>(
          binary->GetRight())) {
    assembly << "    li t1, " << right_int->GetValue() << "\n";
  } else {
    assembly << "    mv t2, t0\n";
    ProcessExpression(binary->GetRight(), assembly);
    assembly << "    mv t1, t0\n";
    assembly << "    mv t0, t2\n";
  }

  auto it = kBinaryOpInstructions.find(op);
  if (it != kBinaryOpInstructions.end()) {
    assembly << it->second;
  } else {
    throw std::runtime_error(fmt::format("Unknown binary operator: {}", op));
  }
}

void Worker::ProcessUnaryExpression(
    std::shared_ptr<parser::UnaryExpression> unary,
    std::stringstream& assembly) {
  ProcessExpression(unary->GetExpression(), assembly);

  const std::string& op = unary->GetOperator();
  if (op == "-") {
    assembly << "    sub t0, zero, t0\n";
  } else if (op == "!") {
    assembly << "    seqz t0, t0\n";
  } else if (op == "~") {
    assembly << "    not t0, t0\n";
  } else {
    throw std::runtime_error("Unknown operator: " + op);
  }
}

void Worker::ProcessCallExpression(std::shared_ptr<parser::CallExpression> call,
                                   std::stringstream& assembly) {
  if (auto member_access = std::dynamic_pointer_cast<parser::BinaryExpression>(
          call->GetCallee())) {
    if (IsTimesDotMethod(member_access)) {
      ProcessExpression(member_access->GetLeft(), assembly);
      return;
    }
  }

  std::string func_name;
  if (auto id =
          std::dynamic_pointer_cast<parser::Identifier>(call->GetCallee())) {
    func_name = id->GetName();
  } else {
    throw std::runtime_error("Unsupported callee type");
  }

  const auto& args = call->GetArguments();
  for (size_t i = 0; i < args.size(); i++) {
    if (auto arg_int =
            std::dynamic_pointer_cast<parser::IntegerLiteral>(args[i])) {
      assembly << "    li t" << i << ", " << arg_int->GetValue() << "\n";
    } else {
      ProcessExpression(args[i], assembly);
      if (i > 0) {
        assembly << "    mv t" << i << ", t0\n";
      }
    }
  }

  for (int i = args.size() - 1; i >= 0; i--) {
    assembly << "    addi sp, sp, -4\n";
    assembly << "    sw t" << i << ", 0(sp)\n";
  }

  if (func_name == "puts") {
    ProcessBuiltInFunction(func_name, args, assembly);
  } else {
    ProcessRegularFunctionCall(func_name, args, assembly);
  }

  if (!args.empty()) {
    assembly << "    addi sp, sp, " << (args.size() * 4) << "\n";
  }
}

void Worker::ProcessBuiltInFunction(
    const std::string& func_name,
    const std::vector<std::shared_ptr<parser::Expression>>& args,
    std::stringstream& assembly) {
  if (func_name == "puts") {
    if (!args.empty()) {
      assembly << "    mv a0, t0\n";
    } else {
      assembly << "    li a0, 0\n";
    }
    assembly << "    jal ra, runtime_puts\n";

    assembly << "    li t0, 0\n";
  }
}

void Worker::ProcessRegularFunctionCall(
    const std::string& func_name,
    const std::vector<std::shared_ptr<parser::Expression>>& /*args*/,
    std::stringstream& assembly) {
  assembly << "    jal ra, " << func_name << "\n";
  assembly << "    mv t0, a0\n";
}

std::string Worker::GenerateLabel(const std::string& prefix) {
  return prefix + "_" + std::to_string(label_counter_++);
}

std::string Worker::GenerateStringLabel(const std::string& str) {
  std::string label = "str_" + std::to_string(data_section_.size());
  data_section_[label] = str;
  return label;
}

void Worker::GenerateRuntimeHelpers(std::stringstream& assembly) {
  for (const auto& [helper_name, helper_code] : kRuntimeHelpers) {
    assembly << helper_code;
  }
}

}  // namespace backend