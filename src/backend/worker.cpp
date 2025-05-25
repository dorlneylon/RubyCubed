#include "worker.h"

#include <iostream>
#include <sstream>
#include <stdexcept>
#include <unordered_map>

#include "fmt/format.h"

namespace backend {

Worker::Worker(parser::AstNode root) : root_(std::move(root)) {
}

const Worker::StringTable Worker::kBinaryOpInstructions = {
    {"+", "    add x5, x5, x6\n"},
    {"-", "    sub x5, x5, x6\n"},
    {"*", "    mul x5, x5, x6\n"},
    {"/", "    div x5, x5, x6\n"},
    {"%", "    rem x5, x5, x6\n"},
    {"<", "    slt x5, x5, x6\n"},
    {">", "    slt x5, x6, x5\n"},
    {"<=", "    slt x5, x6, x5\n    xori x5, x5, 1\n"},
    {">=", "    slt x5, x5, x6\n    xori x5, x5, 1\n"},
    {"==", "    seq x5, x5, x6\n"},
    {"!=", "    sne x5, x5, x6\n"},
    {"&", "    and x5, x5, x6\n"},
    {"|", "    or x5, x5, x6\n"},
    {"^", "    xor x5, x5, x6\n"},
    {"<<", "    sll x5, x5, x6\n"},
    {">>", "    srl x5, x5, x6\n"}};

const Worker::StringTable Worker::kRuntimeHelpers = {
    {"runtime_puts",
     "runtime_puts:\n"
     "    beq x20, x0, runtime_puts_string  # String if x20=0\n"
     "    jal x0, runtime_puts_int          # Else, print integer\n"},
    {"runtime_puts_int",
     "runtime_puts_int:\n"
     "    addi x2, x2, 1488\n"
     "    add x5, x10, x0       # x5 = input number\n"
     "    li x6, 0              # x6 = digit count\n"
     "    beq x5, x0, runtime_zero      # Handle zero directly\n"
     "    li x7, 0              # x7 = sign flag (0=positive)\n"
     "    bge x5, x0, runtime_extract   # Skip if positive\n"
     "    sub x5, x0, x5        # Make number positive\n"
     "    li x7, 1              # Set sign flag\n"
     "runtime_extract:\n"
     "    li x28, 10            # Divisor = 10\n"
     "runtime_loop:\n"
     "    rem x29, x5, x28      # x29 = x5 % 10\n"
     "    addi x29, x29, 48     # Convert to ASCII\n"
     "    addi x2, x2, -1       # Allocate stack cell\n"
     "    sw x2, 0, x29         # Push digit\n"
     "    addi x6, x6, 1        # Increment digit count\n"
     "    div x5, x5, x28       # x5 = x5 / 10\n"
     "    bne x5, x0, runtime_loop      # Repeat until x5=0\n"
     "    beq x7, x0, runtime_print     # Skip '-' if positive\n"
     "    li x5, 45             # Load '-'\n"
     "    ewrite x5             # Print '-'\n"
     "runtime_print:\n"
     "    beq x6, x0, runtime_done      # Exit when all digits printed\n"
     "    lw x5, x2, 0          # Pop digit\n"
     "    ewrite x5             # Print it\n"
     "    addi x2, x2, 1        # Free stack cell\n"
     "    addi x6, x6, -1       # Decrement count\n"
     "    jal x0, runtime_print         # Repeat\n"
     "runtime_zero:\n"
     "    li x5, 48             # '0'\n"
     "    ewrite x5\n"
     "runtime_done:\n"
     "    li x5, 10             # Newline\n"
     "    ewrite x5\n"
     "    jalr x0, x1, 0        # Return\n"},
    {"runtime_puts_string",
     "runtime_puts_string:\n"
     "    lw x5, x10, 0\n"
     "    beq x5, x0, runtime_puts_newline\n"
     "    ewrite x5\n"
     "    addi x10, x10, 1\n"
     "    jal x0, runtime_puts_string\n"},
    {"runtime_puts_newline",
     "runtime_puts_newline:\n"
     "    li x5, 10\n"
     "    ewrite x5\n"
     "    jalr x0, x1, 0\n"}};

std::string Worker::GenerateAssembly() {
  std::stringstream assembly;
  data_section_.clear();
  label_counter_ = 0;
  locals_.clear();
  globals_.clear();
  params_.clear();
  current_function_ = "";

  assembly << "_start:\n";

  ProcessNode(root_, assembly);
  assembly << "    ebreak\n\n";

  if (!data_section_.empty()) {
    for (const auto& str_entry : data_section_) {
      assembly << str_entry.first << ":\n";
      for (char c : str_entry.second) {
        assembly << "    data " << static_cast<int>(c) << " * 1\n";
      }
      assembly << "    data 0 * 1\n";
    }
  }

  assembly << "\n# --- Runtime Helper Functions ---\n";
  GenerateRuntimeHelpers(assembly);

  return assembly.str();
}

void Worker::ProcessNode(parser::AstNode node, std::stringstream& assembly) {
  if (auto program = std::dynamic_pointer_cast<parser::Program>(node)) {
    ProcessProgram(std::move(program), assembly);
    return;
  }

  if (auto expr_stmt =
          std::dynamic_pointer_cast<parser::ExpressionStatement>(node)) {
    ProcessExpressionStatement(std::move(expr_stmt), assembly);
    return;
  }

  if (auto var_decl =
          std::dynamic_pointer_cast<parser::VariableDeclaration>(node)) {
    ProcessVariableDeclaration(std::move(var_decl), assembly);
    return;
  }

  if (auto block = std::dynamic_pointer_cast<parser::BlockStatement>(node)) {
    ProcessBlockStatement(std::move(block), assembly);
    return;
  }

  if (auto if_stmt = std::dynamic_pointer_cast<parser::IfStatement>(node)) {
    ProcessIfStatement(std::move(if_stmt), assembly);
    return;
  }

  if (auto while_stmt =
          std::dynamic_pointer_cast<parser::WhileStatement>(node)) {
    ProcessWhileStatement(std::move(while_stmt), assembly);
    return;
  }

  if (auto times_stmt =
          std::dynamic_pointer_cast<parser::TimesStatement>(node)) {
    ProcessTimesStatement(std::move(times_stmt), assembly);
    return;
  }

  if (auto func_decl =
          std::dynamic_pointer_cast<parser::FunctionDeclaration>(node)) {
    ProcessFunctionDeclaration(std::move(func_decl), assembly);
    return;
  }

  if (auto return_stmt =
          std::dynamic_pointer_cast<parser::ReturnStatement>(node)) {
    ProcessReturnStatement(std::move(return_stmt), assembly);
    return;
  }

  throw std::runtime_error("Unknown node type in ProcessNode");
}

void Worker::ProcessProgram(std::shared_ptr<parser::Program> program,
                            std::stringstream& assembly) {
  std::vector<std::shared_ptr<parser::FunctionDeclaration>> functions;
  std::vector<std::shared_ptr<parser::Statement>> other_statements;

  for (const auto& stmt : program->GetStatements()) {
    if (auto func_decl =
            std::dynamic_pointer_cast<parser::FunctionDeclaration>(stmt)) {
      functions.push_back(func_decl);
    } else {
      other_statements.push_back(stmt);
    }
  }

  for (const auto& stmt : other_statements) {
    ProcessNode(stmt, assembly);
  }

  for (const auto& func : functions) {
    ProcessNode(func, assembly);
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
      const int kInitialOffset = 25000;
      globals_[var_name] = kInitialOffset + globals_.size() * 4;
    }
    int offset = globals_[var_name];
    assembly << "    sw x3, " << offset << ", x5\n";
    return;
  }

  if (locals_.find(var_name) == locals_.end()) {
    locals_[var_name] = -(locals_.size() + 1) * 4;
  }
  int offset = locals_[var_name];
  assembly << "    sw x8, " << offset << ", x5\n";
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
  assembly << "    beq x5, x0, " << else_label << "\n";

  ProcessNode(if_stmt->GetConsequence(), assembly);
  assembly << "    jal x0, " << endif_label << "\n";

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

  assembly << "    jal x0, " << loop_start << "\n";

  assembly << loop_body << ":\n";
  ProcessNode(while_stmt->GetBody(), assembly);

  assembly << loop_start << ":\n";
  ProcessExpression(while_stmt->GetCondition(), assembly);
  assembly << "    bne x5, x0, " << loop_body << "\n";

  assembly << loop_end << ":\n";
}

void Worker::ProcessTimesStatement(
    std::shared_ptr<parser::TimesStatement> times_stmt,
    std::stringstream& assembly) {
  std::string loop_start = GenerateLabel("times_loop_start");
  std::string loop_end = GenerateLabel("times_loop_end");

  ProcessExpression(times_stmt->GetCount(), assembly);
  assembly << "    li x6, 0\n";

  assembly << loop_start << ":\n";
  assembly << "    bge x6, x5, " << loop_end << "\n";

  ProcessNode(times_stmt->GetBody(), assembly);

  assembly << "    addi x6, x6, 1\n";
  assembly << "    jal x0, " << loop_start << "\n";

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

  function_epilogues_[current_function_] = GenerateLabel("func_epilogue");

  assembly << func_decl->GetName() << ":\n";

  GenerateFunctionPrologue(assembly);

  const int kParameters = 8;
  for (size_t i = 0; i < parameters.size() && i < kParameters; i++) {
    assembly << "    lw x" << (5 + i) << ", x8, " << (i + 2) * 4 << "\n";
  }

  ProcessNode(func_decl->GetBody(), assembly);

  assembly << "    li x10, 0\n";

  GenerateFunctionEpilogue(function_epilogues_[current_function_], assembly);

  current_function_ = prev_function;
  locals_ = prev_locals;
  params_ = prev_params;
}

void Worker::GenerateFunctionPrologue(std::stringstream& assembly) {
  assembly << "    addi x2, x2, -4\n";
  assembly << "    sw x2, 0, x8\n";
  assembly << "    addi x2, x2, -4\n";
  assembly << "    sw x2, 0, x1\n";
  assembly << "    add x8, x2, x0\n";
}

void Worker::GenerateFunctionEpilogue(const std::string& label,
                                      std::stringstream& assembly) {
  assembly << label << ":\n";
  assembly << "    add x2, x8, x0\n";
  assembly << "    lw x1, x2, 0\n";
  assembly << "    addi x2, x2, 4\n";
  assembly << "    lw x8, x2, 0\n";
  assembly << "    addi x2, x2, 4\n";
  assembly << "    jalr x0, x1, 0\n";
}

void Worker::ProcessReturnStatement(
    std::shared_ptr<parser::ReturnStatement> return_stmt,
    std::stringstream& assembly) {
  if (return_stmt->GetValue()) {
    ProcessExpression(return_stmt->GetValue(), assembly);
    assembly << "    add x10, x5, x0\n";
  } else {
    assembly << "    li x10, 0\n";
  }

  assembly << "    jal x0, " << function_epilogues_[current_function_] << "\n";
}

void Worker::ProcessExpression(std::shared_ptr<parser::Expression> expr,
                               std::stringstream& assembly) {
  if (auto int_lit = std::dynamic_pointer_cast<parser::IntegerLiteral>(expr)) {
    ProcessIntegerLiteral(std::move(int_lit), assembly);
    return;
  }

  if (auto float_lit = std::dynamic_pointer_cast<parser::FloatLiteral>(expr)) {
    ProcessFloatLiteral(std::move(float_lit), assembly);
    return;
  }

  if (auto str_lit = std::dynamic_pointer_cast<parser::StringLiteral>(expr)) {
    ProcessStringLiteral(std::move(str_lit), assembly);
    return;
  }

  if (auto id = std::dynamic_pointer_cast<parser::Identifier>(expr)) {
    ProcessIdentifier(std::move(id), assembly);
    return;
  }

  if (auto binary = std::dynamic_pointer_cast<parser::BinaryExpression>(expr)) {
    ProcessBinaryExpression(std::move(binary), assembly);
    return;
  }

  if (auto unary = std::dynamic_pointer_cast<parser::UnaryExpression>(expr)) {
    ProcessUnaryExpression(std::move(unary), assembly);
    return;
  }

  if (auto call = std::dynamic_pointer_cast<parser::CallExpression>(expr)) {
    ProcessCallExpression(std::move(call), assembly);
    return;
  }

  throw std::runtime_error("Unknown expression type in ProcessExpression");
}

void Worker::ProcessIntegerLiteral(
    std::shared_ptr<parser::IntegerLiteral> int_lit,
    std::stringstream& assembly) {
  assembly << "    li x5, " << int_lit->GetValue() << "\n";
}

void Worker::ProcessFloatLiteral(
    std::shared_ptr<parser::FloatLiteral> float_lit,
    std::stringstream& assembly) {
  assembly << "    li x5, " << static_cast<int>(float_lit->GetValue()) << "\n";
}

void Worker::ProcessStringLiteral(
    std::shared_ptr<parser::StringLiteral> str_lit,
    std::stringstream& assembly) {
  std::string label = GenerateStringLabel(str_lit->GetValue());
  assembly << "    li x5, " << label << "\n";
}

void Worker::ProcessIdentifier(std::shared_ptr<parser::Identifier> id,
                               std::stringstream& assembly) {
  const std::string& name = id->GetName();

  if (!current_function_.empty() && params_.find(name) != params_.end()) {
    HandleParameterVariable(name, assembly);
    return;
  }

  if (!current_function_.empty() && locals_.find(name) != locals_.end()) {
    HandleLocalVariable(name, assembly);
    return;
  }

  if (globals_.find(name) != globals_.end()) {
    HandleGlobalVariable(name, assembly);
    return;
  }

  throw std::runtime_error("Undefined variable: " + name);
}

void Worker::HandleParameterVariable(const std::string& name,
                                     std::stringstream& assembly) {
  int offset = params_[name];
  assembly << "    lw x5, x8, " << offset << "\n";
}

void Worker::HandleLocalVariable(const std::string& name,
                                 std::stringstream& assembly) {
  int offset = locals_[name];
  assembly << "    lw x5, x8, " << offset << "\n";
}

void Worker::HandleGlobalVariable(const std::string& name,
                                  std::stringstream& assembly) {
  int offset = globals_[name];
  assembly << "    lw x5, x3, " << offset << "\n";
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

  assembly << "    li x5, " << left_value << "\n";
  assembly << "    li x6, " << right_value << "\n";
  assembly << "    add x5, x5, x6\n";
  assembly << "    li x6, " << mul_value << "\n";
  assembly << "    mul x5, x5, x6\n";
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
    assembly << "    li x6, " << right_int->GetValue() << "\n";
  } else {
    assembly << "    add x7, x5, x0\n";
    ProcessExpression(binary->GetRight(), assembly);
    assembly << "    add x6, x5, x0\n";
    assembly << "    add x5, x7, x0\n";
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
    assembly << "    sub x5, x0, x5\n";
  } else if (op == "!") {
    assembly << "    seqz x5, x5\n";
  } else if (op == "~") {
    assembly << "    xori x5, x5, -1\n";
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
      assembly << "    li x" << (i + 5) << ", " << arg_int->GetValue() << "\n";
    } else {
      ProcessExpression(args[i], assembly);
      if (i > 0) {
        assembly << "    add x" << (i + 5) << ", x5, x0\n";
      }
    }
  }

  for (int i = args.size() - 1; i >= 0; i--) {
    assembly << "    addi x2, x2, -4\n";
    assembly << "    sw x2, 0, x" << (i + 5) << "\n";
  }

  if (func_name == "puts") {
    ProcessBuiltInFunction(func_name, args, assembly);
  } else {
    ProcessRegularFunctionCall(func_name, args, assembly);
  }

  if (!args.empty()) {
    assembly << "    addi x2, x2, " << (args.size() * 4) << "\n";
  }
}

void Worker::ProcessBuiltInFunction(
    const std::string& func_name,
    const std::vector<std::shared_ptr<parser::Expression>>& args,
    std::stringstream& assembly) {
  if (func_name == "puts") {
    if (!args.empty()) {
      assembly << "    add x10, x5, x0\n";
      if (std::dynamic_pointer_cast<parser::StringLiteral>(args[0])) {
        assembly << "    li x20, 0\n";
      } else {
        assembly << "    li x20, 1\n";
      }
    } else {
      assembly << "    li x10, 0\n";
      assembly << "    li x20, 0\n";
    }
    assembly << "    jal x1, runtime_puts\n";
    assembly << "    li x5, 0\n";
  }
}

void Worker::ProcessRegularFunctionCall(
    const std::string& func_name,
    const std::vector<std::shared_ptr<parser::Expression>>& /*args*/,
    std::stringstream& assembly) {
  assembly << "    jal x1, " << func_name << "\n";
  assembly << "    add x5, x10, x0\n";
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