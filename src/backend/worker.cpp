#include "worker.h"

#include <cassert>
#include <iostream>
#include <sstream>
#include <unordered_map>
#include <vector>

namespace backend {

Worker::Worker(parser::AstNode root) : root_(root) {
}

std::string Worker::GenerateAssembly() {
  std::stringstream assembly;
  data_section_.clear();
  label_counter_ = 0;
  local_vars_.clear();
  global_vars_.clear();
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

void Worker::ProcessNode(const parser::AstNode& node,
                         std::stringstream& assembly) {
  if (auto program = std::dynamic_pointer_cast<parser::Program>(node)) {
    for (const auto& statement : program->GetStatements()) {
      ProcessNode(statement, assembly);
    }
  } else if (auto expr_stmt =
                 std::dynamic_pointer_cast<parser::ExpressionStatement>(node)) {
    ProcessExpression(expr_stmt->GetExpression(), assembly);
  } else if (auto var_decl =
                 std::dynamic_pointer_cast<parser::VariableDeclaration>(node)) {
    ProcessExpression(var_decl->GetInitializer(), assembly);

    if (!current_function_.empty()) {
      if (local_vars_.find(var_decl->GetName()) == local_vars_.end()) {
        assembly << "    addi sp, sp, -4\n";
        local_vars_[var_decl->GetName()] = local_vars_.size() * 4;
      }
      assembly << "    sw t0, " << local_vars_[var_decl->GetName()] << "(fp)\n";
    } else {
      if (global_vars_.find(var_decl->GetName()) == global_vars_.end()) {
        global_vars_[var_decl->GetName()] = global_vars_.size() * 4;
      }
      assembly << "    sw t0, " << global_vars_[var_decl->GetName()]
               << "(sp)\n";
    }
  } else if (auto if_stmt =
                 std::dynamic_pointer_cast<parser::IfStatement>(node)) {
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
  } else if (auto while_stmt =
                 std::dynamic_pointer_cast<parser::WhileStatement>(node)) {
    std::string loop_start = GenerateLabel("loop_start");
    std::string loop_body = GenerateLabel("loop_body");
    std::string loop_end = GenerateLabel("loop_end");

    assembly << loop_start << ":\n";
    ProcessExpression(while_stmt->GetCondition(), assembly);
    assembly << "    beq t0, zero, " << loop_end << "\n";
    assembly << loop_body << ":\n";
    ProcessNode(while_stmt->GetBody(), assembly);
    assembly << "    jal zero, " << loop_start << "\n";

    assembly << loop_end << ":\n";
  } else if (auto times_stmt =
                 std::dynamic_pointer_cast<parser::TimesStatement>(node)) {
    std::string loop_start = GenerateLabel("times_loop_start");
    std::string loop_end = GenerateLabel("times_loop_end");

    ProcessExpression(times_stmt->GetCount(), assembly);

    assembly << "    mv t2, t0\n";
    assembly << "    li t1, 0\n";
    assembly << loop_start << ":\n";
    assembly << "    bge t1, t2, " << loop_end << "\n";
    ProcessNode(times_stmt->GetBody(), assembly);
    assembly << "    addi t1, t1, 1\n";
    assembly << "    jal zero, " << loop_start << "\n";
    assembly << loop_end << ":\n";
  } else if (auto func_decl =
                 std::dynamic_pointer_cast<parser::FunctionDeclaration>(node)) {
    current_function_ = func_decl->GetName();
    local_vars_.clear();

    assembly << "\n" << current_function_ << ":\n";

    assembly << "    addi sp, sp, -4\n";
    assembly << "    sw fp, 0(sp)\n";
    assembly << "    addi sp, sp, -4\n";
    assembly << "    sw ra, 0(sp)\n";
    assembly << "    mv fp, sp\n";

    int param_offset = 8;
    for (const auto& param : func_decl->GetParameters()) {
      param_vars_[param] = param_offset;
      param_offset += 4;
    }

    ProcessNode(func_decl->GetBody(), assembly);

    std::string epilogue_label = GenerateLabel("epilogue_" + current_function_);
    assembly << epilogue_label << ":\n";

    assembly << "    mv sp, fp\n";
    assembly << "    lw ra, 0(sp)\n";
    assembly << "    addi sp, sp, 4\n";
    assembly << "    lw fp, 0(sp)\n";
    assembly << "    addi sp, sp, 4\n";
    assembly << "    jalr zero, ra, 0\n";

    current_function_ = "";
    param_vars_.clear();
  } else if (auto block =
                 std::dynamic_pointer_cast<parser::BlockStatement>(node)) {
    for (const auto& statement : block->GetStatements()) {
      ProcessNode(statement, assembly);
    }
  } else if (auto return_stmt =
                 std::dynamic_pointer_cast<parser::ReturnStatement>(node)) {
    if (return_stmt->GetValue()) {
      ProcessExpression(return_stmt->GetValue(), assembly);
      assembly << "    mv a0, t0\n";
    } else {
      assembly << "    li a0, 0\n";
    }

    assembly << "    jal zero, epilogue_" << current_function_ << "\n";
  }
}

void Worker::ProcessExpression(const std::shared_ptr<parser::Expression>& expr,
                               std::stringstream& assembly) {
  if (auto binary_expr =
          std::dynamic_pointer_cast<parser::BinaryExpression>(expr)) {
    ProcessExpression(binary_expr->GetLeft(), assembly);
    assembly << "    mv t2, t0\n";

    ProcessExpression(binary_expr->GetRight(), assembly);
    assembly << "    mv t1, t0\n";
    assembly << "    mv t0, t2\n";

    std::string op = binary_expr->GetOperator();
    if (op == "+") {
      assembly << "    add t0, t0, t1\n";
    } else if (op == "-") {
      assembly << "    sub t0, t0, t1\n";
    } else if (op == "*") {
      assembly << "    mul t0, t0, t1\n";
    } else if (op == "/") {
      assembly << "    div t0, t0, t1\n";
    } else if (op == "%") {
      assembly << "    rem t0, t0, t1\n";
    } else if (op == "==") {
      assembly << "    seq t0, t0, t1\n";
    } else if (op == "!=") {
      assembly << "    sne t0, t0, t1\n";
    } else if (op == "<") {
      assembly << "    slt t0, t0, t1\n";
    } else if (op == "<=") {
      assembly << "    sle t0, t0, t1\n";
    } else if (op == ">") {
      assembly << "    slt t0, t1, t0\n";
    } else if (op == ">=") {
      assembly << "    sge t0, t0, t1\n";
    } else if (op == "&&") {
      std::string skip_label = GenerateLabel("and_skip");
      assembly << "    beq t0, zero, " << skip_label << "\n";
      assembly << "    mv t0, t1\n";
      assembly << skip_label << ":\n";
    } else if (op == "||") {
      std::string skip_label = GenerateLabel("or_skip");
      assembly << "    bne t0, zero, " << skip_label << "\n";
      assembly << "    mv t0, t1\n";
      assembly << skip_label << ":\n";
    }
  } else if (auto unary_expr =
                 std::dynamic_pointer_cast<parser::UnaryExpression>(expr)) {
    ProcessExpression(unary_expr->GetExpression(), assembly);

    std::string op = unary_expr->GetOperator();
    if (op == "-") {
      assembly << "    sub t0, zero, t0\n";
    } else if (op == "!") {
      assembly << "    seq t0, t0, zero\n";
    } else if (op == "~") {
      assembly << "    xori t0, t0, -1\n";
    }
  } else if (auto int_lit =
                 std::dynamic_pointer_cast<parser::IntegerLiteral>(expr)) {
    assembly << "    li t0, " << int_lit->GetValue() << "\n";
  } else if (auto float_lit =
                 std::dynamic_pointer_cast<parser::FloatLiteral>(expr)) {
    assembly << "    li t0, " << (int)float_lit->GetValue() << "\n";
  } else if (auto str_lit =
                 std::dynamic_pointer_cast<parser::StringLiteral>(expr)) {
    std::string label = GenerateStringLabel(str_lit->GetValue());
    assembly << "    la t0, " << label << "\n";
  } else if (auto ident = std::dynamic_pointer_cast<parser::Identifier>(expr)) {
    std::string name = ident->GetName();

    if (!current_function_.empty() &&
        local_vars_.find(name) != local_vars_.end()) {
      assembly << "    lw t0, " << local_vars_[name] << "(fp)\n";
    } else if (!current_function_.empty() &&
               param_vars_.find(name) != param_vars_.end()) {
      assembly << "    lw t0, " << param_vars_[name] << "(fp)\n";
    } else if (global_vars_.find(name) != global_vars_.end()) {
      assembly << "    lw t0, " << global_vars_[name] << "(sp)\n";
    } else {
      assembly << "    li t0, 0  # Unknown variable: " << name << "\n";
    }
  } else if (auto call_expr =
                 std::dynamic_pointer_cast<parser::CallExpression>(expr)) {
    std::string func_name;

    if (auto callee_ident = std::dynamic_pointer_cast<parser::Identifier>(
            call_expr->GetCallee())) {
      func_name = callee_ident->GetName();
    }

    if (func_name == "puts") {
      if (call_expr->GetArguments().size() > 0) {
        ProcessExpression(call_expr->GetArguments()[0], assembly);
        assembly << "    mv a0, t0\n";
        assembly << "    jal ra, runtime_puts\n";
        assembly << "    li t0, 0\n";
      }
      return;
    }

    for (auto it = call_expr->GetArguments().rbegin();
         it != call_expr->GetArguments().rend(); ++it) {
      ProcessExpression(*it, assembly);
      assembly << "    addi sp, sp, -4\n";
      assembly << "    sw t0, 0(sp)\n";
    }

    assembly << "    jal ra, " << func_name << "\n";
    if (!call_expr->GetArguments().empty()) {
      assembly << "    addi sp, sp, " << call_expr->GetArguments().size() * 4
               << "\n";
    }

    assembly << "    mv t0, a0\n";
  }
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
  assembly << "runtime_puts:\n";
  assembly << "    # Check if the argument is a string or integer\n";
  assembly << "    blt a0, zero, runtime_puts_int  # Negative means integer "
              "(hacky but works for demo)\n";
  assembly << "    beq a0, zero, runtime_puts_done\n";
  assembly << "runtime_puts_str:\n";
  assembly << "    # Print string\n";
  assembly << "    lb t0, 0(a0)\n";
  assembly << "    beq t0, zero, runtime_puts_newline\n";
  assembly << "    ewrite t0\n";
  assembly << "    addi a0, a0, 1\n";
  assembly << "    jal zero, runtime_puts_str\n";
  assembly << "runtime_puts_int:\n";
  assembly << "    # Print integer\n";
  assembly << "    mv t0, a0\n";
  assembly << "    li t1, 0  # Sign flag\n";
  assembly << "    bge t0, zero, runtime_puts_int_positive\n";
  assembly << "    li t1, 1  # Negative number\n";
  assembly << "    sub t0, zero, t0  # Make positive\n";
  assembly << "runtime_puts_int_positive:\n";
  assembly << "    # Convert to string (simplified for demo)\n";
  assembly << "    addi sp, sp, -12  # Space for digits\n";
  assembly << "    mv t2, sp  # Buffer pointer\n";
  assembly << "    li t3, 10  # Divisor\n";
  assembly << "runtime_puts_int_loop:\n";
  assembly << "    rem t4, t0, t3  # Get last digit\n";
  assembly << "    addi t4, t4, 48  # Convert to ASCII\n";
  assembly << "    sb t4, 0(t2)  # Store digit\n";
  assembly << "    addi t2, t2, 1  # Move buffer pointer\n";
  assembly << "    div t0, t0, t3  # Divide by 10\n";
  assembly << "    bne t0, zero, runtime_puts_int_loop\n";
  assembly << "    # Print minus sign if negative\n";
  assembly << "    beq t1, zero, runtime_puts_int_print\n";
  assembly << "    li t0, 45  # ASCII for '-'\n";
  assembly << "    ewrite t0\n";
  assembly << "runtime_puts_int_print:\n";
  assembly << "    # Print digits in reverse order\n";
  assembly << "    addi t2, t2, -1  # Move back to last digit\n";
  assembly << "runtime_puts_int_print_loop:\n";
  assembly << "    lb t0, 0(t2)  # Load digit\n";
  assembly << "    ewrite t0  # Print digit\n";
  assembly << "    blt t2, sp, runtime_puts_int_done\n";
  assembly << "    addi t2, t2, -1  # Move to previous digit\n";
  assembly << "    jal zero, runtime_puts_int_print_loop\n";
  assembly << "runtime_puts_int_done:\n";
  assembly << "    addi sp, sp, 12  # Restore stack\n";
  assembly << "runtime_puts_newline:\n";
  assembly << "    li t0, 10  # Newline character\n";
  assembly << "    ewrite t0\n";
  assembly << "runtime_puts_done:\n";
  assembly << "    jalr zero, ra, 0  # Return\n";
}

}  // namespace backend