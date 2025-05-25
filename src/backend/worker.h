#pragma once

#include <sstream>
#include <string>
#include <unordered_map>

#include "parser/ast.h"
#include "parser/expression.h"

namespace backend {

class Worker {
 public:
  Worker(parser::AstNode root);

  std::string GenerateAssembly();

 private:
  parser::AstNode root_;
  std::unordered_map<std::string, std::string> data_section_;
  std::unordered_map<std::string, int> local_vars_;
  std::unordered_map<std::string, int> global_vars_;
  std::unordered_map<std::string, int> param_vars_;
  std::string current_function_;
  int label_counter_ = 0;

  void ProcessNode(const parser::AstNode& node, std::stringstream& assembly);
  void ProcessExpression(const std::shared_ptr<parser::Expression>& expr,
                         std::stringstream& assembly);
  std::string GenerateLabel(const std::string& prefix);
  std::string GenerateStringLabel(const std::string& str);
  void GenerateRuntimeHelpers(std::stringstream& assembly);
};

}  // namespace backend