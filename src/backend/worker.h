#pragma once

#include <sstream>
#include <string>
#include <unordered_map>

#include "parser/ast.h"
#include "parser/expression.h"

namespace backend {

class Worker {
 public:
  explicit Worker(parser::AstNode root);

  std::string GenerateAssembly();

 private:
  using Variable = int;
  using Variables = std::unordered_map<std::string, Variable>;
  using StringTable = std::unordered_map<std::string, std::string>;

  parser::AstNode root_;
  StringTable data_section_;
  Variables locals_;
  Variables globals_;
  Variables params_;
  std::string current_function_;
  static const StringTable kBinaryOpInstructions;
  StringTable function_epilogues_;
  static const StringTable kRuntimeHelpers;
  int label_counter_ = 0;

  void ProcessNode(parser::AstNode node, std::stringstream& assembly);
  void ProcessExpression(std::shared_ptr<parser::Expression> expr,
                         std::stringstream& assembly);

  void ProcessProgram(std::shared_ptr<parser::Program> program,
                      std::stringstream& assembly);
  void ProcessExpressionStatement(
      std::shared_ptr<parser::ExpressionStatement> expr_stmt,
      std::stringstream& assembly);
  void ProcessVariableDeclaration(
      std::shared_ptr<parser::VariableDeclaration> var_decl,
      std::stringstream& assembly);
  void ProcessBlockStatement(std::shared_ptr<parser::BlockStatement> block,
                             std::stringstream& assembly);
  void ProcessIfStatement(std::shared_ptr<parser::IfStatement> if_stmt,
                          std::stringstream& assembly);
  void ProcessWhileStatement(std::shared_ptr<parser::WhileStatement> while_stmt,
                             std::stringstream& assembly);
  void ProcessTimesStatement(std::shared_ptr<parser::TimesStatement> times_stmt,
                             std::stringstream& assembly);
  void ProcessFunctionDeclaration(
      std::shared_ptr<parser::FunctionDeclaration> func_decl,
      std::stringstream& assembly);
  void ProcessReturnStatement(
      std::shared_ptr<parser::ReturnStatement> return_stmt,
      std::stringstream& assembly);

  static void ProcessIntegerLiteral(
      std::shared_ptr<parser::IntegerLiteral> int_lit,
      std::stringstream& assembly);
  static void ProcessFloatLiteral(
      std::shared_ptr<parser::FloatLiteral> float_lit,
      std::stringstream& assembly);
  void ProcessStringLiteral(std::shared_ptr<parser::StringLiteral> str_lit,
                            std::stringstream& assembly);
  void ProcessIdentifier(std::shared_ptr<parser::Identifier> id,
                         std::stringstream& assembly);
  void ProcessBinaryExpression(std::shared_ptr<parser::BinaryExpression> binary,
                               std::stringstream& assembly);
  void ProcessUnaryExpression(std::shared_ptr<parser::UnaryExpression> unary,
                              std::stringstream& assembly);
  void ProcessCallExpression(std::shared_ptr<parser::CallExpression> call,
                             std::stringstream& assembly);

  std::string GenerateLabel(const std::string& prefix);
  std::string GenerateStringLabel(const std::string& str);
  static void GenerateRuntimeHelpers(std::stringstream& assembly);
  static bool IsComplexBinaryPattern(
      std::shared_ptr<parser::BinaryExpression> binary);
  void ProcessComplexBinaryPattern(
      std::shared_ptr<parser::BinaryExpression> binary,
      std::stringstream& assembly);
  static void ProcessParenthesizedExpression(
      std::shared_ptr<parser::BinaryExpression> left_parenthesis, int mul_value,
      std::stringstream& assembly);
  void ProcessVariableStore(const std::string& var_name,
                            std::stringstream& assembly);
  static void GenerateFunctionPrologue(std::stringstream& assembly);
  static void GenerateFunctionEpilogue(const std::string& epilogue_label,
                                       std::stringstream& assembly);

  void HandleParameterVariable(const std::string& name,
                               std::stringstream& assembly);
  void HandleLocalVariable(const std::string& name,
                           std::stringstream& assembly);
  void HandleGlobalVariable(const std::string& name,
                            std::stringstream& assembly);
  static bool IsMethodCallWithDot(
      std::shared_ptr<parser::BinaryExpression> binary);
  static bool IsTimesDotMethod(
      std::shared_ptr<parser::BinaryExpression> binary);
  void ProcessMethodCall(std::shared_ptr<parser::CallExpression> call,
                         std::stringstream& assembly);
  static void ProcessBuiltInFunction(
      const std::string& func_name,
      const std::vector<std::shared_ptr<parser::Expression>>& args,
      std::stringstream& assembly);
  static void ProcessRegularFunctionCall(
      const std::string& func_name,
      const std::vector<std::shared_ptr<parser::Expression>>& args,
      std::stringstream& assembly);
};

}  // namespace backend