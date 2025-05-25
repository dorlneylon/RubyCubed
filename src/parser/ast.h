#pragma once

#include <memory>
#include <string>
#include <vector>

#include "expression.h"

namespace parser {

using AstNode = std::shared_ptr<Object>;

class IntegerLiteral : public Expression {
 public:
  explicit IntegerLiteral(int value) : value_(value) {
  }
  int GetValue() const {
    return value_;
  }

 private:
  int value_;
};

class FloatLiteral : public Expression {
 public:
  explicit FloatLiteral(double value) : value_(value) {
  }
  double GetValue() const {
    return value_;
  }

 private:
  double value_;
};

class StringLiteral : public Expression {
 public:
  explicit StringLiteral(const std::string& value) : value_(value) {
  }
  const std::string& GetValue() const {
    return value_;
  }

 private:
  std::string value_;
};

class Identifier : public Expression {
 public:
  explicit Identifier(const std::string& name) : name_(name) {
  }
  const std::string& GetName() const {
    return name_;
  }

 private:
  std::string name_;
};

class BinaryExpression : public Expression {
 public:
  BinaryExpression(std::shared_ptr<Expression> left, const std::string& op,
                   std::shared_ptr<Expression> right)
      : left_(std::move(left)), operator_(op), right_(std::move(right)) {
  }

  std::shared_ptr<Expression> GetLeft() const {
    return left_;
  }
  const std::string& GetOperator() const {
    return operator_;
  }
  std::shared_ptr<Expression> GetRight() const {
    return right_;
  }

 private:
  std::shared_ptr<Expression> left_;
  std::string operator_;
  std::shared_ptr<Expression> right_;
};

class UnaryExpression : public Expression {
 public:
  UnaryExpression(const std::string& op, std::shared_ptr<Expression> expr)
      : operator_(op), expression_(std::move(expr)) {
  }

  const std::string& GetOperator() const {
    return operator_;
  }
  std::shared_ptr<Expression> GetExpression() const {
    return expression_;
  }

 private:
  std::string operator_;
  std::shared_ptr<Expression> expression_;
};

class CallExpression : public Expression {
 public:
  CallExpression(std::shared_ptr<Expression> callee,
                 std::vector<std::shared_ptr<Expression>> arguments)
      : callee_(std::move(callee)), arguments_(std::move(arguments)) {
  }

  std::shared_ptr<Expression> GetCallee() const {
    return callee_;
  }
  const std::vector<std::shared_ptr<Expression>>& GetArguments() const {
    return arguments_;
  }

 private:
  std::shared_ptr<Expression> callee_;
  std::vector<std::shared_ptr<Expression>> arguments_;
};

class Statement : public Object {
 public:
  virtual ~Statement() = default;
};

class ExpressionStatement : public Statement {
 public:
  explicit ExpressionStatement(std::shared_ptr<Expression> expr)
      : expression_(std::move(expr)) {
  }
  std::shared_ptr<Expression> GetExpression() const {
    return expression_;
  }

 private:
  std::shared_ptr<Expression> expression_;
};

class VariableDeclaration : public Statement {
 public:
  VariableDeclaration(const std::string& name,
                      std::shared_ptr<Expression> initializer)
      : name_(name), initializer_(std::move(initializer)) {
  }

  const std::string& GetName() const {
    return name_;
  }
  std::shared_ptr<Expression> GetInitializer() const {
    return initializer_;
  }

 private:
  std::string name_;
  std::shared_ptr<Expression> initializer_;
};

class BlockStatement : public Statement {
 public:
  explicit BlockStatement(std::vector<std::shared_ptr<Statement>> statements)
      : statements_(std::move(statements)) {
  }

  const std::vector<std::shared_ptr<Statement>>& GetStatements() const {
    return statements_;
  }

 private:
  std::vector<std::shared_ptr<Statement>> statements_;
};

class IfStatement : public Statement {
 public:
  IfStatement(std::shared_ptr<Expression> condition,
              std::shared_ptr<Statement> consequence,
              std::shared_ptr<Statement> alternative = nullptr)
      : condition_(std::move(condition)),
        consequence_(std::move(consequence)),
        alternative_(std::move(alternative)) {
  }

  std::shared_ptr<Expression> GetCondition() const {
    return condition_;
  }
  std::shared_ptr<Statement> GetConsequence() const {
    return consequence_;
  }
  std::shared_ptr<Statement> GetAlternative() const {
    return alternative_;
  }

 private:
  std::shared_ptr<Expression> condition_;
  std::shared_ptr<Statement> consequence_;
  std::shared_ptr<Statement> alternative_;
};

class WhileStatement : public Statement {
 public:
  WhileStatement(std::shared_ptr<Expression> condition,
                 std::shared_ptr<Statement> body)
      : condition_(std::move(condition)), body_(std::move(body)) {
  }

  std::shared_ptr<Expression> GetCondition() const {
    return condition_;
  }
  std::shared_ptr<Statement> GetBody() const {
    return body_;
  }

 private:
  std::shared_ptr<Expression> condition_;
  std::shared_ptr<Statement> body_;
};

class TimesStatement : public Statement {
 public:
  TimesStatement(std::shared_ptr<Expression> count,
                 std::shared_ptr<Statement> body)
      : count_(std::move(count)), body_(std::move(body)) {
  }

  std::shared_ptr<Expression> GetCount() const {
    return count_;
  }
  std::shared_ptr<Statement> GetBody() const {
    return body_;
  }

 private:
  std::shared_ptr<Expression> count_;
  std::shared_ptr<Statement> body_;
};

class FunctionDeclaration : public Statement {
 public:
  FunctionDeclaration(const std::string& name,
                      std::vector<std::string> parameters,
                      std::shared_ptr<BlockStatement> body)
      : name_(name),
        parameters_(std::move(parameters)),
        body_(std::move(body)) {
  }

  const std::string& GetName() const {
    return name_;
  }
  const std::vector<std::string>& GetParameters() const {
    return parameters_;
  }
  std::shared_ptr<BlockStatement> GetBody() const {
    return body_;
  }

 private:
  std::string name_;
  std::vector<std::string> parameters_;
  std::shared_ptr<BlockStatement> body_;
};

class ReturnStatement : public Statement {
 public:
  explicit ReturnStatement(std::shared_ptr<Expression> value = nullptr)
      : value_(std::move(value)) {
  }

  std::shared_ptr<Expression> GetValue() const {
    return value_;
  }

 private:
  std::shared_ptr<Expression> value_;
};

class Program : public Object {
 public:
  explicit Program(std::vector<std::shared_ptr<Statement>> statements)
      : statements_(std::move(statements)) {
  }

  const std::vector<std::shared_ptr<Statement>>& GetStatements() const {
    return statements_;
  }

 private:
  std::vector<std::shared_ptr<Statement>> statements_;
};

}  // namespace parser