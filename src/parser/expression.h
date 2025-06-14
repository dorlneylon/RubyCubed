#pragma once

#include <memory>

namespace parser {

class Object {
 public:
  virtual ~Object() = default;
  virtual std::string ToString(int indent = 0) const = 0;
};

class Expression : public Object {
 public:
  virtual ~Expression() = default;
};

class ExpressionBuilder {
 public:
  explicit ExpressionBuilder(std::shared_ptr<Expression> base);
  ExpressionBuilder& AddBinaryOp(const std::string& op,
                                 std::shared_ptr<Expression> right);
  std::shared_ptr<Expression> Build();

 private:
  std::shared_ptr<Expression> expression_;
};

}  // namespace parser