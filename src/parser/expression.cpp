#include "expression.h"

#include "ast.h"

namespace parser {

ExpressionBuilder::ExpressionBuilder(std::shared_ptr<Expression> base)
    : expression_(std::move(base)) {
}

ExpressionBuilder& ExpressionBuilder::AddBinaryOp(
    const std::string& op, std::shared_ptr<Expression> right) {
  expression_ =
      std::make_shared<BinaryExpression>(expression_, op, std::move(right));
  return *this;
}

std::shared_ptr<Expression> ExpressionBuilder::Build() {
  return expression_;
}

}  // namespace parser