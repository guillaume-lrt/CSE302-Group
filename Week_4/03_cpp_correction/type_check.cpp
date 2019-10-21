#include "type_check.h"

namespace bx {
using namespace source;

namespace check {

class TypeChecker : public StmtVisitor, public ExprVisitor {
private:
  SymbolTable const &tyctx;

  inline void panic(std::string const &msg) { throw std::runtime_error(msg); }
  inline char const *ty_to_string(Type ty) {
    if (ty == Type::INT64)
      return "int64";
    if (ty == Type::BOOL)
      return "bool";
    return "<unknown>";
  }

public:
  TypeChecker(SymbolTable const &tyctx) : tyctx{tyctx} {}

  // Statements

  void visit(Assign const &mv) override {
    mv.left->accept(*this);
    mv.right->accept(*this);
    if (mv.left->meta->ty != mv.right->meta->ty)
      panic(std::string{"lhs of type "} + ty_to_string(mv.left->meta->ty) +
            " assigned to rhs of type " + ty_to_string(mv.right->meta->ty));
  }

  void visit(Print const &pr) override { pr.arg->accept(*this); }

  void visit(Block const &bl) override {
    for (auto &stmt : bl.body)
      stmt->accept(*this);
  }

  void visit(IfElse const &ie) override {
    ie.condition->accept(*this);
    if (ie.condition->meta->ty != Type::BOOL)
      panic("if condition is not a bool expression");
    ie.true_branch->accept(*this);
    ie.false_branch->accept(*this);
  }

  void visit(While const &wl) override {
    wl.condition->accept(*this);
    if (wl.condition->meta->ty != Type::BOOL)
      panic("while condition is not a bool expression");
    wl.loop_body->accept(*this);
  }

  // Expressions

  // invariant: after visiting an expression the ty field is never UNKNOWN

  void visit(Variable const &v) override { v.meta->ty = tyctx.at(v.label); }

  void visit(IntConstant const &i) override { i.meta->ty = Type::INT64; }

  void visit(BoolConstant const &b) override { b.meta->ty = Type::BOOL; }

  void visit_checked(Expr const &e, Type expected) {
    e->accept(*this);
    if (e->meta->ty != expected)
      panic(std::string{"type mismatch: expected "} + ty_to_string(expected) +
            ", got " + ty_to_string(e->meta->ty));
  }

  void visit(BinopApp const &bo) override {
    switch (bo.op) {
    case Binop::Add:
    case Binop::Subtract:
    case Binop::Multiply:
    case Binop::Divide:
    case Binop::Modulus:
    case Binop::BitAnd:
    case Binop::BitOr:
    case Binop::BitXor:
    case Binop::Lshift:
    case Binop::Rshift:
      visit_checked(bo.left_arg, Type::INT64);
      visit_checked(bo.right_arg, Type::INT64);
      bo.meta->ty = Type::INT64;
      break;
    case Binop::Lt:
    case Binop::Leq:
    case Binop::Gt:
    case Binop::Geq:
      visit_checked(bo.left_arg, Type::INT64);
      visit_checked(bo.right_arg, Type::INT64);
      bo.meta->ty = Type::BOOL;
      break;
    case Binop::LogAnd:
    case Binop::LogOr:
      visit_checked(bo.left_arg, Type::BOOL);
      visit_checked(bo.right_arg, Type::BOOL);
      bo.meta->ty = Type::BOOL;
      break;
    case Binop::Eq:
    case Binop::Neq:
      bo.left_arg->accept(*this);
      visit_checked(bo.right_arg, bo.left_arg->meta->ty);
      bo.meta->ty = Type::BOOL;
      break;
    }
  }

  void visit(UnopApp const &uo) override {
    switch (uo.op) {
    case Unop::Negate:
    case Unop::BitNot:
      visit_checked(uo.arg, Type::INT64);
      uo.meta->ty = Type::INT64;
      break;
    case Unop::LogNot:
      visit_checked(uo.arg, Type::BOOL);
      uo.meta->ty = Type::BOOL;
      break;
    }
  }
};

void type_check(Program &src_prog) {
  TypeChecker tyc{src_prog.symbol_table};
  for (auto &stmt : src_prog.body)
    stmt->accept(tyc);
}

} // namespace check
} // namespace bx