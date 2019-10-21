#pragma once

#include <iostream>
#include <memory>
#include <string>
#include <type_traits>

#include "antlr4-runtime.h"

namespace bx {

////////////////////////////////////////////////////////////////////////////////
// Source AST

namespace source {

// Types
enum class Type { INT64 = 0, BOOL = 1, UNKNOWN = -1 };

// clang-format off
enum class Binop {
  Add, Subtract, Multiply, Divide, Modulus,
  BitAnd, BitOr, BitXor, Lshift, Rshift,
  Lt, Leq, Gt, Geq, Eq, Neq,
  LogAnd, LogOr
};
// clang-format on
std::ostream &operator<<(std::ostream &, const Binop);

enum class Unop { Negate, BitNot, LogNot };
std::ostream &operator<<(std::ostream &out, const Unop);

////////////////////////////////////////////////////////////////////////////////
// AST Nodes

struct ASTNode {
  virtual std::ostream &print(std::ostream &out) const = 0;
  virtual ~ASTNode() = default;
};
std::ostream &operator<<(std::ostream &out, ASTNode const &e);

////////////////////////////////////////////////////////////////////////////////
// Expressions

class Variable;
class IntConstant;
class BoolConstant;
class UnopApp;
class BinopApp;

struct ExprVisitor {
  virtual void visit(Variable const &) = 0;
  virtual void visit(IntConstant const &) = 0;
  virtual void visit(BoolConstant const &) = 0;
  virtual void visit(UnopApp const &) = 0;
  virtual void visit(BinopApp const &) = 0;
};

struct Expr_ : public ASTNode {
  struct Meta {
    Type ty;
  };
  std::unique_ptr<Meta> meta{new Meta{Type::UNKNOWN}};
  virtual int binding_priority() const { return INT_MAX; }
  virtual void accept(ExprVisitor &vis) = 0;
};
using Expr = std::unique_ptr<Expr_>;

#define MAKE_VISITABLE                                                         \
  void accept(ExprVisitor &vis) final { vis.visit(*this); }

struct Variable : public Expr_ {
  const std::string label;
  Variable(std::string label) : label(label) {}
  std::ostream &print(std::ostream &out) const override;
  MAKE_VISITABLE
};

struct IntConstant : public Expr_ {
  const int64_t value;
  IntConstant(int64_t value) : value(value) {}
  std::ostream &print(std::ostream &out) const override;
  MAKE_VISITABLE
};

struct BoolConstant : public Expr_ {
  const bool value;
  BoolConstant(bool value) : value(value) {}
  std::ostream &print(std::ostream &out) const override;
  MAKE_VISITABLE
};

struct UnopApp : public Expr_ {
  const Unop op;
  const Expr arg;
  UnopApp(Unop op, Expr arg) : op(op), arg{std::move(arg)} {}
  std::ostream &print(std::ostream &out) const override;
  int binding_priority() const override;
  MAKE_VISITABLE
};

struct BinopApp : public Expr_ {
  const Binop op;
  const Expr left_arg, right_arg;
  BinopApp(Expr left_arg, Binop op, Expr right_arg)
      : op(op), left_arg{std::move(left_arg)}, right_arg{std::move(right_arg)} {
  }
  std::ostream &print(std::ostream &out) const override;
  int binding_priority() const override;
  MAKE_VISITABLE
};
#undef MAKE_VISITABLE

////////////////////////////////////////////////////////////////////////////////
// Statements

class Assign;
class Print;
class Block;
class IfElse;
class While;

struct StmtVisitor {
  virtual void visit(Assign const &) = 0;
  virtual void visit(Print const &) = 0;
  virtual void visit(Block const &) = 0;
  virtual void visit(IfElse const &) = 0;
  virtual void visit(While const &) = 0;
};

class Stmt_ : public ASTNode {
public:
  virtual void accept(StmtVisitor &vis) = 0;
};
using Stmt = std::unique_ptr<Stmt_>;

#define MAKE_VISITABLE                                                         \
  void accept(StmtVisitor &vis) final { vis.visit(*this); }

class Print : public Stmt_ {
public:
  const Expr arg;
  Print(Expr arg) : arg{std::move(arg)} {}
  std::ostream &print(std::ostream &out) const override;
  MAKE_VISITABLE
};

class Assign : public Stmt_ {
public:
  const std::unique_ptr<Variable> left;
  const Expr right;
  Assign(std::unique_ptr<Variable> left, Expr right)
      : left{std::move(left)}, right{std::move(right)} {}
  std::ostream &print(std::ostream &out) const override;
  MAKE_VISITABLE
};

class Block : public Stmt_ {
public:
  const std::list<Stmt> body;
  Block(std::list<Stmt> body) : body{std::move(body)} {}
  std::ostream &print(std::ostream &out) const override;
  MAKE_VISITABLE
};

class IfElse : public Stmt_ {
public:
  const Expr condition;
  const Stmt true_branch, false_branch;
  IfElse(Expr condition, Stmt tru, Stmt fals)
      : condition{std::move(condition)}, true_branch{std::move(tru)},
        false_branch{std::move(fals)} {}
  std::ostream &print(std::ostream &out) const override;
  MAKE_VISITABLE
};

class While : public Stmt_ {
public:
  const Expr condition;
  const Stmt loop_body;
  While(Expr condition, Stmt loop_body)
      : condition{std::move(condition)}, loop_body{std::move(loop_body)} {}
  std::ostream &print(std::ostream &out) const override;
  MAKE_VISITABLE
};
#undef MAKE_VISITABLE

////////////////////////////////////////////////////////////////////////////////
// Variable declarations and programs

using SymbolTable = std::map<std::string, Type>;

struct Program {
  const SymbolTable symbol_table;
  const std::list<Stmt> body;
  Program(SymbolTable symbol_table, std::list<Stmt> body)
      : symbol_table{symbol_table}, body{std::move(body)} {}
};
std::ostream &operator<<(std::ostream &out, Program const &prog);

////////////////////////////////////////////////////////////////////////////////
// Parsing

source::Program read_program(std::string file);

} // namespace source

} // namespace bx
