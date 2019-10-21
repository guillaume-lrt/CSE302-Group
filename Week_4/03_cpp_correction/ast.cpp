#include "ast.h"

#include "BXBaseListener.h"
#include "BXLexer.h"
#include "BXParser.h"

namespace bx {

namespace source {

std::ostream &operator<<(std::ostream &out, const Binop op) {
  switch (op) {
    // clang-format off
  case Binop::Add: return out << '+';
  case Binop::Subtract: return out << '-';
  case Binop::Multiply: return out << '*';
  case Binop::Divide: return out << '/';
  case Binop::Modulus: return out << '%';
  case Binop::BitAnd: return out << '&';
  case Binop::BitOr: return out << '|';
  case Binop::BitXor: return out << '^';
  case Binop::Lshift: return out << "<<";
  case Binop::Rshift: return out << ">>";
  case Binop::Lt: return out << "<";
  case Binop::Leq: return out << "<=";
  case Binop::Gt: return out << ">";
  case Binop::Geq: return out << ">=";
  case Binop::Eq: return out << "==";
  case Binop::Neq: return out << "!=";
  case Binop::LogAnd: return out << "&&";
  case Binop::LogOr: return out << "||";
  default: return out << "<?>";
    // clang-format on
  }
}

int BinopApp::binding_priority() const {
  switch (op) {
  case Binop::Lshift:
  case Binop::Rshift:
    return 40;
  case Binop::Lt:
  case Binop::Leq:
  case Binop::Gt:
  case Binop::Geq:
    return 36;
  case Binop::Eq:
  case Binop::Neq:
    return 33;
  case Binop::BitAnd:
    return 30;
  case Binop::BitXor:
    return 20;
  case Binop::BitOr:
    return 10;
  case Binop::LogAnd:
    return 6;
  case Binop::LogOr:
    return 3;
  case Binop::Add:
  case Binop::Subtract:
    return 50;
  case Binop::Multiply:
  case Binop::Divide:
  case Binop::Modulus:
    return 60;
  default:
    return -1;
  }
}

std::ostream &operator<<(std::ostream &out, const Unop op) {
  switch (op) {
    // clang-format off
  case Unop::Negate: return out << '-';
  case Unop::BitNot: return out << "~";
  case Unop::LogNot: return out << "!";
  default: return out << "<?>";
    // clang-format on
  }
}

std::ostream &operator<<(std::ostream &out, ASTNode const &node) {
  return node.print(out);
}

std::ostream &Variable::print(std::ostream &out) const {
  out << this->label;
  return out;
}

std::ostream &IntConstant::print(std::ostream &out) const {
  out << this->value;
  return out;
}

std::ostream &BoolConstant::print(std::ostream &out) const {
  out << (this->value ? "true" : "false");
  return out;
}

int UnopApp::binding_priority() const {
  switch (op) {
  case Unop::BitNot:
  case Unop::Negate:
    return 70;
  case Unop::LogNot:
    return 80;
  default:
    return -1;
  }
}

std::ostream &print_bracketed(std::ostream &out, Expr const &e, bool bracket) {
  if (bracket)
    out << '(';
  out << *e;
  if (bracket)
    out << ')';
  return out;
}

std::ostream &UnopApp::print(std::ostream &out) const {
  bool bracket = this->binding_priority() > this->arg->binding_priority();
  out << this->op << ' ';
  return print_bracketed(out, this->arg, bracket);
}

std::ostream &BinopApp::print(std::ostream &out) const {
  print_bracketed(out, this->left_arg,
                  this->binding_priority() >
                      this->left_arg->binding_priority());
  out << ' ' << this->op << ' ';
  print_bracketed(out, this->right_arg,
                  this->binding_priority() >
                      this->right_arg->binding_priority());
  return out;
}

std::ostream &Print::print(std::ostream &out) const {
  out << "print ";
  return this->arg->print(out) << ';';
}

std::ostream &Assign::print(std::ostream &out) const {
  this->left->print(out);
  out << " = ";
  return this->right->print(out) << ';';
}

std::ostream &Block::print(std::ostream &out) const {
  out << "{ \n";
  for (auto const &stmt : this->body)
    out << *stmt << "\n";
  out << "}";
  return out;
}

std::ostream &IfElse::print(std::ostream &out) const {
  out << "if (" << *(this->condition) << ") ";
  out << *(this->true_branch) << " else " << *(this->false_branch);
  return out;
}

std::ostream &While::print(std::ostream &out) const {
  out << "while (" << *(this->condition) << ") ";
  out << *(this->loop_body);
  return out;
}

inline std::ostream &operator<<(std::ostream &out, Type const &ty) {
  return out << (ty == Type::BOOL ? "bool" : "int64");
}

std::ostream &operator<<(std::ostream &out, Program const &prog) {
  for (auto const &[v, ty] : prog.symbol_table) {
    out << "var " << v << " : " << ty << ";\n";
  }
  for (auto const &stmt : prog.body)
    out << *stmt << '\n';
  return out;
}

class SourceReader : public BXBaseListener {
private:
  SymbolTable symbol_table;
  std::list<Stmt> stmt_stack;
  std::list<Expr> expr_stack;
  Type current_var_ty = Type::INT64;

public:
  Program get_prog() {
    return Program(std::move(this->symbol_table), std::move(this->stmt_stack));
  }

  // handle vardecls

  void enterVarDecl(BXParser::VarDeclContext *ctx) override {
    auto var = ctx->type()->getText();
    current_var_ty = var == "int64" ? Type::INT64 : Type::BOOL;
  }

  void exitVarInit(BXParser::VarInitContext *ctx) override {
    auto var = std::make_unique<Variable>(ctx->VAR()->getText());
    if (symbol_table.find(var->label) != symbol_table.end())
      throw std::runtime_error("redeclared variable: " + var->label);
    symbol_table.insert({var->label, current_var_ty});
    if (ctx->expr()) {
      auto var_init = std::move(expr_stack.back());
      expr_stack.pop_back();
      stmt_stack.push_back(
          std::make_unique<Assign>(std::move(var), std::move(var_init)));
    }
  }

  // handle statements

  void exitMove(BXParser::MoveContext *ctx) override {
    auto dest = std::make_unique<Variable>(ctx->VAR()->getText());
    auto source = std::move(expr_stack.back());
    this->expr_stack.pop_back();
    this->stmt_stack.push_back(
        std::make_unique<Assign>(std::move(dest), std::move(source)));
  }

  void exitPrint(BXParser::PrintContext *) override {
    auto dest = std::move(this->expr_stack.back());
    this->expr_stack.pop_back();
    this->stmt_stack.push_back(std::make_unique<Print>(std::move(dest)));
  }

  void exitBlock(BXParser::BlockContext *ctx) override {
    std::list<Stmt> block_stmts;
    for (auto block_len = ctx->stmt().size(); block_len-- > 0;) {
      block_stmts.push_front(std::move(this->stmt_stack.back()));
      this->stmt_stack.pop_back();
    }
    this->stmt_stack.push_back(std::make_unique<Block>(std::move(block_stmts)));
  }

  void exitWhileLoop(BXParser::WhileLoopContext *) override {
    auto condition = std::move(this->expr_stack.back());
    this->expr_stack.pop_back();
    auto body = std::move(this->stmt_stack.back());
    this->stmt_stack.pop_back();
    this->stmt_stack.push_back(
        std::make_unique<While>(std::move(condition), std::move(body)));
  }

  void exitIfElse(BXParser::IfElseContext *ctx) override {
    auto condition = std::move(this->expr_stack.back());
    this->expr_stack.pop_back();
    Stmt false_branch = std::make_unique<Block>(std::list<Stmt>{});
    if (ctx->ifCont() != nullptr) {
      false_branch = std::move(this->stmt_stack.back());
      this->stmt_stack.pop_back();
    }
    auto true_branch = std::move(this->stmt_stack.back());
    this->stmt_stack.pop_back();
    this->stmt_stack.push_back(std::make_unique<IfElse>(
        std::move(condition), std::move(true_branch), std::move(false_branch)));
  }

  // handle expressions

  void exitVariable(BXParser::VariableContext *ctx) override {
    std::string var = ctx->VAR()->getText();
    if (symbol_table.find(var) == symbol_table.end())
      throw std::runtime_error("unknown variable: " + var);
    this->expr_stack.push_back(std::make_unique<Variable>(var));
  }

  void exitNumber(BXParser::NumberContext *ctx) override {
    this->expr_stack.push_back(
        std::make_unique<IntConstant>(std::stoll(ctx->NUM()->getText())));
  }

  void exitBool(BXParser::BoolContext *ctx) override {
    this->expr_stack.push_back(
        std::make_unique<BoolConstant>(ctx->BOOL()->getText()[0] == 't'));
  }

  void exitUnop(BXParser::UnopContext *ctx) override {
    auto const &op_txt = ctx->op->getText();
    Unop op(op_txt == "-" ? Unop::Negate
                          : op_txt == "~" ? Unop::BitNot : Unop::LogNot);
    auto arg = std::move(this->expr_stack.back());
    this->expr_stack.pop_back();
    this->expr_stack.push_back(std::make_unique<UnopApp>(op, std::move(arg)));
  }

private:
  void processBinop(Binop op) {
    // note: reverse order
    auto right_arg = std::move(this->expr_stack.back());
    this->expr_stack.pop_back();
    auto left_arg = std::move(this->expr_stack.back());
    this->expr_stack.pop_back();
    this->expr_stack.push_back(std::make_unique<BinopApp>(
        std::move(left_arg), op, std::move(right_arg)));
  }

public:
  void exitMultiplicative(BXParser::MultiplicativeContext *ctx) override {
    std::string op = ctx->op->getText();
    this->processBinop(op[0] == '*'
                           ? Binop::Multiply
                           : op[0] == '/' ? Binop::Divide : Binop::Modulus);
  }

  void exitAdditive(BXParser::AdditiveContext *ctx) override {
    this->processBinop(ctx->op->getText()[0] == '+' ? Binop::Add
                                                    : Binop::Subtract);
  }

  void exitShift(BXParser::ShiftContext *ctx) override {
    this->processBinop(ctx->op->getText()[0] == '<' ? Binop::Lshift
                                                    : Binop::Rshift);
  }

  void exitInequation(BXParser::InequationContext *ctx) override {
    auto const &op_txt = ctx->op->getText();
    auto op = op_txt == "<"
                  ? Binop::Lt
                  : op_txt == "<=" ? Binop::Leq
                                   : op_txt == ">" ? Binop::Gt : Binop ::Geq;
    this->processBinop(op);
  }

  void exitEquation(BXParser::EquationContext *ctx) override {
    this->processBinop(ctx->op->getText() == "==" ? Binop::Eq : Binop::Neq);
  }

  void exitBitAnd(BXParser::BitAndContext *) override {
    this->processBinop(Binop::BitAnd);
  }

  void exitBitXor(BXParser::BitXorContext *) override {
    this->processBinop(Binop::BitXor);
  }

  void exitBitOr(BXParser::BitOrContext *) override {
    this->processBinop(Binop::BitOr);
  }

  void exitLogAnd(BXParser::LogAndContext *) override {
    this->processBinop(Binop::LogAnd);
  }

  void exitLogOr(BXParser::LogOrContext *) override {
    this->processBinop(Binop::LogOr);
  }
};

Program read_program(std::string file) {
  std::ifstream stream;
  stream.open(file);
  antlr4::ANTLRInputStream input(stream);
  BXLexer lexer(&input);
  antlr4::CommonTokenStream tokens(&lexer);
  BXParser parser(&tokens);
  antlr4::tree::ParseTree *tree = parser.program();
  SourceReader source_reader;
  antlr4::tree::ParseTreeWalker::DEFAULT.walk(&source_reader, tree);
  return source_reader.get_prog();
}

} // namespace source

} // namespace bx
