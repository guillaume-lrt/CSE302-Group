#include <stdexcept>

#include "ast_rtl.h"
#include "amd64.h"

namespace bx {

namespace rtl {

using source::Type;

int last_pseudo = 0;
inline rtl::Pseudo fresh_pseudo() { return rtl::Pseudo{last_pseudo++}; }

int last_label = 0;
inline rtl::Label fresh_label() { return rtl::Label{last_label++}; }

std::map<std::string, int> global_vars;

/**
 * A common generator for both expressions and statements
 *
 * It could be possible to break this up into multiple generators
 * for statements, int64 expressions, boolean expressions, etc. with modest
 * increase in code complexity.
 */
struct RtlGen : public source::StmtVisitor, public source::ExprVisitor {
  /** input label where "next" instruction will be
   *
   * After code gen:
   *
   *   - for int64 expressions and statements: in_label becomes location of
   *     next instruction
   *
   *   - for bool expressions: in_label becomes location of true branch
   */
  rtl::Label in_label;
  /**
   * For boolean expressions: false_label becomes location of false branch
   */
  rtl::Label false_label{-1};
  /**
   * For int64 expressions: result becomes destination for value
   */
  rtl::Pseudo result{-1};

private:
  source::Program const &srcprog;
  rtl::Callable rtl_callable;

  /**
   * Mapping from variables to pseudos
   */
  std::unordered_map<std::string, rtl::Pseudo> var_table;
  std::unordered_map<std::string, rtl::Pseudo> global_var_table;

  /**
   * Check if the variable is mapped to a pseudo; if not, create a fresh such
   * mapping. Return the pseudo in either case.
   */
  rtl::Pseudo get_pseudo(std::string const v) {
    if (global_vars.find(v) == global_vars.end()) {
      // Not a global var
      if (var_table.find(v) == var_table.end())
        var_table[v] = fresh_pseudo();
      return var_table.at(v);
    }
    if (global_var_table.find(v) == global_var_table.end()) {
      auto pseudo = fresh_pseudo();
      add_sequential([&](auto next) {
          return Load::make(v, 0, pseudo, next);
      });
      global_var_table[v] = pseudo;
    }
    return global_var_table.at(v);
  }

  /**
   * Add an instruction by generating a next label, and then updating in_label
   * to that next label.
   *
   * @param use_label A function that creates an Instr* using the generated next
   * label
   */
  template <typename LabelUser>
  inline void add_sequential(LabelUser use_label) {
    auto next_label = fresh_label();
    rtl_callable.add_instr(in_label, use_label(next_label));
    in_label = next_label;
  }

  /**
   * Force the bool result into an int64 result
   */
  void intify() {
    result = fresh_pseudo();
    auto next_label = fresh_label();
    rtl_callable.add_instr(in_label, Move::make(1, result, next_label));
    rtl_callable.add_instr(false_label, Move::make(0, result, next_label));
    in_label = next_label;
  }

  /**
   * Get a fresh copy of the result to avoid clobbering it
   */
  rtl::Pseudo copy_of_result() {
    auto reg = fresh_pseudo();
    add_sequential([&](auto next) { return Copy::make(result, reg, next); });
    return reg;
  }

public:
  RtlGen(source::Program const &srcprog, std::string const &name)
    : srcprog{srcprog}, rtl_callable{name} {
      int pseudo_count = last_pseudo;

      auto &func = srcprog.callables.at(rtl_callable.name);
      // Basic information
      for (auto &arg : func->args) {
        rtl_callable.input_regs.push_back(get_pseudo(arg.first));
      }
      if (func->return_ty == Type::UNKNOWN) {
        rtl_callable.output_reg = rtl::discard_pr;
      }
      else {
        rtl_callable.output_reg = fresh_pseudo();
      }
      rtl_callable.enter = fresh_label();
      rtl_callable.leave = fresh_label();
      in_label = rtl_callable.enter;

      // New frame
      auto fl = fresh_label();
      auto il = in_label;
      in_label = fl;

      // Save non-volatitle registers
      std::vector<const char*> non_volatile_regs = {
        bx::amd64::reg::rbx,
        bx::amd64::reg::rbp,
        bx::amd64::reg::r12,
        bx::amd64::reg::r13,
        bx::amd64::reg::r14,
        bx::amd64::reg::r15,
      };
      std::vector<rtl::Pseudo> saved_ps;
      for (auto nreg : non_volatile_regs) {
        Pseudo pseudo = fresh_pseudo();
        saved_ps.push_back(pseudo);
        add_sequential([&] (auto next) {
            return CopyMP::make(nreg, pseudo, next);
        });
      }

      // Parse arguments
      int arg_count = func->args.size();
      std::vector<const char*> volatile_regs = {
        bx::amd64::reg::rdi,
        bx::amd64::reg::rsi,
        bx::amd64::reg::rdx,
        bx::amd64::reg::rcx,
        bx::amd64::reg::r8,
        bx::amd64::reg::r9,
      };
      for (int i = 0; i < std::min(arg_count, (int) volatile_regs.size()); i++) {
        add_sequential([&] (auto next) {
            return CopyMP::make(volatile_regs[i], rtl_callable.input_regs[i], next);
        });
      }
      // More than 6 args -> On stack
      if (arg_count > 6) {
        for (int i = 6; i < arg_count; i++) {
          add_sequential([&] (auto next) {
              return LoadParam::make(i-5, rtl_callable.input_regs[i], next);
          });
        }
      }

      func->body->accept(*this);
      
      // Restore callee saved registers
      for (int i = 0; i < 6; i++) {
        add_sequential([&] (auto next) {
            return CopyPM::make(saved_ps[i], non_volatile_regs[i], next);
        });
      }

      // Return value in rax
      if (func->return_ty != Type::UNKNOWN) {
        add_sequential([&] (auto next) {
            return CopyPM::make(rtl_callable.output_reg, bx::amd64::reg::rax, next);
        });
      }
      rtl_callable.add_instr(rtl_callable.leave, Goto::make(in_label));

      // Resize new frame
      pseudo_count -= last_pseudo;
      rtl_callable.add_instr(il, NewFrame::make(fl, pseudo_count));

      // Finish
      add_sequential([&] (auto next) {
          return DelFrame::make(next);
      });
      add_sequential([&] (auto next) {
          return Return::make();
      });
  }

  void visit(source::Assign const &mv) override {
    auto source_reg = get_pseudo(mv.left);
    mv.right->accept(*this);
    if (mv.right->meta->ty == Type::BOOL)
      intify();
    if (global_var_table.find(mv.left) != global_var_table.end()) {
      add_sequential([&](auto next) {
          return Store::make(result, mv.left, 0, next);
      });
    }
    add_sequential([&](auto next) {
      return Copy::make(result, source_reg, next);
    });
  }

  void visit(source::Eval const &e) override {
    e.expr->accept(*this);
    if (e.expr->meta->ty == Type::BOOL) {
      intify();
    }
  }

  void visit(source::Print const &pr) override {
    pr.arg->accept(*this);
    if (pr.arg->meta->ty == Type::BOOL)
      intify();
    auto func =
        pr.arg->meta->ty == Type::INT64 ? "bx_print_int" : "bx_print_bool";
    add_sequential([&](auto next) {
      return CopyPM::make(result, bx::amd64::reg::rdi, next);
    });
    add_sequential([&](auto next) {
      return Call::make(func, 1, next);
    });
  }

  void visit(source::Block const &bl) override {
    for (auto const &stmt : bl.body)
      stmt->accept(*this);
  }

  void visit(source::IfElse const &ie) override {
    ie.condition->accept(*this);
    // save a copy of the outputs
    auto then_label = in_label, else_label = false_label;
    auto next_label = fresh_label();
    // put the then-block at the then_label
    in_label = then_label;
    ie.true_branch->accept(*this);
    rtl_callable.add_instr(in_label, Goto::make(next_label));
    // put the else-block at the else_label
    in_label = else_label;
    ie.false_branch->accept(*this);
    rtl_callable.add_instr(in_label, Goto::make(next_label));
    // now both branches have reached next_label
    in_label = next_label;
  }

  void visit(source::While const &wh) override {
    // save a copy of the while loop enter
    auto while_enter_label = in_label;
    wh.condition->accept(*this);
    // save a copy of the false_label as that is the ultimate exit label
    auto condition_exit_label = false_label;
    wh.loop_body->accept(*this);
    rtl_callable.add_instr(in_label, Goto::make(while_enter_label));
    in_label = condition_exit_label;
  }
  void visit(source::Return const &ret) override {
    if (ret.arg) {
      ret.arg->accept(*this);
      if (ret.arg->meta->ty == Type::BOOL) {
        intify();
      }
      if (rtl_callable.output_reg != rtl::discard_pr) {
        add_sequential([&](auto next) {
          return Copy::make(result, rtl_callable.output_reg, next);
        });
      }
      add_sequential([&](auto next) {
        return CopyPM::make(rtl_callable.output_reg, bx::amd64::reg::rax, next);
      });
    }
    add_sequential([&](auto next) {
      return Goto::make(rtl_callable.leave);
    });
  }

  void visit(source::Variable const &v) override {
    result = get_pseudo(v.label);
    if (v.meta->ty == Type::BOOL) {
      false_label = fresh_label();
      add_sequential([&](auto next) {
        return Ubranch::make(rtl::Ubranch::JNZ, result, next, false_label);
      });
    }
  }

  void visit(source::IntConstant const &k) override {
    result = fresh_pseudo();
    add_sequential([&](auto next) {
      return Move::make(k.value, result, next);
    });
  }

  void visit(source::BoolConstant const &k) override {
    if (k.value) {
      // in_label does not change
      false_label = fresh_label();
    } else {
      false_label = in_label;
      in_label = fresh_label();
    }
  }

  void visit(source::UnopApp const &uo) override {
    uo.arg->accept(*this);
    switch (uo.op) {
    case source::Unop::BitNot:
    case source::Unop::Negate: {
      result = copy_of_result();
      auto rtl_op =
          uo.op == source::Unop::BitNot ? rtl::Unop::NOT : rtl::Unop::NEG;
      add_sequential(
          [&](auto next) { return Unop::make(rtl_op, result, next); });
    } break;
    case source::Unop::LogNot: {
      auto l = false_label;
      false_label = in_label;
      in_label = l;
    } break;
    default:
      throw std::runtime_error("Cannot compile unary operator");
      break;
    }
  }

  void visitIntBinop(source::BinopApp const &bo) {
    rtl::Binop::Code rtl_op;
    // clang-format off
    switch (bo.op) {
    case source::Binop::Add:      rtl_op = rtl::Binop::ADD; break;
    case source::Binop::Subtract: rtl_op = rtl::Binop::SUB; break;
    case source::Binop::Multiply: rtl_op = rtl::Binop::MUL; break;
    case source::Binop::Divide:   rtl_op = rtl::Binop::DIV; break;
    case source::Binop::Modulus:  rtl_op = rtl::Binop::REM; break;
    case source::Binop::BitAnd:   rtl_op = rtl::Binop::AND; break;
    case source::Binop::BitOr:    rtl_op = rtl::Binop::OR;  break;
    case source::Binop::BitXor:   rtl_op = rtl::Binop::XOR; break;
    case source::Binop::Lshift:   rtl_op = rtl::Binop::SAL; break;
    case source::Binop::Rshift:   rtl_op = rtl::Binop::SAR; break;
    default: return; // case not relevant
    }
    // clang-format on
    bo.left_arg->accept(*this);
    auto left_result = copy_of_result();
    bo.right_arg->accept(*this);
    auto right_result = result;
    add_sequential([&](auto next) {
      return Binop::make(rtl_op, right_result, left_result, next);
    });
    result = left_result;
  }

  void visitBoolBinop(source::BinopApp const &bo) {
    if (bo.op != source::Binop::BoolAnd && bo.op != source::Binop::BoolOr)
      return; // case not relevant
    bo.left_arg->accept(*this);
    auto left_true_label = in_label, left_false_label = false_label;
    in_label =
        bo.op == source::Binop::BoolAnd ? left_true_label : left_false_label;
    bo.right_arg->accept(*this);
    auto right_true_label = in_label, right_false_label = false_label;
    if (bo.op == source::Binop::BoolAnd) {
      rtl_callable.add_instr(right_false_label, Goto::make(left_false_label));
      false_label = left_false_label;
    } else {
      rtl_callable.add_instr(right_true_label, Goto::make(left_true_label));
      in_label = left_true_label;
    }
  }

  void visitIneqop(source::BinopApp const &bo) {
    rtl::Bbranch::Code rtl_op;
    // clang-format off
    switch (bo.op) {
    case source::Binop::Lt:  rtl_op = rtl::Bbranch::JL;  break;
    case source::Binop::Leq: rtl_op = rtl::Bbranch::JLE; break;
    case source::Binop::Gt:  rtl_op = rtl::Bbranch::JG;  break;
    case source::Binop::Geq: rtl_op = rtl::Bbranch::JGE; break;
    default: return; // case not relevant
    }
    // clang-format on
    bo.left_arg->accept(*this);
    auto left_result = result; // save
    bo.right_arg->accept(*this);
    auto right_result = result; // save
    false_label = fresh_label();
    add_sequential([&](auto next) {
      return Bbranch::make(rtl_op, left_result, right_result, next, false_label);
    });
  }

  void visitEqop(source::BinopApp const &bo) {
    if (bo.op != source::Binop::Eq && bo.op != source::Binop::Neq)
      return; // case not relevant
    bo.left_arg->accept(*this);
    if (bo.left_arg->meta->ty == Type::BOOL)
      intify();
    auto left_result = result;
    bo.right_arg->accept(*this);
    if (bo.right_arg->meta->ty == Type::BOOL)
      intify();
    false_label = fresh_label();
    auto bbr_op =
        bo.op == source::Binop::Eq ? rtl::Bbranch::JE : rtl::Bbranch::JNE;
    add_sequential([&](auto next) {
      return Bbranch::make(bbr_op, left_result, result, next, false_label);
    });
  }

  void visit(source::BinopApp const &bo) override {
    // try all four visits; at most one of them will work
    visitIntBinop(bo);
    visitBoolBinop(bo);
    visitIneqop(bo);
    visitEqop(bo);
  }

  void visit(source::Call const &p) override {
    std::vector<Pseudo> args;
    for (auto &arg: p.args) {
      arg->accept(*this);
      args.push_back(result);
    }

    int arg_count = args.size();
    std::vector<const char*> volatile_regs = {
      bx::amd64::reg::rdi,
      bx::amd64::reg::rsi,
      bx::amd64::reg::rdx,
      bx::amd64::reg::rcx,
      bx::amd64::reg::r8,
      bx::amd64::reg::r9,
    };
    for (int i = 0; i < std::min(arg_count, (int) volatile_regs.size()); i++) {
      add_sequential([&] (auto next) {
          return CopyPM::make(args[i], volatile_regs[i], next);
      });
    }
    // More than 6 args -> Push to stack in reverse
    if (arg_count > 6) {
      for (int i = arg_count-1; i >= 6; i--) {
        add_sequential([&] (auto next) {
            return Push::make(args[i], next);
        });
      }
    }

    // Ending function
    if (srcprog.callables.at(p.func)->return_ty == Type::UNKNOWN) {
      add_sequential([&] (auto next) {
          return Call::make(p.func, arg_count, next);
      });
      result = rtl::discard_pr;
    }
    else {
      add_sequential([&] (auto next) {
          return Call::make(p.func, arg_count, next);
      });
      add_sequential([&] (auto next) {
          return CopyMP::make(bx::amd64::reg::rax, result, next);
      });
      result = fresh_pseudo();
    }
  }
};

std::pair<rtl::Program, std::map<std::string, int>> transform(source::Program const &src_prog) {
  rtl::Program rtl_prog;
  for (auto const &callable : src_prog.callables) {
    RtlGen gen(src_prog, callable.first);
    // rtl_prog.push_back(callable);
  }

  std::map<std::string, int> global_vars_top;
  for (auto &var : src_prog.global_vars) {
    int *val = var.second->init->eval();
    if (val == NULL)
      std::cerr << "Initialization for " << var.first << " not found.\n";
    global_vars_top[var.first] = *val;
  }
  return {rtl_prog, global_vars_top};
}

} // namespace rtl

} // namespace bx
