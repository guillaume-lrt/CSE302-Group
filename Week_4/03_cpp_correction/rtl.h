#pragma once

#include <algorithm>
#include <cstdint>
#include <iostream>
#include <stdexcept>
#include <unordered_map>
#include <vector>

#include "ast.h"

/** This defines the RTL intermediate language */

namespace bx {

namespace rtl {

struct Label {
  int id;
  bool operator<(Label const &other) const noexcept { return id < other.id; }
  bool operator==(Label const &other) const noexcept { return id == other.id; }
};
std::ostream &operator<<(std::ostream &out, Label const &l);

struct Pseudo {
  int id;
  bool operator==(Pseudo const &other) const noexcept { return id == other.id; }
};
std::ostream &operator<<(std::ostream &out, Pseudo const &r);
constexpr Pseudo discard_pr{-1};

struct Instr;
struct Move;
struct Copy;
struct Binop;
struct Unop;
struct Bbranch;
struct Ubranch;
struct Goto;
struct Call;
struct Return;

struct InstrVisitor {
  virtual ~InstrVisitor() = default;
#define VISIT_FUNCTION(caseclass) virtual void visit(caseclass const &) = 0
  VISIT_FUNCTION(Move);
  VISIT_FUNCTION(Copy);
  VISIT_FUNCTION(Binop);
  VISIT_FUNCTION(Unop);
  VISIT_FUNCTION(Bbranch);
  VISIT_FUNCTION(Ubranch);
  VISIT_FUNCTION(Goto);
  VISIT_FUNCTION(Call);
  VISIT_FUNCTION(Return);
#undef VISIT_FUNCTION
};

struct Instr {
  virtual ~Instr() = default;
  virtual std::ostream &print(std::ostream &out) const = 0;
  virtual void accept(InstrVisitor &vis) = 0;
};

inline std::ostream &operator<<(std::ostream &out, Instr const &i) {
  return i.print(out);
}

#define MAKE_VISITABLE                                                         \
  void accept(InstrVisitor &vis) override { vis.visit(*this); }

struct Move : public Instr {
  int64_t source;
  Pseudo dest;
  Label succ;

  Move(int64_t source, Pseudo dest, Label succ)
      : source{source}, dest{dest}, succ{succ} {}
  std::ostream &print(std::ostream &out) const override {
    return out << "move " << source << ", " << dest << "  --> " << succ;
  }
  MAKE_VISITABLE
};

struct Copy : public Instr {
  Pseudo src, dest;
  Label succ;

  Copy(Pseudo src, Pseudo dest, Label succ)
      : src{src}, dest{dest}, succ{succ} {}
  std::ostream &print(std::ostream &out) const override {
    return out << "copy " << src << ", " << dest << "  --> " << succ;
  }
  MAKE_VISITABLE
};

struct Unop : public Instr {
  enum Code : uint16_t { NEG, NOT };

  Code opcode;
  Pseudo arg;
  Label succ;

  Unop(Code opcode, Pseudo arg, Label succ)
      : opcode{opcode}, arg{arg}, succ{succ} {}
  std::ostream &print(std::ostream &out) const override {
    return out << "unop " << code_map.at(opcode) << ", " << arg << "  --> "
               << succ;
  }
  MAKE_VISITABLE

private:
  static const std::map<Code, char const *> code_map;
};

struct Binop : public Instr {
  enum Code : uint16_t {
    // clang-format off
    ADD, SUB, MUL, DIV, REM, SAL, SAR, AND, OR, XOR
    // clang-format on
  };

  Code opcode;
  Pseudo src, dest;
  Label succ;

  Binop(Code opcode, Pseudo src, Pseudo dest, Label succ)
      : opcode{opcode}, src{src}, dest{dest}, succ{succ} {}
  std::ostream &print(std::ostream &out) const override {
    return out << "binop " << code_map.at(opcode) << ", " << src << ", " << dest
               << "  --> " << succ;
  }
  MAKE_VISITABLE

private:
  static const std::map<Code, char const *> code_map;
};

struct Ubranch : public Instr {
  enum Code : uint16_t { JZ, JNZ };

  Code opcode;
  Pseudo arg;
  Label succ, fail;

  Ubranch(Code opcode, Pseudo arg, Label succ, Label fail)
      : opcode{opcode}, arg{arg}, succ{succ}, fail{fail} {}
  std::ostream &print(std::ostream &out) const override {
    return out << "ubranch " << code_map.at(opcode) << ", " << arg << "  --> "
               << succ << ", " << fail;
  }
  MAKE_VISITABLE

private:
  static const std::map<Code, char const *> code_map;
};

struct Bbranch : public Instr {
  enum Code : uint16_t {
    // clang-format off
    JE,  JL,  JLE,  JG,  JGE,
    JNE, JNL, JNLE, JNG, JNGE
    // clang-format on
  };

  Code opcode;
  Pseudo arg1, arg2;
  Label succ, fail;

  Bbranch(Code opcode, Pseudo arg1, Pseudo arg2, Label succ, Label fail)
      : opcode{opcode}, arg1{arg1}, arg2{arg2}, succ{succ}, fail{fail} {}
  std::ostream &print(std::ostream &out) const override {
    return out << "bbranch " << code_map.at(opcode) << ", " << arg1 << ", "
               << arg2 << "  --> " << succ << ", " << fail;
  }
  MAKE_VISITABLE

private:
  static const std::map<Code, char const *> code_map;
};

struct Goto : public Instr {
  Label succ;

  Goto(Label succ) : succ{succ} {}
  std::ostream &print(std::ostream &out) const override {
    return out << "goto  --> " << succ;
  }
  MAKE_VISITABLE
};

struct Call : public Instr {
  std::string func;
  std::vector<Pseudo> args;
  Pseudo ret;
  Label succ;

  Call(std::string const func, std::vector<Pseudo> args, Pseudo ret, Label succ)
      : func{func}, args{args}, ret{ret}, succ{succ} {}
  Call(char const *func, std::vector<Pseudo> args, Pseudo ret, Label succ)
      : Call{std::string{func}, args, ret, succ} {}

  std::ostream &print(std::ostream &out) const override {
    out << "call " << func << "(";
    for (auto it = args.cbegin(); it != args.cend(); it++) {
      out << *it;
      if (it + 1 != args.cend())
        out << ", ";
    }
    return out << "), " << ret << "  --> " << succ;
  }
  MAKE_VISITABLE
};

struct Return : public Instr {
  Pseudo arg;

  Return(Pseudo arg) : arg{arg} {}
  std::ostream &print(std::ostream &out) const override {
    return out << "return " << arg;
  }
  MAKE_VISITABLE
};
#undef MAKE_VISITABLE

struct LabelHash {
  std::size_t operator()(Label const &l) const noexcept {
    return std::hash<int>{}(l.id);
  }
};
struct LabelEq {
  constexpr bool operator()(Label const &l, Label const &r) const noexcept {
    return l.id == r.id;
  }
};
template <typename V>
using LabelMap = std::unordered_map<Label, V, LabelHash, LabelEq>;

struct Program {
  std::string name;
  Label enter{-1}, leave{-1};
  LabelMap<std::unique_ptr<Instr>> body;
  std::vector<Label> schedule; // the order in which the labels are scheduled
  Program(std::string name) : name{name} {}
  void add_instr(Label lab, Instr *i) {
    auto instr = std::unique_ptr<Instr>{i};
    if (body.find(lab) != body.end())
      throw std::runtime_error("repeated in-label " + lab.id);
    schedule.push_back(lab);
    body.insert({lab, std::move(instr)});
  }
};

} // namespace rtl

} // namespace bx