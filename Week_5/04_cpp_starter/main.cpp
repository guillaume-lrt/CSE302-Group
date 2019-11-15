#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <memory>

#include "antlr4-runtime.h"

#include "ast.h"
#include "rtl.h"
#include "type_check.h"
#include "ast_rtl.h"
#include "rtl_asm.h"
#include "amd64.h"

using namespace bx;

int main(int argc, char *argv[]) {
  const std::string rt_flags = "-L build -lbxrt -Wl,-rpath," +
    std::filesystem::current_path().string() +
    "/build/";

  if (argc >= 2) {
    std::string bx_file{argv[1]};

    if (bx_file.size() < 3 || bx_file.substr(bx_file.size() - 3, 3) != ".bx") {
      std::cerr << "Bad file name: " << bx_file << std::endl;
      std::exit(1);
    }

    auto file_root = bx_file.substr(0, bx_file.size() - 3);

    auto prog = source::read_program(bx_file);
    check::type_check(prog);
    std::cout << bx_file << " parsed and type checked.\n";
    auto p_file = file_root + ".parsed";
    std::ofstream p_out;
    p_out.open(p_file);
    p_out << prog;
    p_out.close();
    std::cout << p_file << " written.\n";

    auto rtl_file = file_root + ".rtl";
    std::pair<rtl::Program, std::map<std::string, int>> transform_res = rtl::transform(prog);
    rtl::Program rtl_prog = transform_res.first;
    std::map<std::string, int> global_vars = transform_res.second;

    std::ofstream rtl_out;
    rtl_out.open(rtl_file);
    for (auto const &gv : prog.global_vars)
      rtl_out << "GLOBAL " << gv.first << " = " << *(gv.second->init) << " : "
        << gv.second->ty << "\n\n";
    for (auto const &rtl_cbl : rtl_prog)
      rtl_out << rtl_cbl << '\n';
    rtl_out.close();
    std::cout << rtl_file << " written.\n";

    auto s_file = file_root + ".s";
    auto asm_prog = rtl_to_asm(rtl_prog);
    std::ofstream s_out(s_file);

    for (auto const &var : global_vars)
      s_out << ".globl " << var.first << "\n";

    s_out << ".section .data\n";
    s_out << ".align 8\n";

    for (auto const &var : global_vars)
      s_out << var.first << ": .quad " << var.second << "\n";

    for (auto const &func : asm_prog)
      for (auto const &l : func)
        s_out << *l;

    s_out.close();
    std::cout << s_file << " written.\n";

    auto exe_file = file_root + ".exe";
    std::string cmd = "gcc -O2 -o " + exe_file + " " + s_file + " " + rt_flags;
    // std::cout << "Running: " << cmd << std::endl;
    if (std::system(cmd.c_str()) != 0) {
      std::cerr << "Could not run gcc successfully!\n";
      std::exit(2);
    }
    std::cout << exe_file << " created.\n";
  }
}
