#include <cstdlib>
#include <iostream>
#include <memory>

#include "antlr4-runtime.h"

#include "amd64.h"
#include "ast.h"
#include "ast_rtl.h"
#include "rtl.h"
#include "rtl_asm.h"
#include "type_check.h"

using namespace bx;

static const std::string rt_file = "bx1rt.c";

int main(int argc, char *argv[]) {

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
    auto rtl_prog = rtl::transform(prog);
    std::ofstream rtl_out;
    rtl_out.open(rtl_file);
    rtl_out << "enter: " << rtl_prog.enter << '\n';
    rtl_out << "leave: " << rtl_prog.leave << '\n';
    rtl_out << "----\n";
    for (auto const &l : rtl_prog.schedule) {
      rtl_out << l << ": " << *(rtl_prog.body.find(l)->second) << '\n';
    }
    rtl_out.close();
    std::cout << rtl_file << " written.\n";

    auto s_file = file_root + ".s";
    auto asm_prog = rtl_to_asm(rtl_prog);

    std::ofstream s_out;
    s_out.open(s_file);
    for (auto const &l : asm_prog)
      s_out << *l;
    s_out.close();
    std::cout << s_file << " written.\n";

    auto exe_file = file_root + ".exe";
    std::string cmd = "gcc -O2 -o " + exe_file + " " + s_file + " " + rt_file;
    // std::cout << "Running: " << cmd << std::endl;
    if (std::system(cmd.c_str()) != 0) {
      std::cerr << "Could not run gcc successfully!\n";
      std::exit(2);
    }
    std::cout << exe_file << " created.\n";
  }
}
