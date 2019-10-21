BX to AMD64
-----------

This is a compiler from the language BX version 1 (BX1) to AMD64
assembly.

The compiler is written in C++.

The grammar of BX is in the file BX.g4, form which the lexer/parser
combination is automatically generated using Antlr v4 (see below).

The syntax is type-cheked in type_check.{h,cpp}.

The RTL language is defined in rtl.{h,cpp}, and the RTL generator
based on bottom-up maximal munch is in ast_rtl.{h,cpp}.

The AMD64 assembly target is defined with amd64.{h,cpp}, and the
code generator from RTL to AMD64 is in rtl_asm.{h,cpp}.


Build Requirements
------------------

Things should already work for the lab computers. If you want to
install it on your own computers, do the following:

1. Antlr4 (http://antlr4.org), which is the parser generator
   recommended to use with Java. Specifically, get:
   - https://www.antlr.org/download/antlr-4.7.2-complete.jar
2. A recent Java 8 (at least version 1.8.0_112)
3. To get the Makefile to work, you will need to change the variables
   ANTLR4_JAR at the top of the file to point to where you placed the
   above antlr jar.
4. You also need the C++ runtime for antlr4:
   - https://www.antlr.org/download/antlr4-cpp-runtime-4.7.2-source.zip
   - alternately, install the libantlr4-runtime and libantlr4-runtime-dev
     packages from your package manager if using a Debain/Ubuntu
     Linux distribution or derivative
5. Once you have the Antlr/C++ runtime working, change the
   fields at the CFLAGS field at the top of the Makefile to point
   to the correct location of the header files.

If you don't use Linux, you will have to update the instructions to
fit your setup.

Development Requirements
------------------------

This project is set up to go with Visual Studio Code. The following
extensions are used:

1. C/C++ support
2. Antlr4 grammar syntax support
3. (Optional) Clang-format support
4. (Optional) CMake support

In addition you will also require CMake version 3.10+. In the X
Salles Info this is installed in /usr/local/cmake-3.14.0/bin
