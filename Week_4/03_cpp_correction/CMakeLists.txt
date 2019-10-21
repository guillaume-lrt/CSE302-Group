### This is a portable distribution of BX. It will compile on standard Linux
### installations and will download and compile the Antlr 4.7.2 runtime as
### part of the build process.

cmake_minimum_required(VERSION 3.10)
project(bx VERSION 2.0 DESCRIPTION "BX 2.0 compiler")

### Change the bx-GRAMMAR and bx-SRC variables below in order
### as your project evolves.
set(bx-GRAMMAR
  ${PROJECT_SOURCE_DIR}/BX.g4
)
set(bx-SRC
  ${PROJECT_SOURCE_DIR}/ast.cpp
  ${PROJECT_SOURCE_DIR}/type_check.cpp
  ${PROJECT_SOURCE_DIR}/rtl.cpp
  ${PROJECT_SOURCE_DIR}/ast_rtl.cpp
  ${PROJECT_SOURCE_DIR}/amd64.cpp
  ${PROJECT_SOURCE_DIR}/rtl_asm.cpp
  ${PROJECT_SOURCE_DIR}/main.cpp
)

################################################################################
################################################################################
## You should not need to change anything in the rest of the file

set(CMAKE_CXX_STANDARD 17)

find_package(Java REQUIRED)

set(ANTLR_EXECUTABLE ${PROJECT_SOURCE_DIR}/tools/antlr-4.7.2-complete.jar)

set(bx-GENERATED_DIR ${PROJECT_SOURCE_DIR}/generated)

set(bx-GENERATED_SRC
  ${bx-GENERATED_DIR}/BXParser.cpp
  ${bx-GENERATED_DIR}/BXLexer.cpp
  ${bx-GENERATED_DIR}/BXListener.cpp
  ${bx-GENERATED_DIR}/BXBaseListener.cpp
)

set_source_files_properties(${bx-GENERATED_SRC}
  PROPERTIES
  GENERATED TRUE
  COMPILE_OPTIONS "-Wno-attributes"
)

add_custom_target(GenerateParser DEPENDS ${bx-GENERATED_SRC})
add_custom_command(OUTPUT ${bx-GENERATED_SRC}
  COMMAND
  ${CMAKE_COMMAND} -E make_directory ${PROJECT_SOURCE_DIR}/generated/
  COMMAND
  "${Java_JAVA_EXECUTABLE}" -jar ${ANTLR_EXECUTABLE} -Dlanguage=Cpp -listener -no-visitor -o ${bx-GENERATED_DIR} -package bx ${bx-GRAMMAR}
  WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
  DEPENDS ${bx-SRC}
)

include_directories(
  ${bx-GENERATED_DIR}
  /usr/include/antlr4-runtime
  /users/profs/info/kaustuv.chaudhuri/build/antlr4/run/usr/local/include/antlr4-runtime
)
link_directories(
  /users/profs/info/kaustuv.chaudhuri/build/antlr4/run/usr/local/lib
)

set_source_files_properties(${bx-SRC} PROPERTIES
  COMPILE_OPTIONS "-Wall;-Wextra;-Wpedantic;-Wno-attributes")

add_executable(bx.exe
  ${bx-SRC}
  ${bx-GENERATED_SRC}
)

add_dependencies(bx.exe GenerateParser)

target_link_libraries(bx.exe antlr4-runtime)

target_link_options(bx.exe PUBLIC "-Wl,-rpath,/usr/local/gcc-9.2.0/lib64")
