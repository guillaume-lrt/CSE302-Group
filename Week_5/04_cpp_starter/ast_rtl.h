#pragma once

#include "ast.h"
#include "rtl.h"

namespace bx {
namespace rtl {

std::pair<rtl::Program, std::map<std::string, int>> transform(source::Program const &prog);

} // namespace rtl
} // namespace bx
