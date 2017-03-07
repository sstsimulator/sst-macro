#ifndef bin_clang_util_h
#define bin_clang_util_h

#include "clangHeaders.h"

inline bool operator<=(const clang::SourceLocation &LHS, const clang::SourceLocation &RHS) {
  return LHS < RHS || LHS == RHS;
}

inline bool operator>(const clang::SourceLocation &LHS, const clang::SourceLocation &RHS) {
  return !(LHS < RHS) && !(LHS == RHS);
}

inline bool operator>=(const clang::SourceLocation &LHS, const clang::SourceLocation &RHS) {
  return !(LHS < RHS);
}

#endif
