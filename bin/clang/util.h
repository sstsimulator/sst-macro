#ifndef bin_clang_util_h
#define bin_clang_util_h

#include "clangHeaders.h"
#include <iostream>

struct PrettyPrinter
{
  clang::LangOptions LangOpts;
  clang::PrintingPolicy Policy;
  llvm::raw_string_ostream os;
  PrettyPrinter() : Policy(LangOpts), os(baseStr)
  {
    LangOpts.CPlusPlus = true;
  }

  ~PrettyPrinter(){
    //must be flushed
    //prior to deleting string
    os.flush();
  }

  void print(clang::Stmt* s){
    s->printPretty(os, nullptr, Policy);
  }

  void dump(std::ostream& sos = std::cout){
    sos << os.str() << std::endl;
  }

  std::string
  str() {
    return os.str();
  }

 private:
  std::string baseStr;
};

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
