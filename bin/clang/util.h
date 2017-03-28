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

  void print(const clang::Stmt* s){
    s->printPretty(os, nullptr, Policy);
  }

  void print(const clang::Decl* d){
    d->print(os, Policy);
  }

  std::string
  print(const clang::BuiltinType* ty){
    return ty->getName(Policy).str();
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

bool isCxx(const std::string& filename);
bool isValidSrc(const std::string& filename);

void errorAbort(clang::SourceLocation loc, clang::CompilerInstance& CI, const std::string& error);

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
