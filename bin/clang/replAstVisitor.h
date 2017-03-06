#ifndef bin_clang_replAstVisitor_h
#define bin_clang_replAstVisitor_h

#include "clangHeaders.h"
#include "findAstVisitor.h"

class ReplGlobalASTVisitor : public clang::RecursiveASTVisitor<ReplGlobalASTVisitor> {
 public:
  ReplGlobalASTVisitor(clang::Rewriter &R, clang::CompilerInstance& C,
                       FindGlobalASTVisitor& F) : TheRewriter(R), CI(C), finder(F) {}

  bool VisitDeclRefExpr(clang::DeclRefExpr* expr);

 private:
  clang::Rewriter& TheRewriter;
  clang::CompilerInstance& CI;
  FindGlobalASTVisitor& finder;

};


#endif
