#ifndef bin_clang_replAstVisitor_h
#define bin_clang_replAstVisitor_h

#include "clangHeaders.h"
#include "findAstVisitor.h"
#include "foundMacro.h"

class ReplGlobalASTVisitor : public clang::RecursiveASTVisitor<ReplGlobalASTVisitor> {
 public:
  ReplGlobalASTVisitor(clang::Rewriter &R, clang::CompilerInstance& C,
                       FindGlobalASTVisitor& F) : TheRewriter(R), CI(C), finder(F), mlist(C) {}

  bool VisitDeclRefExpr(clang::DeclRefExpr* expr);

  bool VisitStmt(clang::Stmt *S);

  bool VisitDecl(clang::Decl *D);

 private:
  clang::Rewriter& TheRewriter;
  clang::CompilerInstance& CI;
  FindGlobalASTVisitor& finder;
  MacroList mlist;

};


#endif
