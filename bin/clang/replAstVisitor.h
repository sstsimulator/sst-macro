#ifndef bin_clang_replAstVisitor_h
#define bin_clang_replAstVisitor_h

#include "clangHeaders.h"
#include "findAstVisitor.h"
#include "foundMacro.h"
#include "pragmas.h"

#define visitFxn(cls) \
  bool Visit##cls(clang::cls* c){ return TestStmtMacro(c); }

class ReplGlobalASTVisitor : public clang::RecursiveASTVisitor<ReplGlobalASTVisitor> {
 public:
  ReplGlobalASTVisitor(clang::Rewriter &R,  FindGlobalASTVisitor& F, std::set<clang::Expr*>& deld) :
    TheRewriter(R), finder(F), visitingGlobal(false), deleted(deld) {}

  bool VisitDeclRefExpr(clang::DeclRefExpr* expr);

  bool VisitCXXNewExpr(clang::CXXNewExpr* expr);

  bool VisitUnaryOperator(clang::UnaryOperator* op);

  bool VisitDecl(clang::Decl *D);

  void setCompilerInstance(clang::CompilerInstance& c){
    CI = &c;
  }

  bool TraverseFunctionTemplateDecl(clang::FunctionTemplateDecl* D);

  bool TraverseCXXMethodDecl(clang::CXXMethodDecl *D);

  void setVisitingGlobal(bool flag){
    visitingGlobal = flag;
  }

  bool VisitStmt(clang::Stmt* s);

  SSTPragmaList&
  getPragmas(){
    return pragmas;
  }

 private:
  bool TestStmtMacro(clang::Stmt* s);

  clang::Rewriter& TheRewriter;
  clang::CompilerInstance* CI;
  FindGlobalASTVisitor& finder;
  SSTPragmaList pragmas;
  bool visitingGlobal;
  std::set<clang::Expr*>& deleted;

};


#endif
