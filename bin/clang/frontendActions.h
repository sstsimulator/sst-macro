#ifndef bin_clang_frontendactions_h
#define bin_clang_frontendactions_h

#include "clangHeaders.h"
#include "astConsumers.h"
#include "globalVarNamespace.h"

class MyFrontendAction : public clang::ASTFrontendAction {
 public:
  MyFrontendAction() : mainFxn(nullptr),
    finder(TheRewriter, globalNS, &mainFxn),
    replacer(TheRewriter, finder, mlist)
  {
  }

  void EndSourceFileAction() override;

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance& CI, clang::StringRef file) override {
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    mlist.setCompilerInstance(CI);
    finder.setCompilerInstance(CI);
    replacer.setCompilerInstance(CI);
    return llvm::make_unique<MyASTConsumer>(TheRewriter, finder, replacer, globalNS, mlist);
  }

 private:
  void VisitMacros();
  bool visitDeclRefExpr(clang::DeclRefExpr* exp, PrettyPrinter& pp);
  bool visitBinaryOperator(clang::BinaryOperator* bop, PrettyPrinter& pp);
  bool visitStmt(clang::Stmt* s, PrettyPrinter& pp);
  bool visitMemberExpr(clang::MemberExpr* exp, PrettyPrinter& pp);
  bool visitImplicitCastExpr(clang::ImplicitCastExpr* exp, PrettyPrinter& pp);
  bool visitCallExpr(clang::CallExpr* exp, PrettyPrinter& pp);

  FindGlobalASTVisitor finder;
  ReplGlobalASTVisitor replacer;
  MacroList mlist;
  clang::Rewriter TheRewriter;
  GlobalVarNamespace globalNS;
  clang::FunctionDecl* mainFxn;

};

#endif
