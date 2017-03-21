#ifndef bin_clang_frontendactions_h
#define bin_clang_frontendactions_h

#include "clangHeaders.h"
#include "astConsumers.h"
#include "globalVarNamespace.h"

class FindAction : public clang::ASTFrontendAction {
 public:
  FindAction();

  void EndSourceFileAction() override;

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance& CI, clang::StringRef file) override {
    return llvm::make_unique<FindASTConsumer>(CI, globalNS);
  }

 private:
  GlobalVarNamespace& globalNS;

};

class ReplaceAction : public clang::ASTFrontendAction {
 public:
  ReplaceAction();

  void EndSourceFileAction() override;

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance& CI, clang::StringRef file) override {
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    finder.setCompilerInstance(CI);
    replacer.setCompilerInstance(CI);
    initPragmas(CI);
    return llvm::make_unique<ReplaceASTConsumer>(TheRewriter, finder, replacer, globalNS);
  }

 private:
  void initPragmas(clang::CompilerInstance& CI);

  FindGlobalASTVisitor finder;
  ReplGlobalASTVisitor replacer;
  clang::Rewriter TheRewriter;
  clang::FunctionDecl* mainFxn;
  GlobalVarNamespace& globalNS;
  std::set<clang::Expr*> deleted;
};

#endif
