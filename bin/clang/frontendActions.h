#ifndef bin_clang_frontendactions_h
#define bin_clang_frontendactions_h

#include "clangHeaders.h"
#include "astConsumers.h"
#include "globalVarNamespace.h"


class ReplaceAction : public clang::ASTFrontendAction {
 public:
  ReplaceAction();

  void EndSourceFileAction() override;

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance& CI, clang::StringRef file) override {
    rewriter_.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    visitor_.setCompilerInstance(CI);
    initPragmas(CI);
    return llvm::make_unique<ReplaceASTConsumer>(rewriter_, visitor_);
  }

 private:
  void initPragmas(clang::CompilerInstance& CI);

  ReplGlobalASTVisitor visitor_;
  clang::Rewriter rewriter_;
  GlobalVarNamespace globalNs_;
  std::set<clang::Expr*> deletedExprs_;
};

#endif
