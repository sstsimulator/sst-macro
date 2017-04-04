#ifndef bin_clang_frontendactions_h
#define bin_clang_frontendactions_h

#include "clangHeaders.h"
#include "astConsumers.h"
#include "globalVarNamespace.h"

class MyFrontendAction : public clang::ASTFrontendAction {
 public:
  MyFrontendAction() : mainFxn(nullptr){}

  void EndSourceFileAction() override;

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance& CI, clang::StringRef file) override {
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return llvm::make_unique<MyASTConsumer>(TheRewriter, CI, globalNS, &mainFxn);
  }

 private:
  clang::Rewriter TheRewriter;
  GlobalVarNamespace globalNS;
  clang::FunctionDecl* mainFxn;

};

#endif
