#ifndef bin_clang_astconsumers_h
#define bin_clang_astconsumers_h

#include "clangHeaders.h"
#include "replAstVisitor.h"
#include "globalVarNamespace.h"

class ReplaceASTConsumer : public clang::ASTConsumer {
 public:
  ReplaceASTConsumer(clang::Rewriter &R, ReplGlobalASTVisitor& r) :
    visitor_(r)
  {
  }

  bool HandleTopLevelDecl(clang::DeclGroupRef DR) override;

 private:
  ReplGlobalASTVisitor& visitor_;

};



#endif
