#ifndef bin_clang_astconsumers_h
#define bin_clang_astconsumers_h

#include "clangHeaders.h"
#include "findAstVisitor.h"
#include "replAstVisitor.h"
#include "globalVarNamespace.h"

class MyASTConsumer : public clang::ASTConsumer {
 public:
  MyASTConsumer(clang::Rewriter &R, clang::CompilerInstance& C,
                GlobalVarNamespace& ns, clang::FunctionDecl** mainPtr) :
    FindVisitor(R,C,ns,mainPtr), ReplVisitor(R,C,FindVisitor),
    globalNS(ns) {}

  bool HandleTopLevelDecl(clang::DeclGroupRef DR) override;

 private:
  FindGlobalASTVisitor FindVisitor;
  ReplGlobalASTVisitor ReplVisitor;
  GlobalVarNamespace& globalNS;

};

#endif
