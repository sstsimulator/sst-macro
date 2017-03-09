#ifndef bin_clang_astconsumers_h
#define bin_clang_astconsumers_h

#include "clangHeaders.h"
#include "findAstVisitor.h"
#include "replAstVisitor.h"
#include "globalVarNamespace.h"

class MyASTConsumer : public clang::ASTConsumer {
 public:
  MyASTConsumer(clang::Rewriter &R, FindGlobalASTVisitor& f, ReplGlobalASTVisitor& r,
                GlobalVarNamespace& ns, MacroList& mlist) :
    globalNS(ns), FindVisitor(f), ReplVisitor(r)
  {
  }

  bool HandleTopLevelDecl(clang::DeclGroupRef DR) override;

 private:
  FindGlobalASTVisitor& FindVisitor;
  ReplGlobalASTVisitor& ReplVisitor;
  GlobalVarNamespace& globalNS;

};

#endif
