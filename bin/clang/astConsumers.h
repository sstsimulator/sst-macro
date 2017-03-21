#ifndef bin_clang_astconsumers_h
#define bin_clang_astconsumers_h

#include "clangHeaders.h"
#include "findAstVisitor.h"
#include "replAstVisitor.h"
#include "firstVisitor.h"
#include "globalVarNamespace.h"

class ReplaceASTConsumer : public clang::ASTConsumer {
 public:
  ReplaceASTConsumer(clang::Rewriter &R, FindGlobalASTVisitor& f, ReplGlobalASTVisitor& r,
                GlobalVarNamespace& ns) :
    globalNS(ns), FindVisitor(f), ReplVisitor(r)
  {
  }

  bool HandleTopLevelDecl(clang::DeclGroupRef DR) override;

 private:
  FindGlobalASTVisitor& FindVisitor;
  ReplGlobalASTVisitor& ReplVisitor;
  GlobalVarNamespace& globalNS;

};

class FindASTConsumer : public clang::ASTConsumer {
public:
 FindASTConsumer(clang::CompilerInstance& CI, GlobalVarNamespace& ns) :
   globalNS(ns), Visitor(ns, CI)
 {
 }

 bool HandleTopLevelDecl(clang::DeclGroupRef DR) override;

private:
 GlobalVarNamespace& globalNS;
 FirstASTVisitor Visitor;

};

#endif
