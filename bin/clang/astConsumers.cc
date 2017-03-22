#include "astConsumers.h"

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;


bool
ReplaceASTConsumer::HandleTopLevelDecl(DeclGroupRef DR)
{
  for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b){
    Decl* d = *b;
    bool isGlobalVar = isa<VarDecl>(d);
    visitor_.setVisitingGlobal(isGlobalVar);
    visitor_.TraverseDecl(*b);
  }
  return true;
}
