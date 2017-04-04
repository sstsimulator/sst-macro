#include "astConsumers.h"

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

bool
MyASTConsumer::HandleTopLevelDecl(DeclGroupRef DR)
{
  for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b){
    Decl* d = *b;
    if (!d->isImplicit()){
      //we have to find a particular set of declarations that
      //will need later replacing/processing
      FindVisitor.TraverseDecl(*b);
    }
    //the replace visitor should visit everything in the source file
    ReplVisitor.TraverseDecl(*b);
  }
  return true;
}
