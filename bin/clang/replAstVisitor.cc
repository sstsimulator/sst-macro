#include "replAstVisitor.h"
#include <iostream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

bool
ReplGlobalASTVisitor::VisitDecl(Decl* d)
{
  return true;
  /** I think this isn't needed - all global var uses
      with macros will end up in statements
  SourceLocation startLoc = d->getLocStart();
  if (startLoc.isMacroID()){
    mlist.append(d);
    return false;
  }
  return true;
  */
}

bool
ReplGlobalASTVisitor::VisitStmt(Stmt *s)
{
  SourceLocation startLoc = s->getLocStart();
  SourceLocation endLoc = s->getLocEnd();
  if (startLoc.isMacroID() && endLoc.isMacroID()){
    FoundMacro& fm = mlist.getOverlappingMacro(s);
  }
  return true;
}

bool
ReplGlobalASTVisitor::VisitDeclRefExpr(DeclRefExpr* expr){
  NamedDecl* decl =  expr->getFoundDecl();
  SourceLocation startLoc = expr->getLocStart();
  SourceLocation endLoc = expr->getLocEnd();
  if (startLoc.isMacroID() && endLoc.isMacroID()){
    bool exists = mlist.hasOverlappingMacro(expr);
    if (exists) return true;
    FoundMacro tmp(*CI, startLoc);
    SourceRange rng(tmp.start, tmp.end);
    finder.replGlobal(decl, rng);
  } else {
    finder.replGlobal(decl, expr->getSourceRange());
  }
  return true;
}


