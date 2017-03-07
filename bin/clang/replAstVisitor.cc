#include "replAstVisitor.h"
#include <iostream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

bool
ReplGlobalASTVisitor::VisitDecl(Decl* d)
{
  SourceLocation startLoc = d->getLocStart();
  if (startLoc.isMacroID()){
    mlist.append(d);
    return false;
  }
  return true;
}

bool
ReplGlobalASTVisitor::VisitStmt(Stmt *s)
{
  SourceLocation startLoc = s->getLocStart();
  if (startLoc.isMacroID()){
    mlist.append(s);
    return false;
  }
  return true;
}

bool
ReplGlobalASTVisitor::VisitDeclRefExpr(DeclRefExpr* expr){
  NamedDecl* decl =  expr->getFoundDecl();
  ValueDecl* val = expr->getDecl();

  SourceLocation startLoc = expr->getLocStart();
  SourceRange replRng;
  if (startLoc.isMacroID()){
    //if somehow we magically got here, then the macro was a direct replacement
    //of a global variable - no special care needed to directly replace it
    FoundMacro fm(CI, startLoc);
    replRng = SourceRange(fm.start, fm.end);
  } else {
    replRng = expr->getSourceRange();
  }

  auto globals = finder.globalVariables();
  auto iter = globals.find(decl);
  if (iter != globals.end()){
    TheRewriter.ReplaceText(replRng, iter->second);
  }

  return true;
}


