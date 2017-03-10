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
    PrettyPrinter pp; pp.print(s);
    SourceManager& SM = CI->getSourceManager();
    FoundMacro& fm = mlist.getOverlappingMacro(s);
  }
  return true;
}

bool
ReplGlobalASTVisitor::VisitUnaryOperator(UnaryOperator* op)
{
  if (visitingGlobal){
    Expr* exp = op->getSubExpr();
    if (isa<DeclRefExpr>(exp)){
      DeclRefExpr* dref = cast<DeclRefExpr>(exp);
      if (finder.isGlobal(dref)){
        std::string errorStr;
        llvm::raw_string_ostream os(errorStr);
        SourceLocation errorLoc = dref->getLocStart();
        errorLoc.print(os, CI->getSourceManager());
        os << " error: cannot yet create pointers to global variables";
        std::cerr << os.str() << std::endl;
        abort();
      }
    }
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
    if (exists) {
      return true;
    }
    FoundMacro tmp(*CI, expr);
    SourceRange rng(tmp.start, tmp.end);
    finder.replGlobal(decl, rng);
  } else {
    finder.replGlobal(decl, expr->getSourceRange());
  }
  return true;
}


