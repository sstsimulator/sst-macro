#include "replAstVisitor.h"
#include <iostream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

bool
ReplGlobalASTVisitor::VisitDecl(Decl* d)
{
  SSTPragma* prg = pragmas.takeMatch(d);
  if (prg){
    //pragma takes precedence
    prg->act(d, TheRewriter);
  }
  return true;
}

bool
ReplGlobalASTVisitor::VisitCXXNewExpr(CXXNewExpr *expr)
{
  if (deleted.find(expr) != deleted.end()){
    //already deleted - do nothing here
    return true;
  }

  std::string allocatedTypeStr = QualType::getAsString(expr->getAllocatedType().split());;
  if (expr->getNumPlacementArgs() == 0){
    PrettyPrinter pp;
    pp.os << "conditional_new<" << allocatedTypeStr << ">(";
    if (expr->isArray()){
      pp.print(expr->getArraySize());
    }
    pp.os << ")";
    TheRewriter.ReplaceText(expr->getSourceRange(), pp.os.str());
  } else {
    //might be a placement new or no-throw new
    Expr* placer = expr->getPlacementArg(0);
    switch(placer->getStmtClass())
    {
      case Stmt::DeclRefExprClass:
      {
        DeclRefExpr* dre = cast<DeclRefExpr>(placer);
        if (dre->getFoundDecl()->getNameAsString() == "sstmac_placement_ptr"){
          break;
        }
      }
      case Stmt::ImplicitCastExprClass:
      case Stmt::CStyleCastExprClass:
      case Stmt::CallExprClass:
      case Stmt::CXXStaticCastExprClass:
      case Stmt::UnaryOperatorClass:
      {
        QualType type = placer->getType();
        if (QualType::getAsString(type.split()) == "void *"){
          PrettyPrinter pp;
          //placement
          pp.os << "placement_new<" << allocatedTypeStr << ">(";
          pp.print(placer);
          if (expr->isArray()){
            pp.os << ",";
            pp.print(expr->getArraySize());
          }
          pp.os << ")";
          TheRewriter.ReplaceText(expr->getSourceRange(), pp.os.str());
        }
      }
      default:
        break;
    }
  }
  return true;
}

bool
ReplGlobalASTVisitor::VisitStmt(Stmt *s)
{
  SSTPragma* prg = pragmas.takeMatch(s);
  if (prg){
    //pragma takes precedence
    prg->act(s, TheRewriter);
    return true;
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
        errorAbort(dref->getLocStart(), *CI,
                   "cannot yet create pointers to global variables");
      }
    }
  }
  return true;
}

bool
ReplGlobalASTVisitor::TraverseFunctionTemplateDecl(FunctionTemplateDecl *D)
{
  if (D->getNameAsString() == "placement_new"){
    return true;
  } else if (D->getNameAsString() == "conditional_new"){
    return true;
  } else {
    TraverseDecl(D->getTemplatedDecl());
    return true;
  }
}

bool
ReplGlobalASTVisitor::VisitDeclRefExpr(DeclRefExpr* expr){
  NamedDecl* decl =  expr->getFoundDecl();
  finder.replGlobal(decl, expr->getSourceRange());
  return true;
}


