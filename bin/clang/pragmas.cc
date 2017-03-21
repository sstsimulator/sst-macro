#include "pragmas.h"
#include <sstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

#define scase(type,s,rw) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s),rw); break \


void
SSTSimplePragmaHandler_base::HandlePragma(clang::Preprocessor &PP,
                  clang::PragmaIntroducerKind Introducer,
                  clang::Token &PragmaTok)
{
  switch (Introducer){
    case PIK_HashPragma:
      break; //great valid
   default: {
     std::stringstream sstr;
     sstr << "Got pragma " << this->getName().str()
           << " using non-hash #pragma. Must use #pragma to ensure no pragmas in macros";
     errorAbort(PragmaTok.getLocation(), CI, sstr.str());
   }
  }

  //the next token should be an eod
  Token eodToken; PP.Lex(eodToken);
  if (!eodToken.is(tok::eod)){
    std::stringstream sstr;
    sstr << "Pragma handler for " << getName().str()
         << " got invalid token type " << eodToken.getName();
    errorAbort(PragmaTok.getLocation(), CI, sstr.str());
  }

  SSTPragma* fsp = allocatePragma();
  PP.EnableBacktrackAtThisPos();
  Token pragmaTarget; PP.Lex(pragmaTarget);
  fsp->name = getName();
  fsp->startLoc = PragmaTok.getLocation();
  fsp->endLoc = pragmaTarget.getEndLoc();
  fsp->CI = &CI;
  fsp->deleted = &deleted;
  pragmas.push_back(fsp);
  PP.Backtrack();
}

void
SSTDeletePragma::act(clang::Stmt* s, clang::Rewriter& r){
  clang::SourceRange rng(s->getLocStart(), s->getLocEnd());
  r.ReplaceText(rng,"");
}

void
SSTDeletePragma::act(clang::Decl* decl, clang::Rewriter& r){
  clang::SourceRange rng(decl->getLocStart(), decl->getLocEnd());
  r.ReplaceText(rng,"");
}

void
SSTMallocPragma::act(Decl *decl, Rewriter &r)
{
}

void
SSTNewPragma::visitDeclStmt(DeclStmt *stmt, Rewriter &r)
{
  if (!stmt->isSingleDecl()){
    errorAbort(stmt->getLocStart(), *CI, "cannot skeletonize multi-declarations");
  }
  Decl* decl = stmt->getSingleDecl();
  if (isa<VarDecl>(decl)){
    VarDecl* vd = cast<VarDecl>(decl);
    if (vd->hasInit()){
      Expr* init = vd->getInit();
      if (isa<CXXNewExpr>(init)){
        //we can directly skeletonize
        std::string type = QualType::getAsString(vd->getType().split());
        std::string name = vd->getNameAsString();
        std::stringstream sstr;
        sstr << type << " " << name << " = nullptr;"; //don't know why - but okay, semicolon needed
        r.ReplaceText(stmt->getSourceRange(), sstr.str());
        deleted->insert(init);
      } else {
        //nope, need extra hacking
        defaultAct(stmt,r);
      }
    }
  } else {
    errorAbort(stmt->getLocStart(), *CI, "sst malloc pragma applied to non-variable declaration");
  }
}

void
SSTNewPragma::visitBinaryOperator(BinaryOperator *op, Rewriter& r)
{
  if (isa<CXXNewExpr>(op->getRHS())){
    PrettyPrinter pp;
    //this better be an equals
    pp.print(op->getLHS());
    pp.os << " = nullptr"; //don't know why - but okay, semicolon not needed
    r.ReplaceText(op->getSourceRange(), pp.os.str());
    deleted->insert(op->getRHS());
  } else {
    defaultAct(op,r);
  }
}

void
SSTNewPragma::defaultAct(Stmt* stmt, Rewriter& r)
{
  r.InsertText(stmt->getLocStart(), "should_skip_operator_new()=true;", false);
  SourceLocation endLoc = stmt->getLocEnd();
  Token edgeTok;
  Lexer::getRawToken(endLoc, edgeTok,
                     CI->getSourceManager(), CI->getLangOpts(),
                     false);
  bool insertAfter = true;
  if (edgeTok.getKind() != tok::semi){
    endLoc = Lexer::findLocationAfterToken(endLoc, tok::semi,
                                 CI->getSourceManager(), CI->getLangOpts(), false);
    //insertAfter = false;
  } else {
    endLoc = Lexer::getLocForEndOfToken(endLoc, 0,
                        CI->getSourceManager(), CI->getLangOpts());
  }
  r.InsertText(endLoc, "should_skip_operator_new()=false;", insertAfter);
}

void
SSTNewPragma::act(Stmt* stmt, Rewriter &r)
{
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt,r);
    scase(BinaryOperator,stmt,r);
    default: //just delete what follows
      defaultAct(stmt,r);
      break;
  }
}

void
SSTNewPragma::act(Decl *decl, Rewriter &r)
{
}

void
SSTMallocPragma::visitBinaryOperator(BinaryOperator *op, Rewriter &r)
{
  PrettyPrinter pp;
  //this better be an equals
  pp.print(op->getLHS());
  pp.os << " = 0"; //don't know why - but okay, semicolon not needed
  r.ReplaceText(op->getSourceRange(), pp.os.str());
}

void
SSTMallocPragma::visitDeclStmt(DeclStmt *stmt, Rewriter &r)
{
  if (!stmt->isSingleDecl()){
    errorAbort(stmt->getLocStart(), *CI, "cannot skeletonize multi-declarations");
  }
  Decl* decl = stmt->getSingleDecl();
  if (isa<VarDecl>(decl)){
    VarDecl* vd = cast<VarDecl>(decl);
    std::string type = QualType::getAsString(vd->getType().split());
    std::string name = vd->getNameAsString();
    std::stringstream sstr;
    sstr << type << " " << name << " = 0;"; //don't know why - but okay, semicolon needed
    r.ReplaceText(stmt->getSourceRange(), sstr.str());
  } else {
    errorAbort(stmt->getLocStart(), *CI, "sst malloc pragma applied to non-variable declaration");
  }

}

void
SSTMallocPragma::act(Stmt *stmt, Rewriter &r)
{
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt,r);
    scase(BinaryOperator,stmt,r);
    default: //just delete what follows
      //SSTDeletePragma::act(stmt,r);
      break;
  }
}


