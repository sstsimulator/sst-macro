#include "pragmas.h"
#include <sstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

#define scase(type,s,rw) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s),rw); break \

#define lexify(x) \
  PP.Lex(x); std::cout << x.getName(); \
  if (x.getKind() == tok::identifier) std::cout << " " << x.getIdentifierInfo()->getNameStart(); \
  std::cout << std::endl;

static int pragmaDepth = 0;
static int maxPragmaDepth = 0;
static SSTPragma* topPragma = nullptr;
std::list<SSTPragma*> pendingPragmas;

void
SSTPragmaHandler::configure(Token& PragmaTok, Preprocessor& PP, SSTPragma* fsp)
{
  if (topPragma == nullptr) topPragma = fsp;
  pragmaDepth++;
  maxPragmaDepth++;
  if (topPragma){
    topPragma->subPragmas.push_back(fsp);
  } else {
    pragmas_.push_back(fsp);
  }
  fsp->name = getName();
  fsp->startLoc = PragmaTok.getLocation();
  fsp->CI = &ci_;
  fsp->visitor = &visitor_;
  fsp->deleted = &deleted_;
  PP.EnableBacktrackAtThisPos(); //this controls the backtrack
  Token pragmaTarget;
  PP.Lex(pragmaTarget); //this might hit another pragma
  fsp->endLoc = pragmaTarget.getEndLoc();
  PP.Backtrack();
  if (pragmaDepth == 1){ //this is the top pragma
    //if we hit multiple pragmas, we might have redundant tokens
    //becaue of recursion weirdness in the lexer
    for (int i=1; i < maxPragmaDepth; ++i){
      Token throwAway;
      PP.Lex(throwAway);
    }
    maxPragmaDepth = 0;
    topPragma = nullptr;
  }
  --pragmaDepth;
}

void
SSTSimplePragmaHandler_base::HandlePragma(clang::Preprocessor &PP,
                  clang::PragmaIntroducerKind Introducer,
                  clang::Token &PragmaTok)
{
  //the next token should be an eod
  Token eodToken; PP.Lex(eodToken);
  if (!eodToken.is(tok::eod)){
    std::stringstream sstr;
    sstr << "Pragma handler for " << getName().str()
         << " got invalid token type " << eodToken.getName();
    errorAbort(PragmaTok.getLocation(), ci_, sstr.str());
  }

  SSTPragma* fsp = allocatePragma();
  configure(PragmaTok, PP, fsp);
}

void
SSTTokenStreamPragmaHandler::HandlePragma(Preprocessor &PP, PragmaIntroducerKind Introducer, Token &PragmaTok)
{
  std::list<Token> tokens;
  Token next; PP.Lex(next);
  while (!next.is(tok::eod)){
    tokens.push_back(next);
    PP.Lex(next);
  }
  SSTPragma* fsp = allocatePragma(tokens);
  configure(PragmaTok, PP, fsp);
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
  SourceLocation insertLoc = Lexer::getLocForEndOfToken(endLoc, 0,
                      CI->getSourceManager(), CI->getLangOpts());
  bool insertAfter = true;
  if (insertLoc.isInvalid()){
    errorAbort(endLoc, *CI, "trouble parsing sst new pragma");
  }
  r.InsertText(insertLoc, "should_skip_operator_new()=false;", insertAfter);
}

void
SSTNewPragma::visitCompoundStmt(clang::CompoundStmt* stmt, Rewriter& r)
{
  defaultAct(stmt,r);
}

void
SSTNewPragma::act(Stmt* stmt, Rewriter &r)
{
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt,r);
    scase(BinaryOperator,stmt,r);
    scase(CompoundStmt,stmt,r);
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
