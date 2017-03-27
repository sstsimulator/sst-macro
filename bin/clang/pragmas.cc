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
  pragmaDepth++;
  maxPragmaDepth++;
  if (topPragma){
    topPragma->subPragmas.push_back(fsp);
  } else {
    pragmas_.push_back(fsp);
    topPragma = fsp;
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
    std::cout << getName().str() << " lexed " << next.getName() << std::endl;
    tokens.push_back(next);
    PP.Lex(next);
  }
  SSTPragma* fsp = allocatePragma(next.getEndLoc(), tokens);
  configure(PragmaTok, PP, fsp);
}

void
SSTDeletePragma::activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg){
  clang::SourceRange rng(s->getLocStart(), s->getLocEnd());
  r.ReplaceText(rng,"");
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
SSTNewPragma::activate(Stmt* stmt, Rewriter &r, PragmaConfig& cfg)
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
SSTMallocPragma::activate(Stmt *stmt, Rewriter &r, PragmaConfig& cfg)
{
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt,r);
    scase(BinaryOperator,stmt,r);
    default: //just delete what follows
      //SSTDeletePragma::act(stmt,r);
      break;
  }
}

SSTPragma*
SSTReplacePragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  if (tokens.size() < 2){
    errorAbort(loc, ci_, "pragma replace requires both a function and replacement text");
  }
  const Token& fxn = tokens.front();
  if (!fxn.is(tok::identifier)){
    errorAbort(loc, ci_, "pragma replace got invalid function name");
  }
  std::stringstream sstr;
  auto iter = tokens.begin(); ++iter; //skip front
  auto end = tokens.end();
  for ( ; iter != end; ++iter){
    const Token& next = *iter;
    switch (next.getKind()){
      case tok::identifier:
        sstr << next.getIdentifierInfo()->getNameStart();
        break;
      case tok::l_paren:
        sstr << '(';
        break;
      case tok::r_paren:
        sstr << ')';
        break;
      case tok::comma:
        sstr << ',';
        break;
      case tok::kw_nullptr:
        sstr << "nullptr";
        break;
      case tok::string_literal:
      case tok::numeric_constant:
      {
        const char* data = next.getLiteralData(); //not null-terminated, direct from buffer
        for (int i=0 ; i < next.getLength(); ++i){
          //must explicitly add chars, this will not hit a \0
          sstr << data[i];
        }
        break;
      }
      default:
        std::cerr << "bad token: " << next.getName() << std::endl;
        errorAbort(loc, ci_, "invalid token in replace pragma");
        break;
    }
  }
  return new SSTReplacePragma(fxn.getIdentifierInfo()->getNameStart(), sstr.str());
}

void
SSTReplacePragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  cfg.functionReplacements[fxn_] = replacement_;
}

void
SSTReplacePragma::deactivate(Stmt *s, PragmaConfig &cfg)
{
  cfg.functionReplacements.erase(fxn_);
}
