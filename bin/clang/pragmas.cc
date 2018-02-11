/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include "pragmas.h"
#include "astVisitor.h"
#include <sstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

#define lexify(x) \
  PP.Lex(x); std::cout << x.getName(); \
  if (x.getKind() == tok::identifier) std::cout << " " << x.getIdentifierInfo()->getNameStart(); \
  std::cout << std::endl;

static int pragmaDepth = 0;
static int maxPragmaDepth = 0;
std::list<SSTPragma*> pendingPragmas;

void getLiteralDataAsString(const Token &tok, std::ostream &os)
{
  const char* data = tok.getLiteralData(); //not null-terminated, direct from buffer
  for (int i=0 ; i < tok.getLength(); ++i){
    //must explicitly add chars, this will not hit a \0
     os << data[i];
  }
}

std::string getLiteralDataAsString(const Token &tok)
{
  std::stringstream os;
  getLiteralDataAsString(tok, os);
  return os.str();
}

void
SSTPragmaHandler::configure(Token& PragmaTok, Preprocessor& PP, SSTPragma* fsp)
{
  switch(fsp->cls){
    case SSTPragma::Keep: //always obey these
      pragmas_.push_back(fsp);
      break;
    default: //otherwise check if we are skeletonizing
      if (visitor_.noSkeletonize()){
        return;
      }
      pragmas_.push_back(fsp);
      break;
  }


  pragmaDepth++;
  maxPragmaDepth++;
  fsp->pragmaList = &pragmas_;
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
  SSTPragma* fsp = allocatePragma(next.getEndLoc(), tokens);
  configure(PragmaTok, PP, fsp);
}

void
SSTDeletePragma::activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg){
  replace(s,r,"",*CI);
  switch(s->getStmtClass()){
  case Stmt::ForStmtClass:
  case Stmt::CompoundStmtClass:
    break;
  default:
    break;
  }
}

void
SSTDeletePragma::activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg){
  replace(d,r,"",*CI);
}

void
SSTEmptyPragma::activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg){
  std::stringstream sstr;
  sstr << "{" << body_;
  if (body_.size()) sstr << ";";
  sstr << "}";
#define prg_case(x,d) case Decl::x: replace(cast<x##Decl>(d)->getBody(), r, sstr.str(),*CI); break
  switch (d->getKind()){
    prg_case(Function,d);
    prg_case(CXXMethod,d);
    default:
      break;
  }
#undef prg_case
}

void
SSTEmptyPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  errorAbort(s->getLocStart(), *CI,
       "pragma empty should only apply to declarations, not statmements");
}

void
SSTKeepPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  cfg.makeNoChanges = true;
}

void
SSTKeepPragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  cfg.makeNoChanges = true;
}

void
SSTNewPragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
#define dcase(type,d,rw) \
  case(clang::Decl::type): \
    visit##type##Decl(clang::cast<type##Decl>(d),rw); break
  switch(d->getKind()){
    default:
      break;
  }
#undef dcase
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
        std::string type = GetAsString(vd->getType());
        std::string name = vd->getNameAsString();
        std::stringstream sstr;
        sstr << type << " " << name << " = nullptr;"; //don't know why - but okay, semicolon needed
        replace(stmt, r, sstr.str(), *CI);
        deleted->insert(init);
      } //boy, I really hope this doesn't allocate any memory
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
    replace(op, r, pp.os.str(),*CI);
    deleted->insert(op->getRHS());
  } //boy, I really hope this doesn't allocate any memory
}

void
SSTNewPragma::activate(Stmt* stmt, Rewriter &r, PragmaConfig& cfg)
{
#define scase(type,s,rw) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s),rw); break
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt,r);
    scase(BinaryOperator,stmt,r);
    default: //well - that was stupid of you
      warn(stmt->getLocStart(), *CI, "pragma new not applied to operator new or binary operator");
      break;
  }
#undef scase
}

void
SSTKeepIfPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  PrettyPrinter pp;
  pp.os << "if (" << ifCond_ << "){ ";
  pp.print(s); //put the original statement in the if
  pp.os << "; } else ";
  r.InsertText(s->getLocStart(), pp.os.str(), false, false);
}

void
SSTBranchPredictPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  if (s->getStmtClass() != Stmt::IfStmtClass){
    errorAbort(s->getLocStart(), *CI, "predicate pragma not applied to if statement");
  }
  IfStmt* ifs = cast<IfStmt>(s);
  replace(ifs->getCond(), r, prediction_, *CI);
}

void
SSTAdvanceTimePragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  std::string replacement;
  if (units_ == "sec"){
    replacement = "sstmac_compute(" + amount_ + ");";
  } else if (units_ == "msec"){
    replacement = "sstmac_msleep(" + amount_ + ");";
  } else if (units_ == "usec"){
    replacement = "sstmac_usleep(" + amount_ + ");";
  } else if (units_ == "nsec"){
    replacement = "sstmac_nanosleep(" + amount_ + ");";
  } else {
    std::string error = "invalid time units: " + units_ +
        ": must be sec, msec, usec, or nsec";
    errorAbort(s->getLocStart(), *CI, error);
  }
  r.InsertText(s->getLocEnd(), replacement);
}

void
SSTCallFunctionPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  r.InsertText(s->getLocStart(), repl_, false);
}

void
SSTMallocPragma::visitBinaryOperator(BinaryOperator *op, Rewriter &r)
{
  PrettyPrinter pp;
  //this better be an equals
  pp.print(op->getLHS());
  pp.os << " = 0"; //don't know why - but okay, semicolon not needed
  replace(op, r, pp.os.str(), *CI);
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
    std::string type = GetAsString(vd->getType());
    std::string name = vd->getNameAsString();
    std::stringstream sstr;
    sstr << type << " " << name << " = 0;"; //don't know why - but okay, semicolon needed
    replace(stmt, r, sstr.str(), *CI);
  } else {
    errorAbort(stmt->getLocStart(), *CI,
               "sst malloc pragma applied to non-variable declaration");
  }
}

void
SSTMallocPragma::activate(Stmt *stmt, Rewriter &r, PragmaConfig& cfg)
{
#define scase(type,s,rw) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s),rw); break
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt,r);
    scase(BinaryOperator,stmt,r);
    default: //just delete what follows
      //SSTDeletePragma::act(stmt,r);
      break;
  }
#undef scase
}

void
SSTReturnPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  if (s->getStmtClass() != Stmt::ReturnStmtClass){
    errorAbort(s->getLocStart(), *CI,
              "pragma return not applied to return statement");
  }
  replace(s, r, "return " + repl_, *CI);
}

void
SSTReturnPragma::activate(Decl* d, Rewriter& r, PragmaConfig& cfg)
{
  if (d->getKind() != Decl::Function){
    errorAbort(d->getLocStart(), *CI,
      "pragma return not applied to function definition");
  }
  FunctionDecl* fd = cast<FunctionDecl>(d);
  if (!fd->hasBody()){
    errorAbort(d->getLocStart(), *CI,
      "pragma return applied to function declaration - must be definition");
  }
  std::string repl = "{ return " + repl_ + "; }";
  replace(fd->getBody(), r, repl, *CI);
}

SSTNullVariablePragma::SSTNullVariablePragma(SourceLocation loc, CompilerInstance& CI,
                                             const std::list<Token> &tokens)
 : SSTPragma(NullVariable)
{
  if (tokens.empty()) return;

  std::set<std::string>* inserter = nullptr;
  auto end = tokens.end();
  for (auto iter=tokens.begin(); iter != end; ++iter){
    const Token& token = *iter;
    std::string next;
    switch(token.getKind()){
    case tok::string_literal:
      next = token.getLiteralData();
      break;
    case tok::kw_new:
      next = "new";
      break;
    case tok::identifier:
      next = token.getIdentifierInfo()->getName().str();
      break;
    default:
      errorAbort(loc, CI, "token to pragma is not a valid string name");
      break;
    }

    if (next == "except"){
      inserter = &nullExcept_;
    } else if (next == "only"){
      inserter = &nullOnly_;
    } else if (next == "new"){
      inserter = &nullNew_;
    } else if (inserter == nullptr){
      errorAbort(loc, CI, "illegal null_variable spec: must begin with 'only', 'except', or 'new'");
    } else {
      inserter->insert(next);
    }
  }
}

void
SSTNullVariablePragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  cfg.nullVariables[d] = this;
}

void
SSTNullVariablePragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  if (s->getStmtClass() == Stmt::DeclStmtClass){
    DeclStmt* ds = cast<DeclStmt>(s);
    for (auto iter=ds->decl_begin(); iter != ds->decl_end(); ++iter){
      activate(*iter,r,cfg);
    }
  } else {
    errorAbort(s->getLocStart(), *CI,
        "pragma null_variable should only apply to declaration statements");
  }
}

SSTNullTypePragma::SSTNullTypePragma(SourceLocation loc, CompilerInstance& CI,
                                     const std::list<Token> &tokens)
 : SSTNullVariablePragma(NullType)
{
  if (tokens.empty()){
    errorAbort(loc, CI, "null_type pragma requires a type argument");
  }

  std::list<std::string> toInsert;
  bool connectPrev = false;
  int templateCount = 0;
  for (const Token& token : tokens){
    std::string next;
    bool connectNext = false;
    switch(token.getKind()){
    case tok::string_literal:
      next = token.getLiteralData();
      break;
    case tok::coloncolon:
      next = "::";
      connectNext = true;
      break;
    case tok::comma:
      next = ",";
      connectNext = connectPrev;
      break;
    case tok::less:
      next = "<";
      connectNext = true;
      templateCount++;
      break;
    case tok::greater:
      next = ">";
      templateCount--;
      connectNext = templateCount > 0;
      break;
    case tok::kw_long:
      next = "long";
      connectNext = connectPrev;
      break;
    case tok::kw_int:
      next = "int";
      connectNext = connectPrev;
      break;
    case tok::kw_double:
      next = "double";
      connectNext = connectPrev;
      break;
    case tok::identifier:
      next = token.getIdentifierInfo()->getName().str();
      break;
    default: {
      std::string err = std::string("token ") + token.getName() + " is not valid in type names";
      errorAbort(loc, CI, err);
      break;
    }
    }
    if (connectPrev || connectNext){
      toInsert.back() += next;
    } else {
      toInsert.push_back(next);
    }
    connectPrev = connectNext;
  }
  newType_ = toInsert.front();
  toInsert.pop_front();
  for (auto& str : toInsert){
    nullExcept_.insert(str);
  }
}

void
SSTNullTypePragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  std::string name;
  switch(d->getKind()){
  case Decl::Var: {
    VarDecl* vd = cast<VarDecl>(d);
    name = vd->getNameAsString();
    break;
  }
  case Decl::Field: {
    FieldDecl* fd = cast<FieldDecl>(d);
    name = fd->getNameAsString();
    break;
  }
  default:
    errorAbort(d->getLocStart(), *CI,
        "null_type pragma can only be applied to field or variable declaration");
  }
  std::string repl = newType_ + " " + name;
  replace(d,r,repl,*CI);
  SSTNullVariablePragma::activate(d,r,cfg);
}

void
SSTPragma::tokenStreamToString(SourceLocation loc,
                               std::list<Token>::const_iterator beg,
                               std::list<Token>::const_iterator end,
                               std::ostream& os,
                               CompilerInstance& CI)
{
  for (auto iter=beg; iter != end; ++iter){
    const Token& next = *iter;
    switch (next.getKind()){
      case tok::identifier:
        os << next.getIdentifierInfo()->getNameStart();
        break;
      case tok::semi:
        os << ";";
        break;
      case tok::l_paren:
        os << '(';
        break;
      case tok::r_paren:
        os << ')';
        break;
      case tok::comma:
        os << ',';
        break;
      case tok::less:
        os << "<";
        break;
      case tok::slash:
        os << "/";
        break;
      case tok::star:
        os << "*";
        break;
      case tok::period:
        os << ".";
        break;
      case tok::kw_return:
        os << "return ";
        break;
      case tok::coloncolon:
        os << "::";
        break;
      case tok::kw_long:
        os << "long";
        break;
      case tok::kw_int:
        os << "int";
        break;
      case tok::kw_double:
        os << "double";
        break;
      case tok::equalequal:
        os << "==";
        break;
      case tok::kw_sizeof:
        os << "sizeof";
        break;
      case tok::minus:
        os << "-";
        break;
      case tok::percent:
        os << "%";
        break;
      case tok::kw_true:
        os << "true";
        break;
      case tok::kw_false:
        os << "false";
        break;
      case tok::arrow:
        os << "->";
        break;
      case tok::kw_nullptr:
        os << "nullptr";
        break;
      case tok::string_literal:
      case tok::numeric_constant:
      {
        getLiteralDataAsString(next, os);
        break;
      }
      default:
        std::cerr << "bad token: " << next.getName() << std::endl;
        errorAbort(loc, CI, "invalid token in replace pragma");
        break;
    }
  }
}

SSTPragma*
SSTNullTypePragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  return new SSTNullTypePragma(loc, ci_, tokens);
}

SSTPragma*
SSTNullVariablePragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  return new SSTNullVariablePragma(loc, ci_, tokens);
}

SSTPragma*
SSTKeepIfPragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(loc, tokens.begin(), tokens.end(), sstr, ci_);
  return new SSTKeepIfPragma(sstr.str());
}

SSTPragma*
SSTReturnPragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(loc, tokens.begin(), tokens.end(), sstr, ci_);
  return new SSTReturnPragma(loc, ci_, sstr.str());
}

SSTPragma*
SSTEmptyPragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(loc, tokens.begin(), tokens.end(), sstr, ci_);
  return new SSTEmptyPragma(sstr.str());
}

SSTPragma*
SSTBranchPredictPragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(loc, tokens.begin(), tokens.end(), sstr, ci_);
  return new SSTBranchPredictPragma(sstr.str());
}

SSTPragma*
SSTAdvanceTimePragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  if (tokens.size() < 2){
    errorAbort(loc, ci_,
               "advance_time pragma needs at least two arguments: <units> <number>");
  }
  auto iter = tokens.begin();
  Token unitTok = *iter;
  std::string units = unitTok.getLiteralData();
  std::stringstream sstr;
  ++iter;
  SSTPragma::tokenStreamToString(loc, iter, tokens.end(), sstr, ci_);
  return new SSTAdvanceTimePragma(units, sstr.str());
}

SSTPragma*
SSTCallFunctionPragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(loc, tokens.begin(), tokens.end(), sstr, ci_);
  sstr << ";"; //semi-colon not required in pragma
  return new SSTCallFunctionPragma(sstr.str());
}
