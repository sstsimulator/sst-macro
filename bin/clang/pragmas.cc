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

void
SSTPragmaHandler::configure(Token& PragmaTok, Preprocessor& PP, SSTPragma* fsp)
{
  //activate no pragmas
  if (visitor_.noSkeletonize())
    return;

  pragmaDepth++;
  maxPragmaDepth++;
  pragmas_.push_back(fsp);
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
SSTNewPragma::visitCXXMethodDecl(CXXMethodDecl* decl, Rewriter& r)
{
  defaultAct(decl->getBody(), r, *CI, true, false);
}

void
SSTNewPragma::visitFunctionDecl(FunctionDecl* decl, Rewriter& r)
{
  defaultAct(decl->getBody(), r, *CI, true, false);
}

void
SSTNewPragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
#define dcase(type,d,rw) \
  case(clang::Decl::type): \
    visit##type##Decl(clang::cast<type##Decl>(d),rw); break
  switch(d->getKind()){
    dcase(Function,d,r);
    dcase(CXXMethod,d,r);
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
        std::string type = QualType::getAsString(vd->getType().split());
        std::string name = vd->getNameAsString();
        std::stringstream sstr;
        sstr << type << " " << name << " = nullptr;"; //don't know why - but okay, semicolon needed
        replace(stmt, r, sstr.str(), *CI);
        deleted->insert(init);
      } else {
        //nope, need extra hacking
        defaultAct(stmt,r,*CI,false,true);
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
    replace(op, r, pp.os.str(),*CI);
    deleted->insert(op->getRHS());
  } else {
    defaultAct(op,r,*CI,false,true);
  }
}

void
SSTNewPragma::defaultAct(Stmt* stmt, Rewriter& r, clang::CompilerInstance& CI,
                         bool insertStartAfter, bool insertStopAfter,
                         bool trailingSemiColon)
{
  return; //don't do this anymore

  SourceLocation insertLoc = stmt->getLocStart();
  if (insertStartAfter){
    insertLoc = Lexer::getLocForEndOfToken(insertLoc, 0,
                        CI.getSourceManager(), CI.getLangOpts());
  }
  r.InsertText(insertLoc, "should_skip_operator_new()++;", false);

  insertLoc = stmt->getLocEnd();
  if (insertStopAfter){
    insertLoc = Lexer::getLocForEndOfToken(insertLoc, 0,
                        CI.getSourceManager(), CI.getLangOpts());
    if (insertLoc.isInvalid()){
      errorAbort(stmt->getLocStart(), CI, "trouble turning off operator new");
    }
  }

  if (trailingSemiColon){
    insertLoc = Lexer::findLocationAfterToken(insertLoc, tok::semi,
                                  CI.getSourceManager(), CI.getLangOpts(), false);
    if (insertLoc.isInvalid()){
      errorAbort(stmt->getLocStart(), CI, "trouble turning off operator new");
    }
  }

  //locations are weird with functions - insert after is always false
  r.InsertText(insertLoc, "should_skip_operator_new()--;", false);
}

void
SSTNewPragma::visitCompoundStmt(clang::CompoundStmt* stmt, Rewriter& r)
{
  defaultAct(stmt,r,*CI,false,true);
}

void
SSTNewPragma::visitForStmt(ForStmt *stmt, Rewriter &r)
{
  return; //don't do this anymore
  r.InsertText(stmt->getLocStart(), "should_skip_operator_new()++;", false);
  r.InsertText(stmt->getLocEnd(), "should_skip_operator_new()--;", false);
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
    scase(CompoundStmt,stmt,r);
    scase(ForStmt,stmt,r);
    default: //just delete what follows
      defaultAct(stmt,r,*CI,false,false,true);
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
SSTPredicatePragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  if (s->getStmtClass() != Stmt::IfStmtClass){
    errorAbort(s->getLocStart(), *CI, "predicate pragma not applied to if statement");
  }
  IfStmt* ifs = cast<IfStmt>(s);
  replace(ifs->getCond(), r, ifCond_, *CI);
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
    std::string type = QualType::getAsString(vd->getType().split());
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
      case tok::percent:
        os << "%";
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
        const char* data = next.getLiteralData(); //not null-terminated, direct from buffer
        for (int i=0 ; i < next.getLength(); ++i){
          //must explicitly add chars, this will not hit a \0
           os << data[i];
        }
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
SSTPredicatePragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(loc, tokens.begin(), tokens.end(), sstr, ci_);
  return new SSTPredicatePragma(sstr.str());
}
