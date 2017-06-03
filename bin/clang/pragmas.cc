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
#include <sstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

#define scase(type,s,rw) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s),rw); break

#define dcase(type,d,rw) \
  case(clang::Decl::type): \
    visit##type##Decl(clang::cast<type##Decl>(d),rw); break

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
  clang::SourceRange rng(s->getLocStart(), s->getLocEnd());
  r.ReplaceText(rng,"");
}

void
SSTNewPragma::visitCXXMethodDecl(CXXMethodDecl* decl, Rewriter& r)
{
  defaultAct(decl->getBody(), r, true, false);
}

void
SSTNewPragma::visitFunctionDecl(FunctionDecl* decl, Rewriter& r)
{
  defaultAct(decl->getBody(), r, true, false);
}

void
SSTNewPragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  switch(d->getKind()){
    dcase(Function,d,r);
    dcase(CXXMethod,d,r);
    default:
      break;
  }
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
        defaultAct(stmt,r,false,true);
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
    defaultAct(op,r,false,true);
  }
}

void
SSTNewPragma::defaultAct(Stmt* stmt, Rewriter& r, bool insertStartAfter, bool insertStopAfter)
{
  SourceLocation startLoc = stmt->getLocStart();
  SourceLocation insertLoc = endLoc;
  if (insertStartAfter){
    insertLoc = Lexer::getLocForEndOfToken(startLoc, 0,
                        CI->getSourceManager(), CI->getLangOpts());
  }
  r.InsertText(insertLoc, "should_skip_operator_new()++;", false);

  SourceLocation endLoc = stmt->getLocEnd();
  insertLoc = endLoc;
  if (insertStopAfter){
    insertLoc = Lexer::getLocForEndOfToken(endLoc, 0,
                        CI->getSourceManager(), CI->getLangOpts());
  }
  if (insertLoc.isInvalid()){
    errorAbort(endLoc, *CI, "trouble parsing sst new pragma");
  }
  //locations are weird with functions - insert after is always false
  r.InsertText(insertLoc, "should_skip_operator_new()--;", false);
}

void
SSTNewPragma::visitCompoundStmt(clang::CompoundStmt* stmt, Rewriter& r)
{
  defaultAct(stmt,r,false,true);
}

void
SSTNewPragma::activate(Stmt* stmt, Rewriter &r, PragmaConfig& cfg)
{
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt,r);
    scase(BinaryOperator,stmt,r);
    scase(CompoundStmt,stmt,r);
    default: //just delete what follows
      defaultAct(stmt,r,false,false);
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

std::string
SSTReplacePragmaHandler::parse(CompilerInstance& CI, SourceLocation loc,
                               const std::list<Token>& tokens, std::ostream& os)
{
  if (tokens.size() < 2){
    errorAbort(loc, CI, "pragma replace requires both a function and replacement text");
  }
  const Token& fxn = tokens.front();
  if (!fxn.is(tok::identifier)){
    errorAbort(loc, CI, "pragma replace got invalid function name");
  }

  auto iter = tokens.begin(); ++iter; //skip front
  auto end = tokens.end();
  for ( ; iter != end; ++iter){
    const Token& next = *iter;
    switch (next.getKind()){
      case tok::identifier:
         os << next.getIdentifierInfo()->getNameStart();
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
  return fxn.getIdentifierInfo()->getNameStart();
}

SSTPragma*
SSTReplacePragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  std::string fxn = parse(ci_, loc, tokens, sstr);
  return new SSTReplacePragma(fxn, sstr.str());
}

SSTPragma*
SSTStartReplacePragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  std::string fxn = SSTReplacePragmaHandler::parse(ci_, loc, tokens, sstr);
  return new SSTStartReplacePragma(fxn, sstr.str());
}

SSTPragma*
SSTStopReplacePragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  std::string fxn = tokens.front().getIdentifierInfo()->getNameStart();
  return new SSTStopReplacePragma(fxn, "not relevant");
}

void
SSTReplacePragma::activate(Decl *d, std::list<std::string>& vars)
{
  VarDecl* vd = cast<VarDecl>(d);
  std::stringstream sstr;
  sstr << vd->getType().getAsString() << " "
       << fxn_ << "(" << replacement_ << ")";
  vars.push_back(sstr.str());
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
