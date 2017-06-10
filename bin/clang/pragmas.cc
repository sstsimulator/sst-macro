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
SSTDeletePragma::replace(clang::Stmt* s, clang::Rewriter& r, const char* repl){
  clang::SourceRange rng(s->getLocStart(), s->getLocEnd());
  r.ReplaceText(rng, repl);
}

void
SSTDeletePragma::activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg){
  replace(s,r,"");
}

void
SSTDeletePragma::activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg){
  cfg.skipNextStmt = true;
#define prg_case(x,d) case Decl::x: replace(cast<x##Decl>(d)->getBody(), r, "{}"); break
  switch (d->getKind()){
    prg_case(Function,d);
    prg_case(CXXMethod,d);
    default:
      break;
  }
#undef prg_case
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
SSTNewPragma::visitForStmt(ForStmt *stmt, Rewriter &r)
{
  r.InsertText(stmt->getLocStart(), "should_skip_operator_new()++;", false);
  r.InsertText(stmt->getLocEnd(), "should_skip_operator_new()--;", false);
}

void
SSTNewPragma::activate(Stmt* stmt, Rewriter &r, PragmaConfig& cfg)
{
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt,r);
    scase(BinaryOperator,stmt,r);
    scase(CompoundStmt,stmt,r);
    scase(ForStmt,stmt,r);
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
      case tok::star:
        os << "*";
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

#include "recurseall.h"

struct ReplacementConfig
{
  ReplacementConfig(const std::string& match, const std::string& repl,
                    std::list<const Expr*>& replaced) :
    matchText(match), replacementText(repl), replacedExprs(replaced) {}
  std::list<const Expr*> parents;
  std::list<const Expr*>& replacedExprs;
  const std::string& matchText;
  const std::string& replacementText;
};

/**
 * @brief The ReplaceStatementPreVisit struct
 *
 * The replace pragma tells us to change expressions matching a string
 * This applies to functions, variables, or array exprs.
 * Consider an #sst pragma replace A
 * For simple variables, this is easy.
 * For arrays, this is harder
 *    For A[i] or A[i][j], we need to drop everything not just the "A" part
 * For functions, also annoying
 *    For A(args), we also need to drop everthing
 * Thus, we need to determine if the "base" or "lhs" of an expr
 * is a DeclRefExpr matching the replace pragma
 * Then we need to replace the entire parent expression
 */
template <bool PreVisit> //whether pre or post visit
struct ReplaceStatementVisit {
  template <class T, class... Args>
  bool operator()(T* t, ExprRole role,
                  CompilerInstance& CI,
                  ReplacementConfig& cfg) {
    return false;
  }

  void visit(const Expr* expr, const std::string& name, ExprRole role,
             CompilerInstance& CI, ReplacementConfig& cfg){
    if (PreVisit && name == cfg.matchText){
      switch (role){
        case ExprRole::ArrayBase:
        case ExprRole::CallFxn:
          cfg.replacedExprs.push_back(cfg.parents.back());
          break;
        default:
          //any other use, means this variable is not "connected" to anything else
          cfg.replacedExprs.push_back(expr);
          break;
      }
    }
  }

  void visit(const Expr* expr, ExprRole role,
             CompilerInstance& CI,
             ReplacementConfig& cfg)
  {
    switch(role){
      case ExprRole::ArrayBase:
      case ExprRole::CallFxn:
        //this is connected to the parent
        //current parent remains the parent
        break;
      default:
        if (PreVisit){
          //break the connection to prev parent
          cfg.parents.push_back(expr);
        } else { //restore the original parent
          cfg.parents.pop_back();
        }
    }
  }

  bool operator()(const ArraySubscriptExpr* expr, ExprRole role,
                  CompilerInstance& CI,
                  ReplacementConfig& cfg){
    visit(expr, role, CI, cfg);
    return false;
  }

  bool operator()(const CXXMemberCallExpr* expr, ExprRole role,
                  CompilerInstance& CI,
                  ReplacementConfig& cfg){
    visit(expr, role, CI, cfg);
    return false;
  }

  bool operator()(const CallExpr* expr, ExprRole role,
                  CompilerInstance& CI,
                  ReplacementConfig& cfg){
    visit(expr, role, CI, cfg);
    return false;
  }

  bool operator()(const DeclRefExpr* expr, ExprRole role,
                  CompilerInstance& CI, ReplacementConfig& cfg){
    visit(expr, expr->getFoundDecl()->getNameAsString(), role, CI, cfg);
    return false;
  }

  bool operator()(const MemberExpr* expr, ExprRole role,
                  CompilerInstance& CI, ReplacementConfig& cfg){
    visit(expr, expr->getFoundDecl()->getNameAsString(), role, CI, cfg);
    return false;
  }

  bool operator()(const DeclStmt* stmt, ExprRole role,
                  CompilerInstance& CI, ReplacementConfig& cfg){
    if (PreVisit){
      const Decl* d = stmt->getSingleDecl();
      if (d && d->getKind() == Decl::Var){
        const VarDecl* vd = cast<VarDecl>(d);
        if (vd->getNameAsString() == cfg.matchText){
          if (vd->hasInit()) {
            cfg.replacedExprs.push_back(vd->getInit());
          } else {
            errorAbort(stmt->getLocStart(), CI,
              "replace pragma applied to declaration with no initializer");
          }
        }
      }
    }
    return false;
  }

};

void
SSTReplacePragma::run(Stmt *s, std::list<const Expr*>& replacedExprs)
{
  using PreVisit=ReplaceStatementVisit<true>;
  using PostVisit=ReplaceStatementVisit<false>;
  ReplacementConfig rcfg(fxn_, replacement_, replacedExprs);
  recurseAll<PreVisit,PostVisit>(s, *CI, rcfg);
  if (replacedExprs.size() == 0){
    std::string error = "replace pragma '" + fxn_ + "' did not match anything";
    errorAbort(s->getLocStart(), *CI, error);
  }
}

void
SSTReplacePragma::run(Stmt *s, Rewriter &r)
{
  std::list<const Expr*> replaced;
  run(s,replaced);
  for (const Expr* e: replaced){
    SourceRange rng(e->getLocStart(), e->getLocEnd());
    r.ReplaceText(rng,replacement_);
  }
}

void
SSTReplacePragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  run(s,r);
}

#define repl_case(kind,d,rw) \
  case Decl::kind: activate##kind##Decl(cast<kind##Decl>(d), rw); break

void
SSTReplacePragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  switch(d->getKind()){
    repl_case(Function,d,r);
    repl_case(Var,d,r);
    repl_case(CXXRecord,d,r);
    default:
      break;
  }
}

void
SSTReplacePragma::activateFunctionDecl(FunctionDecl *d, Rewriter &r)
{
  if (d->hasBody()) run(d->getBody(), r);
}

void
SSTReplacePragma::activateVarDecl(VarDecl *d, Rewriter &r)
{
  if (d->hasInit()) run(d->getInit(), r);
}

void
SSTReplacePragma::activateCXXRecordDecl(CXXRecordDecl *d, Rewriter &r)
{
  if (d->hasBody()) run(d->getBody(), r);
}
