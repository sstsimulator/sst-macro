/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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
#include "replacePragma.h"
#include "recurseAll.h"
#include "astVisitor.h"
#include <sstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

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
        const VarDecl* vd = cast<const VarDecl>(d);
        if (vd->getNameAsString() == cfg.matchText){
          if (vd->hasInit()) {
            cfg.replacedExprs.push_back(vd->getInit());
          } else {
            errorAbort(getStart(stmt), CI,
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
    errorAbort(s, *CI, error);
  }
}

void
SSTReplacePragma::run(Stmt *s, Rewriter& r, std::list<const Expr *> &replaced)
{
  run(s,replaced);
  for (const Expr* e: replaced){
    replace(e,r,replacement_,*CI);
  }
}

void
SSTReplacePragma::run(Stmt *s, Rewriter &r)
{
  std::list<const Expr*> replaced;
  run(s,r,replaced);
}

void
SSTReplacePragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  std::list<const Expr*> replaced;
  run(s,r,replaced);
  for (const Expr* e: replaced){
    if (e->getStmtClass() == Stmt::DeclRefExprClass){
      cfg.deletedRefs.insert(cast<const DeclRefExpr>(e));
    }
  }
}


void
SSTReplacePragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
#define repl_case(kind,d,rw) \
  case Decl::kind: activate##kind##Decl(cast<kind##Decl>(d), rw); break
  switch(d->getKind()){
    repl_case(Function,d,r);
    repl_case(Var,d,r);
    repl_case(CXXRecord,d,r);
    default:
      break;
  }
#undef repl_case
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

void
SSTInitPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
#define repl_case(cls,s,rw) \
  case Stmt::cls##Class: activate##cls(cast<cls>(s), rw); break
  switch(s->getStmtClass()){
    repl_case(DeclStmt,s,r);
    repl_case(BinaryOperator,s,r);
    default:
      errorAbort(s, *CI,
                 "pragma init not applied to initialization statement");
  }
#undef repl_case
}

void
SSTInitPragma::activateBinaryOperator(BinaryOperator* op, Rewriter& r)
{
  replace(op->getRHS(), r, init_, *CI);
  throw StmtDeleteException(op);
}

void
SSTInsteadPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  replace(s, r, repl_, *CI);
  throw StmtDeleteException(s);
}

void
SSTInitPragma::activateDeclStmt(DeclStmt* s, Rewriter& r)
{
  if (!s->isSingleDecl()){
    errorAbort(s, *CI,
               "pragma init cannot apply to multiple declaration");
  }
  Decl* d = s->getSingleDecl();
  if (!isa<VarDecl>(d)){
    errorAbort(s, *CI,
               "pragma init only applies to variable declarations");
  }
  VarDecl* vd = cast<VarDecl>(d);
  if (!vd->hasInit()){
    errorAbort(s, *CI,
               "pragma init applied to variable without initializer");
  }
  replace(vd->getInit(), r, init_, *CI);
  throw StmtDeleteException(s);
}

std::string
SSTReplacePragmaHandler::parse(SourceLocation loc,
    CompilerInstance& CI, const std::list<Token>& tokens, std::ostream& os)
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
  SSTPragma::tokenStreamToString(iter,end,os,CI);

  return fxn.getIdentifierInfo()->getNameStart();
}

SSTPragma*
SSTReplacePragmaHandler::handleSSTPragma(const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  std::string fxn = parse(pragmaLoc_, ci_, tokens, sstr);
  return new SSTReplacePragma(fxn, sstr.str());
}

SSTPragma*
SSTStartReplacePragmaHandler::handleSSTPragma(const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  std::string fxn = SSTReplacePragmaHandler::parse(pragmaLoc_, ci_, tokens, sstr);
  return new SSTStartReplacePragma(fxn, sstr.str());
}

SSTPragma*
SSTStopReplacePragmaHandler::handleSSTPragma(const std::list<Token> &tokens) const
{
  std::string fxn = tokens.front().getIdentifierInfo()->getNameStart();
  return new SSTStopReplacePragma(fxn, "not relevant");
}

SSTPragma*
SSTInitPragmaHandler::handleSSTPragma(const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(tokens.begin(), tokens.end(), sstr, ci_);
  return new SSTInitPragma(sstr.str());
}

SSTPragma*
SSTInsteadPragmaHandler::handleSSTPragma(const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(tokens.begin(), tokens.end(), sstr, ci_);
  return new SSTInsteadPragma(sstr.str());
}

