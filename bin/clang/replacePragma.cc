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
#include "astVisitor.h"
#include <sstream>
#include <list>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

class ChildReplacedVisitor : public RecursiveASTVisitor<ChildReplacedVisitor>
{
 public:
  ChildReplacedVisitor(clang::Expr* parent, std::set<const Expr*>& toReplace) :
     parent_(parent), toReplace_(toReplace), childReplaced_(false)
  {
  }

  bool childReplaced() const {
    return childReplaced_;
  }

  bool VisitStmt(clang::Stmt* stmt){
    for (auto* expr : toReplace_){
      if (stmt == expr){
        toReplace_.erase(expr);
        toReplace_.insert(parent_);
        childReplaced_ = true;
        return false;
      }
    }
    return true;
  }

 private:
  bool childReplaced_;
  clang::Expr* parent_;
  std::set<const Expr*>& toReplace_;
};

class ReplacementPragmaVisitor : public RecursiveASTVisitor<ReplacementPragmaVisitor>
{
  using Parent=RecursiveASTVisitor<ReplacementPragmaVisitor>;
 public:
  ReplacementPragmaVisitor(const std::string& match, CompilerInstance& CI) :
    matchText_(match), ci_(CI)
  {
  }

  bool TraverseDeclStmt(DeclStmt* stmt, DataRecursionQueue* queue = nullptr){
    if (stmt->isSingleDecl() && stmt->getSingleDecl()->getKind() == Decl::Var){
      const Decl* d = stmt->getSingleDecl();
      const VarDecl* vd = cast<const VarDecl>(d);
      if (vd->getNameAsString() == matchText_){
        if (vd->hasInit()) {
          replaced_.insert(vd->getInit());
        } else {
          errorAbort(getStart(stmt), ci_,
            "replace pragma applied to declaration with no initializer");
        }
      }
    }
    return Parent::TraverseDeclStmt(stmt);
  }

  bool matchingDeclRef(Expr* expr){
    auto* underlying = SkeletonASTVisitor::getUnderlyingExpr(expr);
    if (underlying->getStmtClass() == Stmt::DeclRefExprClass){
      DeclRefExpr* dref = cast<DeclRefExpr>(underlying);
      NamedDecl* nd = dref->getFoundDecl();
      if (nd->getNameAsString() == matchText_){
        return true;
      }
    }
    return false;
  }

  bool TraverseArraySubscriptExpr(ArraySubscriptExpr* expr, DataRecursionQueue* queue = nullptr){
    if (matchingDeclRef(expr->getBase())){
      replaced_.insert(expr);
      return true;
    } else {
      Parent::TraverseArraySubscriptExpr(expr);
      ChildReplacedVisitor vis(expr, replaced_);
      vis.TraverseStmt(expr);
      return true;
    }
  }

  bool TraverseCallExpr(CallExpr* expr, DataRecursionQueue* queue = nullptr){
    if (matchingDeclRef(expr->getCallee())){
      replaced_.insert(expr);
      return true;
    } else {
      return Parent::TraverseCallExpr(expr);
    }
  }

  const std::set<const Expr*> replaced() const {
    return replaced_;
  }

 private:
  std::string matchText_;
  std::set<const Expr*> replaced_;
  clang::CompilerInstance& ci_;

};

std::set<const Expr*>
SSTReplacePragma::run(Stmt *s)
{
  ReplacementPragmaVisitor visitor(match_, *CI);
  visitor.TraverseStmt(s);
  return visitor.replaced();
}

void
SSTReplacePragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  std::set<const Expr*> replaced = run(s);
  for (const Expr* e: replaced){
    replace(e,r,replacement_,*CI);
  }
}


void
SSTReplacePragma::activate(Decl *d, Rewriter &r, PragmaConfig & /*cfg*/)
{
#define repl_case(kind,d,rw,cfg) \
  case Decl::kind: activate##kind##Decl(cast<kind##Decl>(d), rw, cfg); break
  switch(d->getKind()){
    repl_case(Function,d,r,cfg);
    repl_case(Var,d,r,cfg);
    repl_case(CXXRecord,d,r,cfg);
    default:
      break;
  }
#undef repl_case
}

void
SSTReplacePragma::activateFunctionDecl(FunctionDecl *d, Rewriter &r, PragmaConfig& cfg)
{
  if (d->hasBody()) activate(d->getBody(), r, cfg);
}

void
SSTReplacePragma::activateVarDecl(VarDecl *d, Rewriter &r, PragmaConfig& cfg)
{
  if (d->hasInit()) activate(d->getInit(), r, cfg);
}

void
SSTReplacePragma::activateCXXRecordDecl(CXXRecordDecl *d, Rewriter &r, PragmaConfig& cfg)
{
  if (d->hasBody()) activate(d->getBody(), r, cfg);
}

void
SSTInitPragma::activate(Stmt *s, Rewriter &r, PragmaConfig & /*cfg*/)
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
SSTInsteadPragma::activate(Stmt *s, Rewriter &r, PragmaConfig & /*cfg*/)
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
SSTReplacePragma::parse(SourceLocation loc,
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

SSTReplacePragma::SSTReplacePragma(SourceLocation loc, CompilerInstance& CI,
    const std::list<Token> &tokens)
{
  std::stringstream sstr;
  match_ = parse(loc, CI, tokens, sstr);
  replacement_ = sstr.str();
}

using namespace pragmas;

static PragmaRegister<SSTStringPragmaShim, SSTInsteadPragma, true> insteadPragma(
    "sst", "instead", SKELETONIZE | PUPPETIZE | SHADOWIZE);

static PragmaRegister<SSTStringPragmaShim, SSTInitPragma, true> initPragma(
    "sst", "init", SKELETONIZE | PUPPETIZE | SHADOWIZE);

static PragmaRegister<SSTTokenListPragmaShim, SSTReplacePragma, true> replacePragma(
    "sst", "replace", SKELETONIZE | PUPPETIZE | SHADOWIZE);

