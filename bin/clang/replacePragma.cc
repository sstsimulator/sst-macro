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
  ReplacementPragmaVisitor(const std::string& match) :
    matchText_(match)
  {
  }

  bool TraverseDeclStmt(DeclStmt* stmt, DataRecursionQueue* = nullptr){
    if (stmt->isSingleDecl() && stmt->getSingleDecl()->getKind() == Decl::Var){
      const Decl* d = stmt->getSingleDecl();
      const VarDecl* vd = cast<const VarDecl>(d);
      if (vd->getNameAsString() == matchText_){
        if (vd->hasInit()) {
          replaced_.insert(vd->getInit());
        } else {
          errorAbort(getStart(stmt), "replace pragma applied to declaration with no initializer");
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

};

std::set<const Expr*>
SSTReplacePragma::run(Stmt *s)
{
  ReplacementPragmaVisitor visitor(match_);
  visitor.TraverseStmt(s);
  return visitor.replaced();
}

void
SSTReplacePragma::activate(Stmt *s)
{
  std::set<const Expr*> replaced = run(s);
  for (const Expr* e: replaced){
    replace(e,replacement_);
  }
}


void
SSTReplacePragma::activate(Decl *d)
{
#define repl_case(kind,d) \
  case Decl::kind: activate##kind##Decl(cast<kind##Decl>(d)); break
  switch(d->getKind()){
    repl_case(Function,d);
    repl_case(Var,d);
    repl_case(CXXRecord,d);
    default:
      break;
  }
#undef repl_case
}

void
SSTReplacePragma::activateFunctionDecl(FunctionDecl *d)
{
  if (d->hasBody()) activate(d->getBody());
}

void
SSTReplacePragma::activateVarDecl(VarDecl *d)
{
  if (d->hasInit()) activate(d->getInit());
}

void
SSTReplacePragma::activateCXXRecordDecl(CXXRecordDecl *d)
{
  if (d->hasBody()) activate(d->getBody());
}

void
SSTInitPragma::activate(Stmt *s)
{
#define repl_case(cls,s) \
  case Stmt::cls##Class: activate##cls(cast<cls>(s)); break
  switch(s->getStmtClass()){
    repl_case(DeclStmt,s);
    repl_case(BinaryOperator,s);
    default:
      errorAbort(s, "pragma init not applied to initialization statement");
  }
#undef repl_case
}

void
SSTInitPragma::activateBinaryOperator(BinaryOperator* op)
{
  replace(op->getRHS(), init_);
  throw StmtDeleteException(op);
}

void
SSTInsteadPragma::activate(Stmt *s)
{
  replace(s, repl_);
  throw StmtDeleteException(s);
}

void
SSTInitPragma::activateDeclStmt(DeclStmt* s)
{
  if (!s->isSingleDecl()){
    errorAbort(s, "pragma init cannot apply to multiple declaration");
  }
  Decl* d = s->getSingleDecl();
  if (!isa<VarDecl>(d)){
    errorAbort(s, "pragma init only applies to variable declarations");
  }
  VarDecl* vd = cast<VarDecl>(d);
  if (!vd->hasInit()){
    errorAbort(s, "pragma init applied to variable without initializer");
  }
  replace(vd->getInit(), init_);
  throw StmtDeleteException(s);
}

std::string
SSTReplacePragma::parse(SourceLocation loc, const std::list<Token>& tokens, std::ostream& os)
{
  if (tokens.size() < 2){
    errorAbort(loc, "pragma replace requires both a function and replacement text");
  }
  const Token& fxn = tokens.front();
  if (!fxn.is(tok::identifier)){
    errorAbort(loc, "pragma replace got invalid function name");
  }

  auto iter = tokens.begin(); ++iter; //skip front
  auto end = tokens.end();
  SSTPragma::tokenStreamToString(iter,end,os);

  return fxn.getIdentifierInfo()->getNameStart();
}

SSTReplacePragma::SSTReplacePragma(SourceLocation loc, const std::list<Token> &tokens)
{
  std::stringstream sstr;
  match_ = parse(loc, tokens, sstr);
  replacement_ = sstr.str();
}

using namespace modes;

static PragmaRegister<SSTStringPragmaShim, SSTInsteadPragma, true> insteadPragma(
    "sst", "instead", SKELETONIZE | PUPPETIZE | SHADOWIZE);

static PragmaRegister<SSTStringPragmaShim, SSTInitPragma, true> initPragma(
    "sst", "init", SKELETONIZE | PUPPETIZE | SHADOWIZE);

static PragmaRegister<SSTTokenListPragmaShim, SSTReplacePragma, true> replacePragma(
    "sst", "replace", SKELETONIZE | PUPPETIZE | SHADOWIZE);

