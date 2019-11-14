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

#include "computePragma.h"
#include "computeVisitor.h"
#include "recurseAll.h"
#include "dataFlow.h"
#include "computeLoops.h"
#include "replacements.h"
#include "validateScope.h"
#include <sstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

void
SSTComputePragma::visitAndReplaceStmt(Stmt* stmt, Rewriter& r, PragmaConfig& cfg)
{
  Loop loop(0); //just treat this is a loop of size 1
  loop.tripCount = "1";
  ComputeVisitor vis(*CI, *pragmaList, nullptr, cfg.astVisitor);
  vis.setContext(stmt);
  vis.addOperations(stmt,loop.body);
  vis.replaceStmt(stmt,r,loop,cfg, nthread_);
}

void
SSTComputePragma::activate(Stmt *stmt, Rewriter &r, PragmaConfig& cfg)
{
#define scase(type,s,rw,cfg) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s),rw,cfg); break
  switch(stmt->getStmtClass()){
    scase(ForStmt,stmt,r,cfg);
    scase(IfStmt,stmt,r,cfg);
    default:
      defaultAct(stmt,r,cfg);
      break;
  }
#undef scase
  cfg.computeMemorySpec = ""; //clear any specs
  throw StmtDeleteException(stmt);
}

void
SSTComputePragma::activate(Decl* d, Rewriter& r, PragmaConfig& cfg)
{
#define dcase(type,d,rw,cfg) \
  case(clang::Decl::type): \
    visit##type##Decl(clang::cast<type##Decl>(d),rw,cfg); break
  switch(d->getKind()){
    dcase(Function,d,r,cfg);
    dcase(CXXMethod,d,r,cfg);
    default:
      break;
  }
#undef dcase
  cfg.computeMemorySpec = ""; //clear any specs
  throw DeclDeleteException(d);
}

void
SSTComputePragma::defaultAct(Stmt *stmt, Rewriter &r, PragmaConfig& cfg)
{
  visitAndReplaceStmt(stmt, r, cfg);
}


void
SSTComputePragma::visitCXXMethodDecl(CXXMethodDecl* decl, Rewriter& r, PragmaConfig& cfg)
{
  if (decl->hasBody()){
    visitAndReplaceStmt(decl->getBody(), r, cfg);
  }
}

void
SSTComputePragma::visitFunctionDecl(FunctionDecl* decl, Rewriter& r, PragmaConfig& cfg)
{
  if (decl->hasBody()){
    visitAndReplaceStmt(decl->getBody(), r, cfg);
  }
}


void
SSTComputePragma::visitIfStmt(IfStmt* stmt, Rewriter& r, PragmaConfig& cfg)
{
  visitAndReplaceStmt(stmt->getThen(), r, cfg);
  if (stmt->getElse()){
    visitAndReplaceStmt(stmt->getElse(), r, cfg);
  }
}

void
SSTComputePragma::replaceForStmt(clang::ForStmt* stmt, CompilerInstance& CI, SSTPragmaList& prgList,
                                 Rewriter& r, PragmaConfig& cfg, SkeletonASTVisitor* visitor,
                                 const std::string& nthread)
{
  visitor->appendComputeLoop(stmt);
  ComputeVisitor vis(CI, prgList, nullptr, visitor); //null, no parent
  Loop loop(0); //depth zeros
  vis.setContext(stmt);
  vis.visitLoop(stmt,loop);
  vis.replaceStmt(stmt,r,loop,cfg, nthread);
  //cfg.skipNextStmt = true;
  visitor->popComputeLoop();
}

void
SSTComputePragma::visitForStmt(ForStmt *stmt, Rewriter &r, PragmaConfig& cfg)
{
  replaceForStmt(stmt, *CI, *pragmaList, r, cfg, cfg.astVisitor, nthread_);
}

void
SSTMemoizeComputePragma::doReplace(SourceLocation startInsert, SourceLocation finalInsert, Stmt* fullStmt,
                                   bool insertStartAfter, bool insertFinalAfter,
                                   Rewriter& r, Expr** callArgs, const ParmVarDecl** callParams)
{
  std::string argsStr;
  if (!fxnArgInputs_.empty()){
    if (!callArgs && !callParams){
      internalError(startInsert, *CI,
           "have function args, but no call args or call params");
    }
    //this better be a function call
    PrettyPrinter pp;
    for (int idx : fxnArgInputs_){
      pp.os << ",";
      if (callArgs) pp.print(callArgs[idx]);
      else          pp.os << callParams[idx]->getNameAsString();
    }
    argsStr = pp.str();
  } else if (!inputs_.empty()) {
    std::stringstream args_sstr;
    for (auto& str : inputs_){
      args_sstr << "," << str;
    }
    argsStr = args_sstr.str();
  }

  if (visitor->mode() == pragmas::MEMOIZE_MODE){
    PresumedLoc ploc = CI->getSourceManager().getPresumedLoc(startInsert);
    int line = ploc.getLine();

    std::stringstream start_sstr;
    start_sstr << "int sstmac_thr_tag" << line << " = sstmac_start_memoize("
       << "\"" << token_ << "\",\"" << model_ << "\"" << "); ";
    r.InsertText(startInsert, start_sstr.str(), insertStartAfter);
    std::stringstream finish_sstr;
    finish_sstr << "; sstmac_finish_memoize" << inputs_.size() << "("
                << "sstmac_thr_tag" << line << ","
                << "\"" << token_ << "\"" << argsStr << ");";

    if (insertFinalAfter) finalInsert = finalInsert.getLocWithOffset(1);
    r.InsertText(finalInsert, finish_sstr.str(), insertFinalAfter);
  } else {
    std::stringstream sstr;
    sstr << "{ sstmac_compute_memoize" << inputs_.size() << "("
       << "\"" << token_ << "\"" << argsStr << "); }";
    if (skeletonize_){
      SourceRange rng(startInsert, finalInsert);
      replace(rng, r, sstr.str(), *CI);
      throw StmtDeleteException(fullStmt);
    } else {
      r.InsertText(startInsert, sstr.str(), insertStartAfter);
    }
  }
}

void
SSTMemoizeComputePragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  Expr** args = nullptr;
  if (!fxnArgInputs_.empty()){
    CallExpr* expr = nullptr;
    switch (s->getStmtClass()){
    case Stmt::CallExprClass:
    case Stmt::CXXMemberCallExprClass:
      expr = cast<CallExpr>(s);
      args = expr->getArgs();
      //doReplace(getStart(s), s->getLocEnd(), s,
      //          false, true, r, expr->getArgs(), nullptr);
      break;
      break;
    default:
      internalError(getStart(expr), *CI,
                 "memoize pragma activated on statement that is not a call expression");
    }
  }

  cfg.astVisitor->getActiveNamespace()->addMemoization(token_, model_);
  doReplace(getStart(s), getEnd(s), s,
              false, true, r, args, nullptr);
}

void
SSTMemoizeComputePragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  FunctionDecl* fd = nullptr;
  switch(d->getKind()){
  case Decl::Function:
  case Decl::CXXMethod:
    fd = cast<FunctionDecl>(d);
    break;
  default:
    errorAbort(d, *CI, "memoize pragma applied to declaration that is not a function");
  }

  if (!givenName_){
    token_ = fd->getNameAsString();
  }

  cfg.astVisitor->getActiveNamespace()->addMemoization(token_, model_);

  auto iter = cfg.functionPragmas.find(fd->getCanonicalDecl());
  if (iter == cfg.functionPragmas.end()){
    cfg.functionPragmas[fd->getCanonicalDecl()].insert(this);
    //first time hitting the function decl -  configure it
    //don't do modifications yet
    //just in case this gets called twice for a weird reason
    fxnArgInputs_.clear();
    for (auto& str : inputs_){
      bool found = false;
      for (int i=0; i < fd->getNumParams(); ++i){
        ParmVarDecl* pvd = fd->getParamDecl(i);
        if (pvd->getNameAsString() == str){
          found = true;
          fxnArgInputs_.push_back(i);
          break;
        }
      }
      if (!found){
        std::string error = "memoization input " + str
            + " to function declaration could not be matched to any parameter";
        errorAbort(d, *CI, error);
      }
    }
  }

  if (fd->isThisDeclarationADefinition() && fd->getBody()){
    if (fd->getBody()->getStmtClass() != Stmt::CompoundStmtClass){
      internalError(fd, *CI, "function decl body is not a compound statement");
    }

    if (written_.find(fd) == written_.end()){
      CompoundStmt* cs = cast<CompoundStmt>(fd->getBody());
      std::vector<const ParmVarDecl*> params(fd->getNumParams());
      for (int i=0; i < fd->getNumParams(); ++i){
        params[i] = fd->getParamDecl(i);
      }
      bool replaceBody = skeletonize_ && visitor->mode() != pragmas::MEMOIZE_MODE;
      if (cs->body_front()){
        doReplace(replaceBody ? getStart(cs) : getStart(cs->body_front()),
                  getEnd(cs), cs,
                  false, false, r, nullptr, params.data());
      }
      written_.insert(fd);
    }

  }
}

void
SSTImplicitStatePragma::doReplace(SourceLocation startInsert, SourceLocation finalInsert,
                                  bool insertStartAfter, bool insertFinalAfter,
                                  Rewriter& r, const std::map<std::string,std::string>& values)
{
  std::string state_type = visitor->mode() == pragmas::MEMOIZE_MODE ? "memoize" : "compute";

  bool first = true;
  std::stringstream start_sstr;
  start_sstr << "sstmac_set_implicit_" << state_type << "_state" << values_.size() << "(";
  for (auto& pair : values){
    if (!first) start_sstr << ",";
    start_sstr << pair.first << "," << pair.second;
    first = false;
  }
  start_sstr << "); ";
  r.InsertText(startInsert, start_sstr.str(), insertStartAfter);

  first = true;
  std::stringstream finish_sstr;
  finish_sstr << "; sstmac_unset_implicit_" << state_type << "_state" << values_.size() << "(";
  for (auto& pair : values){
    if (!first) finish_sstr << ",";
    first = false;
    finish_sstr << pair.first;
  }
  finish_sstr << ");";

  if (insertFinalAfter) finalInsert = finalInsert.getLocWithOffset(1);
  r.InsertText(finalInsert, finish_sstr.str(), insertFinalAfter);
}

void
SSTImplicitStatePragma::activate(Stmt *s, Rewriter &r, PragmaConfig & /*cfg*/)
{
  doReplace(getStart(s), getEnd(s),
            false, true, r, values_);
}

void
SSTImplicitStatePragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  FunctionDecl* fd = nullptr;
  switch(d->getKind()){
  case Decl::Function:
  case Decl::CXXMethod:
    fd = cast<FunctionDecl>(d);
    break;
  default:
    errorAbort(d, *CI, "implicit state pragma applied to declaration that is not a function");
  }

  auto& set = cfg.functionPragmas[fd->getCanonicalDecl()];
  if (set.find(this) == set.end()){
    set.insert(this);
    //first time hitting the function decl -  configure it
    //don't do modifications yet
    //just in case this gets called twice for a weird reason
    fxnArgValues_.clear();
    for (auto& pair : values_){
      bool found = false;
      for (int i=0; i < fd->getNumParams(); ++i){
        ParmVarDecl* pvd = fd->getParamDecl(i);
        if (pvd->getNameAsString() == pair.second){
          found = true;
          fxnArgValues_[pair.first] = i;
          break;
        }
      }
      if (!found){
        std::string error = "memoization input " + pair.first
            + " to function declaration could not be matched to any parameter";
        errorAbort(d, *CI, error);
      }
    }
  }

  if (fd->isThisDeclarationADefinition() && fd->getBody()){
    if (fd->getBody()->getStmtClass() != Stmt::CompoundStmtClass){
      internalError(fd, *CI, "function decl body is not a compound statement");
    }
    if (written_.find(fd) == written_.end()){
      CompoundStmt* cs = cast<CompoundStmt>(fd->getBody());
      std::vector<const ParmVarDecl*> params(fd->getNumParams());
      std::map<std::string,std::string> values;
      for (auto& pair : fxnArgValues_){
        values[pair.first] = fd->getParamDecl(pair.second)->getNameAsString();
      }
      if (cs->body_front()){
        doReplace(getStart(cs->body_front()),
                  getEnd(cs), false, false, r, values);
      }
      written_.insert(fd);
    }
  }
}

void
SSTMemoryPragma::activate(Stmt * /*s*/, Rewriter & /*r*/, PragmaConfig &cfg)
{
  cfg.computeMemorySpec = memSpec_;
}


enum OpenMPProperty {
  OMP_NTHREAD,
  OMP_NONE
};

std::string
SSTOpenMPParallelPragma::numThreads(SourceLocation  /*loc*/, CompilerInstance & /*CI*/, const std::list<Token> &tokens)
{
  static const std::map<std::string, OpenMPProperty> omp_property_map = {
    {"num_threads", OMP_NTHREAD},
  };
  std::string nthread;
  OpenMPProperty activeProp = OMP_NONE;
  for (const Token& t : tokens){
    switch (t.getKind()){
    case tok::identifier:
    {
      auto next = t.getIdentifierInfo()->getName().str();
      switch (activeProp) {
      case OMP_NONE:
      {
        auto iter = omp_property_map.find(next);
        if (iter != omp_property_map.end()){
          activeProp = iter->second;
        }
        break;
      }
      case OMP_NTHREAD:
      {
        nthread = next;
        activeProp = OMP_NONE;
        break;
      }
      }
      break;
    }
    case tok::string_literal:
    case tok::numeric_constant:
    {
      switch(activeProp) {
      case OMP_NTHREAD:
      {
        nthread = getLiteralDataAsString(t);
        activeProp = OMP_NONE;
        break;
      }
      case OMP_NONE:
        break;
      }
    }
    default:
      break;
  } //end switch
  } //end for

  return nthread;
}

SSTOpenMPParallelPragma::SSTOpenMPParallelPragma(SourceLocation loc, CompilerInstance& ci,
                                                 const std::list<Token> &tokens)
  : SSTComputePragma(numThreads(loc,ci,tokens))
{ 
}

SSTMemoizeComputePragma::SSTMemoizeComputePragma(clang::SourceLocation loc, clang::CompilerInstance& CI,
                                                 std::map<std::string, std::list<std::string>>&& in_args)
{
  auto args = in_args;
  auto iter = args.find("skeletonize");
  if (iter != args.end()){
    std::string val = iter->second.front();
    if (val == "true"){
      skeletonize_ = true;
    } else if (val == "false"){
      skeletonize_ = false;
    } else {
      errorAbort(loc, CI, "skeletonize argument must be true/false");
    }
    args.erase(iter);
  }

  iter = args.find("inputs");
  std::list<std::string> inputs;
  if (iter != args.end()){
    inputs_ = std::move(iter->second);
    args.erase(iter);
  }

  std::string model = "null";
  iter = args.find("model");
  if (iter != args.end()){
    model_ = iter->second.front();
    args.erase(iter);
  }

  std::string name;
  iter = args.find("name");
  if (iter != args.end()){
    token_ = iter->second.front();
    args.erase(iter);
    givenName_ = true;
  } else {
    PresumedLoc ploc = CI.getSourceManager().getPresumedLoc(loc);
    std::stringstream token_sstr;
    token_sstr << ploc.getFilename() << ":" << ploc.getLine();
    token_ = token_sstr.str();
  }

  if (!args.empty()){
    //we got passed an invalid argument
    std::stringstream sstr;
    sstr << "got invalid args for memoize pragma: ";
    for (auto& pair : args){
      sstr << pair.first << ",";
    }
    errorAbort(loc, CI, sstr.str());
  }
}


SSTImplicitStatePragma::SSTImplicitStatePragma(clang::SourceLocation loc, clang::CompilerInstance& CI,
    std::map<std::string, std::list<std::string>>&& in_args)
{
  for (auto& pair : in_args){
    if (pair.second.size() > 1){
      std::string error = "cannot specify multiple values for implicit state " + pair.first;
      errorAbort(loc, CI, error);
    }
    if (pair.second.empty()){
      values_[pair.first] = "0";
    } else {
      values_[pair.first] = pair.second.front();
    }
  }
}

using namespace pragmas;

static PragmaRegister<SSTStringPragmaShim, SSTLoopCountPragma, true> loopCountPragma(
    "sst", "loop_count", SKELETONIZE | PUPPETIZE | SHADOWIZE);
static PragmaRegister<SSTStringPragmaShim, SSTMemoryPragma, true> memoryPragma(
    "sst", "memory", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTNoArgsPragmaShim, SSTComputePragma, true> computePragma(
    "sst", "compute", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTNoArgsPragmaShim, SSTComputePragma, true> alwaysComputePragma(
    "sst", "always_compute", SKELETONIZE | SHADOWIZE | ENCAPSULATE);
// static PragmaRegister<SSTArgMapPragmaShim, SSTMemoizeComputePragma, true> memoizePragma(
//     "sst", "memoize", MEMOIZE | SKELETONIZE);
static PragmaRegister<SSTArgMapPragmaShim, SSTImplicitStatePragma, true> implictPragma(
    "sst", "implicit_state", MEMOIZE | SKELETONIZE);
static PragmaRegister<SSTTokenListPragmaShim, SSTOpenMPParallelPragma, true> ompPragmaSkel(
    "omp", "parallel", SKELETONIZE);
static PragmaRegister<SSTTokenListPragmaShim, SSTOpenMPParallelPragma, false> ompPragmaPup(
    "omp", "parallel", PUPPETIZE);
