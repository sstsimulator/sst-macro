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
#include "dataFlow.h"
#include "computeLoops.h"
#include "validateScope.h"
#include <sstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

SSTLoopCountPragma::SSTLoopCountPragma(SourceLocation loc, const std::list<clang::Token>& tokens)
{
  if (tokens.size() != 1){
    errorAbort(loc, "loop_count pragma should taken a single token as the pragma argument");
  }
  loopCountToken_ = tokens.front();
  loopCount_ = getSingleString(tokens);
}

void
SSTLoopCountPragma::transformWhileLoop(Stmt* s)
{
  WhileStmt* ws = cast<WhileStmt>(s);
  FunctionDecl* fd = CompilerGlobals::pragmaConfig.fxnContexts.back();
  Expr* loop_count = tokenToExpr(fd, loopCountToken_, getStart(ws->getCond()));
  IdentifierInfo* II = &CompilerGlobals::CI().getPreprocessor().getIdentifierTable().get("loop_variable");
  TypeSourceInfo *TInfo = fd->getASTContext().getTrivialTypeSourceInfo(loop_count->getType(), getStart(ws->getCond()));
  VarDecl* loopVar = VarDecl::Create(fd->getASTContext(), fd,
                                     getStart(ws->getCond()), getEnd(ws->getCond()),
                                     II, fd->getASTContext().IntTy, TInfo,
                                     SC_None);
  llvm::APInt api(32,0,true);
  IntegerLiteral* zero = IntegerLiteral::Create(fd->getASTContext(), api,
                                                fd->getASTContext().IntTy, getEnd(ws->getCond()));
  loopVar->setInit(zero);
  DeclGroupRef dgr(loopVar);
  DeclStmt* init_stmt = new (fd->getASTContext()) DeclStmt(dgr, getStart(ws->getCond()), getEnd(ws->getCond()));

  DeclRefExpr* cond_ref = DeclRefExpr::Create(
     fd->getASTContext(),
     loopVar->getQualifierLoc(),
     SourceLocation(),
     loopVar,
     false,
     getStart(ws->getCond()),
     loopVar->getType(),
     VK_LValue,
     loopVar);

  BinaryOperator* cond_stmt = new (fd->getASTContext()) BinaryOperator(cond_ref, loop_count, BO_LT,
                                             fd->getASTContext().IntTy, VK_RValue,
                                             OK_Ordinary, getStart(ws->getCond()), FPOptions());

  DeclRefExpr* inc_ref = DeclRefExpr::Create(
     fd->getASTContext(),
     loopVar->getQualifierLoc(),
     SourceLocation(),
     loopVar,
     false,
     getStart(ws->getCond()),
     loopVar->getType(),
     VK_LValue,
     loopVar);

  UnaryOperator* inc_stmt = new (fd->getASTContext()) UnaryOperator(inc_ref, UO_PostInc, loop_count->getType(),
                                                          VK_RValue, OK_Ordinary, getStart(ws->getCond()));

  ForStmt* fs = new (fd->getASTContext()) ForStmt(fd->getASTContext(), init_stmt, cond_stmt, loopVar, inc_stmt,
                     ws->getBody(), getStart(ws), getStart(ws->getCond()), getEnd(ws->getCond()));

  auto iter = CompilerGlobals::pragmaConfig.findStmtBlockMatch(ws);
  if (iter == CompilerGlobals::pragmaConfig.currentStmtBlockEnd()){
    internalError(ws, "while-stmt is not part of an enclosing CompountStmt");
  }
  *iter = fs;
}

void
SSTLoopCountPragma::transformForLoop(Stmt* s)
{
  ForStmt* fs = cast<ForStmt>(s);
  FunctionDecl* fd = CompilerGlobals::pragmaConfig.fxnContexts.back();
  Expr* loop_count = tokenToExpr(fd, loopCountToken_, getStart(fs->getCond()));
  IdentifierInfo* II = &CompilerGlobals::CI().getPreprocessor().getIdentifierTable().get("loop_variable");
  TypeSourceInfo *TInfo = fd->getASTContext().getTrivialTypeSourceInfo(loop_count->getType(), getStart(fs->getInit()));
  VarDecl* loopVar = VarDecl::Create(fd->getASTContext(), fd,
                                     getStart(fs->getInit()), getEnd(fs->getInit()),
                                     II, fd->getASTContext().IntTy, TInfo,
                                     SC_None);
  llvm::APInt api(32,0,true);
  IntegerLiteral* zero = IntegerLiteral::Create(fd->getASTContext(), api,
                                                fd->getASTContext().IntTy, getEnd(fs->getInit()));
  loopVar->setInit(zero);
  DeclGroupRef dgr(loopVar);
  DeclStmt* init_stmt = new (fd->getASTContext()) DeclStmt(dgr, getStart(fs->getInit()), getEnd(fs->getInit()));

  DeclRefExpr* cond_ref = DeclRefExpr::Create(
     fd->getASTContext(),
     loopVar->getQualifierLoc(),
     SourceLocation(),
     loopVar,
     false,
     getStart(fs->getCond()),
     loopVar->getType(),
     VK_LValue,
     loopVar);

  BinaryOperator* cond_stmt = new (fd->getASTContext()) BinaryOperator(cond_ref, loop_count, BO_LT,
                                             fd->getASTContext().IntTy, VK_RValue,
                                             OK_Ordinary, getStart(fs->getCond()), FPOptions());

  DeclRefExpr* inc_ref = DeclRefExpr::Create(
     fd->getASTContext(),
     loopVar->getQualifierLoc(),
     SourceLocation(),
     loopVar,
     false,
     getStart(fs->getInc()),
     loopVar->getType(),
     VK_LValue,
     loopVar);

  UnaryOperator* inc_stmt = new (fd->getASTContext()) UnaryOperator(inc_ref, UO_PostInc, loop_count->getType(),
                                                          VK_RValue, OK_Ordinary, getStart(fs->getInc()));

  fs->setInit(init_stmt);
  fs->setCond(cond_stmt);
  fs->setInc(inc_stmt);
  fs->setConditionVariable(fd->getASTContext(), loopVar);
}

void
SSTLoopCountPragma::activate(Stmt *s)
{
  if (isa<WhileStmt>(s)){
    transformWhileLoop(s);
  } else if (isa<ForStmt>(s)){
    transformForLoop(s);
  } else {
    errorAbort(s, "loop_count pragma not applied to ForStmt or WhileStmt");
  }
}

void
SSTComputePragma::visitAndReplaceStmt(Stmt* stmt)
{
  Loop loop(0); //just treat this is a loop of size 1
  loop.tripCount = "1";
  ComputeVisitor vis{};
  vis.setContext(stmt);
  vis.addOperations(stmt,loop.body);
  vis.replaceStmt(stmt,loop,nthread_);
}

void
SSTComputePragma::activate(Stmt *stmt)
{
#define scase(type,s) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s)); break
  switch(stmt->getStmtClass()){
    scase(ForStmt,stmt);
    scase(IfStmt,stmt);
    default:
      defaultAct(stmt);
      break;
  }
#undef scase
  CompilerGlobals::pragmaConfig.computeMemorySpec = ""; //clear any specs
  throw StmtDeleteException(stmt);
}

void
SSTComputePragma::activate(Decl* d)
{
#define dcase(type,d) \
  case(clang::Decl::type): \
    visit##type##Decl(clang::cast<type##Decl>(d)); break
  switch(d->getKind()){
    dcase(Function,d);
    dcase(CXXMethod,d);
    default:
      break;
  }
#undef dcase
  CompilerGlobals::pragmaConfig.computeMemorySpec = ""; //clear any specs
  throw DeclDeleteException(d);
}

void
SSTComputePragma::defaultAct(Stmt *stmt)
{
  visitAndReplaceStmt(stmt);
}


void
SSTComputePragma::visitCXXMethodDecl(CXXMethodDecl* decl)
{
  if (decl->hasBody()){
    visitAndReplaceStmt(decl->getBody());
  }
}

void
SSTComputePragma::visitFunctionDecl(FunctionDecl* decl)
{
  if (decl->hasBody()){
    visitAndReplaceStmt(decl->getBody());
  }
}


void
SSTComputePragma::visitIfStmt(IfStmt* stmt)
{
  visitAndReplaceStmt(stmt->getThen());
  if (stmt->getElse()){
    visitAndReplaceStmt(stmt->getElse());
  }
}

void
SSTComputePragma::replaceForStmt(clang::ForStmt* stmt, const std::string& nthread)
{
  ComputeVisitor vis{}; //null, no parent
  Loop loop(0); //depth zeros
  vis.setContext(stmt);
  vis.visitLoop(stmt,loop);
  vis.replaceStmt(stmt,loop,nthread);
  //cfg.skipNextStmt = true;
}

void
SSTComputePragma::visitForStmt(ForStmt *stmt)
{
  replaceForStmt(stmt, nthread_);
}

void
SSTMemoizeComputePragma::doReplace(SourceLocation startInsert, SourceLocation finalInsert, Stmt* fullStmt,
                                   bool insertStartAfter, bool insertFinalAfter,
                                   Expr** callArgs, const ParmVarDecl** callParams)
{
  std::string argsStr;
  if (!fxnArgInputs_.empty()){
    if (!callArgs && !callParams){
      internalError(startInsert, "have function args, but no call args or call params");
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

  if (CompilerGlobals::mode == modes::MEMOIZE_MODE){
    PresumedLoc ploc = CompilerGlobals::SM().getPresumedLoc(startInsert);
    int line = ploc.getLine();

    std::stringstream start_sstr;
    start_sstr << "int sstmac_thr_tag" << line << " = sstmac_start_memoize("
       << "\"" << token_ << "\",\"" << model_ << "\"" << "); ";
    CompilerGlobals::rewriter.InsertText(startInsert, start_sstr.str(), insertStartAfter);
    std::stringstream finish_sstr;
    finish_sstr << "; sstmac_finish_memoize" << inputs_.size() << "("
                << "sstmac_thr_tag" << line << ","
                << "\"" << token_ << "\"" << argsStr << ");";

    if (insertFinalAfter) finalInsert = finalInsert.getLocWithOffset(1);
    CompilerGlobals::rewriter.InsertText(finalInsert, finish_sstr.str(), insertFinalAfter);
  } else {
    std::stringstream sstr;
    sstr << "{ sstmac_compute_memoize" << inputs_.size() << "("
       << "\"" << token_ << "\"" << argsStr << "); }";
    if (skeletonize_){
      SourceRange rng(startInsert, finalInsert);
      replace(rng, sstr.str());
      throw StmtDeleteException(fullStmt);
    } else {
      CompilerGlobals::rewriter.InsertText(startInsert, sstr.str(), insertStartAfter);
    }
  }
}

void
SSTMemoizeComputePragma::activate(Stmt *s)
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
      internalError(getStart(expr), "memoize pragma activated on statement that is not a call expression");
    }
  }

  CompilerGlobals::pragmaConfig.visitor.skeleton->getActiveNamespace()->addMemoization(token_, model_);
  doReplace(getStart(s), getEnd(s), s,
            false, true, args, nullptr);
}

void
SSTMemoizeComputePragma::activate(Decl *d)
{
  FunctionDecl* fd = nullptr;
  switch(d->getKind()){
  case Decl::Function:
  case Decl::CXXMethod:
    fd = cast<FunctionDecl>(d);
    break;
  default:
    errorAbort(d, "memoize pragma applied to declaration that is not a function");
  }

  if (!givenName_){
    token_ = fd->getNameAsString();
  }

  CompilerGlobals::pragmaConfig.visitor.skeleton->getActiveNamespace()->addMemoization(token_, model_);

  auto iter = CompilerGlobals::pragmaConfig.functionPragmas.find(fd->getCanonicalDecl());
  if (iter == CompilerGlobals::pragmaConfig.functionPragmas.end()){
    CompilerGlobals::pragmaConfig.functionPragmas[fd->getCanonicalDecl()].insert(this);
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
        errorAbort(d, error);
      }
    }
  }

  if (fd->isThisDeclarationADefinition() && fd->getBody()){
    if (fd->getBody()->getStmtClass() != Stmt::CompoundStmtClass){
      internalError(fd, "function decl body is not a compound statement");
    }

    if (written_.find(fd) == written_.end()){
      CompoundStmt* cs = cast<CompoundStmt>(fd->getBody());
      std::vector<const ParmVarDecl*> params(fd->getNumParams());
      for (int i=0; i < fd->getNumParams(); ++i){
        params[i] = fd->getParamDecl(i);
      }
      bool replaceBody = skeletonize_ && CompilerGlobals::mode != modes::MEMOIZE_MODE;
      if (cs->body_front()){
        doReplace(replaceBody ? getStart(cs) : getStart(cs->body_front()),
                  getEnd(cs), cs,
                  false, false, nullptr, params.data());
      }
      written_.insert(fd);
    }

  }
}

void
SSTMemoryPragma::activate(Stmt *s)
{
  CompilerGlobals::pragmaConfig.computeMemorySpec = memSpec_;
}


enum OpenMPProperty {
  OMP_NTHREAD,
  OMP_NONE
};

std::string
SSTOpenMPParallelPragma::numThreads(SourceLocation loc, const std::list<Token> &tokens)
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

SSTOpenMPParallelPragma::SSTOpenMPParallelPragma(SourceLocation loc, const std::list<Token> &tokens)
  : SSTComputePragma(numThreads(loc,tokens))
{ 
}

SSTMemoizeComputePragma::SSTMemoizeComputePragma(clang::SourceLocation loc,
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
      errorAbort(loc,"skeletonize argument must be true/false");
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
    PresumedLoc ploc = CompilerGlobals::SM().getPresumedLoc(loc);
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
    errorAbort(loc, sstr.str());
  }
}


using namespace modes;

static PragmaRegister<SSTTokenListPragmaShim, SSTLoopCountPragma, true> loopCountPragma(
    "sst", "loop_count", SKELETONIZE | PUPPETIZE | SHADOWIZE);
static PragmaRegister<SSTStringPragmaShim, SSTMemoryPragma, true> memoryPragma(
    "sst", "memory", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTNoArgsPragmaShim, SSTComputePragma, true> computePragma(
    "sst", "compute", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTNoArgsPragmaShim, SSTComputePragma, true> alwaysComputePragma(
    "sst", "always_compute", SKELETONIZE | SHADOWIZE | ENCAPSULATE);
// static PragmaRegister<SSTArgMapPragmaShim, SSTMemoizeComputePragma, true> memoizePragma(
//     "sst", "memoize", MEMOIZE | SKELETONIZE);
static PragmaRegister<SSTTokenListPragmaShim, SSTOpenMPParallelPragma, true> ompPragmaSkel(
    "omp", "parallel", SKELETONIZE);
static PragmaRegister<SSTTokenListPragmaShim, SSTOpenMPParallelPragma, false> ompPragmaPup(
    "omp", "parallel", PUPPETIZE);
