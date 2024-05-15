/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

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

#if CLANG_VERSION_MAJOR >= 7
  #define UnaryOperatorCtor(...) UnaryOperator(__VA_ARGS__, false)
#else
  #define UnaryOperatorCtor(...) UnaryOperator(__VA_ARGS__)
#endif

#if CLANG_VERSION_MAJOR >= 5
  #define BinaryOperatorCtor(...) BinaryOperator(__VA_ARGS__, FPOptions())
#else
  #define BinaryOperatorCtor(...) BinaryOperator(__VA_ARGS__, false)
#endif

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
  FunctionDecl* fd = CompilerGlobals::astContextLists.enclosingFunctionDecls.back();
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

  BinaryOperator* cond_stmt = new (fd->getASTContext()) BinaryOperatorCtor(cond_ref, loop_count, BO_LT,
                                             fd->getASTContext().IntTy, VK_RValue,
                                             OK_Ordinary, getStart(ws->getCond()));

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


  UnaryOperator* inc_stmt = new (fd->getASTContext()) UnaryOperatorCtor(inc_ref, UO_PostInc, loop_count->getType(),
                                                          VK_RValue, OK_Ordinary, getStart(ws->getCond()));

  ForStmt* fs = new (fd->getASTContext()) ForStmt(fd->getASTContext(), init_stmt, cond_stmt, loopVar, inc_stmt,
                     ws->getBody(), getStart(ws), getStart(ws->getCond()), getEnd(ws->getCond()));

  auto iter = CompilerGlobals::astContextLists.findStmtBlockMatch(ws);
  if (iter == CompilerGlobals::astContextLists.currentStmtBlockEnd()){
    internalError(ws, "while-stmt is not part of an enclosing CompountStmt");
  }
  *iter = fs;
}

void
SSTLoopCountPragma::transformForLoop(Stmt* s)
{
  ForStmt* fs = cast<ForStmt>(s);
  FunctionDecl* fd = CompilerGlobals::astContextLists.enclosingFunctionDecls.back();
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

  BinaryOperator* cond_stmt = new (fd->getASTContext()) BinaryOperatorCtor(cond_ref, loop_count, BO_LT,
                                             fd->getASTContext().IntTy, VK_RValue,
                                             OK_Ordinary, getStart(fs->getCond()));

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

  UnaryOperator* inc_stmt = new (fd->getASTContext()) UnaryOperatorCtor(inc_ref, UO_PostInc, loop_count->getType(),
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
SSTMemoryPragma::activate(Stmt *s)
{
  CompilerGlobals::astNodeMetadata.computeMemoryOverrides[s] = memSpec_;
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
