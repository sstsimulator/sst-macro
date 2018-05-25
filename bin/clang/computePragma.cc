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

SSTLoopCountPragma::SSTLoopCountPragma(const std::list<Token> &tokens) :
  SSTPragma(LoopCount)
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(startLoc, tokens.begin(), tokens.end(), sstr, *CI);
  loopCount_ = sstr.str();
}

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
SSTMemoryPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  cfg.computeMemorySpec = memSpec_;
}

SSTPragma*
SSTMemoryPragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(loc, tokens.begin(), tokens.end(), sstr, ci_);
  return new SSTMemoryPragma(sstr.str());
}

enum OpenMPProperty {
  OMP_NTHREAD,
  OMP_NONE
};

SSTPragma*
SSTOpenMPParallelPragmaHandler::allocatePragma(SourceLocation loc, const std::list<Token> &tokens) const
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

  return new SSTComputePragma(nthread);
}

