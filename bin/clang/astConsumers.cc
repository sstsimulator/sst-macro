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

#include "astConsumers.h"

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

void
SkeletonASTConsumer::initNullWhitelist()
{
  nullWhitelist_.insert("gasnetc_AMReplyLongM");
  nullWhitelist_.insert("_gasnet_put");
  nullWhitelist_.insert("gasnetc_AMRequestLongM");
  nullWhitelist_.insert("gasnetc_AMRequestLongAsyncM");
  nullWhitelist_.insert("gasnetc_AMReplyMediumM");
  nullWhitelist_.insert("gasnetc_AMRequestMediumM");
}

bool
SkeletonASTConsumer::HandleTopLevelDecl(DeclGroupRef DR)
{
  for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b){
    Decl* d = *b;
    switch(d->getKind()){
     case Decl::Function: {
        FunctionDecl* fd = cast<FunctionDecl>(d);
        //the function decl will have its body filled in later
        //possibly - we need to make sure to only add the function once
        if (fd->isThisDeclarationADefinition()){
          //also, we only really care about the definition anyway
          allDecls_.push_back(d);
        }
        if (isNullWhitelisted(fd->getNameAsString())){
          visitor_.pragmaConfig_.nullSafeFunctions[fd] = nullptr;
        }
      }
      break;
     default:
      allDecls_.push_back(d);
      break;
    }
    firstPass_.TraverseDecl(d);
    //delay processing to force all template instances to be generated
    //visitor_.setTopLevelScope(d);
    //bool isGlobalVar = isa<VarDecl>(d);
    //visitor_.setVisitingGlobal(isGlobalVar);
    //visitor_.TraverseDecl(d);
    //visitor_.setVisitingGlobal(false); //and reset
    //allDecls_.push_back(d);
  }
  return true;
}

void
SkeletonASTConsumer::run()
{
  try {
    for (Decl* d : allDecls_){
      visitor_.setTopLevelScope(d);
      bool isGlobalVar = isa<VarDecl>(d);
      visitor_.setVisitingGlobal(isGlobalVar);
      visitor_.TraverseDecl(d);
      visitor_.setVisitingGlobal(false); //and reset
    }
  } catch (StmtDeleteException& e) {
    //e.deleted->dump();
    std::string error = std::string("unhandled delete exception on expression")
        + " of type " + e.deleted->getStmtClassName();
    internalError(e.deleted->getLocStart(), visitor_.getCompilerInstance(), error);
  }

}
