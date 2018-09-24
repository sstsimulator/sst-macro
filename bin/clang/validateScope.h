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
#ifndef bin_clang_validate_scope_h
#define bin_clang_validate_scope_h

#include "clangHeaders.h"
#include "replacements.h"
#include "recurseAll.h"
#include "astVisitor.h"
#include <sstream>

struct VariableScope {
  clang::SourceLocation start;
  clang::SourceLocation stop;
  SkeletonASTVisitor* astContext;
  bool inScope(const clang::DeclRefExpr* expr){
    if (astContext->isGlobal(expr)){
      return true;
    }

    if (stop.isInvalid()){
      return false; //well crap, null scope
    }

    clang::SourceLocation loc = expr->getFoundDecl()->getLocStart();
    return loc > start && loc < stop;
  }
};

struct ValidateScope {
  bool operator()(const clang::Expr* e, 
                  ExprRole role, Replacements& repls,
                  clang::CompilerInstance& CI, 
                  VariableScope& scope){
    //if this has been replaced, stop recursing
    return repls.exprs.find(e) != repls.exprs.end();
  }

  bool operator()(const clang::Stmt* s, 
                  ExprRole role, Replacements& repls,
                  clang::CompilerInstance& CI, 
                  VariableScope& scope){
    //do nothing
    return false; //but don't stop recursing
  }

  bool operator()(const clang::DeclRefExpr* expr, 
                  ExprRole role, Replacements& repls,
                  clang::CompilerInstance& CI, 
                  VariableScope& scope){
    const clang::ValueDecl* d = expr->getDecl();

    //this might be already replaced - then we don't need to worry about it
    if (repls.decls.find(expr->getDecl()) != repls.decls.end()){
      return false;
    } else if (repls.exprs.find(expr) != repls.exprs.end()){
      return true;
    }

    //some types we skip
    switch(d->getKind()){
      case clang::Decl::EnumConstant:
      case clang::Decl::CXXMethod:
        return false;
      default: break;
    }

    if (!scope.inScope(expr)){
      std::stringstream sstr;
      sstr << "control variable '" << d->getNameAsString()
           << "' declared at "
           << d->getLocStart().printToString(CI.getSourceManager())
           << " of type " << d->getDeclKindName()
           << " which is inside skeletonized block starting at "
           << scope.stop.printToString(CI.getSourceManager())
           << " - must use #pragma sst replace";
      errorAbort(expr->getLocStart(), CI, sstr.str());
    }
    return false;
  }
};

#endif
