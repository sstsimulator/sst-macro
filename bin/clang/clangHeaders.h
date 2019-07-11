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

#ifndef CLANGHEADERS_H
#define CLANGHEADERS_H

#include "clang/Parse/Parser.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Expr.h"
#include "clang/Lex/Lexer.h"
#include <clang/Lex/Preprocessor.h>
#include <clang/Sema/Sema.h>
#include <clang/Basic/Version.h>
#include <llvm/Support/CommandLine.h>

#define UNARYOP_LIST()                                                         \
  OPERATOR(PostInc) OPERATOR(PostDec) OPERATOR(PreInc) OPERATOR(PreDec)        \
      OPERATOR(AddrOf) OPERATOR(Deref) OPERATOR(Plus) OPERATOR(Minus)          \
      OPERATOR(Not) OPERATOR(LNot) OPERATOR(Real) OPERATOR(Imag)               \
      OPERATOR(Extension) OPERATOR(Coawait)

// All binary operators (excluding compound assign operators).
#define BINOP_LIST()                                                           \
  OPERATOR(PtrMemD) OPERATOR(PtrMemI) OPERATOR(Mul) OPERATOR(Div)              \
      OPERATOR(Rem) OPERATOR(Add) OPERATOR(Sub) OPERATOR(Shl) OPERATOR(Shr)    \
      OPERATOR(LT) OPERATOR(GT) OPERATOR(LE) OPERATOR(GE) OPERATOR(EQ)         \
      OPERATOR(NE) OPERATOR(And) OPERATOR(Xor) OPERATOR(Or) OPERATOR(LAnd)     \
      OPERATOR(LOr) OPERATOR(Assign) OPERATOR(Comma)

// All compound assign operators.
#define CAO_LIST()                                                             \
  OPERATOR(Mul) OPERATOR(Div) OPERATOR(Rem) OPERATOR(Add) OPERATOR(Sub)        \
      OPERATOR(Shl) OPERATOR(Shr) OPERATOR(And) OPERATOR(Or) OPERATOR(Xor)

#if CLANG_VERSION_MAJOR <= 5
#define GetTypeString(...) clang::QualType::getAsString(__VA_ARGS__)
#else
#define GetTypeString(...) clang::QualType::getAsString(__VA_ARGS__, Printing::policy)
#endif

struct Printing
{
  static clang::LangOptions langOpts;
  static clang::PrintingPolicy policy;
};

static inline std::string GetAsString(const clang::Type* ty){
  if (clang::isa<clang::DecltypeType>(ty)){
    const clang::DecltypeType* dt = clang::cast<clang::DecltypeType>(ty);
    return GetTypeString(dt, clang::Qualifiers());
  } else {
    return GetTypeString(ty, clang::Qualifiers());
  }
}

static inline std::string GetAsString(clang::QualType qty){
  if (clang::isa<clang::DecltypeType>(qty.getTypePtr())){
    const clang::DecltypeType* dt = clang::cast<clang::DecltypeType>(qty.getTypePtr());
    return GetTypeString(dt->getUnderlyingType().split());
  } else {
    return GetTypeString(qty.split());
  }
}

static inline clang::SourceLocation getStart(const clang::Stmt* s){
#if CLANG_VERSION_MAJOR >= 8
  return s->getBeginLoc();
#else
  return s->getLocStart();
#endif
}

static inline clang::SourceLocation getStart(const clang::Decl* decl){
#if CLANG_VERSION_MAJOR >= 8
  return decl->getBeginLoc();
#else
  return decl->getLocStart();
#endif
}

static inline clang::SourceLocation getEnd(const clang::Decl* decl){
#if CLANG_VERSION_MAJOR >= 8
  return decl->getEndLoc();
#else
  return decl->getLocEnd();
#endif
}

static inline clang::SourceLocation getEnd(const clang::Stmt* s){
#if CLANG_VERSION_MAJOR >= 8
  return s->getEndLoc();
#else
  return s->getLocEnd();
#endif
}

#endif // CLANGHEADERS_H
