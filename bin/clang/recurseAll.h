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
#ifndef sstmac_bin_clang_RECURSEALL_H
#define sstmac_bin_clang_RECURSEALL_H

#include "clangHeaders.h"
#include <utility>



enum class ExprRole {
  ArrayBase,
  ArrayIdx,
  CallFxn,
  CallArg,
  BinOpLHS,
  BinOpRHS,
  ForInit,
  ForCond,
  ForInc,
  ForBody,
  SubExpr,
  IfCond,
  IfBody,
  IfElse,
  IfInit,
  Standalone,
  ThisPtr,
  ReturnValue
};

static const char* tostr(ExprRole role)
{
#define enum_case(x) case ExprRole::x: return #x
  switch(role){
  enum_case(ArrayBase);
  enum_case(ArrayIdx);
  enum_case(CallFxn);
  enum_case(CallArg);
  enum_case(BinOpLHS);
  enum_case(BinOpRHS);
  enum_case(ForInit);
  enum_case(ForCond);
  enum_case(ForInc);
  enum_case(ForBody);
  enum_case(SubExpr);
  enum_case(IfCond);
  enum_case(IfBody);
  enum_case(IfElse);
  enum_case(IfInit);
  enum_case(Standalone);
  enum_case(ThisPtr);
  enum_case(ReturnValue);
  }
#undef enum_case
}

namespace pvt {

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

template <class PreVisit, class PostVisit, class... Args>
struct RecurseAll {

  void recurse(const Stmt* stmt, ExprRole role, Args&& ...args){
    #define recurse_case(type,stmt,role,...) \
      case(clang::Stmt::type##Class): { \
      auto ptr = clang::cast<type>(stmt); \
      bool stop = PreVisit()(ptr,role,std::forward<Args>(args)...); \
      if (!stop){ \
       recurse##type(clang::cast<type>(stmt),role,__VA_ARGS__); \
       PostVisit()(ptr,role,std::forward<Args>(args)...); \
      }; \
      break; \
    }
    //std::cout << "Recursing " << stmt->getStmtClassName() << " " << stmt << std::endl;
   switch(stmt->getStmtClass()){
    recurse_case(ForStmt,stmt,role,std::forward<Args>(args)...)
    recurse_case(CompoundStmt,stmt,role,std::forward<Args>(args)...)
    recurse_case(IfStmt,stmt,role,std::forward<Args>(args)...)
    recurse_case(BinaryOperator,stmt,role,std::forward<Args>(args)...)
    recurse_case(ParenExpr,stmt,role,std::forward<Args>(args)...)
    recurse_case(CallExpr,stmt,role,std::forward<Args>(args)...)
    recurse_case(CXXMemberCallExpr,stmt,role,std::forward<Args>(args)...)
    recurse_case(CXXOperatorCallExpr,stmt,role,std::forward<Args>(args)...)
    recurse_case(MemberExpr,stmt,role,std::forward<Args>(args)...)
    recurse_case(DeclRefExpr,stmt,role,std::forward<Args>(args)...)
    recurse_case(ImplicitCastExpr,stmt,role,std::forward<Args>(args)...)
    recurse_case(CStyleCastExpr,stmt,role,std::forward<Args>(args)...)
    recurse_case(CXXFunctionalCastExpr,stmt,role,std::forward<Args>(args)...);
    recurse_case(ArraySubscriptExpr,stmt,role,std::forward<Args>(args)...)
    recurse_case(DeclStmt,stmt,role,std::forward<Args>(args)...)
    recurse_case(ReturnStmt,stmt,role,std::forward<Args>(args)...)
    default:
      break;
   }
#undef recurse_case
  }

  void recurseReturnStmt(const ReturnStmt* stmt, ExprRole role, Args&& ...args){
    recurse(stmt->getRetValue(), ExprRole::ReturnValue, std::forward<Args>(args)...);
  }

  void recurseForStmt(const ForStmt* stmt, ExprRole role, Args&& ...args){
    recurse(stmt->getInit(), ExprRole::ForInit, std::forward<Args>(args)...);
    recurse(stmt->getCond(), ExprRole::ForCond, std::forward<Args>(args)...);
    recurse(stmt->getInc(), ExprRole::ForInc, std::forward<Args>(args)...);
    recurse(stmt->getBody(), ExprRole::ForBody, std::forward<Args>(args)...);
  }

  void recurseCompoundStmt(const CompoundStmt* stmt, ExprRole role, Args&& ...args){
    auto end = stmt->child_end();
    for (auto iter=stmt->child_begin(); iter != end; ++iter){
      recurse(*iter, ExprRole::Standalone, std::forward<Args>(args)...);
    }
  }

  void recurseIfStmt(const IfStmt* stmt, ExprRole role, Args&& ...args){
    recurse(stmt->getCond(), ExprRole::IfCond, std::forward<Args>(args)...);
    recurse(stmt->getThen(), ExprRole::IfBody, std::forward<Args>(args)...);
    if (stmt->getElse()) recurse(stmt->getElse(), ExprRole::IfElse, std::forward<Args>(args)...);
    if (stmt->getInit()) recurse(stmt->getInit(), ExprRole::IfInit, std::forward<Args>(args)...);
  }

  void recurseDeclStmt(const DeclStmt* stmt, ExprRole role, Args&& ...args){
    const Decl* D = stmt->getSingleDecl();
    if (D){
      switch(D->getKind()){
        case Decl::Var: {
          const VarDecl* vd = cast<const VarDecl>(D);
          if (vd->hasInit()){
            recurse(vd->getInit(), ExprRole::Standalone,
                    std::forward<Args>(args)...);
          }
          break;
        }
        default:
         break;
      }
    }
  }

  void recurseBinaryOperator(const BinaryOperator* op, ExprRole role, Args&& ...args){
    recurse(op->getLHS(), ExprRole::BinOpLHS, std::forward<Args>(args)...);
    recurse(op->getRHS(), ExprRole::BinOpRHS, std::forward<Args>(args)...);
  }

  void recurseParenExpr(const ParenExpr* expr, ExprRole role, Args&& ...args){
    //parentheticalness doesn't change the "role" of the variable
    recurse(expr->getSubExpr(), role, std::forward<Args>(args)...);
  }

  void recurseCallExpr(const CallExpr* expr, ExprRole role, Args&& ...args){
    recurse(expr->getCallee(), ExprRole::CallFxn, std::forward<Args>(args)...);
    for (int i=0; i < expr->getNumArgs(); ++i){
      recurse(expr->getArg(i), ExprRole::CallArg, std::forward<Args>(args)...);
    }
  }

  void recurseCXXFunctionalCastExpr(const CXXFunctionalCastExpr* expr, ExprRole role, Args&& ...args){
    recurse(expr->getSubExpr(), role, std::forward<Args>(args)...);
  }

  void recurseCXXMemberCallExpr(const CXXMemberCallExpr* expr, ExprRole role, Args&& ...args){
    //recurse(expr->getImplicitObjectArgument(), ExprRole::ThisPtr, std::forward<Args>(args)...);
    recurse(expr->getCallee(), ExprRole::CallFxn, std::forward<Args>(args)...);
    for (int i=0; i < expr->getNumArgs(); ++i){
      recurse(expr->getArg(i), ExprRole::CallArg, std::forward<Args>(args)...);
    }
  }

  void recurseCXXOperatorCallExpr(const CXXOperatorCallExpr* expr, ExprRole role, Args&& ...args){
    //recurse(expr->getImplicitObjectArgument(), ExprRole::ThisPtr, std::forward<Args>(args)...);
    recurse(expr->getCallee(), ExprRole::CallFxn, std::forward<Args>(args)...);
    for (int i=0; i < expr->getNumArgs(); ++i){
      recurse(expr->getArg(i), ExprRole::CallArg, std::forward<Args>(args)...);
    }
  }


  void recurseMemberExpr(const MemberExpr* expr, ExprRole role, Args&& ...args){
    recurse(expr->getBase(), ExprRole::ThisPtr, std::forward<Args>(args)...);
  }

  void recurseDeclRefExpr(const DeclRefExpr* expr, ExprRole role, Args&& ...args){
    //nothing to do - there are no sub expressions
  }

  void recurseImplicitCastExpr(const ImplicitCastExpr* expr, ExprRole role, Args&& ...args){
    //implicit cast doesn't change "the role" of the variable
    recurse(expr->getSubExpr(), role, std::forward<Args>(args)...);
  }

  void recurseCStyleCastExpr(const CStyleCastExpr* expr, ExprRole role, Args&& ...args){
    //implicit cast doesn't change "the role" of the variable
    recurse(expr->getSubExpr(), role, std::forward<Args>(args)...);
  }

  void recurseArraySubscriptExpr(const ArraySubscriptExpr* expr, ExprRole role, Args&& ...args){
    recurse(expr->getBase(), ExprRole::ArrayBase, std::forward<Args>(args)...);
    recurse(expr->getIdx(), ExprRole::ArrayIdx, std::forward<Args>(args)...);
  }
};

}

struct NullVisit
{
  template <class T, class... Args>
  bool operator()(T* t, ExprRole role, Args&& ...args){ return false; }
};

template <class PreVisit, class PostVisit, class... Args>
void recurseAll(clang::Stmt* stmt, Args&& ...args){
  pvt::RecurseAll<PreVisit,PostVisit,Args...>().recurse(stmt, ExprRole::Standalone,
                                         std::forward<Args>(args)...);
}

#endif // RECURSEALL_H
