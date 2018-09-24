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
#ifndef bin_clang_compute_visitor_h
#define bin_clang_compute_visitor_h

#include "clangHeaders.h"
#include "computeLoops.h"
#include "replacements.h"
#include "dataFlow.h"
#include "pragmas.h"

class ComputeVisitor  {
 public:
  //97 = 'a', for debug printing
  ComputeVisitor(clang::CompilerInstance& c, SSTPragmaList& plist, ComputeVisitor* par,
                 SkeletonASTVisitor* ctxt) :
    CI(c), idCount(97), currentGeneration(1), pragmas(plist), parent(par),
    context(ctxt)
  {}

  void replaceStmt(clang::Stmt* stmt, clang::Rewriter& r, Loop& loop, PragmaConfig& cfg,
                   const std::string& nthread);

  void setContext(clang::Stmt* stmt);

  void visitLoop(clang::ForStmt* stmt, Loop& loop);

  void addOperations(clang::Stmt* stmt, Loop::Body& body, bool isLHS = false);

 private:
  struct Variable {
    uint16_t id;
    uint32_t generation;
  };

  uint32_t idCount; //0 is sentinel for not inited
  uint32_t currentGeneration; //0 is sentinel for not inited
  std::map<MemoryLocation,AccessHistory,MemoryLocationCompare> arrays;
  std::map<clang::NamedDecl*,Variable> variables;
  //alloy any statement as key to cover whiles and fors
  std::map<clang::Stmt*,std::string> explicitLoopCounts_;
  clang::CompilerInstance& CI;
  SSTPragmaList& pragmas;
  Replacements repls;
  clang::SourceLocation scopeStartLine;
  ComputeVisitor* parent;
  SkeletonASTVisitor* context;

  Variable& getVariable(clang::NamedDecl* decl){
    Variable& var = variables[decl];
    if (var.id == 0){
      var.id = idCount++;
    }
    return var;
  }

  void visitAccessDeclRefExpr(clang::DeclRefExpr* expr, MemoryLocation& mloc);

  void visitAccessCompoundStmt(clang::CompoundStmt* stmt, Loop::Body& body, MemoryLocation& mloc);

  void visitAccessBinaryOperator(clang::BinaryOperator* op, Loop::Body& body, MemoryLocation& mloc);

  void visitAccessParenExpr(clang::ParenExpr* expr, Loop::Body& body, MemoryLocation& mloc);

  void visitAccessImplicitCastExpr(clang::ImplicitCastExpr* expr, Loop::Body& body, MemoryLocation& mloc);

  void visitAccessCStyleCastExpr(clang::CStyleCastExpr* expr, Loop::Body& body, MemoryLocation& mloc);

  void visitAccessArraySubscriptExpr(clang::ArraySubscriptExpr* expr, Loop::Body& body, MemoryLocation& mloc,
                              bool updateDependence = true);

  void checkMemoryAccess(clang::Stmt* stmt, Loop::Body& body, MemoryLocation& mloc);

  void visitBodyForStmt(clang::ForStmt* stmt, Loop::Body& body);

  void visitBodyCompoundStmt(clang::CompoundStmt* stmt, Loop::Body& body);

  void visitBodyIfStmt(clang::IfStmt* stmt, Loop::Body& body);

  void visitBodySwitchStmt(clang::SwitchStmt* stmt, Loop::Body& body);

  void visitBodyCompoundAssignOperator(clang::CompoundAssignOperator* op, Loop::Body& body);

  void visitBodyBinaryOperator(clang::BinaryOperator* op, Loop::Body& body);

  void visitBodyUnaryOperator(clang::UnaryOperator* op, Loop::Body& body);

  void visitBodyParenExpr(clang::ParenExpr* expr, Loop::Body& body);

  void visitBodyArraySubscriptExpr(clang::ArraySubscriptExpr* expr, Loop::Body& body, bool isLHS);

  void visitBodyDeclRefExpr(clang::DeclRefExpr* expr, Loop::Body& body, bool isLHS);

  void visitBodyImplicitCastExpr(clang::ImplicitCastExpr* expr, Loop::Body& body, bool isLHS);

  void visitBodyCStyleCastExpr(clang::CStyleCastExpr* expr, Loop::Body& body, bool isLHS);

  void visitBodyCallExpr(clang::CallExpr* expr, Loop::Body& body);

  void visitBodyDeclStmt(clang::DeclStmt* stmt, Loop::Body& body);

  void visitBodyWhileStmt(clang::WhileStmt* stmt, Loop::Body& body);

  void checkStmtPragmas(clang::Stmt* s);

  void visitStrideCompoundStmt(clang::CompoundStmt* stmt, ForLoopSpec* spec);

  void visitStrideCompoundAssignOperator(clang::CompoundAssignOperator* op, ForLoopSpec* spec);

  void visitStrideUnaryOperator(clang::UnaryOperator* op, ForLoopSpec* spec);

  void getStride(clang::Stmt* stmt, ForLoopSpec* spec);

  void validateLoopControlExpr(clang::Expr* rhs);

  void visitPredicateBinaryOperator(clang::BinaryOperator* op, ForLoopSpec* spec);

  void getPredicateVariables(clang::Expr* expr, ForLoopSpec* spec);

  void visitInitialDeclStmt(clang::DeclStmt* stmt, ForLoopSpec* spec);

  void visitInitialBinaryOperator(clang::BinaryOperator* op, ForLoopSpec* spec);

  void getInitialVariables(clang::Stmt* stmt, ForLoopSpec* spec);

  /**
   * @brief checkPredicateMax
   * @param max
   * @param os
   * @return true if a special replacement was printed to os, false otherwise
   */
  bool checkPredicateMax(clang::Expr* max, std::ostream& os);

  std::string getTripCount(ForLoopSpec* spec);

  void addLoopContribution(std::ostream& os, Loop& loop);

};

#endif
