/**
Copyright 2009-2022 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2022, NTESS

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
#ifndef bin_clang_computePragma_h
#define bin_clang_computePragma_h

#include "pragmas.h"

class SSTComputePragma : public SSTPragma {
  friend class ComputeVisitor;
 public:
  SSTComputePragma(){}

  static void replaceForStmt(clang::ForStmt* stmt, const std::string& nthread);

 private:
  void activate(clang::Stmt *stmt) override;
  void activate(clang::Decl* decl) override;
  void defaultAct(clang::Stmt* stmt);
  void visitForStmt(clang::ForStmt* stmt);
  void visitCXXMethodDecl(clang::CXXMethodDecl* decl);
  void visitFunctionDecl(clang::FunctionDecl* decl);
  void visitIfStmt(clang::IfStmt* stmt);
  void visitAndReplaceStmt(clang::Stmt* stmt);

  std::string nthread_;

 protected:
  SSTComputePragma(const std::string& nthread) :
    nthread_(nthread){}


};

class SSTAlwaysComputePragma : public SSTComputePragma
{
 public:
  SSTAlwaysComputePragma() : SSTComputePragma() {}
};

class SSTMemoryPragma : public SSTPragma {
 public:
  SSTMemoryPragma(const std::string& memSpec) :
    memSpec_(memSpec){}
 private:
  void activate(clang::Stmt *s) override;
  std::string memSpec_;
};

class SSTLoopCountPragma : public SSTPragma {
 public:
  SSTLoopCountPragma(clang::SourceLocation loc, const std::list<clang::Token>& tokens);

  const std::string& count() const {
    return loopCount_;
  }

  void activate(clang::Stmt *s) override;

  bool firstPass() const override {
    return true;
  }

 private:
  void transformWhileLoop(clang::Stmt* s);
  void transformForLoop(clang::Stmt* s);
  clang::Token loopCountToken_;
  std::string loopCount_;
};

class SSTOpenMPParallelPragma : public SSTComputePragma
{
 public:
  SSTOpenMPParallelPragma(clang::SourceLocation loc,
                         const std::list<clang::Token>& tokens);
 private:
  std::string numThreads(clang::SourceLocation loc,
                         const std::list<clang::Token>& tokens);
};





#endif
