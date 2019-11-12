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
#ifndef bin_clang_replacePragma_h
#define bin_clang_replacePragma_h

#include "pragmas.h"

class SSTReplacePragma : public SSTPragma {
 protected:
  std::string match_;
  std::string replacement_;
 public:
  SSTReplacePragma(clang::SourceLocation loc, clang::CompilerInstance& CI,
                   const std::list<clang::Token>& tokens);

  static std::string parse(clang::SourceLocation loc,
      clang::CompilerInstance& CI,
      const std::list<clang::Token>& tokens, std::ostream& os);

  const std::string& replacement() const {
    return replacement_;
  }

  const std::string& fxn() const {
    return match_;
  }

  std::list<const clang::Expr*> run(clang::Stmt* s);
  void run(clang::Stmt* s, clang::Rewriter& r);
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;
  void activate(clang::Decl *d, clang::Rewriter &r, PragmaConfig &cfg) override;
 private:
  void run(clang::Stmt* s, clang::Rewriter& rw, std::list<const clang::Expr*>& replaced);
  void activateFunctionDecl(clang::FunctionDecl* d, clang::Rewriter& r);
  void activateVarDecl(clang::VarDecl* d, clang::Rewriter& r);
  void activateCXXRecordDecl(clang::CXXRecordDecl* d, clang::Rewriter& r);
};

class SSTStartReplacePragma : public SSTReplacePragma {
 public:
  SSTStartReplacePragma(clang::SourceLocation loc, clang::CompilerInstance& CI,
                        const std::list<clang::Token>& tokens) :
    SSTReplacePragma(loc,CI,tokens){}

  void activate(clang::Rewriter&, PragmaConfig& cfg) override {
    cfg.replacePragmas[match_] = this;
  }
};

class SSTStopReplacePragma : public SSTPragma {
 public:
  SSTStopReplacePragma(const std::string& fxn) :
    fxn_(fxn){}

  void activate(clang::Stmt*  /*s*/, clang::Rewriter&  /*r*/, PragmaConfig& cfg) override {
    cfg.replacePragmas.erase(fxn_);
  }
 private:
  std::string fxn_;
};

class SSTInsteadPragma : public SSTPragma {
 public:
  SSTInsteadPragma(const std::string& repl) : repl_(repl) {}

  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

 private:
  std::string repl_;
};

class SSTInitPragma : public SSTPragma {
 public:
  SSTInitPragma(const std::string& repl) : init_(repl) {}

  const std::string& init() const {
    return init_;
  }

  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

 private:
  void activateDeclStmt(clang::DeclStmt* s, clang::Rewriter& r);
  void activateBinaryOperator(clang::BinaryOperator* op, clang::Rewriter& r);
  std::string init_;
};




#endif
