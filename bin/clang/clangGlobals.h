/**
Copyright 2009-2019 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2019, NTESS

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

#ifndef sstmac_clang_globals_h
#define sstmac_clang_globals_h

#include <map>
#include <list>
#include "clangHeaders.h"

namespace clang {
  class CompilerInstance;
  class ASTContext;
  class SourceManager;
  class LangOptions;
  class Sema;
}

namespace modes {

enum ModeMask {
  ENCAPSULATE = 1 << 0,
  MEMOIZE = 1 << 1,
  SKELETONIZE = 1 << 2,
  SHADOWIZE = 1 << 3,
  PUPPETIZE = 1 << 4
};

enum Mode {
  ENCAPSULATE_MODE = 0,
  MEMOIZE_MODE = 1,
  SKELETONIZE_MODE = 2,
  SHADOWIZE_MODE = 3,
  PUPPETIZE_MODE = 4,
  NUM_MODES
};

}

class SkeletonASTVisitor;
class FirstPassASTVisitor;
struct SSTPragma;
class SSTReplacePragma;
class SSTNullVariablePragma;
struct PragmaConfig {
  std::map<clang::Decl*,SSTNullVariablePragma*> nullVariables;
  std::map<clang::FunctionDecl*,std::set<SSTPragma*>> functionPragmas;
  std::map<const clang::DeclContext*,SSTNullVariablePragma*> nullSafeFunctions;
  std::vector<std::pair<SSTPragma*, std::string>> globalCppFunctionsToWrite;
  std::set<std::string> newParams;
  std::string dependentScopeGlobal;
  std::string computeMemorySpec;
  std::list<clang::FunctionDecl*> fxnContexts;
  std::map<clang::IfStmt*,std::string> predicatedBlocks;
  std::list<clang::CompoundStmt*> stmtBlocks;
  union {
    SkeletonASTVisitor* skeleton;
    FirstPassASTVisitor* firstPass;
  } visitor;

  clang::CompoundStmt::body_iterator currentStmtBlockBegin(){
    return stmtBlocks.back()->body_begin();
  }

  clang::CompoundStmt::body_iterator findStmtBlockMatch(clang::Stmt* s){
    clang::CompoundStmt* block = stmtBlocks.back();
    for (auto* iter = block->body_begin(); iter != block->body_end(); ++iter){
      if (*iter == s) return iter;
    }
    return block->body_end();
  }

  clang::CompoundStmt::body_iterator currentStmtBlockEnd(){
    return stmtBlocks.back()->body_end();
  }

  bool makeNoChanges = false;

  PragmaConfig() = default;
};

struct CompilerGlobals {

  static modes::Mode mode;
  static int modeMask;
  static PragmaConfig pragmaConfig;
  static clang::Rewriter rewriter;

  static bool modeActive(int mask){
    return modeMask & mask;
  }

  static int getActiveMode(){
    return mode;
  }

  static llvm::cl::OptionCategory sstmacCategoryOpt;
  static llvm::cl::opt<bool> memoizeOpt;
  static llvm::cl::opt<bool> skeletonizeOpt;
  static llvm::cl::opt<bool> shadowizeOpt;
  static llvm::cl::opt<bool> puppetizeOpt;
  static llvm::cl::opt<bool> encapsulateOpt;
  static llvm::cl::opt<std::string> includeListOpt;
  static llvm::cl::opt<bool> verboseOpt;
  static llvm::cl::opt<bool> refactorMainOpt;
  static llvm::cl::opt<bool> noRefactorMainOpt;
  static clang::CompilerInstance* ci;
  static clang::CompilerInstance& CI() {
    return *ci;
  }
  static clang::SourceManager& SM() {
    return ci->getSourceManager();
  }
  static clang::ASTContext& ASTContext() {
    return ci->getASTContext();
  }
  static const clang::LangOptions& LangOpts() {
    return ci->getLangOpts();
  }
  static clang::Sema& Sema() {
    return ci->getSema();
  }

  static std::list<std::string> includePaths;

  static bool refactorMain;

  static void setup(clang::CompilerInstance* ci);
};

#endif
