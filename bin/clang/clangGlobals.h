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

/**
 * @brief The ASTContextLists struct
 * This contains data structures that keep track of the enclosing context
 * of the currently being visited statement
 */
struct ASTContextLists {
  std::list<clang::FunctionDecl*> enclosingFunctionDecls;
  std::list<clang::CompoundStmt*> compoundStmtBlocks;
  clang::CompoundStmt::body_iterator currentStmtBlockBegin(){
    return compoundStmtBlocks.back()->body_begin();
  }

  clang::CompoundStmt::body_iterator findStmtBlockMatch(clang::Stmt* s){
    clang::CompoundStmt* block = compoundStmtBlocks.back();
    for (auto* iter = block->body_begin(); iter != block->body_end(); ++iter){
      if (*iter == s) return iter;
    }
    return block->body_end();
  }

  clang::CompoundStmt::body_iterator currentStmtBlockEnd(){
    return compoundStmtBlocks.back()->body_end();
  }
};

/**
 * @brief The ASTNodeMetadata struct
 * This contains data structures that keep track of specifing annotations or markings
 * on specific AST nodes that are required for correct mutations or rewrites later
 */
struct ASTNodeMetadata {
  std::map<clang::Decl*,SSTNullVariablePragma*> nullVariables;
  std::map<const clang::DeclContext*,SSTNullVariablePragma*> nullSafeFunctions;
  std::map<clang::Stmt*, std::string> computeMemoryOverrides;
  std::map<clang::IfStmt*,std::string> predicatedBlocks;
  std::string dependentScopeGlobal;
};

/**
 * @brief The ToolInfoRegistration struct
 * This contains data structures that keep track of things that must be registered
 * in a separate .cpp file and linked in. These are usually things like global variable
 * registrations with the simulator.
 */
struct ToolInfoRegistration {
  std::vector<std::pair<SSTPragma*, std::string>> globalCppFunctionsToWrite;
  std::set<std::string> extraInputFileParams;
};

struct PragmaConfig {
  bool makeNoChanges = false;

  PragmaConfig() = default;
};

struct CompilerGlobals {

  static modes::Mode mode;
  static int modeMask;
  static PragmaConfig pragmaConfig;
  static ASTContextLists astContextLists;
  static ASTNodeMetadata astNodeMetadata;
  static ToolInfoRegistration toolInfoRegistration;
  static clang::Rewriter rewriter;

  static bool modeActive(int mask){
    return modeMask & mask;
  }

  static int getActiveMode(){
    return mode;
  }

  static union {
    SkeletonASTVisitor* skeleton;
    FirstPassASTVisitor* firstPass;
  } visitor;

  static void setVisitor(SkeletonASTVisitor& v){
    visitor.skeleton = &v;
  }

  static void setVisitor(FirstPassASTVisitor& v){
    visitor.firstPass = &v;
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

  /**
   This contains the list of system include root paths.
   This stores "real" paths, which are absolute paths after following all symlinks
  */
  static std::vector<std::string> realSystemIncludePaths;

  static bool refactorMain;

  static void setup(clang::CompilerInstance* ci);
};

#endif
