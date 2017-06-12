/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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

#ifndef bin_clang_replAstVisitor_h
#define bin_clang_replAstVisitor_h

#include "clangHeaders.h"
#include "pragmas.h"
#include "globalVarNamespace.h"

#define visitFxn(cls) \
  bool Visit##cls(clang::cls* c){ return TestStmtMacro(c); }

class ReplGlobalASTVisitor : public clang::RecursiveASTVisitor<ReplGlobalASTVisitor> {
 public:
  ReplGlobalASTVisitor(clang::Rewriter &R,
                       GlobalVarNamespace& ns,
                       std::set<clang::Expr*>& deld) :
    rewriter_(R), visitingGlobal_(false), deletedExprs_(deld),
    globalNs_(ns), currentNs_(&ns),
    insideCxxMethod_(0),
    foundCMain_(false), keepGlobals_(false), noSkeletonize_(true)
  {
    initHeaders();
    initReservedNames();
    initMPICalls();
    initConfig();
  }

  bool VisitStmt(clang::Stmt* S);

  bool VisitDecl(clang::Decl* D);

  bool VisitDeclRefExpr(clang::DeclRefExpr* expr);

  bool VisitCXXNewExpr(clang::CXXNewExpr* expr);

  bool VisitCXXDeleteExpr(clang::CXXDeleteExpr* expr);

  bool VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* expr);

  bool VisitUnaryOperator(clang::UnaryOperator* op);

  bool VisitCallExpr(clang::CallExpr* expr);

  void setCompilerInstance(clang::CompilerInstance& c){
    ci_ = &c;
  }

  /**
   * @brief VisitVarDecl We only need to visit variables once down the AST.
   *        No pre or post operations.
   * @param D
   * @return
   */
  bool VisitVarDecl(clang::VarDecl* D);

  bool VisitDeclStmt(clang::DeclStmt* S);

  /**
   * @brief TraverseNamespaceDecl We have to traverse namespaces.
   *        We need pre and post operations. We have to explicitly recurse subnodes.
   * @param D
   * @return
   */
  bool TraverseNamespaceDecl(clang::NamespaceDecl* D);

  bool TraverseCXXRecordDecl(clang::CXXRecordDecl* D);

  bool TraverseFunctionDecl(clang::FunctionDecl* D);

  bool TraverseFunctionTemplateDecl(clang::FunctionTemplateDecl* D);

  bool TraverseCXXMethodDecl(clang::CXXMethodDecl *D);

  bool TraverseCXXConstructorDecl(clang::CXXConstructorDecl* D);

  bool dataTraverseStmtPre(clang::Stmt* S);

  bool dataTraverseStmtPost(clang::Stmt* S);

  void replaceGlobalUse(clang::NamedDecl* decl, clang::SourceRange rng);

  bool isGlobal(clang::DeclRefExpr* expr){
    return globals_.find(expr->getFoundDecl()) != globals_.end();
  }

  SSTPragmaList& getPragmas(){
    return pragmas_;
  }

  const std::map<clang::NamedDecl*,std::string>&
  globalVariables() const {
    return globals_;

  }

  void setVisitingGlobal(bool flag){
    visitingGlobal_ = flag;
  }

  bool hasCStyleMain() const {
    return foundCMain_;
  }

  const std::string& getAppName() const {
    return mainName_;
  }

 private:
  bool shouldVisitDecl(clang::VarDecl* D);

  void initHeaders();

  void initReservedNames();

  void initMPICalls();

  void initConfig();

  void replaceMain(clang::FunctionDecl* mainFxn);

 private:
  clang::Rewriter& rewriter_;
  clang::CompilerInstance* ci_;
  SSTPragmaList pragmas_;
  bool visitingGlobal_;
  std::set<clang::Expr*>& deletedExprs_;
  GlobalVarNamespace& globalNs_;
  GlobalVarNamespace* currentNs_;
  std::map<clang::NamedDecl*,std::string> globals_;
  std::set<std::string> globalsDeclared_;
  bool useAllHeaders_;
  int insideCxxMethod_;
  std::list<clang::FunctionDecl*> fxn_contexts_;
  std::list<clang::CXXRecordDecl*> class_contexts_;
  bool foundCMain_;
  std::string mainName_;
  bool keepGlobals_;
  bool noSkeletonize_;
  std::set<std::string> validHeaders_;
  std::set<std::string> reservedNames_;
  PragmaConfig pragma_config_;
  std::map<clang::Stmt*,SSTPragma*> activePragmas_;

  typedef void (ReplGlobalASTVisitor::*MPI_Call)(clang::CallExpr* expr);
  std::map<std::string, MPI_Call> mpiCalls_;

 private:
  void visitCollective(clang::CallExpr* expr);
  void visitReduce(clang::CallExpr* expr);
  void visitPt2Pt(clang::CallExpr* expr);
  bool checkDeclStaticClassVar(clang::VarDecl* D);
  bool checkInstanceStaticClassVar(clang::VarDecl* D);
  bool checkStaticFxnVar(clang::VarDecl* D);
  bool checkGlobalVar(clang::VarDecl* D);
  bool checkStaticFileVar(clang::VarDecl* D);
  clang::SourceLocation getEndLoc(const clang::VarDecl* D);
  bool insideClass() const {
    return !class_contexts_.empty();
  }
  bool insideFxn() const {
    return !fxn_contexts_.empty();
  }

  typedef enum {
    Extern, //regular global variable (C-style)
    CxxStatic, //c++ static class variable
  } GlobalRedirect_t;

  /**
   * @brief replaceGlobalVar
   * @param scope_prefix       A unique string needed for file-local or ns-local variables
   * @param init_scope_prefix  A unique string needed when the initializers move out of scope
   *              (as in static function or class vars) to avoid name conflicts on the initializer
   * @param externVarsInsertLoc The location in the file to declare extern vars from SST/macro.
   *                            May be invalid to indicate no insertion should be done.
   * @param getRefInsertLoc     The location in the file to insert symbol redirect function get_X()
   * @param D             The variable declaration being deglobalized
   * @return  Standard clang continue return
   */
  bool setupGlobalVar(const std::string& scope_prefix,
                      const std::string& init_scope_prefix,
                      clang::SourceLocation externVarsInsertLoc,
                      clang::SourceLocation getRefInsertLoc,
                      GlobalRedirect_t red_ty,
                      clang::VarDecl* D);

  /**
   * @brief defineGlobalVarInitializers
   *        Dump the text needed to define new global variables for
   *        initializing thread-local variables
   * @param scope_unique_var_name A modified variable name unique to its scope
   * @param var_name    The name of the variable in original source code
   * @param init_prefix A prefix required
   * @param os  A stream to dump output to
   */
  void defineGlobalVarInitializers(
     const std::string& scope_unique_var_name,
     const std::string& var_name,
     const std::string& init_scope_prefix,
     llvm::raw_string_ostream& os);

  /**
   * @brief declareStaticInitializers
   *        Similar to #defineGlobalVarInitializers, but we are in class declaration
   *        and unable to define the symbols. Just declare static variables to be
   *        defined later.
   * @param scope_unique_var_name
   * @param os
   */
  void declareStaticInitializers(
     const std::string& scope_unique_var_name,
     llvm::raw_string_ostream& os);

  /**
   * @brief declareSSTExternVars
   * Declare all the external SST global variables we need to access
   * @param insertLoc The source location where the text should go
   */
  void declareSSTExternVars(clang::SourceLocation insertLoc);
};


#endif
