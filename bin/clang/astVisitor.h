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

class FirstPassASTVistor : public clang::RecursiveASTVisitor<FirstPassASTVistor>
{
 public:
  FirstPassASTVistor(SSTPragmaList& prgs, clang::Rewriter& rw,
                     PragmaConfig& cfg);

  bool VisitDecl(clang::Decl* d);
  bool VisitStmt(clang::Stmt* s);

 private:
  SSTPragmaList& pragmas_;
  PragmaConfig& pragmaConfig_;
  clang::Rewriter& rewriter_;
  bool noSkeletonize_;
};

/**
 * @brief The SkeletonASTVisitor class
 *
 * Go through every node in the abstract syntax tree and perform
 * certain skeletonization operations on them using the rewriter object.
 * For certain nodes, we Visit. This means performing a single visit
 * operation in isolation. We do not need parent or children nodes in a Vist.
 * For certain nodes, we must Traverse. The means performing a pre-visit operation
 * before visiting and child nodes. A post-visit operation is then performed
 * after visiting all child nodes. A traversal also gives the option
 * to cancel all visits to child nodes.
 */
class SkeletonASTVisitor : public clang::RecursiveASTVisitor<SkeletonASTVisitor> {
 public:
  SkeletonASTVisitor(clang::Rewriter &R,
                     GlobalVarNamespace& ns,
                     std::set<clang::Stmt*>& deld,
                     PragmaConfig& cfg) :
    rewriter_(R), visitingGlobal_(false), deletedStmts_(deld),
    globalNs_(ns), currentNs_(&ns),
    insideCxxMethod_(0),
    foundCMain_(false), keepGlobals_(false), noSkeletonize_(true),
    pragmaConfig_(cfg)
  {
    initHeaders();
    initReservedNames();
    initMPICalls();
    initConfig();
    pragmaConfig_.astVisitor = this;
  }

  bool isGlobal(const clang::DeclRefExpr* expr) const {
    return globals_.find(expr->getFoundDecl()) != globals_.end();
  }

  PragmaConfig& getPragmaConfig() {
    return pragmaConfig_;
  }

  std::string getGlobalReplacement(clang::NamedDecl* decl) const {
    auto iter = globals_.find(decl);
    if (iter == globals_.end()){
      errorAbort(decl->getLocStart(), *ci_,
                 "getting global replacement for non-global variable");
    }
    return iter->second;
  }

  bool noSkeletonize() const {
    return noSkeletonize_;
  }

  void setCompilerInstance(clang::CompilerInstance& c){
    ci_ = &c;
  }

  /**
   * @brief VisitStmt Activate any pragmas associated with this statement
   * This function is not called if a more specific matching function is found
   * @param S
   * @return
   */
  bool VisitStmt(clang::Stmt* S);

  /**
   * @brief VisitDecl Activate any pragmas associated with this declaration
   * This function is not called if a more specific matching function is found
   * @param D
   * @return
   */
  bool VisitDecl(clang::Decl* D);

  /**
   * @brief VisitDeclRefExpr Examine the usage of a variable to determine
   * if it is either a global variable or a pragma null_variable and therefore
   * requires a rewrite
   * @param expr
   * @return
   */
  bool VisitDeclRefExpr(clang::DeclRefExpr* expr);

  bool TraverseReturnStmt(clang::ReturnStmt* stmt, DataRecursionQueue* queue = nullptr);

  bool TraverseMemberExpr(clang::MemberExpr* expr, DataRecursionQueue* queue = nullptr);

  /**
   * @brief VisitCXXNewExpr Capture all usages of operator new. Rewrite all
   * operator new calls into SST-specific memory management functions.
   * @param expr
   * @return
   */
  bool VisitCXXNewExpr(clang::CXXNewExpr* expr);

  bool VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* expr);

  bool VisitArraySubscriptExpr(clang::ArraySubscriptExpr* expr);

  /**
   * @brief VisitCXXNewExpr Capture all usages of operator delete. Rewrite all
   * operator delete calls into SST-specific memory management functions.
   * @param expr
   * @return
   */
  bool TraverseCXXDeleteExpr(clang::CXXDeleteExpr* expr, DataRecursionQueue* = nullptr);

  /**
   * @brief VisitCXXMemberCallExpr Certain function calls get redirected to
   * calls on member functions of SST classes. See if this is one such instance.
   * Certain rewriting operations might be required. This most often occurs
   * for MPI calls to convert payloads into null buffers.
   * @param expr
   * @return
   */
  bool TraverseCXXMemberCallExpr(clang::CXXMemberCallExpr* expr, DataRecursionQueue* queue = nullptr);

  /**
   * @brief VisitUnaryOperator Currently no rewrite operations are performed.
   * Unary operators are not valid in certain global variables usages.
   * Validate that this unary operator is not a violation.
   * @param op
   * @return
   */
  bool VisitUnaryOperator(clang::UnaryOperator* op);

  /**
   * @brief VisitVarDecl We only need to visit variables once down the AST.
   *        No pre or post operations.
   * @param D
   * @return
   */
  bool visitVarDecl(clang::VarDecl* D);

  bool TraverseVarDecl(clang::VarDecl* D);

  /**
   * @brief Activate any pragmas associated with this.
   * In contrast to VisitStmt, call expressions can only have special replace pragmas.
   * We have to push this onto the contexts list in case we have any null variables
   * @param expr
   * @return
   */
  bool TraverseCallExpr(clang::CallExpr* expr, DataRecursionQueue* queue = nullptr);

  /**
   * @brief TraverseNamespaceDecl We have to traverse namespaces.
   *        We need pre and post operations. We have to explicitly recurse subnodes.
   * @param D
   * @return
   */
  bool TraverseNamespaceDecl(clang::NamespaceDecl* D);

  /**
   * @brief TraverseCXXRecordDecl We have to traverse record declarations.
   *        In the pre-visit, we have to push the Decl onto a class contexts list
   *        In the post-visit, we pop the Decl off the list
   * @param D
   * @return
   */
  bool TraverseCXXRecordDecl(clang::CXXRecordDecl* D);

  /**
   * @brief TraverseFunctionDecl  We have to traverse function declarations
   *        In the pre-visit, we have to push the Decl onto a function contexts list
   *        In the post-visit, we pop the Decl off the list
   * @param D
   * @return
   */
  bool TraverseFunctionDecl(clang::FunctionDecl* D);

  /**
   * @brief TraverseForStmt We have to traverse for loops.
   *        In the pre-visit, we have to push the ForStmt onto a context list
   *        In the post-visit, we have to pop the ForStmt off the list
   * @param S
   * @return
   */
  bool TraverseForStmt(clang::ForStmt* S, DataRecursionQueue* queue = nullptr);

  bool TraverseUnaryOperator(clang::UnaryOperator* op, DataRecursionQueue* queue = nullptr);

  bool TraverseBinaryOperator(clang::BinaryOperator* op, DataRecursionQueue* queue = nullptr);

  bool TraverseCompoundAssignOperator(clang::CompoundAssignOperator* op, DataRecursionQueue* queue = nullptr);

  bool TraverseIfStmt(clang::IfStmt* S, DataRecursionQueue* queue = nullptr);

  bool TraverseCompoundStmt(clang::CompoundStmt* S, DataRecursionQueue* queue = nullptr);

  bool TraverseDeclStmt(clang::DeclStmt* op, DataRecursionQueue* queue = nullptr);

#define OPERATOR(NAME) \
  bool TraverseBin##NAME(clang::BinaryOperator* op, DataRecursionQueue* queue = nullptr){ \
    return TraverseBinaryOperator(op,queue); \
  }
  BINOP_LIST()
#undef OPERATOR

#define OPERATOR(NAME) \
  bool TraverseUnary##NAME(clang::UnaryOperator* op, DataRecursionQueue* queue = nullptr){ \
    return TraverseUnaryOperator(op,queue); \
  }
  UNARYOP_LIST()
#undef OPERATOR

#define OPERATOR(NAME) \
  bool TraverseBin##NAME##Assign(clang::CompoundAssignOperator* op, DataRecursionQueue* queue = nullptr){ \
    return TraverseCompoundAssignOperator(op,queue); \
  }
  CAO_LIST()
#undef OPERATOR

  /**
   * @brief TraverseFunctionTemplateDecl We have to traverse template functions
   * No special pre or post visit is performed. However, there are certain SST/macro
   * template functions that should be skipped. If this is a SST/macro function template,
   * do not visit any of the child nodes.
   * @param D
   * @return
   */
  bool TraverseFunctionTemplateDecl(clang::FunctionTemplateDecl* D);

  /**
   * @brief TraverseCXXMethodDecl We have to traverse function declarations
   *        In the pre-visit, we have to push the Decl onto a function contexts list
   *        In the post-visit, we pop the Decl off the list
   * @param D
   * @return
   */
  bool TraverseCXXMethodDecl(clang::CXXMethodDecl *D);

  /**
   * @brief TraverseCXXConstructorDecl We have to traverse constructors
   *        If the constructor is actually a template instantation of a specific type,
   *        we should skip it and not visit any of the child nodes
   * @param D
   * @return
   */
  bool TraverseCXXConstructorDecl(clang::CXXConstructorDecl* D);

  /**
   * @brief dataTraverseStmtPost Call after the traversal of all statements.
   * Certain pragmas might have deactivate operations to be performed after traversing
   * the statement. If there are any activate pragmas, deactivate them now
   * @param S
   * @return
   */
  bool dataTraverseStmtPre(clang::Stmt* S);

  /**
   * @brief dataTraverseStmtPost Call after the traversal of all statements.
   * Certain pragmas might have deactivate operations to be performed after traversing
   * the statement. If there are any activate pragmas, deactivate them now
   * @param S
   * @return
   */
  bool dataTraverseStmtPost(clang::Stmt* S);

  SSTPragmaList& getPragmas(){
    return pragmas_;
  }

  void setVisitingGlobal(bool flag){
    visitingGlobal_ = flag;
  }

  void setTopLevelScope(clang::Decl* d){
    currentTopLevelScope_ = d;
  }

  clang::Decl* getTopLevelScope() const {
    return currentTopLevelScope_;
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
  clang::Decl* currentTopLevelScope_;
  clang::Rewriter& rewriter_;
  clang::CompilerInstance* ci_;
  SSTPragmaList pragmas_;
  bool visitingGlobal_;
  std::set<clang::FunctionDecl*> templateDefinitions_;
  std::set<clang::Stmt*>& deletedStmts_;
  GlobalVarNamespace& globalNs_;
  GlobalVarNamespace* currentNs_;
  std::map<const clang::NamedDecl*,std::string> globals_;
  std::set<std::string> globalsDeclared_;
  bool useAllHeaders_;
  int insideCxxMethod_;
  std::list<clang::FunctionDecl*> fxn_contexts_;
  std::list<clang::CXXRecordDecl*> class_contexts_;
  std::list<clang::ForStmt*> loop_contexts_;
  std::list<clang::Stmt*> stmt_contexts_;

  typedef enum { LHS, RHS } BinOpSide;
  std::list<BinOpSide> sides_;

  bool foundCMain_;
  std::string mainName_;
  bool keepGlobals_;
  bool noSkeletonize_;
  std::set<std::string> validHeaders_;
  std::set<std::string> reservedNames_;
  PragmaConfig& pragmaConfig_;
  std::map<clang::Stmt*,SSTPragma*> activePragmas_;
  std::set<clang::FunctionDecl*> keepWithNullArgs_;
  std::set<clang::FunctionDecl*> deleteWithNullArgs_;

  typedef void (SkeletonASTVisitor::*MPI_Call)(clang::CallExpr* expr);
  std::map<std::string, MPI_Call> mpiCalls_;

 private:
  bool isNullVariable(clang::Decl* d) const {
    return pragmaConfig_.nullVariables.find(d) != pragmaConfig_.nullVariables.end();
  }

  SSTNullVariablePragma* getNullVariable(clang::Decl* d) const {
    auto iter = pragmaConfig_.nullVariables.find(d);
    if (iter != pragmaConfig_.nullVariables.end()){
      return iter->second;
    }
    return nullptr;
  }

  void replaceGlobalUse(clang::NamedDecl* decl, clang::SourceRange rng);

  clang::Expr* getUnderlyingExpr(clang::Expr *e);

  void deleteNullVariableStmt(clang::Stmt* use_stmt, clang::Decl* d);
  bool activatePragmasForStmt(clang::Stmt* S);
  bool activatePragmasForDecl(clang::Decl* D);
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
   * @brief deleteStmt Delete a statement completely in the source-to-source
   * @param s
   */
  void deleteStmt(clang::Stmt* s);

  /**
   * @brief declareSSTExternVars
   * Declare all the external SST global variables we need to access
   * @param insertLoc The source location where the text should go
   */
  void declareSSTExternVars(clang::SourceLocation insertLoc);

  bool keepWithNullArgs(clang::FunctionDecl* decl) const {
    return keepWithNullArgs_.find(decl) != keepWithNullArgs_.end();
  }

  bool deleteWithNullArgs(clang::FunctionDecl* decl) const {
    return deleteWithNullArgs_.find(decl) != keepWithNullArgs_.end();
  }

};


#endif
