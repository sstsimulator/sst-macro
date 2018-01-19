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
 * operation in isolation. We do not need parent or children nodes in a Visit.
 * For certain nodes, we must Traverse. The means performing a pre-visit operation
 * before visiting child nodes. A post-visit operation is then performed
 * after visiting all child nodes. A traversal also gives the option
 * to cancel all visits to child nodes.
 */
class SkeletonASTVisitor : public clang::RecursiveASTVisitor<SkeletonASTVisitor> {
 private:
  struct AnonRecord {
    clang::RecordDecl* decl;
    std::string structType;  //union or struct
    std::string retType;
    bool isFxnStatic;
    //struct X_anonymous_type - gives unique typename to anonymous truct
    std::string typeName;
    AnonRecord() : decl(nullptr), isFxnStatic(false) {}
  };

  struct ArrayInfo {
    bool needsTypedef() const {
      return !typedefString.empty();
    }

    std::string typedefString;
    std::string typedefName;
    std::string retType;
    bool isFxnStatic;
    bool needsDeref;

    ArrayInfo() : isFxnStatic(false), needsDeref(true) {}
  };

 public:
  SkeletonASTVisitor(clang::Rewriter &R,
                     GlobalVarNamespace& ns,
                     std::set<clang::Stmt*>& deld,
                     PragmaConfig& cfg) :
    rewriter_(R), visitingGlobal_(false), deletedStmts_(deld),
    globalNs_(ns), currentNs_(&ns),
    insideCxxMethod_(0),
    foundCMain_(false), keepGlobals_(false), noSkeletonize_(true),
    pragmaConfig_(cfg),
    numRelocations_(0),
    reconstructCount_(0)
  {
    initHeaders();
    initReservedNames();
    initMPICalls();
    initConfig();
    pragmaConfig_.astVisitor = this;
  }

  bool isGlobal(const clang::DeclRefExpr* expr) const {
    return globals_.find(mainDecl(expr)) != globals_.end();
  }

  PragmaConfig& getPragmaConfig() {
    return pragmaConfig_;
  }

  std::string needGlobalReplacement(clang::NamedDecl* decl) {
    const clang::Decl* md = mainDecl(decl);
    globalsTouched_.back().insert(md);
    auto iter = globals_.find(md);
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

  bool VisitTypedefDecl(clang::TypedefDecl* D);

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
   * @brief VisitVarDecl We only need to visit variables once down the AST.
   *        No pre or post operations.
   * @param D
   * @return Whether to skip visiting this variable's initialization
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

  bool TraverseFieldDecl(clang::FieldDecl* fd, DataRecursionQueue* queue = nullptr);

  bool TraverseInitListExpr(clang::InitListExpr* expr, DataRecursionQueue* queue = nullptr);

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
  clang::NamespaceDecl* getOuterNamespace(clang::Decl* D);

  bool shouldVisitDecl(clang::VarDecl* D);

  void initHeaders();

  void initReservedNames();

  void initMPICalls();

  void initConfig();

  void replaceMain(clang::FunctionDecl* mainFxn);

 private:
  //whether we are allowed to use global variables in statements
  //or if they are disallowed because they would break deglobalizer
  clang::Decl* currentTopLevelScope_;
  clang::Rewriter& rewriter_;
  clang::CompilerInstance* ci_;
  std::map<clang::RecordDecl*,clang::TypedefDecl*> typedef_structs_;
  SSTPragmaList pragmas_;
  bool visitingGlobal_;
  std::set<clang::FunctionDecl*> templateDefinitions_;
  std::set<clang::Stmt*>& deletedStmts_;
  GlobalVarNamespace& globalNs_;
  GlobalVarNamespace* currentNs_;

  /** These should always index by the canonical decl */
  std::map<const clang::Decl*,std::string> globals_;
  std::map<const clang::Decl*,std::string> scopedNames_;

  static inline const clang::Decl* mainDecl(const clang::Decl* d){
    return d->getCanonicalDecl();
  }

  static inline const clang::Decl* mainDecl(const clang::DeclRefExpr* dr){
    return dr->getDecl()->getCanonicalDecl();
  }

  std::string activeGlobalScopedName_;

  bool activeGlobal() const {
    return !activeGlobalScopedName_.empty();
  }

  void clearActiveGlobal() {
    activeGlobalScopedName_.clear();
  }

  void setActiveGlobalScopedName(const std::string& str) {
    activeGlobalScopedName_ = str;
  }

  const std::string& activeGlobalScopedName() const {
    return activeGlobalScopedName_;
  }

  /**
   * @brief addRelocation
   * @param op  The unary operator creating the pointer
   * @param dr  The global variable being pointed to
   * @param member  Optionally a specific field of the global variable being pointed to
   */
  void addRelocation(clang::UnaryOperator* op, clang::DeclRefExpr* dr,
                     clang::ValueDecl* member = nullptr);

  std::set<std::string> globalsDeclared_;
  bool useAllHeaders_;
  int insideCxxMethod_;

  std::map<clang::FunctionDecl*, std::map<std::string, int>> static_fxn_var_counts_;
  std::list<clang::FunctionDecl*> fxn_contexts_;
  std::list<clang::CXXRecordDecl*> class_contexts_;
  std::list<clang::ForStmt*> loop_contexts_;
  std::list<clang::Stmt*> stmt_contexts_;

  std::list<clang::VarDecl*> activeDecls_;
  std::list<clang::Expr*> activeInits_;

  int numRelocations_;

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
  std::set<clang::DeclRefExpr*> alreadyReplaced_;

  struct GlobalStandin {
    bool fxnStatic;
    std::string replText;
    GlobalStandin() : fxnStatic(false){}
  };

  std::map<const clang::Decl*,GlobalStandin> globalStandins_;
  std::list<int> initIndices_;
  std::list<clang::FieldDecl*> activeFieldDecls_;
  std::list<std::set<const clang::Decl*>> globalsTouched_;

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

  void replaceGlobalUse(clang::DeclRefExpr* expr, clang::SourceRange rng);

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
  bool checkFileVar(const std::string& filePrefix, clang::VarDecl* D);
  /**
   * @brief getEndLoc
   * Find and return the position after the starting point where a statement ends
   * i.e. find the next semi-colon after the starting point
   * @param startLoc
   * @return
   */
  clang::SourceLocation getEndLoc(clang::SourceLocation startLoc);
  bool insideClass() const {
    return !class_contexts_.empty();
  }
  bool insideFxn() const {
    return !fxn_contexts_.empty();
  }

  typedef enum {
    Global, //regular global variable (C-style)
    FileStatic,
    CxxStatic, //c++ static class variable
    FxnStatic
  } GlobalVariable_t;

  /**
   * @brief replaceGlobalVar
   * @param varNameScopePrefix       A unique string needed for file-local or ns-local variable names
   * @param clsScope            A string identifying all nested class scopes, e.g. (A::B::)
   * @param externVarsInsertLoc The location in the file to declare extern vars/fxns from SST/macro.
   *                            May be invalid to indicate no insertion should be done.
   * @param varSizeOfInsertLoc  The location in the file to insert int size_X variable
   * @param offsetInsertLoc     The location to insert the 'extern int __offset_X' declaration
   *                            May be invalid to indicate insertion just at end of var declaration
   * @param insertOffsetAfter Whether to insert the offset declaration before/after the given location
   * @param D             The variable declaration being deglobalized
   * @param staticFxnInfo For static function variables, we will want any info on anonymous records
   *                      returned to us via this variable
   * @return  Whether to skip visiting this variable's initialization
   */
   bool setupGlobalVar(const std::string& varNameScopeprefix,
                      const std::string& clsScope,
                      clang::SourceLocation externVarsInsertLoc,
                      clang::SourceLocation varSizeOfInsertLoc,
                      clang::SourceLocation offsetInsertLoc,
                      bool insertOffsetAfter,
                      GlobalVariable_t global_var_ty,
                      clang::VarDecl* D,
                      clang::SourceLocation declEnd = clang::SourceLocation());

   /**
    * @brief checkAnonStruct See if the type of the variable is an
    *       an anonymous union or struct. Fill in info in rec if so.
    * @param D    The variable being visited
    * @param rec  In-out paramter, info struct to fill in
    * @return If D has anonymous struct type, return the passed-in rec struct
    *         If D is not an anonymous struct, return nullptrs
    */
   AnonRecord* checkAnonStruct(clang::VarDecl* D, AnonRecord* rec);

   clang::RecordDecl* checkCombinedStructVarDecl(clang::VarDecl* D);

   /**
    * @brief checkArray See if the type of the variable is an array
    *       and fill in the array info if so.
    * @param D    The variable being visited
    * @param info In-out parameter, array info to fill in
    * @return If D has array type, return the passed-in info struct.
    *         If D does not have array type, return nullptr
    */
   ArrayInfo* checkArray(clang::VarDecl* D, ArrayInfo* info);

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

  struct cArrayConfig {
    std::string fundamentalTypeString;
    clang::QualType fundamentalType;
    std::stringstream arrayIndices;
  };

  void traverseFunctionBody(clang::Stmt* s);

  void getArrayType(const clang::Type* ty, cArrayConfig& cfg);

  void setFundamentalTypes(clang::QualType qt, cArrayConfig& cfg);

  struct ReconstructedType {
    int typeIndex;
    std::list<clang::QualType> fundamentalFieldTypes;
    std::list<const clang::RecordDecl*> classFieldTypes;
    std::list<const clang::RecordDecl*> newClassFieldTypes;
    std::list<std::pair<std::string,std::string>> arrayTypes;
    std::set<const clang::RecordDecl*> structDependencies;
  };

  int reconstructCount_;

  void reconstructType(clang::SourceLocation typeDeclCutoff,
                      clang::QualType qt, ReconstructedType& rt,
                      std::map<const clang::RecordDecl*, ReconstructedType>& newTypes);

  void reconstructType(clang::SourceLocation typeDeclCutoff,
                      const clang::RecordDecl* rd, ReconstructedType& rt,
                      std::map<const clang::RecordDecl*, ReconstructedType>& newTypes);

  void addRecordField(clang::SourceLocation typeDeclCutoff,
                     const clang::RecordDecl* rd, ReconstructedType& rt,
                     std::map<const clang::RecordDecl*, ReconstructedType>& newTypes);

  void addReconstructionDependency(clang::SourceLocation typeDeclCutoff,
                                   const clang::Type* ty, ReconstructedType& rt);


  void addTypeReconstructionText(const clang::RecordDecl* rd, ReconstructedType& rt,
                                std::map<const clang::RecordDecl*, ReconstructedType>& newTypes,
                                std::set<const clang::RecordDecl*>& alreadyDone,
                                std::ostream& os);

  std::string getTypeNameForSizing(clang::SourceLocation typeDeclCutoff, clang::QualType qt,
                                  std::map<const clang::RecordDecl*, ReconstructedType>& newTypes);

  std::string getRecordTypeName(const clang::RecordDecl* rd);

};


#endif
