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

#ifndef bin_clang_replAstVisitor_h
#define bin_clang_replAstVisitor_h

#include "clangHeaders.h"
#include "pragmas.h"
#include "globalVarNamespace.h"

#include <unordered_set>

#define visitFxn(cls) \
  bool Visit##cls(clang::cls* c){ return TestStmtMacro(c); }

struct StmtDeleteException : public std::runtime_error
{
  StmtDeleteException(clang::Stmt* deld) :
    std::runtime_error("deleted expression"), deleted (deld){
  }
  clang::Stmt* deleted;
};

struct DeclDeleteException : public std::runtime_error
{
  DeclDeleteException(clang::Decl* deld) :
    std::runtime_error("deleted expression"), deleted (deld){
  }
  clang::Decl* deleted;
};

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
  friend class SkeletonASTConsumer;

 private:
  struct AnonRecord {
    clang::RecordDecl* decl;
    bool typeNameAdded;
    std::string structType;  //union or struct
    std::string retType;
    bool isFxnStatic;
    //struct X_anonymous_type - gives unique typename to anonymous truct
    std::string typeName;
    AnonRecord() : decl(nullptr), isFxnStatic(false), typeNameAdded(false) {}
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

  static constexpr int IndexResetter = -1;

  static bool indexIsSet(int idx){
    return idx != IndexResetter;
  }

 public:
  SkeletonASTVisitor(clang::Rewriter &R,
                     GlobalVarNamespace& ns,
                     std::set<clang::Stmt*>& deld,
                     PragmaConfig& cfg) :
    rewriter_(R), visitingGlobal_(false), deletedStmts_(deld),
    globalNs_(ns), currentNs_(&ns),
    insideCxxMethod_(0), activeBinOpIdx_(-1),
    foundCMain_(false), keepGlobals_(false), noSkeletonize_(true),
    refactorMain_(true),
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

  void registerNewKeywords(std::ostream& os);

  PragmaConfig& getPragmaConfig() {
    return pragmaConfig_;
  }

  clang::CompilerInstance& getCompilerInstance() {
    return *ci_;
  }

  bool isCxx() const {
    return ci_->getLangOpts().CPlusPlus;
  }

  std::string needGlobalReplacement(clang::NamedDecl* decl) {
    const clang::Decl* md = mainDecl(decl);
    if (globalsTouched_.empty()){
      errorAbort(decl->getLocStart(), *ci_,
                 "internal error: globals touched array is empty");
    }
    globalsTouched_.back().insert(md);
    auto iter = globals_.find(md);
    if (iter == globals_.end()){
      errorAbort(decl->getLocStart(), *ci_,
                 "getting global replacement for non-global variable");
    }
    return iter->second.reusableText;
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
   * @brief TravsersetDecl Activate any pragmas associated with this declaration
   * This function is not called if a more specific matching function is found
   * @param D
   * @return
   */
  bool TraverseDecl(clang::Decl* D);

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

  bool TraverseArraySubscriptExpr(clang::ArraySubscriptExpr* expr, DataRecursionQueue* = nullptr);

  /**
   * @brief VisitCXXNewExpr Capture all usages of operator delete. Rewrite all
   * operator delete calls into SST-specific memory management functions.
   * @param expr
   * @return
   */
  bool TraverseCXXDeleteExpr(clang::CXXDeleteExpr* expr, DataRecursionQueue* = nullptr);

  bool TraverseLambdaExpr(clang::LambdaExpr* expr);

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

  bool TraverseUnresolvedLookupExpr(clang::UnresolvedLookupExpr* expr,
                                    DataRecursionQueue* queue = nullptr);

  bool VisitCXXDependentScopeMemberExpr(clang::CXXDependentScopeMemberExpr* expr);

  bool VisitDependentScopeDeclRefExpr(clang::DependentScopeDeclRefExpr* expr);

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

  bool TraverseDoStmt(clang::DoStmt* S, DataRecursionQueue* queue = nullptr);

  bool TraverseDecltypeTypeLoc(clang::DecltypeTypeLoc loc);

  bool TraverseWhileStmt(clang::WhileStmt* S, DataRecursionQueue* queue = nullptr);

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

  clang::SourceLocation getVariableNameLocationEnd(clang::VarDecl* D);

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

  void appendComputeLoop(clang::ForStmt* stmt){
    computeLoops_.push_back(stmt);
  }

  void popComputeLoop(){
    computeLoops_.pop_back();
  }

 private:
  clang::NamespaceDecl* getOuterNamespace(clang::Decl* D);

  void getTemplatePrefixString(std::ostream& os, clang::TemplateParameterList* theList);

  void getTemplateParamsString(std::ostream& os, clang::TemplateParameterList* theList);

  bool shouldVisitDecl(clang::VarDecl* D);

  void initHeaders();

  void initReservedNames();

  void initMPICalls();

  void initConfig();

  void replaceMain(clang::FunctionDecl* mainFxn);

  /**
   * @brief getCleanTypeName
   * Return the name of the type (with full template parameters, if applicable) but
   * without any struct,class decorators
   * @param ty
   * @return The name of the type without any struct,class decorators
   */
  std::string getCleanTypeName(clang::QualType ty);

  std::string getCleanName(const std::string& in);

  std::string eraseAllStructQualifiers(const std::string& name);

  void addCppGlobalCallExprString(PrettyPrinter& pp, clang::CallExpr* expr, clang::QualType qt);

  void addCppGlobalCtorString(PrettyPrinter& pp, clang::CXXConstructExpr* expr, bool leadingComma);

  /**
   * @brief addInContextGlobalDeclarations
   * For a given function or lambda body, add the necessary code at beginning/end
   * to bring in all the required global variables
   * @param body
   */
  void addInContextGlobalDeclarations(clang::Stmt* body);

  clang::CXXConstructExpr* getCtor(clang::VarDecl* vd);

 private:
  //whether we are allowed to use global variables in statements
  //or if they are disallowed because they would break deglobalizer
  clang::Decl* currentTopLevelScope_;
  clang::Rewriter& rewriter_;
  clang::CompilerInstance* ci_;
  std::map<clang::RecordDecl*,clang::TypedefDecl*> typedefStructs_;
  SSTPragmaList pragmas_;
  bool visitingGlobal_;
  std::set<clang::FunctionDecl*> templateDefinitions_;
  std::set<clang::Stmt*>& deletedStmts_;
  std::list<clang::CXXConstructorDecl*> ctorContexts_;
  GlobalVarNamespace& globalNs_;
  GlobalVarNamespace* currentNs_;

  /** These should always index by the canonical decl */
  struct GlobalReplacement {
     //used at beginning of function so that variable can be reused
    //without redoing all the lookup work
    std::string reusableText;
    //used in ctors and other contexts when variable must be directly
    //accessed and cannot be reused
    std::string oneOffText;
    bool append;
    GlobalReplacement(const std::string& reusable,
                      const std::string& oneOff,
                      bool app) :
      append(app), reusableText(reusable), oneOffText(oneOff) {}
  };

  std::map<const clang::Decl*,GlobalReplacement> globals_;
  std::set<const clang::Decl*> variableTemplates_;
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


  /**
   * @brief propagateNullness This decl is initialized using a null variable
   *        Propagate nullness to this new variable
   * @param dest  The new variable that becomes null
   * @param src   The existing null variable
   */
  void propagateNullness(clang::Decl* dest, clang::Decl* src);

  bool deleteMemberExpr(SSTNullVariablePragma* prg, clang::MemberExpr* expr,
                        clang::NamedDecl* decl);

  /**
   * @brief replaceNullWithEmptyType
   *  Given an expression tied to a null variable, replace it with
   *  a default-constructed type
   * @param type  The type of the expression
   * @param toRepl  The expr to replace
   */
  void replaceNullWithEmptyType(clang::QualType type, clang::Expr* toRepl);

  void deleteNullVariableExpr(clang::Expr* expr);

  clang::Stmt* replaceNullVariableStmt(clang::Stmt* stmt, const std::string& repl);

  /**
   * @brief checkNullAssignments
   * @param nd  The null declaration being "propagated" to other values
   * @param hasReplacement  Whether the null declaration has a sensible null replacement
   * @return The outer most expression being assigned to
   */
  clang::Stmt* checkNullAssignments(clang::NamedDecl* nd, bool hasReplacement);

  void nullifyIfStmt(clang::IfStmt* if_stmt, clang::Decl* d);

  void addTransitiveNullInformation(clang::NamedDecl* nd, std::ostream& os,
                                    SSTNullVariablePragma* prg);

  /**
   * @brief visitNullVariable
   * Visit an expression contained a null variable. Perform any relevant deletions,
   * safety checks, and log any "propagation" of the null variable into other variables
   * @param expr
   * @param nd
   */
  void visitNullVariable(clang::Expr* expr, clang::NamedDecl* nd);

  void tryVisitNullVariable(clang::Expr* expr, clang::NamedDecl* nd);

  void nullDereferenceError(clang::Expr* expr, const std::string& varName);

  /**
   * @brief deleteNullVariableMember
   * @param nullVarDecl The member declaration that is null
   * @param expr The member expr whose base is the null variable
   * @return Whether this member access should be deleted
   */
  bool deleteNullVariableMember(clang::NamedDecl* nullVarDecl, clang::MemberExpr* expr);

  void setActiveGlobalScopedName(const std::string& str) {
    activeGlobalScopedName_ = str;
  }

  const std::string& activeGlobalScopedName() const {
    return activeGlobalScopedName_;
  }

  /**
   * @brief addInitializers For an initializer A(x,y,z)
   *        add the text "x,y,z" to the ostream, if there is an init
   * @param D   The variable declaration
   * @param os  The os to be appended to
   * @param leadingComma If not empty, add a leading comma
   * @return Whether the initializer was a call expression
   */
  bool addInitializers(clang::VarDecl* D, std::ostream& os, bool leadingComma);

  void executeCurrentReplacements();

  void replace(clang::SourceRange rng, const std::string& repl);

  void replace(clang::Expr* expr, const std::string& repl){
    replace(expr->getSourceRange(), repl);
  }

  void replace(clang::Decl* decl, const std::string& repl){
    replace(decl->getSourceRange(), repl);
  }

  bool insideTemplateFxn() const {
    if (fxnContexts_.empty()) return false;
    clang::FunctionDecl* fd = fxnContexts_.back();
    return fd->isDependentContext();
  }

  bool isThreadLocal(clang::VarDecl* D) const {
    switch (D->getTSCSpec()){
      case clang::TSCS___thread:
      case clang::TSCS_thread_local:
      case clang::TSCS__Thread_local:
        return true;
      default:
        return false;
    }
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

  std::map<clang::FunctionDecl*, std::map<std::string, int>> staticFxnVarCounts_;
  std::list<clang::FunctionDecl*> fxnContexts_;
  std::list<clang::CXXRecordDecl*> classContexts_;
  std::list<clang::Stmt*> loopContexts_; //both fors and whiles
  /* a subset of loop contexts, only those loops that are skeletonized */
  std::list<clang::ForStmt*> computeLoops_;
  std::list<clang::Stmt*> stmtContexts_;
  std::list<clang::Decl*> assignments_;
  std::list<clang::Expr*> activeDerefs_;
  std::list<clang::IfStmt*> activeIfs_;
  std::list<clang::MemberExpr*> memberAccesses_;
  std::map<clang::Stmt*,clang::Stmt*> extendedReplacements_;
  std::set<clang::DeclContext*> innerStructTagsDeclared_;

  /**
   * @brief dependentStaticMembers_
   * Static variables of template classes that lead to
   * CXXDependentScopeMemberExpr in which a global variable cannot be recognized
   * because it is hidden behind template parameters
   */
  std::map<std::string,clang::VarDecl*> dependentStaticMembers_;

  std::list<std::list<std::pair<clang::SourceRange,std::string>>> stmtReplacements_;

  std::list<clang::VarDecl*> activeDecls_;
  std::list<clang::Expr*> activeInits_;

  int numRelocations_;

  typedef enum { LHS, RHS } BinOpSide;
  std::vector<std::pair<clang::BinaryOperator*,BinOpSide>> binOps_;
  int activeBinOpIdx_;

  std::list<clang::ParmVarDecl*> activeFxnParams_;

  bool foundCMain_;
  bool refactorMain_;
  std::string mainName_;
  bool keepGlobals_;
  bool noSkeletonize_;
  std::set<std::string> ignoredHeaders_;
  std::set<std::string> reservedNames_;
  PragmaConfig& pragmaConfig_;
  std::set<clang::DeclRefExpr*> alreadyReplaced_;
  std::set<std::string> globalVarWhitelist_;

  friend class PragmaActivateGuard;
  struct PragmaActivateGuard {
    PragmaActivateGuard(clang::Stmt* s, SkeletonASTVisitor* visitor, bool doVisit = true) :
      visitor_(visitor)
    {
      ++visitor->pragmaConfig_.pragmaDepth;
      if (doVisit){
        doCtor(s);
      }
    }

    PragmaActivateGuard(clang::Decl* d, SkeletonASTVisitor* visitor, bool doVisit = true) :
      visitor_(visitor)
    {
      ++visitor->pragmaConfig_.pragmaDepth;
      if (doVisit){
        doCtor(d);
      }
    }

    void reactivate(clang::Decl* d, SSTPragma* prg){
      ++visitor_->pragmaConfig_.pragmaDepth;
      activePragmas_.push_back(prg);
      prg->activate(d, visitor_->rewriter_, visitor_->pragmaConfig_);
    }

    ~PragmaActivateGuard();

    bool skipVisit() const {
      return skipVisit_;
    }

   private:
    template <class T> void doCtor(T* t){
      myPragmas_ = visitor_->pragmas_.getMatches<T>(t);
      //this removes all inactivate pragmas from myPragmas_
      init();
      for (SSTPragma* prg : myPragmas_){
        //pragma takes precedence - must occur in pre-visit
        activePragmas_.push_back(prg);
        prg->activate(t, visitor_->rewriter_, visitor_->pragmaConfig_);
      }
    }

    void init();

    bool skipVisit_;
    std::list<SSTPragma*> myPragmas_;
    std::list<SSTPragma*> activePragmas_;
    SkeletonASTVisitor* visitor_;
  };

  struct GlobalStandin {
    bool fxnStatic;
    bool threadLocal;
    std::string replText;
    GlobalStandin() : fxnStatic(false), threadLocal(false) {}
  };

  std::map<const clang::Decl*,GlobalStandin> globalStandins_;
  std::list<int> initIndices_;
  std::list<clang::FieldDecl*> activeFieldDecls_;
  std::list<std::set<const clang::Decl*>> globalsTouched_;
  std::set<std::string> sstmacFxnPrepends_;

  typedef void (SkeletonASTVisitor::*MPI_Call)(clang::CallExpr* expr);
  std::map<std::string, MPI_Call> mpiCalls_;

 private:
  /**
   * Exception-safe pushing back on a list
   * Forces clean up even if exceptions get thrown
   */
  template <class T>
  struct PushGuard {
    template <class U>
    PushGuard(std::list<T>& theList, U&& t) : myList(theList) {
      myList.push_back(t);
    }

    ~PushGuard(){ myList.pop_back(); }

    void swap(T&& t){
      myList.pop_back();
      myList.push_back(t);
    }

    std::list<T>& myList;
  };

  template <class T, class U>
  struct InsertGuard {
    InsertGuard(std::map<T*,U*>& theMap, T* t, U* u) :
      myMap(theMap), myKey(t) {
      myMap.emplace(t,u);
    }

    ~InsertGuard(){ myMap.erase(myKey); }

    std::map<T*,U*>& myMap;
    T* myKey;
  };

  template <class T>
  struct VectorPushGuard {
    template <class... Args>
    VectorPushGuard(std::vector<T>& theVec, Args&& ...args) : myVec(theVec) {
      myVec.emplace_back(std::forward<Args>(args)...);
    }

    template <class... Args>
    void swap(Args&& ...args){
      myVec.pop_back();
      myVec.emplace_back(std::forward<Args>(args)...);
    }

    ~VectorPushGuard(){ myVec.pop_back(); }

    std::vector<T>& myVec;
  };

  struct IndexGuard {
    IndexGuard(int& t, int value) {
      if (t < 0) {
        t = value;
        myPtr = &t;
      } else { //don't overwrite
        myPtr = nullptr;
      }
    }

    ~IndexGuard(){ if (myPtr) *myPtr = IndexResetter; }

    int* myPtr;
  };

  template <class T>
  struct EmplaceGuard {
    EmplaceGuard(std::list<T>& theList) : myList(theList) {
      myList.emplace_back();
    }

    ~EmplaceGuard(){ myList.pop_back(); }

    std::list<T>& myList;
  };

  struct IncrementGuard {
    IncrementGuard(int& idx) : myIdx(idx)
    {
      ++myIdx;
    }
    ~IncrementGuard(){ --myIdx; }
    int& myIdx;
  };

  template <class Lambda>
  void goIntoContext(clang::Stmt* stmt, Lambda&& l){
    stmtContexts_.push_back(stmt);
    stmtReplacements_.emplace_back();
    bool deleted = false;
    try {
      l();
    } catch (StmtDeleteException& e) {
      if (stmt != e.deleted){
        stmtContexts_.pop_back(); //must pop back now
        stmtReplacements_.pop_back();
        //nope! not me - pass it along
        throw e;
      }
      deleted = true;
    }
    stmtContexts_.pop_back();
    if (!deleted) executeCurrentReplacements();
    stmtReplacements_.pop_back();
  }

  bool isNullVariable(clang::Decl* d) const {
    return pragmaConfig_.nullVariables.find(d) != pragmaConfig_.nullVariables.end();
  }

  bool isValidAssignment(clang::Decl* lhs, clang::Expr* rhs);

  bool isNullSafeFunction(const clang::DeclContext* dc) const {
    return pragmaConfig_.nullSafeFunctions.find(dc) != pragmaConfig_.nullSafeFunctions.end();
  }

  SSTNullVariablePragma* getNullVariable(clang::Decl* d) const {
    auto iter = pragmaConfig_.nullVariables.find(d);
    if (iter != pragmaConfig_.nullVariables.end()){
      return iter->second;
    }
    return nullptr;
  }

  void maybeReplaceGlobalUse(clang::DeclRefExpr* expr, clang::SourceRange rng);

  /**
   * @brief getUnderlyingExpr Follow through parentheses and casts
   *  to the "significant" expression underneath
   * @param e The input expression that might have casts/parens
   * @return  The underlying expression
   */
  clang::Expr* getUnderlyingExpr(clang::Expr *e);

  /**
   * @brief getFinalExpr Similar to #getUnderlyingExpr, but also
   *  follow through unary operators.
   * @param e The input expression that might have casts/parens/unary ops
   * @return  The underlying expression
   */
  clang::Expr* getFinalExpr(clang::Expr *e);

  void replaceNullVariableConnectedContext(clang::Expr* expr, const std::string& repl);

  void deleteNullVariableStmt(clang::Stmt* stmt);
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
    return !classContexts_.empty();
  }
  bool insideFxn() const {
    return !fxnContexts_.empty();
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

  std::unordered_set<std::string> validHeaders_;

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

  void arrayFxnPointerTypedef(clang::VarDecl* D, SkeletonASTVisitor::ArrayInfo* info,
                              std::stringstream& sstr);


  /**
   * @brief getOriginalDeclaration
   * Fight through all the templates and find the fundamental underlying declaration
   * @param vd
   * @return
   */
  const clang::Decl* getOriginalDeclaration(clang::VarDecl* vd);
};


#endif
