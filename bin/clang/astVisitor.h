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

static constexpr int IndexResetter = -1;

/**
 * Exception-safe pushing back and popping back on a list
 * within a given function to add/remove things from a context list
 * Forces clean up even if exceptions get thrown
 */
template <class T>
struct PushGuard {
  template <class U>
  PushGuard(std::list<T>& theList, U&& t) : myList(theList) {
    myList.push_back(std::forward<U>(t));
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

class FirstPassASTVisitor : public clang::RecursiveASTVisitor<FirstPassASTVisitor>
{
  using Parent = clang::RecursiveASTVisitor<FirstPassASTVisitor>;
 public:
  friend struct PragmaActivateGuard;

  FirstPassASTVisitor(SSTPragmaList& pragmas);

  void preVisitTopLevelDecl(clang::Decl* /*d*/){}
  void postVisitTopLevelDecl(clang::Decl* /*d*/){}
  void finalizePass(){}

  bool VisitDecl(clang::Decl* d);
  bool VisitStmt(clang::Stmt* s);

  bool TraverseFunctionDecl(clang::FunctionDecl* fd, DataRecursionQueue* queue = nullptr);

  bool TraverseCompoundStmt(clang::CompoundStmt* cs, DataRecursionQueue* queue = nullptr);

  SSTPragmaList& getPragmas(){
    return pragmas_;
  }

 private:
  SSTPragmaList& pragmas_;

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
  friend struct PragmaActivateGuard;
  using Parent=clang::RecursiveASTVisitor<SkeletonASTVisitor>;
 private:
  struct AnonRecord {
    clang::RecordDecl* decl;
    bool typeNameAdded;
    std::string structType;  //union or struct
    std::string retType;
    bool isFxnStatic;
    //struct X_anonymous_type - gives unique typename to anonymous truct
    std::string typeName;
    AnonRecord() : 
      decl(nullptr), 
      typeNameAdded(false),
      isFxnStatic(false)
    {}
  };

  struct ArrayInfo {
    bool needsTypedef() const {
      return !typedefDeclString.empty();
    }

    std::string typedefDeclString;
    std::string typedefName;
    std::string fqTypedefName;
    std::string retType;
    bool isFxnStatic;
    bool needsDeref;

    ArrayInfo() : isFxnStatic(false), needsDeref(true) {}
  };


  /** These should always index by the canonical decl */
  struct GlobalReplacement {
    //used at beginning of function so that variable can be reused
    //without redoing all the lookup work
    std::string reusableText;
    //used in ctors and other contexts when variable must be directly
    //accessed inline and cannot use a cached version
    std::string inlineUseText;
    //whether the replacement text should be appended to the variable name
    //or replace the variable name entirely
    bool append;
    GlobalReplacement(const std::string& reusable,
                      const std::string& oneOff,
                      bool app) :
      reusableText(reusable),
      inlineUseText(oneOff),
      append(app)
    {}
  };

  struct GlobalVariableReplacement {
    ArrayInfo* arrayInfo;
    AnonRecord* anonRecord;
    std::string scopeUniqueVarName;
    bool useAccessor;
    bool isFxnStatic;
    bool needFullNamespace;
    std::string typeStr;
    std::string retType;
    std::string classScope;
    bool threadLocal;

    GlobalVariableReplacement(const std::string& uniqueName,
                              bool useAcc, bool isStatic, bool needNs, bool tls) :
      arrayInfo(nullptr),
      anonRecord(nullptr),
      scopeUniqueVarName(uniqueName),
      useAccessor(useAcc),
      isFxnStatic(isStatic),
      needFullNamespace(needNs),
      threadLocal(tls)
    {
    }

    ~GlobalVariableReplacement(){
      if (arrayInfo) delete arrayInfo;
      if (anonRecord) delete anonRecord;
    }

  };

  struct GlobalStandin {
    bool fxnStatic;
    bool threadLocal;
    std::string replText;
    GlobalStandin() : fxnStatic(false), threadLocal(false) {}
  };

  typedef enum {
    Global, //regular global variable (C-style)
    FileStatic,
    CxxStatic, //c++ static class variable
    FxnStatic
  } GlobalVariable_t;

  struct cArrayConfig {
    std::string fundamentalTypeString;
    clang::QualType fundamentalType;
    std::stringstream arrayIndices;
  };

  struct ReplaceGlobalsPrinterHelper : public clang::PrinterHelper {
    ReplaceGlobalsPrinterHelper(SkeletonASTVisitor* parent) :
      parent_(parent)
    {
    }

    bool handledStmt(clang::Stmt *E, clang::raw_ostream &OS) override;

   private:
    SkeletonASTVisitor* parent_;
  };

  static bool indexIsSet(int idx){
    return idx != IndexResetter;
  }

 public:
  SkeletonASTVisitor(SSTPragmaList& pragmas,
      GlobalVarNamespace& ns) :
    pragmas_(pragmas),
    activeBinOpIdx_(-1),
    foundCMain_(false), 
    refactorMain_(true),
    insideCxxMethod_(0), 
    globalNs_(ns), 
    currentNs_(&ns),
    visitingGlobal_(false),
    keepGlobals_(false)
  {
    initHeaders();
    initReservedNames();
    initMPICalls();
  }

  /**
   * @brief delayedInsertBefore Totally obnoxious that I have to do this
   * Two inserts might occur on similar locations
   * If this "might" happen, I have to delay the insert
   * Until I'm sure I have concatenated all overlapping ones
   * otherwise I get a segfault from Clang
   * @param vd
   * @param repl
   */
  void delayedInsertAfter(clang::VarDecl* vd, const std::string& repl){
    unsigned pos = getStart(vd).getRawEncoding();
    auto iter = declsToInsertAfter_.find(pos);
    if (iter == declsToInsertAfter_.end()){
      declsToInsertAfter_[pos] = {vd,repl};
    } else {
      auto& pair = declsToInsertAfter_[pos];
      pair.first = vd;
      pair.second += repl;
    }
  }

  /**
   * @brief getUnderlyingExpr Follow through parentheses and casts
   *  to the "significant" expression underneath
   * @param e The input expression that might have casts/parens
   * @return  The underlying expression
   */
  static clang::Expr* getUnderlyingExpr(clang::Expr *e);

  bool isGlobal(const clang::DeclRefExpr* expr) const {
    return globals_.find(mainDecl(expr)) != globals_.end();
  }

  friend struct ReplaceGlobalsPrinterHelper;
  std::string printWithGlobalsReplaced(clang::Stmt* stmt);

  void registerNewKeywords(std::ostream& os);

  GlobalVarNamespace* getActiveNamespace() const {
    return currentNs_;
  }

  bool isCxx() const {
    return CompilerGlobals::CI().getLangOpts().CPlusPlus;
  }

  std::string needGlobalReplacement(clang::NamedDecl* decl) {
    const clang::Decl* md = mainDecl(decl);
    if (globalsTouched_.empty()){
      errorAbort(decl, "internal error: globals touched array is empty");
    }
    globalsTouched_.back().insert(md);
    auto iter = globals_.find(md);
    if (iter == globals_.end()){
      errorAbort(decl, "getting global replacement for non-global variable");
    }
    return iter->second.reusableText;
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
   * if it is either a global variable or a pragma null_ptr and therefore
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

  bool TraverseVarTemplateDecl(clang::VarTemplateDecl* D);

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

  bool TraverseCXXDestructorDecl(clang::CXXDestructorDecl* D);

  clang::SourceLocation getVariableNameLocationEnd(clang::VarDecl* D);

  SSTPragmaList& getPragmas(){
    return pragmas_;
  }

  void preVisitTopLevelDecl(clang::Decl* d);
  void postVisitTopLevelDecl(clang::Decl* d);
  void finalizePass();

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

  void getTemplatePrefixString(std::ostream& os, clang::TemplateParameterList* theList);

  void getTemplateParamsString(std::ostream& os, clang::TemplateParameterList* theList);

  bool shouldVisitDecl(clang::VarDecl* D);

  void initHeaders();

  void initReservedNames();

  void initMPICalls();

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

  /**
   * @brief addInContextGlobalDeclarations
   * For a given function or lambda body, add the necessary code at beginning/end
   * to bring in all the required global variables
   * @param body
   */
  void addInContextGlobalDeclarations(clang::Stmt* body);

  clang::CXXConstructExpr* getCtor(clang::VarDecl* vd);

 private:
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

  void executeCurrentReplacements();

  void replace(clang::SourceRange rng, const std::string& repl);

  void replace(clang::Expr* expr, const std::string& repl){
    replace(expr->getSourceRange(), repl);
  }

  void replace(clang::Decl* decl, const std::string& repl){
    replace(decl->getSourceRange(), repl);
  }

  bool insideTemplateFxn() const {
    if (CompilerGlobals::astContextLists.enclosingFunctionDecls.empty()) return false;
    clang::FunctionDecl* fd = CompilerGlobals::astContextLists.enclosingFunctionDecls.back();
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

  GlobalVariableReplacement setupGlobalReplacement(clang::VarDecl* vd, const std::string& namePrefix,
                          bool useAccessor, bool isFxnStatic, bool needFullNs);

  void registerGlobalReplacement(clang::VarDecl* D, GlobalVariableReplacement* repl);
  bool setupClassStaticVarDecl(clang::VarDecl* D);
  bool setupCGlobalVar(clang::VarDecl* D, const std::string& scopePrefix);
  bool setupCppGlobalVar(clang::VarDecl* D, const std::string& scopePrefix);
  bool setupFunctionStaticCpp(clang::VarDecl* D, const std::string& scopePrefix);
  bool setupFunctionStaticC(clang::VarDecl* D, const std::string& scopePrefix);

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
    return CompilerGlobals::astNodeMetadata.nullVariables.find(d) !=
            CompilerGlobals::astNodeMetadata.nullVariables.end();
  }

  bool isValidAssignment(clang::Decl* lhs, clang::Expr* rhs);

  bool isNullSafeFunction(const clang::DeclContext* dc) const {
    return CompilerGlobals::astNodeMetadata.nullSafeFunctions.find(dc) !=
          CompilerGlobals::astNodeMetadata.nullSafeFunctions.end();
  }

  SSTNullVariablePragma* getNullVariable(clang::Decl* d) const {
    auto iter = CompilerGlobals::astNodeMetadata.nullVariables.find(d);
    if (iter != CompilerGlobals::astNodeMetadata.nullVariables.end()){
      return iter->second;
    }
    return nullptr;
  }

  void maybeReplaceGlobalUse(clang::DeclRefExpr* expr, clang::SourceRange rng);

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
  bool haveActiveFxnParam() const {
    if (activeFxnParams_.empty()) return false;
    return activeFxnParams_.back();
  }
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
    return !CompilerGlobals::astContextLists.enclosingFunctionDecls.empty();
  }

   /**
    * @brief checkAnonStruct See if the type of the variable is an
    *       an anonymous union or struct. Fill in info in rec if so.
    * @param D    The variable being visited
    * @return If D has anonymous struct type, return the passed-in rec struct
    *         If D is not an anonymous struct, return nullptrs
    */
   AnonRecord* checkAnonStruct(clang::VarDecl* D);

   clang::RecordDecl* checkCombinedStructVarDecl(clang::VarDecl* D);

   /**
    * @brief checkArray See if the type of the variable is an array
    *       and fill in the array info if so.
    * @param D    The variable being visited
    * @return If D has array type, return the passed-in info struct.
    *         If D does not have array type, return nullptr
    */
   ArrayInfo* checkArray(clang::VarDecl* D);

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

  void traverseFunctionBody(clang::Stmt* s);

  bool doTraverseLambda(clang::LambdaExpr* expr);

  void getArrayType(const clang::Type* ty, cArrayConfig& cfg);

  void setFundamentalTypes(clang::QualType qt, cArrayConfig& cfg);

  bool maybePrintGlobalReplacement(clang::VarDecl* vd, llvm::raw_ostream& os);

  void arrayFxnPointerTypedef(clang::VarDecl* D, SkeletonASTVisitor::ArrayInfo* info,
                              std::stringstream& sstr);


  /**
   * @brief getOriginalDeclaration
   * Fight through all the templates and find the fundamental underlying declaration
   * @param vd
   * @return
   */
  const clang::Decl* getOriginalDeclaration(clang::VarDecl* vd);

 private:
  SSTPragmaList& pragmas_;
  clang::Decl* currentTopLevelScope_;

  /**
   * Variables used for deciding whether a global variable in a header
   * needs to be refactored
   */
  std::unordered_set<std::string> validHeaders_;
  std::set<std::string> ignoredHeaders_;

  /**
    Variables used in tracking deletions or null variable
    propagations
  */
  //most deletions are tracked through exceptions
  //however, sometimes a call expr must "lookahead" and delete arguments before
  //they are traversed in the natural course of AST traversal
  //note here any arguments that are modified/deleted
  std::set<clang::Expr*> deletedArgsCurrentCallExpr_;
  std::list<clang::MemberExpr*> memberAccesses_;
  std::map<clang::Stmt*,clang::Stmt*> extendedReplacements_;
  typedef enum { LHS, RHS } BinOpSide;
  std::vector<std::pair<clang::BinaryOperator*,BinOpSide>> binOps_;
  int activeBinOpIdx_;

  /**
   * @brief dependentStaticMembers_
   * Static variables of template classes that lead to
   * CXXDependentScopeMemberExpr in which a global variable cannot be recognized
   * because it is hidden behind template parameters
   */
  std::map<std::string,clang::VarDecl*> dependentStaticMembers_;

  /**
   * variables used for refactoring the main function
   */
  bool foundCMain_;
  bool refactorMain_;
  std::string mainName_;

  /** Special lookups for ensuring clean rewriter.
   *  Multiple insert calls can cause issues.
   */
  std::map<unsigned, std::pair<clang::VarDecl*,std::string>> declsToInsertAfter_;
  std::list<std::list<std::pair<clang::SourceRange,std::string>>> stmtReplacements_;

  /**
   * Here start the "context" lists that track information about parent
   * nodes in the AST
   */
  std::list<clang::ParmVarDecl*> activeFxnParams_;
  std::list<int> initIndices_;
  std::list<clang::FieldDecl*> activeFieldDecls_;
  std::list<clang::CXXConstructorDecl*> ctorContexts_;
  std::list<std::set<const clang::Decl*>> globalsTouched_;
  std::list<clang::VarDecl*> activeDecls_;
  std::list<clang::Expr*> activeInits_;
  std::list<clang::CXXRecordDecl*> classContexts_;
  std::list<clang::Stmt*> loopContexts_; //both fors and whiles
  std::list<clang::Stmt*> stmtContexts_;
  std::list<clang::Expr*> activeDerefs_;
  std::list<clang::IfStmt*> activeIfs_;
  int insideCxxMethod_;

  /** Lookup tables for functions or variables needing special operations */
  std::set<std::string> sstmacFxnPrepends_;
  typedef void (SkeletonASTVisitor::*MPI_Call)(clang::CallExpr* expr);
  std::map<std::string, MPI_Call> mpiCalls_;
  std::set<std::string> reservedNames_;
  std::set<std::string> globalVarWhitelist_;

  /** Lookup tables and other members used for replacing global
   *  variables
   */
  GlobalVarNamespace& globalNs_;
  GlobalVarNamespace* currentNs_;
  bool visitingGlobal_;
  std::map<const clang::Decl*,GlobalReplacement> globals_;
  std::set<const clang::Decl*> variableTemplates_;
  std::map<const clang::Decl*,std::string> scopedNames_;
  bool keepGlobals_;
  std::set<clang::DeclRefExpr*> alreadyReplaced_;
  std::map<const clang::Decl*,GlobalStandin> globalStandins_;
  //C-style structs that have typedef'd names we can use doing global replacements
  std::map<clang::RecordDecl*,clang::TypedefDecl*> typedefStructs_;
  //used to assign a unique int ID to each static function variable
  std::map<clang::FunctionDecl*, std::map<std::string, int>> staticFxnVarCounts_;
};

struct PragmaActivateGuard {
  template <class T> //either decl/stmt
  PragmaActivateGuard(T* t, SkeletonASTVisitor* visitor, bool doVisit = true) :
    PragmaActivateGuard(t, visitor->pragmas_, doVisit, false/*2nd pass*/)
  {
  }

  template <class T>
  PragmaActivateGuard(T* t, FirstPassASTVisitor* visitor, bool doVisit = true) :
    PragmaActivateGuard(t, visitor->pragmas_, doVisit, true/*1st pass*/)
  {
  }

  ~PragmaActivateGuard();

  bool skipVisit() const {
    return skipVisit_;
  }

 private:
  template <class T> //either decl/stmt
  PragmaActivateGuard(T* t,
       SSTPragmaList& pragmas,
       bool doVisit, bool firstPass) :
    skipVisit_(false),
    pragmas_(pragmas)
  {
    myPragmas_ = [&]{
      auto tmp = pragmas_.getMatches<T>(t, firstPass);
      if(doVisit){
        return tmp;
      } else {
        return decltype(tmp){};
      }
    }();

    //this removes all inactivate pragmas from myPragmas_
    for (SSTPragma* prg : myPragmas_){
      if (prg->deleteOnUse){
        deletePragmaText(prg);
      }
      prg->activate(t);
      if (CompilerGlobals::pragmaConfig.makeNoChanges){
        skipVisit_ = true;
        CompilerGlobals::pragmaConfig.makeNoChanges = false;
      }
    }

  }

  void deletePragmaText(SSTPragma* prg);

  bool skipVisit_;
  std::list<SSTPragma*> myPragmas_;
  SSTPragmaList& pragmas_;

};

class GlobalVariableVisitor : public clang::RecursiveASTVisitor<GlobalVariableVisitor> {
 public:
  GlobalVariableVisitor(clang::VarDecl*  /*D*/, SkeletonASTVisitor* parent) :
    visitedGlobals_(false),
    parent_(parent)
  {
  }

  bool visitedGlobals() const {
    return visitedGlobals_;
  }

  bool VisitDeclRefExpr(clang::DeclRefExpr* expr);

  bool VisitCallExpr(clang::CallExpr* expr);

 private:
  bool visitedGlobals_;
  SkeletonASTVisitor* parent_;
};


#endif
