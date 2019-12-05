
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

#ifndef bin_clang_pragmas_h
#define bin_clang_pragmas_h

#include "clangHeaders.h"
#include "util.h"
#include <set>
#include <cstdint>
#include <functional>

class SkeletonASTVisitor;
struct SSTPragma;
class SSTReplacePragma;
class SSTNullVariablePragma;
class SSTNullVariableGeneratorPragma;
struct PragmaConfig {
  std::map<std::string,SSTReplacePragma*> replacePragmas;
  std::map<clang::Decl*,SSTNullVariablePragma*> nullVariables;
  std::map<clang::FunctionDecl*,std::set<SSTPragma*>> functionPragmas;
  std::map<const clang::DeclContext*,SSTNullVariablePragma*> nullSafeFunctions;
  std::vector<std::pair<SSTPragma*, std::string>> globalCppFunctionsToWrite;
  std::set<std::string> newParams;
  std::string dependentScopeGlobal;
  std::string computeMemorySpec;
  std::list<std::pair<SSTNullVariablePragma*,clang::TypedefDecl*>> pendingTypedefs;
  std::list<clang::FunctionDecl*> fxnContexts;
  std::map<clang::IfStmt*,std::string> predicatedBlocks;
  std::list<clang::CompoundStmt*> stmtBlocks;

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

  int pragmaDepth = 0;

  bool makeNoChanges = false;
  SkeletonASTVisitor* astVisitor = nullptr;
  SSTNullVariableGeneratorPragma* nullifyDeclarationsPragma = nullptr;

  PragmaConfig() = default; 
};

template <typename T>             
std::uintptr_t pragmaID() noexcept {
  static const char pragmaID = 0; 
  return std::uintptr_t(&pragmaID);               
}  

namespace pragmas {
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

using PragmaArgMap = std::map<std::string, std::list<std::string>>;
struct SSTPragmaList;
struct SSTPragma {
  clang::StringRef name;
  clang::SourceLocation pragmaDirectiveLoc;
  clang::SourceLocation startPragmaLoc;
  clang::SourceLocation endPragmaLoc;
  clang::SourceLocation targetLoc;
  clang::CompilerInstance* CI = nullptr;
  SkeletonASTVisitor* visitor = nullptr;
  SSTPragmaList* pragmaList = nullptr;

  int depth;
  bool deleteOnUse;
  std::uintptr_t classId;

  void print(){
    std::cout << "pragma " << name.str() << " from "
              << startPragmaLoc.printToString(CI->getSourceManager())
              << " to " << targetLoc.printToString(CI->getSourceManager())
              << std::endl;
  }

  template <class T>
  bool matches(T* s){
    return startPragmaLoc < getStart(s) && getStart(s) <= targetLoc;
  }

  template <class T> static std::uintptr_t id() {
    return pragmaID<T>();
  }

  /**
   * @brief firstPass AST gets visited twice - once in a first pass to fill
   * in declarations/definitions a second pass to actually make changes.
   * Some pragmas need to be visited on the first pass. Most do not.
   * @param d tag parameter, whether declarations should be visited first pass
   * @return
   */
  virtual bool firstPass(const clang::Decl*  /*d*/) const {
    return false;
  }

  virtual bool firstPass(const clang::Stmt*  /*s*/) const {
    return false;
  }

  static std::string getSingleString(const std::list<clang::Token>& tokens, clang::CompilerInstance& CI);

  //TODO this could really use some documentation, what does it parse and return
  static PragmaArgMap getMap(
      clang::SourceLocation loc, clang::CompilerInstance& CI, const std::list<clang::Token>& tokens);

  SSTPragma() = default;
  int getActiveMode() const;

  virtual void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) = 0;
  virtual void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig &cfg){} //not required
  virtual void deactivate(PragmaConfig& cfg){} //not required

  static void tokenStreamToString(
      std::list<clang::Token>::const_iterator beg,
      std::list<clang::Token>::const_iterator end,
      std::ostream& os, clang::CompilerInstance& CI);

};

struct SSTPragmaList {
  void push_back(SSTPragma* p){
    pragmas.push_back(p);
  }

  void erase(SSTPragma* prg){
    auto end=pragmas.end();
    for (auto iter=pragmas.begin(); iter != end; ++iter){
      SSTPragma* test = *iter;
      if (test == prg){
        pragmas.erase(iter);
        return;
      }
    }
  }

  template <class T>
  std::list<SSTPragma*>
  getMatches(T* t, bool firstPass = false){
    std::list<SSTPragma*> ret;
    auto end = pragmas.end();
    auto iter=pragmas.begin();
    while (iter != end){
      auto tmp = iter++;
      SSTPragma* p = *tmp;
      bool match = p->matches<T>(t);
      if (match){
        if (firstPass){
          if (p->firstPass(t)){
            pragmas.erase(tmp);
            ret.push_back(p);
          }
        } else {
          pragmas.erase(tmp);
          ret.push_back(p);
        }
      }
    }
    return ret;
  }

  std::list<SSTPragma*> pragmas;
};


class SSTPragmaHandler : public clang::PragmaHandler {

 public:
  void HandlePragma(clang::Preprocessor &PP,
                    clang::PragmaIntroducerKind Introducer,
                    clang::Token &PragmaTok) override;

  bool deleteOnUse() const {
    return deleteOnUse_;
  }


 protected:
  SSTPragmaHandler(const std::string& name,
                   bool deleteOnUse,
                   SSTPragmaList& plist,
                   clang::CompilerInstance& ci,
                   SkeletonASTVisitor& visitor) :
    PragmaHandler(name), 
    ci_(ci),
    pragmas_(plist), 
    visitor_(visitor), 
    deleteOnUse_(deleteOnUse)
  {}

  clang::CompilerInstance& ci_;
  clang::SourceLocation pragmaLoc_;

 private:
  /**
   * @brief configure Assuming the PP lex position is currently on eod,
   *        configure the source locations of the pragma and initialize fields
   * @param PP
   * @param pragma
   */
  void configure(bool deleteOnUse, clang::Token& PragmaTok,
                 clang::Preprocessor& PP, SSTPragma* pragma);

  std::map<std::string, std::list<std::string>> getTokenMap(const std::list<clang::Token>& tokens) const;

  virtual SSTPragma* allocatePragma(const std::list<clang::Token>& tokens) const = 0;

  SSTPragmaList& pragmas_;
  SkeletonASTVisitor& visitor_;
  bool deleteOnUse_;

  /** I hate doing it this way, but Clang sort of forces me
   * I have to register a generic callback for directives
   * However, that callback has no way of knowing what handler gets invoked
   * All I can do is stash information here for the handler to use
   * when it does get invoked */
  friend struct PragmaPPCallback;
  static clang::SourceLocation pragmaDirectiveLoc;
};

template <class T>
class SSTPragmaHandlerInstance : public SSTPragmaHandler
{
 public:
  /**
   * @brief SSTSimplePragmaHandler Constructor for pragma handlers for pragmas of the form
   *        #pragma sst name
   * @param name  The string identifying the pragma
   * @param plist The pragma list to append to
   */
  SSTPragmaHandlerInstance(const std::string& name,
                           bool deleteOnUse,
                           SSTPragmaList& plist,
                           clang::CompilerInstance& CI,
                           SkeletonASTVisitor& visitor) :
    SSTPragmaHandler(name, deleteOnUse, plist, CI, visitor)
  {}

 private:
  /**
   * For standard pragmas of the form #pragma sst myPragma arg1(x) arg2(y,z)
   * Gather a map of the form { "arg1" : {"x"}, "arg2" : {"y", "z" } }
   * @param loc
   * @param args Keys are parameter names, the list of string arguments passed to each one
   * @return the pragma object
   */
   SSTPragma * allocatePragma(const std::list<clang::Token> &tokens) const override {
     return new T(pragmaLoc_, ci_, tokens);
   }
};

template <class T> struct SSTNoArgsPragmaShim : public T
{
  SSTNoArgsPragmaShim(clang::SourceLocation  /*loc*/, clang::CompilerInstance&  /*CI*/,
                  const std::list<clang::Token>&  /*tokens*/) :
    T() {
      this->classId = pragmaID<T>();
    }
};

template <class T> struct SSTStringPragmaShim : public T
{
  using T::getSingleString;
  SSTStringPragmaShim(clang::SourceLocation  /*loc*/, clang::CompilerInstance& CI,
                  const std::list<clang::Token>& tokens) :
    T(getSingleString(tokens,CI)) //just a single string gets passed up
  {
      this->classId = pragmaID<T>();
  }

};

template <class T> struct SSTTokenListPragmaShim : public T
{
  SSTTokenListPragmaShim(clang::SourceLocation loc, clang::CompilerInstance& CI,
                     const std::list<clang::Token>& tokens) :
    T(loc, CI, tokens) { 
      this->classId = pragmaID<T>();
    }
};

template <class T> struct SSTArgMapPragmaShim : public T
{
  using T::getMap;
  SSTArgMapPragmaShim(clang::SourceLocation loc, clang::CompilerInstance& CI,
                  const std::list<clang::Token>& tokens) :
    T(loc, CI, getMap(loc, CI, tokens))
  { 
      this->classId = pragmaID<T>();
  }
};

class SSTStackAllocPragma : public SSTPragma
{
 public:
  SSTStackAllocPragma(clang::SourceLocation loc, clang::CompilerInstance& CI,
                      std::map<std::string, std::list<std::string>>&& args);

  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

 private:
  std::string stackSize_;
  std::string mdataSize_;
  std::string toFree_;
};

struct PragmaHandlerFactoryBase {
  virtual clang::PragmaHandler*
  getHandler(SSTPragmaList& plist,
             clang::CompilerInstance& CI,
             SkeletonASTVisitor& visitor) = 0;
 protected:
  PragmaHandlerFactoryBase(const std::string& name) :
    name_(name)
  {
  }

  const std::string& name() const {
    return name_;
  }

 private:
  std::string name_;
};

template <class T, bool deleteOnUse>
struct PragmaHandlerFactory : public PragmaHandlerFactoryBase {
  PragmaHandlerFactory(const std::string& name) :
    PragmaHandlerFactoryBase(name)
  {
  }

  clang::PragmaHandler*
  getHandler(SSTPragmaList &plist, clang::CompilerInstance &CI,
             SkeletonASTVisitor &visitor) override {
    return new SSTPragmaHandlerInstance<T>(name(), deleteOnUse,
                                           plist, CI, visitor);
  }
};


struct SSTPragmaNamespace;
struct PragmaRegisterMap {

  static SSTPragmaNamespace* getNamespace(const std::string& ns);

  static const std::map<std::string, SSTPragmaNamespace*>& namespaces() {
    return *namespaces_;
  }

 private:
  static std::map<std::string, SSTPragmaNamespace*>* namespaces_;

};


struct SSTPragmaNamespace {

  SSTPragmaNamespace(const std::string &name) :
    name_(name)
  {
  }

  const std::string name() const {
    return name_;
  }

  void addFactory(int modeMask, const std::string& name,
                  bool deleteOnUse, PragmaHandlerFactoryBase* factory);

  PragmaHandlerFactoryBase* getFactory(pragmas::Mode m, const std::string& name);

  const std::set<std::string>& names() const {
    return pragmaNames_;
  }

 private:
  std::string name_;
  std::set<std::string> pragmaNames_;
  std::map<std::string, PragmaHandlerFactoryBase*> factories_[pragmas::NUM_MODES];

};

template <template <class U, typename...> class PragmaType, class T, bool deleteOnUse, 
         typename ...PragmaTypeArgs>
struct PragmaRegister {

  PragmaRegister(const std::string& ns, const std::string& name, int modeMask){
    SSTPragmaNamespace* nsObj = PragmaRegisterMap::getNamespace(ns);
    nsObj->addFactory(modeMask, name, deleteOnUse,
                      new PragmaHandlerFactory<PragmaType<T, PragmaTypeArgs...>
                      ,deleteOnUse>(name));
  }

};

/**
 * @brief The SSTDoNothingPragma struct
 * Use as fill-in for pragmas which should not be activated in a given mode
 */
struct SSTDoNothingPragma : public SSTPragma {
  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override {}
  void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override {}
};

class SSTReturnPragma : public SSTPragma {
 public:
  SSTReturnPragma(const std::string& replText) :
    repl_(replText)
  {}

  std::string replacement() const {
    return repl_;
  }

 private:
  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override;

  std::string repl_;
};

class SSTGlobalVariablePragma : public SSTPragma {
 public:
  SSTGlobalVariablePragma(const std::string& name) :
    name_(name)
  {}

 private:
  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override;

  std::string name_;
};

class SSTNullVariablePragma : public SSTPragma {
 public:
  SSTNullVariablePragma(clang::SourceLocation loc, clang::CompilerInstance& CI,
                        const std::list<clang::Token>& tokens);

  SSTNullVariablePragma() :
    declAppliedTo_(nullptr),
    transitiveFrom_(nullptr),
    nullSafe_(false), 
    deleteAll_(false),
    skelComputes_(false)
  {}

  virtual SSTNullVariablePragma* clone() const {
    SSTNullVariablePragma* ret = new SSTNullVariablePragma;
    clone_into(ret);
    return ret;
  }

  bool firstPass(const clang::Decl*  /*d*/) const override {
    return true;
  }

  bool hasReplacement() const {
    return !replacement_.empty();
  }

  void setTransitive(SSTNullVariablePragma* prg){
    transitiveFrom_ = prg;
  }

  SSTNullVariablePragma* getTransitive() const {
    return transitiveFrom_;
  }

  bool isTransitive() const {
    return transitiveFrom_;
  }

  clang::NamedDecl* getAppliedDecl() const {
    return declAppliedTo_;
  }

  const std::string& getReplacement() const {
    return replacement_;
  }

  bool hasOnly() const {
    return !nullOnly_.empty();
  }

  bool hasExceptions() const {
    return !nullExcept_.empty() || !nullNew_.empty();
  }

  bool noExceptions() const {
    return nullExcept_.empty() && nullOnly_.empty() && nullNew_.empty();
  }

  bool isOnly(clang::NamedDecl* d) const {
    return nullOnly_.find(d->getNameAsString()) != nullOnly_.end();
  }

  bool isException(clang::NamedDecl* d) const {
    return (nullExcept_.find(d->getNameAsString()) != nullExcept_.end()) ||
           (nullNew_.find(d->getNameAsString()) != nullNew_.end());
  }

  bool isNullifiedNew(clang::NamedDecl* d) const {
    return nullNew_.find(d->getNameAsString()) != nullNew_.end();
  }

  bool keepCtor() const {
    return !nullOnly_.empty() || !nullExcept_.empty() || !nullNew_.empty();
  }

  bool deleteAll() const {
    return deleteAll_;
  }

  bool skeletonizeCompute() const {
    return skelComputes_;
  }

 protected:
  void clone_into(SSTNullVariablePragma* cln) const {
    cln->replacement_ = replacement_;
    cln->nullOnly_ = nullOnly_;
    cln->nullExcept_ = nullExcept_;
    cln->nullNew_ = nullNew_;
    cln->targetNames_ = targetNames_;
    cln->nullSafe_ = nullSafe_;
    cln->deleteAll_ = deleteAll_;
    cln->skelComputes_ = skelComputes_;
  }

  void doActivate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg);

  void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override;
  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override;

  clang::NamedDecl* declAppliedTo_;
  SSTNullVariablePragma* transitiveFrom_;

  std::list<std::string> extras_;
  std::set<std::string> nullOnly_;
  std::set<std::string> nullExcept_;
  std::set<std::string> nullNew_;
  std::string replacement_;
  std::set<std::string> targetNames_;
  bool nullSafe_;
  bool deleteAll_;
  bool skelComputes_;
};

class SSTNullVariableStopPragma : public SSTPragma {
 public:
  SSTNullVariableStopPragma(){}

  void activate(clang::Stmt*  /*s*/, clang::Rewriter&  /*r*/, PragmaConfig& cfg) override {
    cfg.nullifyDeclarationsPragma = nullptr;
  }
};

class SSTNullVariableGeneratorPragma : public SSTPragma {
 public:
  SSTNullVariableGeneratorPragma(clang::SourceLocation  /*loc*/, clang::CompilerInstance&  /*CI*/,
                        const std::list<clang::Token>& tokens) :
    tokens_(tokens)
  {
  }

  SSTNullVariablePragma* generate(clang::Decl* d, clang::CompilerInstance& CI) const {
    return new SSTNullVariablePragma(getStart(d), CI, tokens_);
  }

  void activate(clang::Stmt*  /*s*/, clang::Rewriter&  /*r*/, PragmaConfig& cfg) override {
    cfg.nullifyDeclarationsPragma = this;
  }

 private:
  std::list<clang::Token> tokens_;
};

class SSTNullTypePragma : public SSTNullVariablePragma
{
 public:
  SSTNullTypePragma(clang::SourceLocation loc, clang::CompilerInstance& CI,
                   const std::list<clang::Token>& tokens);

  void activate(clang::Decl *d, clang::Rewriter &r, PragmaConfig &cfg) override;

  SSTNullVariablePragma* clone() const override {
    SSTNullTypePragma* ret = new SSTNullTypePragma;
    ret->newType_ = newType_;
    clone_into(ret);
    return ret;
  }

  std::string newType() const {
    return newType_;
  }

 private:
  std::string newType_;

  //for cloning
  SSTNullTypePragma(){}

};

class SSTDeletePragma : public SSTPragma {
 public:
  SSTDeletePragma(){}
 private:
  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override;
};

class SSTEmptyPragma : public SSTPragma {
 public:
  SSTEmptyPragma(const std::string& body) : body_(body) {}
 private:
  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override;
  std::string body_;
};

class SSTMallocPragma : public SSTDeletePragma {
 public:
  SSTMallocPragma(){}
 private:
  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void visitDeclStmt(clang::DeclStmt* stmt, clang::Rewriter& r);
  void visitBinaryOperator(clang::BinaryOperator* op, clang::Rewriter& r);
};

class SSTKeepPragma : public SSTPragma {
 public:
  SSTKeepPragma(){}
 protected:
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

  void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override;

  void deactivate(PragmaConfig &cfg) override {
    cfg.makeNoChanges = false;
  }
};

class SSTKeepIfPragma : public SSTPragma {
 public:
  SSTKeepIfPragma(const std::string& ifCond)
    : ifCond_(ifCond)
  {}
 private:
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;
  std::string ifCond_;
};

class SSTAssumeTruePragma : public SSTPragma {
 public:
  SSTAssumeTruePragma() {}

 private:
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

};

class SSTAssumeFalsePragma : public SSTPragma {
 public:
  SSTAssumeFalsePragma() {}

 private:
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

};

class SSTBranchPredictPragma : public SSTPragma {
 public:
  SSTBranchPredictPragma(const std::string& prd)
    : prediction_(prd)
  {}
  const std::string& prediction() const {
    return prediction_;
  }
  bool firstPass(const clang::Stmt *s) const override {
    return true;
  }
 private:
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

  std::string prediction_;
};

class SSTOverheadPragma : public SSTPragma {
 public:
  SSTOverheadPragma(const std::string& paramName)
    : paramName_(paramName)
  {}
 private:
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

  std::string paramName_;
};

class SSTAdvanceTimePragma : public SSTPragma {
 public:
  SSTAdvanceTimePragma(clang::SourceLocation loc, clang::CompilerInstance& CI,
                       const std::list<clang::Token>& tokens);

 private:
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;
  std::string units_;
  std::string amount_;
};

class SSTCallFunctionPragma : public SSTPragma {
 public:
  SSTCallFunctionPragma(clang::SourceLocation loc, clang::CompilerInstance& CI,
                        const std::list<clang::Token> &tokens);

  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

 private:
  std::string repl_;
};

class SSTNewPragma : public SSTPragma {
 public:
  SSTNewPragma(){}

 private:
  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter &r, PragmaConfig& cfg) override;
  void visitDeclStmt(clang::DeclStmt *stmt, clang::Rewriter &r);
  void visitBinaryOperator(clang::BinaryOperator *op, clang::Rewriter& r);
};

class SSTNonnullFieldsPragma : public SSTNullVariablePragma {
 public:
  SSTNonnullFieldsPragma(clang::SourceLocation loc, clang::CompilerInstance& CI,
                        const std::list<clang::Token>& tokens);

 private:
  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter &r, PragmaConfig& cfg) override;
  bool firstPass(const clang::Decl*  /*d*/) const override { return false; }
  std::set<std::string> nonnullFields_;

};

class SSTNullFieldsPragma : public SSTNullVariablePragma {
 public:
  SSTNullFieldsPragma(clang::SourceLocation loc, clang::CompilerInstance& CI,
                      const std::list<clang::Token>& tokens);

 private:
  bool firstPass(const clang::Decl*  /*d*/) const override { return false; }
  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter &r, PragmaConfig& cfg) override;

  std::set<std::string> nullFields_;
};

#endif
