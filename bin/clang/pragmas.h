
/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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
#include "clangGlobals.h"
#include "util.h"
#include <set>
#include <cstdint>
#include <functional>

template <typename T>             
std::uintptr_t pragmaID() noexcept {
  static const char pragmaID = 0; 
  return std::uintptr_t(&pragmaID);               
}  

namespace pragmas {
  enum PassType {
    MODIFY_AST = 1,
    ANALYZE = 2,
    REWRITE = 4
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
  bool deleteOnUse;
  std::uintptr_t classId;

  void print(){
    std::cout << "pragma " << name.str() << " from "
              << startPragmaLoc.printToString(CompilerGlobals::SM())
              << " to " << targetLoc.printToString(CompilerGlobals::SM())
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
  virtual bool firstPass() const {
    return false;
  }

  static std::string getSingleString(const std::list<clang::Token>& tokens);

  //TODO this could really use some documentation, what does it parse and return
  static PragmaArgMap getMap(clang::SourceLocation loc, const std::list<clang::Token>& tokens);

  SSTPragma() = default;
  int getActiveMode() const;

  virtual void activate(clang::Stmt* s) = 0;
  virtual void activate(clang::Decl* /*d*/){} //not required
  virtual void deactivate(){} //not required

  static void tokenStreamToString(
      std::list<clang::Token>::const_iterator beg,
      std::list<clang::Token>::const_iterator end,
      std::ostream& os);

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
          if (p->firstPass()){
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
#if CLANG_VERSION_MAJOR >= 9
  void HandlePragma(clang::Preprocessor &PP,
                    clang::PragmaIntroducer Introducer,
                    clang::Token &PragmaTok) override;
#else
  void HandlePragma(clang::Preprocessor &PP,
                    clang::PragmaIntroducerKind Introducer,
                    clang::Token &PragmaTok) override;
#endif

  bool deleteOnUse() const {
    return deleteOnUse_;
  }

 private:
  void handlePragmaImpl(clang::Preprocessor &PP,
                        clang::Token &PragmaTok);

 protected:
  SSTPragmaHandler(const std::string& name,
                   bool deleteOnUse,
                   SSTPragmaList& plist) :
    PragmaHandler(name), 
    pragmas_(plist), 
    deleteOnUse_(deleteOnUse)
  {}

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
                           SSTPragmaList& plist) :
    SSTPragmaHandler(name, deleteOnUse, plist)
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
     return new T(pragmaLoc_, tokens);
   }
};

template <class T> struct SSTNoArgsPragmaShim : public T
{
  SSTNoArgsPragmaShim(clang::SourceLocation /*loc*/, const std::list<clang::Token>& /*tokens*/) :
    T() {
      this->classId = pragmaID<T>();
    }
};

template <class T> struct SSTStringPragmaShim : public T
{
  using T::getSingleString;
  SSTStringPragmaShim(clang::SourceLocation /*loc*/, const std::list<clang::Token>& tokens) :
    T(getSingleString(tokens)) //just a single string gets passed up
  {
      this->classId = pragmaID<T>();
  }

};

template <class T> struct SSTTokenListPragmaShim : public T
{
  SSTTokenListPragmaShim(clang::SourceLocation loc, const std::list<clang::Token>& tokens) :
    T(loc, tokens) {
      this->classId = pragmaID<T>();
    }
};

template <class T> struct SSTArgMapPragmaShim : public T
{
  using T::getMap;
  SSTArgMapPragmaShim(clang::SourceLocation loc, const std::list<clang::Token>& tokens) :
    T(loc, getMap(loc, tokens))
  { 
      this->classId = pragmaID<T>();
  }
};

class SSTStackAllocPragma : public SSTPragma
{
 public:
  SSTStackAllocPragma(clang::SourceLocation loc, std::map<std::string, std::list<std::string>>&& args);

  void activate(clang::Stmt *s) override;

 private:
  std::string stackSize_;
  std::string mdataSize_;
  std::string toFree_;
};

struct PragmaHandlerFactoryBase {
  virtual clang::PragmaHandler* getHandler(SSTPragmaList& plist) = 0;

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
  getHandler(SSTPragmaList &plist) override {
    return new SSTPragmaHandlerInstance<T>(name(), deleteOnUse, plist);
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

  PragmaHandlerFactoryBase* getFactory(modes::Mode m, const std::string& name);

  const std::set<std::string>& names() const {
    return pragmaNames_;
  }

 private:
  std::string name_;
  std::set<std::string> pragmaNames_;
  std::map<std::string, PragmaHandlerFactoryBase*> factories_[modes::NUM_MODES];

};

template <template <class U, typename...> class PragmaType, class T,
          bool deleteOnUse,
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
  void activate(clang::Stmt* /*s*/) override {}
  void activate(clang::Decl* /*d*/) override {}
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
  void activate(clang::Stmt* s) override;
  void activate(clang::Decl* d) override;

  std::string repl_;
};

class SSTGlobalVariablePragma : public SSTPragma {
 public:
  SSTGlobalVariablePragma(const std::string& name) :
    name_(name)
  {}

 private:
  void activate(clang::Stmt* s) override;
  void activate(clang::Decl* d) override;

  std::string name_;
};

class SSTNullVariablePragma : public SSTPragma {
 public:
  SSTNullVariablePragma(clang::SourceLocation loc, std::map<std::string, std::list<std::string>>&& args);

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

  bool firstPass() const override {
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

  void doActivate(clang::Decl* d);

  void activate(clang::Decl* d) override;
  void activate(clang::Stmt* s) override;

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

class SSTNullTypePragma : public SSTNullVariablePragma
{
 public:
  SSTNullTypePragma(clang::SourceLocation loc, const std::list<clang::Token>& tokens);

  void activate(clang::Decl *d) override;

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
  void activate(clang::Stmt* s) override;
  void activate(clang::Decl* d) override;
};

class SSTEmptyPragma : public SSTPragma {
 public:
  SSTEmptyPragma(const std::string& body) : body_(body) {}
 private:
  void activate(clang::Stmt* s) override;
  void activate(clang::Decl* d) override;
  std::string body_;
};

class SSTMallocPragma : public SSTDeletePragma {
 public:
  SSTMallocPragma(){}
 private:
  void activate(clang::Stmt *stmt) override;
  void visitDeclStmt(clang::DeclStmt* stmt);
  void visitBinaryOperator(clang::BinaryOperator* op);
};

class SSTKeepPragma : public SSTPragma {
 public:
  SSTKeepPragma(){}
 protected:
  void activate(clang::Stmt *s) override;

  void activate(clang::Decl* d) override;

  void deactivate() override {
    CompilerGlobals::pragmaConfig.makeNoChanges = false;
  }
};

class SSTKeepIfPragma : public SSTPragma {
 public:
  SSTKeepIfPragma(const std::string& ifCond)
    : ifCond_(ifCond)
  {}
 private:
  void activate(clang::Stmt *s) override;
  std::string ifCond_;
};

class SSTAssumeTruePragma : public SSTPragma {
 public:
  SSTAssumeTruePragma() {}

 private:
  void activate(clang::Stmt *s) override;

};

class SSTAssumeFalsePragma : public SSTPragma {
 public:
  SSTAssumeFalsePragma() {}

 private:
  void activate(clang::Stmt *s) override;

};

class SSTBranchPredictPragma : public SSTPragma {
 public:
  SSTBranchPredictPragma(const std::string& prd)
    : prediction_(prd)
  {}
  const std::string& prediction() const {
    return prediction_;
  }
  bool firstPass() const override {
    return true;
  }
 private:
  void activate(clang::Stmt *s) override;

  std::string prediction_;
};

class SSTOverheadPragma : public SSTPragma {
 public:
  SSTOverheadPragma(const std::string& paramName)
    : paramName_(paramName)
  {}
 private:
  void activate(clang::Stmt *s) override;

  std::string paramName_;
};

class SSTAdvanceTimePragma : public SSTPragma {
 public:
  SSTAdvanceTimePragma(clang::SourceLocation loc, const std::list<clang::Token>& tokens);

 private:
  void activate(clang::Stmt *s) override;
  std::string units_;
  std::string amount_;
};

class SSTCallFunctionPragma : public SSTPragma {
 public:
  SSTCallFunctionPragma(clang::SourceLocation loc, const std::list<clang::Token> &tokens);

  void activate(clang::Stmt *s) override;

 private:
  std::string repl_;
};

class SSTNewPragma : public SSTPragma {
 public:
  SSTNewPragma(){}

 private:
  void activate(clang::Stmt *stmt) override;
  void activate(clang::Decl* d) override;
  void visitDeclStmt(clang::DeclStmt *stmt);
  void visitBinaryOperator(clang::BinaryOperator *op);
};

class SSTNonnullFieldsPragma : public SSTNullVariablePragma {
 public:
  SSTNonnullFieldsPragma(clang::SourceLocation loc, std::map<std::string, std::list<std::string>>&& args);

 private:
  void activate(clang::Stmt *stmt) override;
  void activate(clang::Decl* d) override;
  bool firstPass() const override { return false; }
  std::set<std::string> nonnullFields_;

};

class SSTNullFieldsPragma : public SSTNullVariablePragma {
 public:
  SSTNullFieldsPragma(clang::SourceLocation loc, std::map<std::string, std::list<std::string>>&& args);

 private:
  bool firstPass() const override { return false; }
  void activate(clang::Stmt *stmt) override;
  void activate(clang::Decl* d) override;

  std::set<std::string> nullFields_;
};

#endif
