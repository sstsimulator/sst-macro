
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

class SkeletonASTVisitor;

struct SSTPragma;
struct SSTReplacePragma;
struct SSTNullVariablePragma;
struct SSTNullVariableGeneratorPragma;
struct PragmaConfig {
  int pragmaDepth;
  bool makeNoChanges;
  std::map<std::string,SSTReplacePragma*> replacePragmas;
  std::map<clang::Decl*,SSTNullVariablePragma*> nullVariables;
  std::map<clang::FunctionDecl*,std::set<SSTPragma*>> functionPragmas;
  std::map<const clang::DeclContext*,SSTNullVariablePragma*> nullSafeFunctions;
  std::set<const clang::DeclRefExpr*> deletedRefs;
  std::set<std::string> newParams;
  std::string dependentScopeGlobal;
  SkeletonASTVisitor* astVisitor;
  PragmaConfig() : pragmaDepth(0),
    makeNoChanges(false),
    nullifyDeclarationsPragma(nullptr)
  {}
  std::string computeMemorySpec;
  std::list<std::pair<SSTNullVariablePragma*,clang::TypedefDecl*>> pendingTypedefs;
  SSTNullVariableGeneratorPragma* nullifyDeclarationsPragma;
};

struct SSTPragmaList;
struct SSTPragma {
  typedef enum {
    Replace=0,
    Delete=1,
    Compute=2,
    New=3,
    Keep=4,
    NullVariable=5,
    LoopCount=6,
    Init=7,
    Return=8,
    NullType=9,
    KeepIf=10,
    Memory=11,
    Instead=12,
    BranchPredict=13,
    AdvanceTime=14,
    CallFunction=15,
    AlwaysCompute=16,
    GlobalVariable=17,
    Overhead=18,
    NonnullFields=19,
    NullFields=20,
    StartNullDeclarations=21,
    StopNullDeclarations=22,
    Memoize=23,
    StackAlloc=24,
    ImplicitState=25
  } class_t;
  clang::StringRef name;
  clang::SourceLocation pragmaDirectiveLoc;
  clang::SourceLocation startPragmaLoc;
  clang::SourceLocation endPragmaLoc;
  clang::SourceLocation targetLoc;
  clang::CompilerInstance* CI;
  SkeletonASTVisitor* visitor;
  SSTPragmaList* pragmaList;
  class_t cls;
  int depth;

  void print(){
    std::cout << "pragma " << name.str() << " from "
              << startPragmaLoc.printToString(CI->getSourceManager())
              << " to " << targetLoc.printToString(CI->getSourceManager())
              << std::endl;
  }

  template <class T>
  bool matches(T* s){
    return startPragmaLoc < s->getLocStart() && s->getLocStart() <= targetLoc;
  }

  virtual bool reusable() const {
    return false;
  }

  /**
   * @brief firstPass AST gets visited twice - once in a first pass to fill
   * in declarations/definitions a second pass to actually make changes.
   * Some pragmas need to be visited on the first pass. Most do not.
   * @param d tag parameter, whether declarations should be visited first pass
   * @return
   */
  virtual bool firstPass(const clang::Decl* d) const {
    return false;
  }

  virtual bool firstPass(const clang::Stmt* s) const {
    return false;
  }

  SSTPragma(class_t _cls) : cls(_cls){}

  virtual void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) = 0;
  virtual void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig &cfg){} //not required
  virtual void deactivate(PragmaConfig& cfg){} //not required

  static void tokenStreamToString(
      std::list<clang::Token>::const_iterator beg,
      std::list<clang::Token>::const_iterator end,
      std::ostream& os, clang::CompilerInstance& CI);

};

std::string getLiteralDataAsString(const clang::Token& tok);

void getLiteralDataAsString(const clang::Token &tok, std::ostream& os);

class SSTReturnPragma : public SSTPragma {
 public:
  SSTReturnPragma(clang::CompilerInstance& CI,
                  const std::string& replText) :
    repl_(replText), SSTPragma(Return)
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
  SSTGlobalVariablePragma(clang::CompilerInstance& CI,
                          const std::string& name) :
    name_(name), SSTPragma(GlobalVariable)
  {}

 private:
  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override;

  std::string name_;
};

class SSTNullVariablePragma : public SSTPragma {
 public:
  SSTNullVariablePragma(clang::CompilerInstance& CI,
                        const std::list<clang::Token>& tokens);

  SSTNullVariablePragma() : SSTPragma(NullVariable),
    nullSafe_(false), deleteAll_(false),
    declAppliedTo_(nullptr),
    transitiveFrom_(nullptr),
    skelComputes_(false)
  {}

  virtual SSTNullVariablePragma* clone() const {
    SSTNullVariablePragma* ret = new SSTNullVariablePragma;
    clone_into(ret);
    return ret;
  }

  bool firstPass(const clang::Decl* d) const override {
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

  SSTNullVariablePragma(SSTPragma::class_t cls) : SSTPragma(cls){}

  void doActivate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg);

  virtual void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override;
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
  SSTNullVariableStopPragma() : SSTPragma(StopNullDeclarations) {}

  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override {
    cfg.nullifyDeclarationsPragma = nullptr;
  }
};

class SSTNullVariableGeneratorPragma : public SSTPragma {
 public:
  SSTNullVariableGeneratorPragma(clang::CompilerInstance& CI,
                        const std::list<clang::Token>& tokens) :
    tokens_(tokens),
    SSTPragma(StartNullDeclarations)
  {
  }

  SSTNullVariablePragma* generate(clang::Decl* d, clang::CompilerInstance& CI) const {
    return new SSTNullVariablePragma(CI, tokens_);
  }

  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override {
    cfg.nullifyDeclarationsPragma = this;
  }

 private:
  std::list<clang::Token> tokens_;
};

class SSTNullTypePragma : public SSTNullVariablePragma
{
 public:
  SSTNullTypePragma(clang::CompilerInstance& CI,
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
  SSTDeletePragma() : SSTPragma(Delete) {}
 private:
  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override;
};

class SSTEmptyPragma : public SSTPragma {
 public:
  SSTEmptyPragma(const std::string& body) : SSTPragma(Delete), body_(body) {}
 private:
  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override;
  std::string body_;
};

class SSTMallocPragma : public SSTDeletePragma {
 private:
  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void visitDeclStmt(clang::DeclStmt* stmt, clang::Rewriter& r);
  void visitBinaryOperator(clang::BinaryOperator* op, clang::Rewriter& r);
};

class SSTKeepPragma : public SSTPragma {
 public:
  SSTKeepPragma() : SSTPragma(Keep) {}
 protected:
  virtual void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

  virtual void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg) override;

  void deactivate(PragmaConfig &cfg) override {
    cfg.makeNoChanges = false;
  }
};

class SSTKeepIfPragma : public SSTPragma {
 public:
  SSTKeepIfPragma(const std::string& ifCond)
    : ifCond_(ifCond), SSTPragma(KeepIf)
  {}
 private:
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg);
  std::string ifCond_;
};

class SSTBranchPredictPragma : public SSTPragma {
 public:
  SSTBranchPredictPragma(const std::string& prd)
    : prediction_(prd), SSTPragma(BranchPredict)
  {}
  const std::string& prediction() const {
    return prediction_;
  }
  bool reusable() const override {
    return true;
  }
 private:
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

  std::string prediction_;
};

class SSTOverheadPragma : public SSTPragma {
 public:
  SSTOverheadPragma(const std::string& paramName)
    : paramName_(paramName), SSTPragma(Overhead)
  {}
 private:
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

  std::string paramName_;
};

class SSTAdvanceTimePragma : public SSTPragma {
 public:
  SSTAdvanceTimePragma(const std::string& units, const std::string& amount) :
    units_(units), amount_(amount), SSTPragma(AdvanceTime)
  {}

 private:
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg);
  std::string units_;
  std::string amount_;
};

class SSTCallFunctionPragma : public SSTPragma {
 public:
  SSTCallFunctionPragma(const std::string& repl) : repl_(repl), SSTPragma(CallFunction) {}

  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

 private:
  std::string repl_;
};

class SSTNewPragma : public SSTPragma {
 public:
  SSTNewPragma() : SSTPragma(New) {}

 private:
  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter &r, PragmaConfig& cfg) override;
  void visitDeclStmt(clang::DeclStmt *stmt, clang::Rewriter &r);
  void visitBinaryOperator(clang::BinaryOperator *op, clang::Rewriter& r);
};

class SSTNonnullFieldsPragma : public SSTNullVariablePragma {
 public:
  SSTNonnullFieldsPragma(clang::CompilerInstance& CI,
                        const std::list<clang::Token>& tokens);

 private:
  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter &r, PragmaConfig& cfg) override;
  bool firstPass(const clang::Decl* d) const override { return false; }
  std::set<std::string> nonnullFields_;

};

class SSTNullFieldsPragma : public SSTNullVariablePragma {
 public:
  SSTNullFieldsPragma(clang::CompilerInstance& CI,
                      const std::list<clang::Token>& tokens);

 private:
  bool firstPass(const clang::Decl* d) const override { return false; }
  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter &r, PragmaConfig& cfg) override;

  std::set<std::string> nullFields_;
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
    std::list<SSTPragma*> ret = getPulled(t);
    auto end = pragmas.end();
    auto iter=pragmas.begin();
    while (iter != end){
      auto tmp = iter++;
      SSTPragma* p = *tmp;
      bool match = p->matches<T>(t);
      if (match){
        if (firstPass){
          if (p->firstPass(t)){
            if (p->reusable()){
              appendPulled(t,p);
            }
            pragmas.erase(tmp);
            ret.push_back(p);
          }
        } else {
          if (p->reusable()){
            appendPulled(t,p);
          }
          pragmas.erase(tmp);
          ret.push_back(p);
        }
      }
    }
    return ret;
  }

  void appendPulled(clang::Stmt* s, SSTPragma* prg){
    //for "stacked" pragmas, should visit BASE first
    //the BASE is the driving pragma and all the rest
    //are "modifiers" for the base
    pulledStmts[s].push_back(prg);
  }

  void appendPulled(clang::Decl* d, SSTPragma* prg){
    //for "stacked" pragmas, should visit BASE first
    //the BASE is the driving pragma and all the rest
    //are "modifiers" for the base
    pulledDecls[d].push_back(prg);
  }

  std::list<SSTPragma*>
  getPulled(clang::Stmt* s) const {
    auto iter = pulledStmts.find(s);
    if (iter != pulledStmts.end()){
      return iter->second;
    } else {
      return std::list<SSTPragma*>();
    }
  }

  std::list<SSTPragma*>
  getPulled(clang::Decl* d) const {
    auto iter = pulledDecls.find(d);
    if (iter != pulledDecls.end()){
      return iter->second;
    } else {
      return std::list<SSTPragma*>();
    }
  }

  std::list<SSTPragma*> pragmas;
  std::map<clang::Stmt*,std::list<SSTPragma*>> pulledStmts;
  std::map<clang::Decl*,std::list<SSTPragma*>> pulledDecls;
};


class SSTPragmaHandler : public clang::PragmaHandler {

 public:
  void HandlePragma(clang::Preprocessor &PP,
                    clang::PragmaIntroducerKind Introducer,
                    clang::Token &PragmaTok) override;

 protected:
  SSTPragmaHandler(const char* name, SSTPragmaList& plist,
                   clang::CompilerInstance& ci,
                   SkeletonASTVisitor& visitor) :
    PragmaHandler(name), pragmas_(plist), ci_(ci),
    visitor_(visitor)
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
  void configure(clang::Token& PragmaTok, clang::Preprocessor& PP, SSTPragma* pragma);

  virtual SSTPragma* handleSSTPragma(const std::list<clang::Token>& tokens) const = 0;

  SSTPragmaList& pragmas_;
  SkeletonASTVisitor& visitor_;

  /** I hate doing it this way, but Clang sort of forces me
   * I have to register a generic callback for directives
   * However, that callback has no way of knowing what handler gets invoked
   * All I can do is stash information here for the handler to use
   * when it does get invoked */
  friend struct PragmaPPCallback;
  static clang::SourceLocation pragmaDirectiveLoc;

};

template <class T>
class SSTSimplePragmaHandler : public SSTPragmaHandler
{
 protected:
  /**
   * @brief SSTSimplePragmaHandler Constructor for pragma handlers for pragmas of the form
   *        #pragma sst name
   * @param name  The string identifying the pragma
   * @param plist The pragma list to append to
   */
  SSTSimplePragmaHandler(const char* name, SSTPragmaList& plist,
                         clang::CompilerInstance& CI,
                         SkeletonASTVisitor& visitor) :
    SSTPragmaHandler(name, plist, CI, visitor)
  {}

 private:
  /**
   * @brief allocatePragma
   * @return A derived type that performs the correct pragma operation for name
   */
  SSTPragma* handleSSTPragma(const std::list<clang::Token>& tokens) const override {
    return new T;
  }
};

class SSTDeletePragmaHandler : public SSTSimplePragmaHandler<SSTDeletePragma> {
 public:
  SSTDeletePragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                         SkeletonASTVisitor& visitor) :
    SSTSimplePragmaHandler<SSTDeletePragma>("delete", plist, CI, visitor)
  {}
};

class SSTMallocPragmaHandler : public SSTSimplePragmaHandler<SSTMallocPragma> {
 public:
  SSTMallocPragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                         SkeletonASTVisitor& visitor) :
   SSTSimplePragmaHandler<SSTMallocPragma>("malloc", plist, CI, visitor)
  {}
};

class SSTNewPragmaHandler : public SSTSimplePragmaHandler<SSTNewPragma> {
 public:
  SSTNewPragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                      SkeletonASTVisitor& visitor) :
   SSTSimplePragmaHandler<SSTNewPragma>("new", plist, CI, visitor)
  {}
};

class SSTKeepPragmaHandler : public SSTSimplePragmaHandler<SSTKeepPragma> {
 public:
  SSTKeepPragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                      SkeletonASTVisitor& visitor) :
   SSTSimplePragmaHandler<SSTKeepPragma>("keep", plist, CI, visitor)
  {}
};

class SSTStringMapPragmaHandler : public SSTPragmaHandler
{
 public:
  SSTPragma* handleSSTPragma(const std::list<clang::Token>& tokens) const override;

 protected:
  /**
   * @brief SSTSimplePragmaHandler Constructor for pragma handlers for pragmas of the form
   *        #pragma sst name
   * @param name  The string identifying the pragma
   * @param plist The pragma list to append to
   */
  SSTStringMapPragmaHandler(const char* name, SSTPragmaList& plist,
                         clang::CompilerInstance& CI,
                         SkeletonASTVisitor& visitor) :
    SSTPragmaHandler(name, plist, CI, visitor)
  {}

 private:
  /**
   * For standard pragmas of the form #pragma sst myPragma arg1(x) arg2(y,z)
   * Gather a map of the form { "arg1" : {"x"}, "arg2" : {"y", "z" } }
   * @param loc
   * @param args Keys are parameter names, the list of string arguments passed to each one
   * @return the pragma object
   */
  virtual SSTPragma* allocatePragma(const std::map<std::string, std::list<std::string>>& args) const = 0;
};

class SSTNullTypePragmaHandler : public SSTPragmaHandler
{
 public:
  SSTNullTypePragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor) :
     SSTPragmaHandler("null_type", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const override;
};

class SSTKeepIfPragmaHandler : public SSTPragmaHandler
{
 public:
  SSTKeepIfPragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor) :
     SSTPragmaHandler("keep_if", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const override;
};

class SSTEmptyPragmaHandler : public SSTPragmaHandler {
 public:
  SSTEmptyPragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                         SkeletonASTVisitor& visitor) :
    SSTPragmaHandler("empty", plist, CI, visitor)
  {}
 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const override;
};

class SSTNullVariablePragmaHandler : public SSTPragmaHandler
{
 public:
  SSTNullVariablePragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor) :
     SSTPragmaHandler("null_variable", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const override;
};

class SSTNullVariableGeneratorPragmaHandler : public SSTPragmaHandler
{
 public:
  SSTNullVariableGeneratorPragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor) :
     SSTPragmaHandler("start_null_variable", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const override;
};

class SSTNullVariableStopPragmaHandler : public SSTSimplePragmaHandler<SSTNullVariableStopPragma>
{
 public:
  SSTNullVariableStopPragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor) :
     SSTSimplePragmaHandler<SSTNullVariableStopPragma>("stop_null_variable", plist, CI, visitor){}
};


class SSTGlobalVariablePragmaHandler : public SSTPragmaHandler
{
 public:
  SSTGlobalVariablePragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor) :
     SSTPragmaHandler("global", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const override;

};

class SSTReturnPragmaHandler : public SSTPragmaHandler
{
 public:
  SSTReturnPragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor) :
     SSTPragmaHandler("return", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const override;

};

class SSTBranchPredictPragmaHandler : public SSTPragmaHandler
{
 public:
  SSTBranchPredictPragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor) :
     SSTPragmaHandler("branch_predict", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const override;

};

class SSTAdvanceTimePragmaHandler : public SSTPragmaHandler
{
 public:
  SSTAdvanceTimePragmaHandler(SSTPragmaList& plist,
                       clang::CompilerInstance& CI,
                       SkeletonASTVisitor& visitor) :
    SSTPragmaHandler("advance_time", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const override;

};

class SSTCallFunctionPragmaHandler : public SSTPragmaHandler
{
 public:
  SSTCallFunctionPragmaHandler(SSTPragmaList& plist,
                       clang::CompilerInstance& CI,
                       SkeletonASTVisitor& visitor) :
    SSTPragmaHandler("call", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const override;

};

class SSTOverheadPragmaHandler : public SSTPragmaHandler
{
 public:
  SSTOverheadPragmaHandler(SSTPragmaList& plist,
                       clang::CompilerInstance& CI,
                       SkeletonASTVisitor& visitor) :
    SSTPragmaHandler("overhead", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const override;

};

class SSTNonnullFieldsPragmaHandler : public SSTPragmaHandler
{
 public:
  SSTNonnullFieldsPragmaHandler(SSTPragmaList& plist,
                      clang::CompilerInstance& CI,
                      SkeletonASTVisitor& visitor) :
    SSTPragmaHandler("nonnull_fields", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const;
};

class SSTNullFieldsPragmaHandler : public SSTPragmaHandler
{
 public:
  SSTNullFieldsPragmaHandler(SSTPragmaList& plist,
                      clang::CompilerInstance& CI,
                      SkeletonASTVisitor& visitor) :
   SSTPragmaHandler("null_fields", plist, CI, visitor){}

 private:
  SSTPragma* handleSSTPragma(const std::list<clang::Token> &tokens) const;
};

class SSTStackAllocPragma : public SSTPragma
{
 public:
  SSTStackAllocPragma(const std::string& stackSize,
                      const std::string& mdataSize,
                      const std::string& toFree) :
     SSTPragma(StackAlloc), stackSize_(stackSize), mdataSize_(mdataSize), toFree_(toFree) {}

  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg) override;

 private:
  std::string stackSize_;
  std::string mdataSize_;
  std::string toFree_;
};

class SSTStackAllocPragmaHandler : public SSTStringMapPragmaHandler
{
 public:
  SSTStackAllocPragmaHandler(SSTPragmaList& plist,
                      clang::CompilerInstance& CI,
                      SkeletonASTVisitor& visitor) :
    SSTStringMapPragmaHandler("stack", plist, CI, visitor){}

 private:
  SSTPragma* allocatePragma(const std::map<std::string, std::list<std::string>>& args) const override;

};

#endif
