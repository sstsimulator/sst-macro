
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

struct SSTReplacePragma;
struct SSTNullVariablePragma;
struct SSTNullVariableGeneratorPragma;
struct PragmaConfig {
  int pragmaDepth;
  bool makeNoChanges;
  std::map<std::string,SSTReplacePragma*> replacePragmas;
  std::map<clang::Decl*,SSTNullVariablePragma*> nullVariables;
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
    StopNullDeclarations=22
  } class_t;
  clang::StringRef name;
  clang::SourceLocation startLoc;
  clang::SourceLocation endLoc;
  clang::CompilerInstance* CI;
  SkeletonASTVisitor* visitor;
  std::set<clang::Stmt*>* deleted;
  SSTPragmaList* pragmaList;
  class_t cls;

  void print(){
    std::cout << "pragma " << name.str() << " from "
              << startLoc.printToString(CI->getSourceManager())
              << " to " << endLoc.printToString(CI->getSourceManager())
              << std::endl;
  }

  template <class T>
  bool matches(T* s){
    return startLoc < s->getLocStart() && s->getLocStart() <= endLoc;
  }

  virtual bool reusable() const {
    return false;
  }

  /**
   * @brief firstPass AST gets visited twice - once in a first pass to fill
   * in declarations/definitions a second pass to actually make changes.
   * Some pragmas need to be visited on the first pass. Most do not.
   * @return
   */
  virtual bool firstPass() const {
    return false;
  }

  SSTPragma(class_t _cls) : cls(_cls){}

  virtual void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) = 0;
  virtual void activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig &cfg){} //not required
  virtual void deactivate(PragmaConfig& cfg){} //not required

  static void tokenStreamToString(clang::SourceLocation loc,
      std::list<clang::Token>::const_iterator beg,
      std::list<clang::Token>::const_iterator end,
      std::ostream& os, clang::CompilerInstance& CI);

};

std::string getLiteralDataAsString(const clang::Token& tok);

void getLiteralDataAsString(const clang::Token &tok, std::ostream& os);

class SSTReturnPragma : public SSTPragma {
 public:
  SSTReturnPragma(clang::SourceLocation loc,
                  clang::CompilerInstance& CI,
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
  SSTGlobalVariablePragma(clang::SourceLocation loc,
                  clang::CompilerInstance& CI,
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
  SSTNullVariablePragma(clang::SourceLocation loc,
                        clang::CompilerInstance& CI,
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
  SSTNullVariableGeneratorPragma(clang::SourceLocation loc,
                        clang::CompilerInstance& CI,
                        const std::list<clang::Token>& tokens) :
    tokens_(tokens),
    SSTPragma(StartNullDeclarations)
  {
  }

  SSTNullVariablePragma* generate(clang::Decl* d, clang::CompilerInstance& CI) const {
    return new SSTNullVariablePragma(d->getLocStart(), CI, tokens_);
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
  SSTNullTypePragma(clang::SourceLocation loc,
                   clang::CompilerInstance& CI,
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
  SSTNonnullFieldsPragma(clang::SourceLocation loc,
                        clang::CompilerInstance& CI,
                        const std::list<clang::Token>& tokens);

 private:
  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void activate(clang::Decl* d, clang::Rewriter &r, PragmaConfig& cfg) override;
  bool firstPass() const override { return false; }
  std::set<std::string> nonnullFields_;

};

class SSTNullFieldsPragma : public SSTNullVariablePragma {
 public:
  SSTNullFieldsPragma(clang::SourceLocation loc,
                        clang::CompilerInstance& CI,
                        const std::list<clang::Token>& tokens);

 private:
  bool firstPass() const override { return false; }
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
          if (p->firstPass()){
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

 protected:
  SSTPragmaHandler(const char* name, SSTPragmaList& plist,
                   clang::CompilerInstance& ci,
                   SkeletonASTVisitor& visitor,
                   std::set<clang::Stmt*>& deleted) :
    PragmaHandler(name), pragmas_(plist), ci_(ci),
    deleted_(deleted), visitor_(visitor)
  {}
  SSTPragmaList& pragmas_;
  clang::CompilerInstance& ci_;
  SkeletonASTVisitor& visitor_;
  std::set<clang::Stmt*>& deleted_;

  /**
   * @brief configure Assuming the PP lex position is currently on eod,
   *        configure the source locations of the pragma and initialize fields
   * @param PP
   * @param pragma
   */
  void configure(clang::Token& PragmaTok, clang::Preprocessor& PP, SSTPragma* pragma);

};

/**
 * @brief The SSTSimplePragmaHandler_base class
 * Used for pragmas of the form #pragma sst action
 * Here action is a single keyword that takes no arguments and acts on the next expression
 */
class SSTSimplePragmaHandler_base : public SSTPragmaHandler {
 public:
  void HandlePragma(clang::Preprocessor &PP,
                    clang::PragmaIntroducerKind Introducer,
                    clang::Token &PragmaTok);

 protected:
  SSTSimplePragmaHandler_base(const char* name, SSTPragmaList& plist,
                              clang::CompilerInstance& CI,
                              SkeletonASTVisitor& visitor,
                              std::set<clang::Stmt*>& deld) :
    SSTPragmaHandler(name, plist, CI, visitor, deld)
  {}

 private:
  virtual SSTPragma* allocatePragma() = 0;
};

template <class T>
class SSTSimplePragmaHandler : public SSTSimplePragmaHandler_base
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
                         SkeletonASTVisitor& visitor,
                         std::set<clang::Stmt*>& deld) :
    SSTSimplePragmaHandler_base(name, plist, CI, visitor, deld)
  {}

 private:
  /**
   * @brief allocatePragma
   * @return A derived type that performs the correct pragma operation for name
   */
  SSTPragma* allocatePragma(){
    return new T;
  }
};

class SSTDeletePragmaHandler : public SSTSimplePragmaHandler<SSTDeletePragma> {
 public:
  SSTDeletePragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                         SkeletonASTVisitor& visitor, std::set<clang::Stmt*>& deld) :
    SSTSimplePragmaHandler<SSTDeletePragma>("delete", plist, CI, visitor, deld)
  {}
};

class SSTMallocPragmaHandler : public SSTSimplePragmaHandler<SSTMallocPragma> {
 public:
  SSTMallocPragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                         SkeletonASTVisitor& visitor, std::set<clang::Stmt*>& deld) :
   SSTSimplePragmaHandler<SSTMallocPragma>("malloc", plist, CI, visitor, deld)
  {}
};

class SSTNewPragmaHandler : public SSTSimplePragmaHandler<SSTNewPragma> {
 public:
  SSTNewPragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                      SkeletonASTVisitor& visitor, std::set<clang::Stmt*>& deld) :
   SSTSimplePragmaHandler<SSTNewPragma>("new", plist, CI, visitor, deld)
  {}
};

class SSTKeepPragmaHandler : public SSTSimplePragmaHandler<SSTKeepPragma> {
 public:
  SSTKeepPragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                      SkeletonASTVisitor& visitor, std::set<clang::Stmt*>& deld) :
   SSTSimplePragmaHandler<SSTKeepPragma>("keep", plist, CI, visitor, deld)
  {}
};

class SSTTokenStreamPragmaHandler : public SSTPragmaHandler
{
 public:
  void HandlePragma(clang::Preprocessor &PP,
                   clang::PragmaIntroducerKind Introducer,
                   clang::Token &PragmaTok);

 protected:
  /**
   * @brief SSTSimplePragmaHandler Constructor for pragma handlers for pragmas of the form
   *        #pragma sst name
   * @param name  The string identifying the pragma
   * @param plist The pragma list to append to
   */
  SSTTokenStreamPragmaHandler(const char* name, SSTPragmaList& plist,
                         clang::CompilerInstance& CI,
                         SkeletonASTVisitor& visitor,
                         std::set<clang::Stmt*>& deld) :
    SSTPragmaHandler(name, plist, CI, visitor, deld)
  {}

 private:
  virtual SSTPragma* allocatePragma(clang::SourceLocation loc, //for error printing
                                    const std::list<clang::Token>& tokens) const = 0;
};

class SSTNullTypePragmaHandler : public SSTTokenStreamPragmaHandler
{
 public:
  SSTNullTypePragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor,
                        std::set<clang::Stmt*>& deld) :
     SSTTokenStreamPragmaHandler("null_type", plist, CI, visitor, deld){}

 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc,
                            const std::list<clang::Token> &tokens) const;
};

class SSTKeepIfPragmaHandler : public SSTTokenStreamPragmaHandler
{
 public:
  SSTKeepIfPragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor,
                        std::set<clang::Stmt*>& deld) :
     SSTTokenStreamPragmaHandler("keep_if", plist, CI, visitor, deld){}

 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc,
                            const std::list<clang::Token> &tokens) const;
};

class SSTEmptyPragmaHandler : public SSTTokenStreamPragmaHandler {
 public:
  SSTEmptyPragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                         SkeletonASTVisitor& visitor, std::set<clang::Stmt*>& deld) :
    SSTTokenStreamPragmaHandler("empty", plist, CI, visitor, deld)
  {}
 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc,
                           const std::list<clang::Token> &tokens) const;
};

class SSTNullVariablePragmaHandler : public SSTTokenStreamPragmaHandler
{
 public:
  SSTNullVariablePragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor,
                        std::set<clang::Stmt*>& deld) :
     SSTTokenStreamPragmaHandler("null_variable", plist, CI, visitor, deld){}

 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc,
                            const std::list<clang::Token> &tokens) const;
};

class SSTNullVariableGeneratorPragmaHandler : public SSTTokenStreamPragmaHandler
{
 public:
  SSTNullVariableGeneratorPragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor,
                        std::set<clang::Stmt*>& deld) :
     SSTTokenStreamPragmaHandler("start_null_variable", plist, CI, visitor, deld){}

 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc,
                            const std::list<clang::Token> &tokens) const;
};

class SSTNullVariableStopPragmaHandler : public SSTSimplePragmaHandler<SSTNullVariableStopPragma>
{
 public:
  SSTNullVariableStopPragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor,
                        std::set<clang::Stmt*>& deld) :
     SSTSimplePragmaHandler<SSTNullVariableStopPragma>("stop_null_variable", plist, CI, visitor, deld){}
};


class SSTGlobalVariablePragmaHandler : public SSTTokenStreamPragmaHandler
{
 public:
  SSTGlobalVariablePragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor,
                        std::set<clang::Stmt*>& deld) :
     SSTTokenStreamPragmaHandler("global", plist, CI, visitor, deld){}

 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc,
                            const std::list<clang::Token> &tokens) const;

};

class SSTReturnPragmaHandler : public SSTTokenStreamPragmaHandler
{
 public:
  SSTReturnPragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor,
                        std::set<clang::Stmt*>& deld) :
     SSTTokenStreamPragmaHandler("return", plist, CI, visitor, deld){}

 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc,
                            const std::list<clang::Token> &tokens) const;

};

class SSTBranchPredictPragmaHandler : public SSTTokenStreamPragmaHandler
{
 public:
  SSTBranchPredictPragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        SkeletonASTVisitor& visitor,
                        std::set<clang::Stmt*>& deld) :
     SSTTokenStreamPragmaHandler("branch_predict", plist, CI, visitor, deld){}

 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc,
                            const std::list<clang::Token> &tokens) const;

};

class SSTAdvanceTimePragmaHandler : public SSTTokenStreamPragmaHandler
{
 public:
  SSTAdvanceTimePragmaHandler(SSTPragmaList& plist,
                       clang::CompilerInstance& CI,
                       SkeletonASTVisitor& visitor,
                       std::set<clang::Stmt*>& deld) :
    SSTTokenStreamPragmaHandler("advance_time", plist, CI, visitor, deld){}

 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc,
                            const std::list<clang::Token> &tokens) const;

};

class SSTCallFunctionPragmaHandler : public SSTTokenStreamPragmaHandler
{
 public:
  SSTCallFunctionPragmaHandler(SSTPragmaList& plist,
                       clang::CompilerInstance& CI,
                       SkeletonASTVisitor& visitor,
                       std::set<clang::Stmt*>& deld) :
    SSTTokenStreamPragmaHandler("call", plist, CI, visitor, deld){}

 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc,
                            const std::list<clang::Token> &tokens) const;

};

class SSTOverheadPragmaHandler : public SSTTokenStreamPragmaHandler
{
 public:
  SSTOverheadPragmaHandler(SSTPragmaList& plist,
                       clang::CompilerInstance& CI,
                       SkeletonASTVisitor& visitor,
                       std::set<clang::Stmt*>& deld) :
    SSTTokenStreamPragmaHandler("overhead", plist, CI, visitor, deld){}

 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc,
                            const std::list<clang::Token> &tokens) const;

};

class SSTNonnullFieldsPragmaHandler : public SSTTokenStreamPragmaHandler
{
public:
 SSTNonnullFieldsPragmaHandler(SSTPragmaList& plist,
                      clang::CompilerInstance& CI,
                      SkeletonASTVisitor& visitor,
                      std::set<clang::Stmt*>& deld) :
   SSTTokenStreamPragmaHandler("nonnull_fields", plist, CI, visitor, deld){}

private:
 SSTPragma* allocatePragma(clang::SourceLocation loc,
                           const std::list<clang::Token> &tokens) const;
};

class SSTNullFieldsPragmaHandler : public SSTTokenStreamPragmaHandler
{
public:
 SSTNullFieldsPragmaHandler(SSTPragmaList& plist,
                      clang::CompilerInstance& CI,
                      SkeletonASTVisitor& visitor,
                      std::set<clang::Stmt*>& deld) :
   SSTTokenStreamPragmaHandler("null_fields", plist, CI, visitor, deld){}

private:
 SSTPragma* allocatePragma(clang::SourceLocation loc,
                           const std::list<clang::Token> &tokens) const;
};


#endif
