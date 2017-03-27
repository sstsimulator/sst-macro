#ifndef bin_clang_pragmas_h
#define bin_clang_pragmas_h

#include "clangHeaders.h"
#include "util.h"
#include <set>

class ReplGlobalASTVisitor;

struct PragmaConfig {
  int pragmaDepth;
  std::map<std::string, std::string> functionReplacements;
  PragmaConfig() : pragmaDepth(0) {}
};

struct SSTPragma {
  clang::StringRef name;
  clang::SourceLocation startLoc;
  clang::SourceLocation endLoc;
  clang::CompilerInstance* CI;
  ReplGlobalASTVisitor* visitor;
  std::set<clang::Expr*>* deleted;
  std::list<SSTPragma*> subPragmas;

  template <class T>
  bool matches(T* s){
    return startLoc < s->getLocStart() && s->getLocStart() <= endLoc;
  }

  virtual void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) = 0;
  virtual void deactivate(clang::Stmt* s, PragmaConfig& cfg){} //not required
};

class SSTDeletePragma : public SSTPragma {
 protected:
  void activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg) override;
};

class SSTMallocPragma : public SSTDeletePragma {
  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void visitDeclStmt(clang::DeclStmt* stmt, clang::Rewriter& r);
  void visitBinaryOperator(clang::BinaryOperator* op, clang::Rewriter& r);
};

class SSTNewPragma : public SSTPragma {
  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void visitDeclStmt(clang::DeclStmt *stmt, clang::Rewriter &r);
  void visitCompoundStmt(clang::CompoundStmt *stmt, clang::Rewriter& r);
  void visitBinaryOperator(clang::BinaryOperator *op, clang::Rewriter& r);
  void defaultAct(clang::Stmt* stmt, clang::Rewriter& r);
};

class SSTComputePragma : public SSTPragma {
  friend class ComputeVisitor;

  void activate(clang::Stmt *stmt, clang::Rewriter &r, PragmaConfig& cfg) override;
  void defaultAct(clang::Stmt* stmt, clang::Rewriter &r);
  void visitForStmt(clang::ForStmt* stmt, clang::Rewriter& r);
};

class SSTReplacePragma : public SSTPragma {
  std::string fxn_;
  std::string replacement_;
 public:
  SSTReplacePragma(const std::string& fxn, const std::string& replace) :
    fxn_(fxn), replacement_(replace)
  {
  }
  void activate(clang::Stmt *s, clang::Rewriter &r, PragmaConfig &cfg);
  void deactivate(clang::Stmt *s, PragmaConfig& cfg);

};

struct SSTPragmaList {
  void push_back(SSTPragma* p){
    pragmas.push_back(p);
  }

  template <class T>
  SSTPragma*
  takeMatch(T* t){
    auto end = pragmas.end();
    for (auto iter=pragmas.begin(); iter != end; ++iter){
      SSTPragma* p = *iter;
      bool match = p->matches<T>(t);
      if (match){
        pragmas.erase(iter);
        return p;
      }
    }
    return nullptr;
  }

  std::list<SSTPragma*> pragmas;
};

class SSTPragmaHandler : public clang::PragmaHandler {

 protected:
  SSTPragmaHandler(const char* name, SSTPragmaList& plist,
                   clang::CompilerInstance& ci,
                   ReplGlobalASTVisitor& visitor,
                   std::set<clang::Expr*>& deleted) :
    PragmaHandler(name), pragmas_(plist), ci_(ci),
    deleted_(deleted), visitor_(visitor)
  {}
  SSTPragmaList& pragmas_;
  clang::CompilerInstance& ci_;
  ReplGlobalASTVisitor& visitor_;
  std::set<clang::Expr*>& deleted_;

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
                              ReplGlobalASTVisitor& visitor,
                              std::set<clang::Expr*>& deld) :
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
                         ReplGlobalASTVisitor& visitor,
                         std::set<clang::Expr*>& deld) :
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
                         ReplGlobalASTVisitor& visitor, std::set<clang::Expr*>& deld) :
    SSTSimplePragmaHandler<SSTDeletePragma>("delete", plist, CI, visitor, deld)
  {}
};

class SSTMallocPragmaHandler : public SSTSimplePragmaHandler<SSTMallocPragma> {
 public:
  SSTMallocPragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                         ReplGlobalASTVisitor& visitor, std::set<clang::Expr*>& deld) :
   SSTSimplePragmaHandler<SSTMallocPragma>("malloc", plist, CI, visitor, deld)
  {}
};

class SSTNewPragmaHandler : public SSTSimplePragmaHandler<SSTNewPragma> {
 public:
  SSTNewPragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                      ReplGlobalASTVisitor& visitor, std::set<clang::Expr*>& deld) :
   SSTSimplePragmaHandler<SSTNewPragma>("new", plist, CI, visitor, deld)
  {}
};

class SSTComputePragmaHandler : public SSTSimplePragmaHandler<SSTComputePragma> {
 public:
  SSTComputePragmaHandler(SSTPragmaList& plist, clang::CompilerInstance& CI,
                      ReplGlobalASTVisitor& visitor, std::set<clang::Expr*>& deld) :
   SSTSimplePragmaHandler<SSTComputePragma>("compute", plist, CI, visitor, deld)
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
                         ReplGlobalASTVisitor& visitor,
                         std::set<clang::Expr*>& deld) :
    SSTPragmaHandler(name, plist, CI, visitor, deld)
  {}

 private:
  virtual SSTPragma* allocatePragma(clang::SourceLocation loc, //for error printing
                                    const std::list<clang::Token>& tokens) const = 0;
};

class SSTOpenMPParallelPragmaHandler : public SSTTokenStreamPragmaHandler
{
 public:
  SSTOpenMPParallelPragmaHandler(SSTPragmaList& plist,
                         clang::CompilerInstance& CI,
                         ReplGlobalASTVisitor& visitor,
                         std::set<clang::Expr*>& deld) :
      SSTTokenStreamPragmaHandler("parallel", plist, CI, visitor, deld){}
 private:
  SSTPragma* allocatePragma(clang::SourceLocation loc, const std::list<clang::Token> &tokens) const {
    //this actually just maps cleanly into a compute pragma
    return new SSTComputePragma;
  }
};

class SSTReplacePragmaHandler : public SSTTokenStreamPragmaHandler
{
public:
 SSTReplacePragmaHandler(SSTPragmaList& plist,
                        clang::CompilerInstance& CI,
                        ReplGlobalASTVisitor& visitor,
                        std::set<clang::Expr*>& deld) :
     SSTTokenStreamPragmaHandler("replace", plist, CI, visitor, deld){}

private:
 SSTPragma* allocatePragma(clang::SourceLocation loc, const std::list<clang::Token> &tokens) const;
};

#endif
