#ifndef bin_clang_pragmas_h
#define bin_clang_pragmas_h

#include "clangHeaders.h"
#include "util.h"
#include <set>

class ReplGlobalASTVisitor;

struct SSTPragma {
  clang::StringRef name;
  clang::SourceLocation startLoc;
  clang::SourceLocation endLoc;
  clang::CompilerInstance* CI;
  ReplGlobalASTVisitor* visitor;
  std::set<clang::Expr*>* deleted;

  template <class T>
  bool matches(T* s){
    return startLoc < s->getLocStart() && s->getLocStart() <= endLoc;
  }

  virtual void act(clang::Stmt* s, clang::Rewriter& r) = 0;
  virtual void act(clang::Decl* decl, clang::Rewriter& r) = 0;
};

class SSTDeletePragma : public SSTPragma {
 protected:
  void act(clang::Stmt* s, clang::Rewriter& r);
  void act(clang::Decl* decl, clang::Rewriter& r);
};

class SSTMallocPragma : public SSTDeletePragma {
  void act(clang::Decl *decl, clang::Rewriter &r);
  void act(clang::Stmt *stmt, clang::Rewriter &r);
  void visitDeclStmt(clang::DeclStmt* stmt, clang::Rewriter& r);
  void visitBinaryOperator(clang::BinaryOperator* op, clang::Rewriter& r);
};

class SSTNewPragma : public SSTPragma {
  void act(clang::Decl *decl, clang::Rewriter &r);
  void act(clang::Stmt *stmt, clang::Rewriter &r);
  void visitDeclStmt(clang::DeclStmt *stmt, clang::Rewriter &r);
  void visitCompoundStmt(clang::CompoundStmt *stmt, clang::Rewriter& r);
  void visitBinaryOperator(clang::BinaryOperator *op, clang::Rewriter& r);
  void defaultAct(clang::Stmt* stmt, clang::Rewriter& r);
};

class SSTComputePragma : public SSTPragma {
  friend class ComputeVisitor;

  void act(clang::Decl *decl, clang::Rewriter &r);
  void act(clang::Stmt *stmt, clang::Rewriter &r);
  void defaultAct(clang::Stmt* stmt, clang::Rewriter &r);
  void visitForStmt(clang::ForStmt* stmt, clang::Rewriter& r);
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

#endif
