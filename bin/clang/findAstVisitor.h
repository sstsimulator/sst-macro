#ifndef bin_clang_findAstVisitor_h
#define bin_clang_findAstVisitor_h

#include "clangHeaders.h"
#include "globalVarNamespace.h"
#include "util.h"

class FindGlobalASTVisitor : public clang::RecursiveASTVisitor<FindGlobalASTVisitor> {
 public:
  FindGlobalASTVisitor(clang::Rewriter &R, GlobalVarNamespace& ns, clang::FunctionDecl** mainPtr) :
    TheRewriter(R), globalNS(ns), mainFxn(mainPtr), currentNS(&ns), useAllHeaders(false)
  {
    initHeaders();
  }

  void setCompilerInstance(clang::CompilerInstance& c){
    CI = &c;
  }

  /**
   * @brief VisitVarDecl We only need to visit variables once down the AST.
   *        No pre or post operations.
   * @param D
   * @return
   */
  bool VisitVarDecl(clang::VarDecl* D);

  bool VisitParmVarDecl(clang::ParmVarDecl* D){
    return false;
  }

  bool TraverseRecordDecl(clang::TagDecl* D){
    return false;
  }

  bool TraverseFunctionDecl(clang::FunctionDecl* D){
    if (D->isMain()){
      *mainFxn = D;
    }
    return false;
  }

  const std::map<clang::NamedDecl*,std::string>&
  globalVariables() const {
    return globals;

  }

  void initHeaders();

  static bool validSrc(const std::string& filename);

  /**
   * @brief TraverseNamespaceDecl We have to traverse namespaces.
   *        We need pre and post operations. We have to explicitly recurse subnodes.
   * @param D
   * @return
   */
  bool TraverseNamespaceDecl(clang::NamespaceDecl* D);

  void replGlobal(clang::NamedDecl* decl, clang::SourceRange rng);

  /**
   * @brief printNewDeclRef
   * @param expr
   * @param pp
   * @return If the ref expr referes to a global variable
   */
  bool printNewDeclRef(clang::DeclRefExpr* expr, PrettyPrinter& pp);

  bool isGlobal(clang::DeclRefExpr* expr){
    return globals.find(expr->getFoundDecl()) != globals.end();
  }

 private:
  clang::Rewriter& TheRewriter;
  clang::CompilerInstance* CI;
  clang::FunctionDecl** mainFxn;
  GlobalVarNamespace& globalNS;
  GlobalVarNamespace* currentNS;
  std::map<clang::NamedDecl*,std::string> globals;
  std::set<std::string> globalsDeclared;
  std::set<std::string> validHeaders;
  bool useAllHeaders;

};

#endif
