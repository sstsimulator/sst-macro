#ifndef bin_clang_findAstVisitor_h
#define bin_clang_findAstVisitor_h

#include "clangHeaders.h"
#include "globalVarNamespace.h"

class FindGlobalASTVisitor : public clang::RecursiveASTVisitor<FindGlobalASTVisitor> {
 public:
  FindGlobalASTVisitor(clang::Rewriter &R, clang::CompilerInstance& C,
                       GlobalVarNamespace& ns, clang::FunctionDecl** mainPtr) :
    TheRewriter(R), CI(C), globalNS(ns), mainFxn(mainPtr){}

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

  static bool validSrc(const std::string& filename);

  /**
   * @brief TraverseNamespaceDecl We have to traverse namespaces.
   *        We need pre and post operations. We have to explicitly recurse subnodes.
   * @param D
   * @return
   */
  bool TraverseNamespaceDecl(clang::NamespaceDecl* D);

 private:
  clang::Rewriter& TheRewriter;
  clang::CompilerInstance& CI;
  clang::FunctionDecl** mainFxn;
  GlobalVarNamespace& globalNS;
  GlobalVarNamespace* currentNS;
  std::map<clang::NamedDecl*,std::string> globals;

};

#endif
