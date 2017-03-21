#ifndef bin_clang_firstVisitor_h
#define bin_clang_firstVisitor_h

#include "clangHeaders.h"
#include "globalVarNamespace.h"
#include "util.h"

class FirstASTVisitor : public clang::RecursiveASTVisitor<FirstASTVisitor> {
 public:
  FirstASTVisitor(GlobalVarNamespace& ns, clang::CompilerInstance& c) :
    CI(c), globalNS(ns), currentNS(&ns), useAllHeaders(false)
  {
    initHeaders();
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
    return false;
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

 private:
  clang::CompilerInstance& CI;
  bool useAllHeaders;
  GlobalVarNamespace* currentNS;
  GlobalVarNamespace& globalNS;
  std::set<std::string> validHeaders;
};

#endif
