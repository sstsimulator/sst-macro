#include "clangHeaders.h"
#include "globalVarNamespace.h"
#include "findAstVisitor.h"
#include <iostream>
#include <fstream>
#include <stdlib.h>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;


bool
FindGlobalASTVisitor::VisitVarDecl(VarDecl* D){
  SourceLocation startLoc = D->getLocStart();
  std::string filename = CI->getSourceManager().getFilename(startLoc).str();

  if (isa<ParmVarDecl>(D)){
    return false;
  }

  auto iter = currentNS->validVars.find(D->getName().str());
  if (iter == currentNS->validVars.end())
    return true; //this is not a global variable to worry about

  if (!currentNS->isPrefixSet){
    currentNS->setFilePrefix(filename.c_str());
  }

  std::string str;
  llvm::raw_string_ostream os(str);

  std::string& varRepl = globals[D];
  std::string sstVarName;
  if (D->getStorageClass() == StorageClass::SC_Static){
    //static, local scope
    //we lose static-ness in the deglobalization so make it have a unique name
    sstVarName = currentNS->filePrefix() + D->getNameAsString();
  } else {
    //global, we can keep the name as is
    sstVarName = D->getNameAsString();
  }

  // roundabout way to get the type of the variable
  std::string retType;
  const Type* ty  = D->getType().getTypePtr();
  bool isC99array = ty->isArrayType();
  //varRepl will hold the replacement text that we will use in the map
  if (isC99array){
    const ArrayType* aty = ty->getAsArrayTypeUnsafe();
    retType = QualType::getAsString(aty->getElementType().split()) + "*";
    varRepl = "(" + currentNS->nsPrefix() + "get_" + sstVarName + "())";
  } else {
    retType = QualType::getAsString(D->getType().split()) + "*";
    varRepl = "(*" + currentNS->nsPrefix() + "get_" + sstVarName + "())";
  }


  std::string varName = D->getNameAsString();
  bool notDeclared = globalsDeclared.find(varName) == globalsDeclared.end();
  if (notDeclared){
    os << " extern int __offset_" << sstVarName << "; "
       << "extern int sstmac_global_stacksize; "
       << "static inline " << retType << " get_" << sstVarName << "(){ "
       << " int stack; int* stackPtr = &stack; "
       << " uintptr_t localStorage = ((uintptr_t) stackPtr/sstmac_global_stacksize)*sstmac_global_stacksize; "
       << " char* offsetPtr = *((char**)localStorage) + __offset_" << sstVarName << "; "
       << "return (((" << retType << ")((void*)offsetPtr))); "
       << "}";
    globalsDeclared.insert(varName);
  }
  if (!D->hasExternalStorage()){
    currentNS->replVars.insert(sstVarName);
    os << "void* __ptr_" << sstVarName << " = &" << D->getNameAsString() << "; "
       << "int __sizeof_" << sstVarName << " = sizeof(" << D->getNameAsString() << ");";
  }
  os << "  ";

  SourceLocation endLoc = Lexer::findLocationAfterToken(D->getLocEnd(), tok::semi,
                                 CI->getSourceManager(), CI->getLangOpts(), false);
  TheRewriter.InsertText(endLoc, os.str());
  return true;
}

/**
 * @brief TraverseNamespaceDecl We have to traverse namespaces.
 *        We need pre and post operations. We have to explicitly recurse subnodes.
 * @param D
 * @return
 */
bool
FindGlobalASTVisitor::TraverseNamespaceDecl(NamespaceDecl* D){
  GlobalVarNamespace* stash = currentNS;
  GlobalVarNamespace& next = currentNS->subspaces[D->getNameAsString()];
  next.appendNamespace(currentNS->ns, D->getNameAsString());

  currentNS = &next;
  auto end = D->decls_end();
  for (auto iter=D->decls_begin(); iter != end; ++iter){
    TraverseDecl(*iter);
  }
  currentNS = stash;
  return true;
}

void
FindGlobalASTVisitor::replGlobal(NamedDecl* decl, SourceRange replRng)
{
  auto iter = globals.find(decl);
  if (iter != globals.end()){
    TheRewriter.ReplaceText(replRng, iter->second);
  }
}

