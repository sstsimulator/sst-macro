#include "clangHeaders.h"
#include "globalVarNamespace.h"
#include "findAstVisitor.h"
#include <iostream>

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

  if (!validSrc(filename)){
    return false;
  }

  if (currentNS->isPrefixSet){
    currentNS->setFilePrefix(filename.c_str());
  }

  FileID id = CI->getSourceManager().getFileID(startLoc);
  SourceLocation headerLoc = CI->getSourceManager().getIncludeLoc(id);

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

  if (!D->hasExternalStorage()){
    //we need to track non-extern variables
    //we must create a tmp C++ file that explicitly defines them
    currentNS->vars.insert(sstVarName);
    os << "void* __ptr_" << sstVarName
       << " = &" << D->getNameAsString() << ";\n";
    os << "const int __sizeof_" << sstVarName
       << " = sizeof(" << D->getNameAsString() << ");\n";
  }

  //if (D->isStaticDataMember()){
  //  return false;
  //}

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

  // add the variable that stores the TLS offset
  os << "extern int __offset_" << sstVarName << ";\n"
     << "extern int sstmac_global_stacksize;\n";
  // add the line function that fetches the TLS storage location
  os << "static inline " << retType
     << " get_" << sstVarName << "(){\n"
     << " int stack; int* stackPtr = &stack;\n"
     << " uintptr_t localStorage = ((uintptr_t) stackPtr/sstmac_global_stacksize)*sstmac_global_stacksize;\n" //find bottom of stack
     << " char* offsetPtr = *((char**)localStorage) + __offset_" << sstVarName << ";\n" //add the variable's offset to get the TLS
     << " return (((" << retType << ")((void*)offsetPtr)));\n"
     << "}\n";


  /** find end of decl - need it for replacements */
  SourceLocation endLoc = Lexer::findLocationAfterToken(D->getLocEnd(), tok::semi,
                               CI->getSourceManager(), CI->getLangOpts(), true);
  TheRewriter.InsertText(endLoc, os.str());
  return true;
}


bool
FindGlobalASTVisitor::validSrc(const std::string& filename){
  //this is really dirty and not very resilient - but I don't know how to fix this yet
  //for now just check to see if this is actually a valid source file
  size_t size = filename.size();
  if (size == 0) return false;
  std::string suffix4; if (size >= 4) suffix4 = filename.substr(size-4,3);
  std::string suffix3; if (size >= 3) suffix3 = filename.substr(size-3,3);
  std::string suffix2 = filename.substr(size-2,2);
  bool valid = suffix4 == ".cpp" || suffix3 == ".cc" || suffix2 == ".c" || suffix4 == ".cxx";
  return valid;
}

/**
 * @brief TraverseNamespaceDecl We have to traverse namespaces.
 *        We need pre and post operations. We have to explicitly recurse subnodes.
 * @param D
 * @return
 */
bool
FindGlobalASTVisitor::TraverseNamespaceDecl(NamespaceDecl* D){
  SourceLocation startLoc = D->getLocStart();
  std::string filename = CI->getSourceManager().getFilename(startLoc).str();
  if (!validSrc(filename)){
    return false;
  }

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

bool
FindGlobalASTVisitor::printNewDeclRef(DeclRefExpr* expr, PrettyPrinter& pp)
{
  NamedDecl* decl = expr->getFoundDecl();
  auto iter = globals.find(decl);
  if (iter == globals.end()){
    pp.print(expr);
    return false;
  } else {
    pp.os << iter->second;
    return true;
  }
}

void
FindGlobalASTVisitor::replGlobal(NamedDecl* decl, SourceRange replRng)
{
  auto iter = globals.find(decl);
  if (iter != globals.end()){
    TheRewriter.ReplaceText(replRng, iter->second);
  }
}

