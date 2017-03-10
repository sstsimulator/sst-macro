#include "clangHeaders.h"
#include "globalVarNamespace.h"
#include "findAstVisitor.h"
#include <iostream>
#include <fstream>
#include <stdlib.h>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

void
FindGlobalASTVisitor::initHeaders()
{
  const char* headerListFile = getenv("SSTMAC_HEADERS");
  if (headerListFile == nullptr){
    const char* allHeaders = getenv("SSTMAC_ALL_HEADERS");
    if (allHeaders == nullptr){
      std::cerr << "No header file specified through environment variable SSTMAC_HEADERS" << std::endl;
    }
    return;
  }

  std::ifstream ifs(headerListFile);
  if (!ifs.good()){
    std::cerr << "Bad header list file from environment SSTMAC_HEADERS=" << headerListFile << std::endl;
    abort();
  }

  std::string line;
  char fullpath[256];
  while (ifs.good()){
    std::getline(ifs, line);
    validHeaders.insert(fullpath);
  }
}

bool
FindGlobalASTVisitor::VisitVarDecl(VarDecl* D){
  SourceLocation startLoc = D->getLocStart();
  std::string filename = CI->getSourceManager().getFilename(startLoc).str();

  if (isa<ParmVarDecl>(D)){
    return false;
  }


  FileID id = CI->getSourceManager().getFileID(startLoc);
  SourceLocation headerLoc = CI->getSourceManager().getIncludeLoc(id);
  if (headerLoc.isValid() && !useAllHeaders){
    //we are inside a header
    char fullpathBuffer[1024];
    const char* fullpath = realpath(filename.c_str(), fullpathBuffer);
    //this is not in the list of valid headers, skipping
    if (validHeaders.find(fullpath) == validHeaders.end()){
      return true;
    }
  }

  SourceLocation nextLoc = headerLoc;
  while (nextLoc.isValid()){
    //this might be deep inside nested includes
    headerLoc = nextLoc;
    FileID id = CI->getSourceManager().getFileID(nextLoc);
    nextLoc = CI->getSourceManager().getIncludeLoc(id);
  }

  bool isHeader = headerLoc.isValid();

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

  if (isHeader) os << "\n"; //need to hop down from header line

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
    os << " SSTMAC_DECL_GLOBAL_VAR(" << retType << "," << sstVarName << ")";
    globalsDeclared.insert(varName);
  }
  if (!D->hasExternalStorage()){
    currentNS->vars.insert(sstVarName);
    os << " SSTMAC_DEF_GLOBAL_VAR(" << retType << "," << D->getNameAsString() << "," << sstVarName << ")";
  }
  os << "  ";

  SourceLocation endLoc = headerLoc;
  if (headerLoc.isValid()){
    //the declaration is inside a header file, which means delaying the fetcher function
    //until after the include direction
    //find the end of the header include directive
    //but first figure out what kind of include we have
    Token t; Lexer::getRawToken(headerLoc, t, CI->getSourceManager(), CI->getLangOpts(), true);
    if (t.getKind() == tok::string_literal){ //include "X"
      //just need to find end of string literal
      endLoc = Lexer::getLocForEndOfToken(headerLoc, 0, CI->getSourceManager(), CI->getLangOpts());
    } else if (t.getKind() == tok::less){ //include <X>
      //find end of > token
      while (t.getKind() != tok::greater){
        endLoc = Lexer::getLocForEndOfToken(endLoc, 0, CI->getSourceManager(), CI->getLangOpts());
        Lexer::getRawToken(endLoc, t, CI->getSourceManager(), CI->getLangOpts(), true);
      }
      endLoc = Lexer::getLocForEndOfToken(endLoc, 0, CI->getSourceManager(), CI->getLangOpts());
    }
  } else {
    // find end of decl in source file - need it for replacements */
    endLoc = Lexer::findLocationAfterToken(D->getLocEnd(), tok::semi,
                                 CI->getSourceManager(), CI->getLangOpts(), false);
  }
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

