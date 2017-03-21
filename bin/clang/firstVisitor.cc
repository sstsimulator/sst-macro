#include "clangHeaders.h"
#include "globalVarNamespace.h"
#include "firstVisitor.h"
#include <iostream>
#include <fstream>
#include <stdlib.h>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

void
FirstASTVisitor::initHeaders()
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
FirstASTVisitor::VisitVarDecl(VarDecl* D){
  if (isa<ParmVarDecl>(D)){
    return false;
  }

  SourceLocation startLoc = D->getLocStart();
  FileID id = CI.getSourceManager().getFileID(startLoc);
  SourceLocation headerLoc = CI.getSourceManager().getIncludeLoc(id);
  if (headerLoc.isValid() && !useAllHeaders){
    //we are inside a header
    char fullpathBuffer[1024];
    std::string filename = CI.getSourceManager().getFilename(startLoc).str();
    const char* fullpath = realpath(filename.c_str(), fullpathBuffer);
    //this is not in the list of valid headers, skipping
    if (validHeaders.find(fullpath) == validHeaders.end()){
      return true;
    }
  }

  currentNS->validVars.insert(D->getName().str());
  return true;
}


/**
 * @brief TraverseNamespaceDecl We have to traverse namespaces.
 *        We need pre and post operations. We have to explicitly recurse subnodes.
 * @param D
 * @return
 */
bool
FirstASTVisitor::TraverseNamespaceDecl(NamespaceDecl* D){
  SourceLocation startLoc = D->getLocStart();
  std::string filename = CI.getSourceManager().getFilename(startLoc).str();
  if (!isValidSrc(filename)){
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


