#include "util.h"

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

bool
isValidSrc(const std::string& filename){
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

bool
isCxx(const std::string& filename){
  //this is really dirty and not very resilient - but I don't know how to fix this yet
  //for now just check to see if this is actually a valid source file
  size_t size = filename.size();
  if (size == 0) return false;
  std::string suffix4; if (size >= 4) suffix4 = filename.substr(size-4,3);
  std::string suffix3; if (size >= 3) suffix3 = filename.substr(size-3,3);
  bool valid = suffix4 == ".cpp" || suffix3 == ".cc" || suffix4 == ".cxx";
  return valid;
}

void
errorAbort(SourceLocation loc, CompilerInstance &CI, const std::string &error)
{
  std::string errorStr;
  llvm::raw_string_ostream os(errorStr);
  loc.print(os, CI.getSourceManager());
  os << " error: " << error;
  std::cerr << os.str() << std::endl;
  abort();
}


