#include "util.h"

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

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


