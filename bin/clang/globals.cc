#include "clangHeaders.h"
#include "globalVarNamespace.h"
#include "frontendActions.h"

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

static llvm::cl::OptionCategory ToolingSampleCategory("Tooling Sample");

int main(int argc, const char** argv) {
  CommonOptionsParser op(argc, argv, ToolingSampleCategory);
  ClangTool Tool(op.getCompilations(), op.getSourcePathList());
  int rc =  Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
  return rc;
}




