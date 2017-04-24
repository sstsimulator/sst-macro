#include "clangHeaders.h"
#include "globalVarNamespace.h"
#include "frontendActions.h"

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

static llvm::cl::OptionCategory ToolingSampleCategory("Tooling Sample");

int main(int argc, const char** argv) {
  CommonOptionsParser firstOp(argc, argv, ToolingSampleCategory);
  const std::vector<std::string>& sources = firstOp.getSourcePathList();
  //we parse the original source, modify the pp-ed source
  ClangTool Tool(firstOp.getCompilations(), sources);
  int rc = Tool.run(newFrontendActionFactory<ReplaceAction>().get());
  return rc;
}




