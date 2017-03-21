#include "clangHeaders.h"
#include "globalVarNamespace.h"
#include "frontendActions.h"

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

static llvm::cl::OptionCategory ToolingSampleCategory("Tooling Sample");

int main(int argc, const char** argv) {
  CommonOptionsParser op(argc, argv, ToolingSampleCategory);
  const std::vector<std::string>& sources = op.getSourcePathList();
  //we parse the original source, modify the pp-ed source
  std::vector<std::string> modSources(1); modSources[0] = "pp." + sources[0];
  ClangTool FirstPassTool(op.getCompilations(), sources);
  int rc = FirstPassTool.run(newFrontendActionFactory<FindAction>().get());
  if (rc != 0)
    return rc;

  ClangTool Src2SrcTool(op.getCompilations(), modSources);
  rc =  Src2SrcTool.run(newFrontendActionFactory<ReplaceAction>().get());
  return rc;
}




