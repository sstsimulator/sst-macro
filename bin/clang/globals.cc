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
  ClangTool FirstPassTool(firstOp.getCompilations(), sources);
  int rc = FirstPassTool.run(newFrontendActionFactory<FindAction>().get());
  if (rc != 0)
    return rc;

  const std::string& origSrc = sources[0];

  std::vector<std::string> modSources(1); modSources[0] = "pp." + origSrc;
  const char* nullArgs[4];
  nullArgs[0] = "null";
  nullArgs[1] = modSources[0].c_str();
  nullArgs[2] = "--";
  int nullArgc = 3;

  if (isCxx(origSrc)){
    nullArgs[nullArgc] = "-std=c++1y";
    ++nullArgc;
  }

  CommonOptionsParser secondOp(nullArgc, nullArgs, ToolingSampleCategory);
  ClangTool Src2SrcTool(secondOp.getCompilations(), modSources);
  rc =  Src2SrcTool.run(newFrontendActionFactory<ReplaceAction>().get());
  return rc;
}




