#include "frontendActions.h"
#include "globalVarNamespace.h"
#include "pragmas.h"
#include <sstream>
#include <fstream>
#include <iostream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;


ReplaceAction::ReplaceAction() :
  visitor_(rewriter_, globalNs_, deletedExprs_)
{
}

#define scase(type,s,pp) \
  case(clang::Stmt::type##Class): \
    return visit##type(clang::cast<type>(s),pp)

void
ReplaceAction::initPragmas(CompilerInstance& CI)
{
  CI.getPreprocessor().AddPragmaHandler("sst",
    new SSTDeletePragmaHandler(visitor_.getPragmas(), CI, visitor_, deletedExprs_));
  CI.getPreprocessor().AddPragmaHandler("sst",
    new SSTMallocPragmaHandler(visitor_.getPragmas(), CI, visitor_, deletedExprs_));
  CI.getPreprocessor().AddPragmaHandler("sst",
    new SSTNewPragmaHandler(visitor_.getPragmas(), CI, visitor_, deletedExprs_));
}

void
ReplaceAction::EndSourceFileAction()
{
  SourceManager &SM = rewriter_.getSourceMgr();
  std::string sourceFile = SM.getFileEntryForID(SM.getMainFileID())->getName().str();
  std::string sstSourceFile, sstGlobalFile;
  std::size_t lastSlashPos = sourceFile.find_last_of("/");
  if (lastSlashPos == std::string::npos){
    sstSourceFile = "sst." + sourceFile;
    sstGlobalFile = "sstGlobals." + sourceFile + ".cpp";
  } else {
    lastSlashPos++;
    sstSourceFile = sourceFile.substr(0, lastSlashPos) + "sst." + sourceFile.substr(lastSlashPos);
    sstGlobalFile = sourceFile.substr(0, lastSlashPos) + "sstGlobals." + sourceFile.substr(lastSlashPos) + ".cpp";
  }

  std::error_code rc;
  llvm::raw_fd_ostream fs(sstSourceFile, rc, llvm::sys::fs::F_RW);
  rewriter_.getEditBuffer(rewriter_.getSourceMgr().getMainFileID()).write(fs);
  fs.close();



  std::ofstream ofs(sstGlobalFile.c_str());
  if (ofs.good()){
    //add the header files needed
    ofs << "#include <sstmac/software/process/global.h>\n\n";
    globalNs_.genSSTCode(ofs,"");
    if (visitor_.hasCStyleMain()){
      const char* appname = getenv("SSTMAC_APP_NAME");
      if (appname == nullptr){
        llvm::errs() << "Cannot refactor main function unless SSTMAC_APP_NAME environment var is defined\n";
        exit(EXIT_FAILURE);
      }
      ofs << "int user_skeleton_main_init_fxn(const char* name, int (*foo)(int,char**));\n"
         << "extern \"C\" int sstmac_user_main_" << appname << "(int argc, char** argv);\n"
         << "static int dont_ignore_this = user_skeleton_main_init_fxn("
           << "\"" << appname << "\",sstmac_user_main_" << appname << ");\n\n";
    }
  } else {
    llvm::errs() << "Failed opening " << sstGlobalFile << "\n";
    exit(EXIT_FAILURE);
  }
  ofs.close();
}
