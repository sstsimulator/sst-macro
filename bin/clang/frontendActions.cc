#include "frontendActions.h"
#include "globalVarNamespace.h"
#include "pragmas.h"
#include <sstream>
#include <fstream>
#include <iostream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;


GlobalVarNamespace globals;

FindAction::FindAction() : globalNS(globals) {}

void
FindAction::EndSourceFileAction()
{
}

ReplaceAction::ReplaceAction() : mainFxn(nullptr),
  finder(TheRewriter, globals, &mainFxn),
  replacer(TheRewriter, finder, deleted),
  globalNS(globals)
{
}

#define scase(type,s,pp) \
  case(clang::Stmt::type##Class): \
    return visit##type(clang::cast<type>(s),pp)

void
ReplaceAction::initPragmas(CompilerInstance& CI)
{
  CI.getPreprocessor().AddPragmaHandler("sst",
    new SSTDeletePragmaHandler(replacer.getPragmas(), CI, deleted));
  CI.getPreprocessor().AddPragmaHandler("sst",
    new SSTMallocPragmaHandler(replacer.getPragmas(), CI, deleted));
  CI.getPreprocessor().AddPragmaHandler("sst",
    new SSTNewPragmaHandler(replacer.getPragmas(), CI, deleted));
}

void
ReplaceAction::EndSourceFileAction()
{
  SourceManager &SM = TheRewriter.getSourceMgr();

  std::string sourceFile = SM.getFileEntryForID(SM.getMainFileID())->getName().str();
  std::string suffix2 = sourceFile.substr(sourceFile.size()-2,2);
  bool isC = suffix2 == ".c";

  if (mainFxn && isC){
    std::stringstream sstr;
    sstr << "#define SSTPP_QUOTE(name) #name\n"
         << "#define SSTPP_STR(name) SSTPP_QUOTE(name)\n"
         << "#define SST_APP_NAME_QUOTED SSTPP_STR(sstmac_app_name)\n"
         << "const char* SSTMAC_USER_APPNAME = SST_APP_NAME_QUOTED;\n"
         << "#undef main\n"
         << "#define main SSTMAC_USER_MAIN\n";
    TheRewriter.InsertText(mainFxn->getLocStart(), sstr.str(), false);
  }

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
  TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(fs);
  fs.close();



  std::ofstream ofs(sstGlobalFile.c_str());
  if (ofs.good()){
    //add the header files needed
    ofs << "#include <sstmac/software/process/global.h>\n\n";
    globalNS.genSSTCode(ofs,"");
    if (mainFxn && isC){
      ofs << "int user_skeleton_main_init_fxn(const char* name, int (*foo)(int,char**));\n"
         << "extern const char* SSTMAC_USER_APPNAME;\n"
         << "extern \"C\" int SSTMAC_USER_MAIN(int argc, char** argv);\n"
         << "static int dont_ignore_this = user_skeleton_main_init_fxn(SSTMAC_USER_APPNAME, SSTMAC_USER_MAIN);\n\n";
    }
  } else {
    llvm::errs() << "Failed opening " << sstGlobalFile << "\n";
    abort();
  }
  ofs.close();
}
