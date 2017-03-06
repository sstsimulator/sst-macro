#include "frontendActions.h"
#include "globalVarNamespace.h"
#include <sstream>
#include <fstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;


void
MyFrontendAction::EndSourceFileAction()
{
  SourceManager &SM = TheRewriter.getSourceMgr();

  if (mainFxn){
    std::stringstream sstr;
    sstr << "#define SSTPP_QUOTE(name) #name\n"
         << "#define SSTPP_STR(name) SSTPP_QUOTE(name)\n"
         << "#define SST_APP_NAME_QUOTED SSTPP_STR(sstmac_app_name)\n"
         << "const char SSTMAC_USER_APPNAME[] = SST_APP_NAME_QUOTED;\n"
         << "#undef main\n"
         << "#define main SSTMAC_USER_MAIN\n";
    TheRewriter.InsertText(mainFxn->getLocStart(), sstr.str(), false);
  }

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
  TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(fs);
  fs.close();

  std::ofstream ofs(sstGlobalFile.c_str());
  if (ofs.good()){
    //add the header files needed
    ofs << "#include <sstmac/software/process/global.h>\n\n";
    globalNS.genSSTCode(ofs,"");
    if (mainFxn){
      ofs << "int user_skeleton_main_init_fxn(const char* name, int (*foo)(int,char**));\n"
         << "extern const char SSTMAC_USER_APPNAME[];\n"
         << "extern \"C\" int SSTMAC_USER_MAIN(int argc, char** argv);\n"
         << "static int dont_ignore_this = user_skeleton_main_init_fxn(SSTMAC_USER_APPNAME, SSTMAC_USER_MAIN);\n\n";
    }
  } else {
    llvm::errs() << "Failed opening " << sstGlobalFile << "\n";
    abort();
  }
  ofs.close();
}
