#include "frontendActions.h"
#include "globalVarNamespace.h"
#include <sstream>
#include <fstream>
#include <iostream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;


#define scase(type,s,pp) \
  case(clang::Stmt::type##Class): \
    return visit##type(clang::cast<type>(s),pp) \

bool
MyFrontendAction::visitDeclRefExpr(clang::DeclRefExpr* exp, PrettyPrinter& pp)
{
  bool ret = finder.printNewDeclRef(exp,pp);
  return ret;
}

bool
MyFrontendAction::visitBinaryOperator(clang::BinaryOperator* bop, PrettyPrinter& pp)
{
  bool foundGlobal = visitStmt(bop->getLHS(), pp);
  pp.os << bop->getOpcodeStr().str();
  foundGlobal |= visitStmt(bop->getRHS(), pp);
  return foundGlobal;
}

bool
MyFrontendAction::visitCallExpr(CallExpr *exp, PrettyPrinter &pp)
{
  bool foundGlobal = visitStmt(exp->getCallee(),pp);
  pp.os << "(";
  int numArgs = exp->getNumArgs();
  for (int i=0; i < numArgs; ++i){
    if (i > 0) pp.os << ",";
    Expr* arg = exp->getArg(i);
    foundGlobal |= visitStmt(arg, pp);
  }
  pp.os << ")";
  return foundGlobal;
}

bool
MyFrontendAction::visitMemberExpr(MemberExpr *exp, PrettyPrinter &pp)
{
  bool foundGlobal = visitStmt(exp->getBase(), pp);
  if (exp->isArrow()){
    pp.os << "->";
  } else {
    pp.os << ".";
  }
  pp.os << exp->getMemberDecl()->getName();
  return foundGlobal;
}

bool
MyFrontendAction::visitImplicitCastExpr(ImplicitCastExpr *exp, PrettyPrinter &pp)
{
  return visitStmt(exp->getSubExpr(), pp);
}

bool
MyFrontendAction::visitArraySubscriptExpr(ArraySubscriptExpr* exp, PrettyPrinter& pp)
{
  bool foundGlobal = visitStmt(exp->getBase(), pp);
  pp.os << "[";
  foundGlobal |= visitStmt(exp->getIdx(), pp);
  pp.os << "]";
  return foundGlobal;
}

bool
MyFrontendAction::visitParenExpr(ParenExpr *exp, PrettyPrinter &pp)
{
  pp.os << "(";
  bool foundGlobal = visitStmt(exp->getSubExpr(), pp);
  pp.os << ")";
  return foundGlobal;
}

bool
MyFrontendAction::visitCStyleCastExpr(CStyleCastExpr *exp, PrettyPrinter &pp)
{
  pp.os << "(" << QualType::getAsString(exp->getTypeAsWritten().split()) << ")";
  return visitStmt(exp->getSubExpr(), pp);
}

bool
MyFrontendAction::visitCompoundAssignOperator(CompoundAssignOperator *exp, PrettyPrinter &pp)
{
  return visitBinaryOperator(exp, pp);
}

bool
MyFrontendAction::visitStmt(clang::Stmt* s, PrettyPrinter& pp)
{
  switch (s->getStmtClass()){
    scase(BinaryOperator,s,pp);
    scase(DeclRefExpr,s,pp);
    scase(MemberExpr,s,pp);
    scase(ImplicitCastExpr,s,pp);
    scase(CallExpr,s,pp);
    scase(ArraySubscriptExpr,s,pp);
    scase(ParenExpr,s,pp);
    scase(CStyleCastExpr,s,pp);
    scase(CompoundAssignOperator,s,pp);
    default:
      //std::cout << "Missing statement case " << s->getStmtClassName() << std::endl;
      pp.print(s);
      return false;
  }
}

void
MyFrontendAction::VisitMacros()
{
  for (FoundMacro& fm : mlist.macros){
    PrettyPrinter pp;
    bool foundGlobal = visitStmt(fm.stmt,pp);
    if (foundGlobal){
      TheRewriter.ReplaceText(fm.range(), pp.str());
    } else {
      //std::cout << "NOT replacing macro " << pp.str() << std::endl;
    }
  }
}

void
MyFrontendAction::EndSourceFileAction()
{
  SourceManager &SM = TheRewriter.getSourceMgr();

  VisitMacros();

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
