#include <sstream>
#include <string>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/Expr.h"
#include "clang/Lex/Lexer.h"
#include <iostream>
#include <set>
#include <map>
#include <fstream>
#include <string>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

static llvm::cl::OptionCategory ToolingSampleCategory("Tooling Sample");

struct GlobalVarNamespace
{
  GlobalVarNamespace() : isPrefixSet(false) {}

  std::string ns;
  std::set<std::string> vars;
  std::map<std::string, GlobalVarNamespace> subspaces;
  char uniqueFilePrefix[256];
  bool isPrefixSet;

  bool empty() const {
    return vars.empty() && subspaces.empty();
  }

  void setFilePrefix(const char* name){
    ::strcpy(uniqueFilePrefix, name);
    int len = ::strlen(uniqueFilePrefix);
    for (int i=0; i < len; ++i){
      switch (uniqueFilePrefix[i]){
        case '-':
        case '/':
        case '.':
          uniqueFilePrefix[i] = '_';
          break;
      }
    }
  }

  void appendNamespace(const std::string& nestedNS, const std::string& newNS){
    if (ns.size() == 0){
      ns = nestedNS + "::" + newNS + "::";
    }
  }

  const std::string& nsPrefix() const {
    return ns;
  }

  const char* filePrefix() const {
    return uniqueFilePrefix;
  }

  void genSSTCode(std::ostream& os, const std::string& indent){
    for (const std::string& var : vars){
      os << indent << "int __offset_" << var << " = 0;\n";
      os << indent << "extern const int __sizeof_" << var << ";\n";
      os << indent << "extern void* __ptr_" << var << ";\n";
      os << indent << "sstmac::GlobalVariable __gv_" << var
              << "(__offset_" << var
              << ",__sizeof_" << var
              << ",__ptr_" << var
              << ");\n";
    }
    for (auto& pair : subspaces){
      os << indent << "namespace " << pair.first << " {\n";
      pair.second.genSSTCode(os, indent + " ");
      os << indent << "}\n";
    }
  }

};

std::map<NamedDecl*,std::string> globals;
GlobalVarNamespace globalNamespace;
GlobalVarNamespace* currentNamespace = &globalNamespace;

class FindGlobalASTVisitor : public RecursiveASTVisitor<FindGlobalASTVisitor> {
 public:
  FindGlobalASTVisitor(Rewriter &R, CompilerInstance& C) : TheRewriter(R), CI(C) {}


  /**
   * @brief VisitVarDecl We only need to visit variables once down the AST.
   *        No pre or post operations.
   * @param D
   * @return
   */
  bool VisitVarDecl(VarDecl* D){
    SourceLocation startLoc = D->getLocStart();
    std::string filename = CI.getSourceManager().getFilename(startLoc).str();
    if (!validSrc(filename)){
      return false;
    }

    if (!currentNamespace->isPrefixSet){
      currentNamespace->setFilePrefix(filename.c_str());
    }

    std::string str;
    llvm::raw_string_ostream os(str);

    std::string& varRepl = globals[D];
    std::string sstVarName;
    if (D->getStorageClass() == StorageClass::SC_Static){
      //static, local scope
      //we lose static-ness in the deglobalization so make it have a unique name
      sstVarName = currentNamespace->filePrefix() + D->getNameAsString();
    } else {
      //global, we can keep the name as is
      sstVarName = D->getNameAsString();
    }

    if (!D->hasExternalStorage()){
      //we need to track non-extern variables
      //we must create a tmp C++ file that explicitly defines them
      currentNamespace->vars.insert(sstVarName);
      os << "void* __ptr_" << sstVarName
         << " = &" << D->getNameAsString() << ";\n";
      os << "const int __sizeof_" << sstVarName
         << " = sizeof(" << D->getNameAsString() << ");\n";
    }

    //if (D->isStaticDataMember()){
    //  return false;
    //}

    // roundabout way to get the type of the variable
    std::string retType;
    const Type* ty  = D->getType().getTypePtr();
    bool isC99array = ty->isArrayType();
    if (isC99array){
      const ArrayType* aty = ty->getAsArrayTypeUnsafe();
      retType = QualType::getAsString(aty->getElementType().split()) + "**";
    } else {
      retType = QualType::getAsString(D->getType().split()) + "*";
    }

    // add the variable that stores the TLS offset
    os << "extern int __offset_" << sstVarName << ";\n"
       << "extern int sstmac_global_stacksize;\n";
    // add the line function that fetches the TLS storage location
    os << "static inline " << retType
       << " get_" << sstVarName << "(){\n"
       << " int stack; int* stackPtr = &stack;\n"
       << " uintptr_t localStorage = ((uintptr_t) stackPtr/sstmac_global_stacksize)*sstmac_global_stacksize" //find bottom of stack
       << " + __offset_" << sstVarName << ";\n" //add the variable's offset to get the TLS
       << " return (((" << retType << ")((void*)localStorage)));\n"
       << "}\n";

    //now put the replacement that we will use in the map
    varRepl = "(*" + currentNamespace->nsPrefix() + "get_" + sstVarName + "())";

    /** find end of decl - need it for replacements */
    SourceLocation endLoc = Lexer::findLocationAfterToken(D->getLocEnd(), tok::semi,
                                 CI.getSourceManager(), CI.getLangOpts(), true);
    TheRewriter.InsertText(endLoc, os.str());
    return true;
  }

  bool TraverseFunctionDecl(FunctionDecl* D){
    return false;
  }

  static bool validSrc(const std::string& filename){
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

  /**
   * @brief TraverseNamespaceDecl We have to traverse namespaces.
   *        We need pre and post operations. We have to explicitly recurse subnodes.
   * @param D
   * @return
   */
  bool TraverseNamespaceDecl(NamespaceDecl* D){
    SourceLocation startLoc = D->getLocStart();
    std::string filename = CI.getSourceManager().getFilename(startLoc).str();
    if (!validSrc(filename)){
      return false;
    }

    GlobalVarNamespace* stash = currentNamespace;
    GlobalVarNamespace& next = currentNamespace->subspaces[D->getNameAsString()];
    next.appendNamespace(currentNamespace->ns, D->getNameAsString());

    currentNamespace = &next;
    auto end = D->decls_end();
    for (auto iter=D->decls_begin(); iter != end; ++iter){
      TraverseDecl(*iter);
    }
    currentNamespace = stash;
    return true;
  }

 private:
  Rewriter& TheRewriter;
  CompilerInstance& CI;

};

class ReplGlobalASTVisitor : public RecursiveASTVisitor<ReplGlobalASTVisitor> {
 public:
  ReplGlobalASTVisitor(Rewriter &R, CompilerInstance& C) : TheRewriter(R), CI(C) {}

  bool VisitDeclRefExpr(DeclRefExpr* expr){
    NamedDecl* decl =  expr->getFoundDecl();
    auto iter = globals.find(decl);
    if (iter != globals.end()){
      TheRewriter.ReplaceText(expr->getSourceRange(), iter->second);
    }

    return true;
  }

 private:
  Rewriter& TheRewriter;
  CompilerInstance& CI;

};

class MyASTConsumer : public ASTConsumer {
 public:
  MyASTConsumer(Rewriter &R, CompilerInstance& C) :
    FindVisitor(R,C), ReplVisitor(R,C) {}

  bool HandleTopLevelDecl(DeclGroupRef DR) override {
    for (DeclGroupRef::iterator b = DR.begin(), e = DR.end(); b != e; ++b){
      Decl* d = *b;
      if (!d->isImplicit()){
        //we have to find a particular set of declarations that
        //will need later replacing/processing
        FindVisitor.TraverseDecl(*b);
      }
      //the replace visitor should visit everything in the source file
      ReplVisitor.TraverseDecl(*b);
    }
    return true;
  }

 private:
  FindGlobalASTVisitor FindVisitor;
  ReplGlobalASTVisitor ReplVisitor;

};

class MyFrontendAction : public ASTFrontendAction {
 public:
  MyFrontendAction() {}

  void EndSourceFileAction() override {
    SourceManager &SM = TheRewriter.getSourceMgr();

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
      globalNamespace.genSSTCode(ofs,"");
    } else {
      llvm::errs() << "Failed opening " << sstGlobalFile << "\n";
      abort();
    }
    ofs.close();
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance& CI, StringRef file) override {
    TheRewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
    return llvm::make_unique<MyASTConsumer>(TheRewriter, CI);
  }

 private:
  Rewriter TheRewriter;
};

int main(int argc, const char** argv) {
  CommonOptionsParser op(argc, argv, ToolingSampleCategory);
  ClangTool Tool(op.getCompilations(), op.getSourcePathList());
  int rc =  Tool.run(newFrontendActionFactory<MyFrontendAction>().get());
  return rc;
}




