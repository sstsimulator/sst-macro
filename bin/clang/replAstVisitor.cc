#include "replAstVisitor.h"
#include <iostream>
#include <fstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;


bool
ReplGlobalASTVisitor::VisitCXXNewExpr(CXXNewExpr *expr)
{
  if (deletedExprs_.find(expr) != deletedExprs_.end()){
    //already deleted - do nothing here
    return true;
  }

  std::string allocatedTypeStr = expr->getAllocatedType().getAsString();
  if (expr->getNumPlacementArgs() == 0){
    PrettyPrinter pp;
    if (expr->isArray()){
      pp.os << "conditional_array_new<" << allocatedTypeStr << ">(";
      pp.print(expr->getArraySize());
      pp.os << ")";
    } else {
      pp.os << "conditional_new<" << allocatedTypeStr << ">(";
      const Expr* ctor = expr->getConstructExpr();
      if (ctor){
        pp.print(ctor);
      }
      pp.os << ")";
    }
    rewriter_.ReplaceText(expr->getSourceRange(), pp.os.str());
  } else {
    //might be a placement new or no-throw new
    Expr* placer = expr->getPlacementArg(0);
    switch(placer->getStmtClass())
    {
      case Stmt::DeclRefExprClass:
      {
        DeclRefExpr* dre = cast<DeclRefExpr>(placer);
        if (dre->getFoundDecl()->getNameAsString() == "sstmac_placement_ptr"){
          break;
        }
      }
      case Stmt::ImplicitCastExprClass:
      case Stmt::CStyleCastExprClass:
      case Stmt::CallExprClass:
      case Stmt::CXXStaticCastExprClass:
      case Stmt::UnaryOperatorClass:
      {
        QualType type = placer->getType();
        if (QualType::getAsString(type.split()) == "void *"){
          PrettyPrinter pp;
          //placement
          pp.os << "placement_new<" << allocatedTypeStr << ">(";
          pp.print(placer);
          if (expr->isArray()){
            pp.os << ",";
            pp.print(expr->getArraySize());
          } else {
            const Expr* ctor = expr->getConstructExpr();
            if (ctor){
              pp.print(ctor);
            }
          }
          pp.os << ")";
          rewriter_.ReplaceText(expr->getSourceRange(), pp.os.str());
        }
      }
      default:
        break;
    }
  }
  return true;
}


bool
ReplGlobalASTVisitor::VisitCXXDeleteExpr(CXXDeleteExpr* expr)
{
  std::string allocatedTypeStr = expr->getDestroyedType().getAsString();
  PrettyPrinter pp;
  if (expr->isArrayForm()){
    pp.os << "conditional_delete_array";
  } else {
    pp.os << "conditional_delete";
  }
  pp.os << "<" << allocatedTypeStr << ">"
        << "(";
  pp.print(expr->getArgument());
  pp.os << ")";
  rewriter_.ReplaceText(expr->getSourceRange(), pp.os.str());
  return true;
}


bool
ReplGlobalASTVisitor::VisitUnaryOperator(UnaryOperator* op)
{
  if (visitingGlobal_){
    Expr* exp = op->getSubExpr();
    if (isa<DeclRefExpr>(exp)){
      DeclRefExpr* dref = cast<DeclRefExpr>(exp);
      if (isGlobal(dref)){
        errorAbort(dref->getLocStart(), *ci_,
                   "cannot yet create global variable pointers to other global variables");
      }
    }
  }
  return true;
}

bool
ReplGlobalASTVisitor::VisitDeclRefExpr(DeclRefExpr* expr){
  NamedDecl* decl =  expr->getFoundDecl();
  replGlobal(decl, expr->getSourceRange());
  return true;
}

void
ReplGlobalASTVisitor::initReservedNames()
{
  reservedNames_.insert("dont_ignore_this");
}

void
ReplGlobalASTVisitor::initHeaders()
{
  const char* headerListFile = getenv("SSTMAC_HEADERS");
  if (headerListFile == nullptr){
    const char* allHeaders = getenv("SSTMAC_ALL_HEADERS");
    if (allHeaders == nullptr){
      std::cerr << "WARNING: No header list specified through environment variable SSTMAC_HEADERS" << std::endl;
    }
    return;
  }

  std::ifstream ifs(headerListFile);
  if (!ifs.good()){
    std::cerr << "Bad header list from environment SSTMAC_HEADERS=" << headerListFile << std::endl;
    exit(EXIT_FAILURE);
  }

  std::string line;
  char fullpath[256];
  while (ifs.good()){
    std::getline(ifs, line);
    validHeaders_.insert(fullpath);
  }
}

bool
ReplGlobalASTVisitor::shouldVisitDecl(VarDecl* D){
  if (isa<ParmVarDecl>(D) || D->isImplicit()){
    return false;
  }

  SourceLocation startLoc = D->getLocStart();
  PresumedLoc ploc = ci_->getSourceManager().getPresumedLoc(startLoc);
  SourceLocation headerLoc = ploc.getIncludeLoc();
  //std::cout << ploc.getFilename() << std::boolalpha << " " << headerLoc.isValid() << std::endl;

  bool useAllHeaders = false;
  std::set<std::string> validHeaders;
  if (headerLoc.isValid() && !useAllHeaders){
    //we are inside a header
    char fullpathBuffer[1024];
    std::string filename = ci_->getSourceManager().getFilename(startLoc).str();
    const char* fullpath = realpath(filename.c_str(), fullpathBuffer);
    //is this in the list of valid headers
    return validHeaders.find(fullpath) != validHeaders.end();
  }
  //not a header - good to go
  return true;
}

bool
ReplGlobalASTVisitor::VisitCallExpr(CallExpr* expr)
{
  FunctionDecl* callee = expr->getDirectCallee();
  if (callee && pragma_config_.pragmaDepth){
    auto iter = pragma_config_.functionReplacements.find(callee->getNameAsString());
    if (iter != pragma_config_.functionReplacements.end()){
      rewriter_.ReplaceText(expr->getSourceRange(), iter->second);
    }
  }
  return true;
}

bool
ReplGlobalASTVisitor::VisitVarDecl(VarDecl* D){
  bool valid = shouldVisitDecl(D);
  if (!valid)
    return true;

  /** not a global variable */
  if (insideClass_ || insideFxn_)
    return true;

  if (reservedNames_.find(D->getNameAsString()) != reservedNames_.end()){
    return true;
  }

  SourceLocation startLoc = D->getLocStart();
  std::string filename = ci_->getSourceManager().getFilename(startLoc).str();

  if (!currentNs_->isPrefixSet){
    currentNs_->setFilePrefix(filename.c_str());
  }

  std::string str;
  llvm::raw_string_ostream os(str);

  std::string& varRepl = globals_[D];
  std::string sstVarName;
  if (D->getStorageClass() == StorageClass::SC_Static){
    //static, local scope
    //we lose static-ness in the deglobalization so make it have a unique name
    sstVarName = currentNs_->filePrefix() + D->getNameAsString();
  } else {
    //global, we can keep the name as is
    sstVarName = D->getNameAsString();
  }

  // roundabout way to get the type of the variable
  std::string retType;
  const Type* ty  = D->getType().getTypePtr();
  bool isC99array = ty->isArrayType();
  //varRepl will hold the replacement text that we will use in the map
  if (isC99array){
    const ArrayType* aty = ty->getAsArrayTypeUnsafe();
    retType = QualType::getAsString(aty->getElementType().split()) + "*";
    varRepl = "(" + currentNs_->nsPrefix() + "get_" + sstVarName + "())";
  } else {
    retType = QualType::getAsString(D->getType().split()) + "*";
    varRepl = "(*" + currentNs_->nsPrefix() + "get_" + sstVarName + "())";
  }


  std::string varName = D->getNameAsString();
  bool notDeclared = globalsDeclared_.find(varName) == globalsDeclared_.end();
  if (notDeclared){
    os << " extern int __offset_" << sstVarName << "; "
       << "extern int sstmac_global_stacksize; "
       << "static inline " << retType << " get_" << sstVarName << "(){ "
       << " int stack; int* stackPtr = &stack; "
       << " uintptr_t localStorage = ((uintptr_t) stackPtr/sstmac_global_stacksize)*sstmac_global_stacksize; "
       << " char* offsetPtr = *((char**)localStorage) + __offset_" << sstVarName << "; "
       << "return (((" << retType << ")((void*)offsetPtr))); "
       << "}";
    globalsDeclared_.insert(varName);
  }
  if (!D->hasExternalStorage()){
    currentNs_->replVars.insert(sstVarName);
    os << "void* __ptr_" << sstVarName << " = &" << D->getNameAsString() << "; "
       << "int __sizeof_" << sstVarName << " = sizeof(" << D->getNameAsString() << ");";
  }
  os << "  ";

  SourceLocation endLoc = Lexer::findLocationAfterToken(D->getLocEnd(), tok::semi,
                                 ci_->getSourceManager(), ci_->getLangOpts(), false);
  rewriter_.InsertText(endLoc, os.str());
  return true;
}

void
ReplGlobalASTVisitor::replaceMain(clang::FunctionDecl* mainFxn)
{
  SourceManager &SM = rewriter_.getSourceMgr();
  std::string sourceFile = SM.getFileEntryForID(SM.getMainFileID())->getName().str();
  std::string suffix2 = sourceFile.substr(sourceFile.size()-2,2);
  bool isC = suffix2 == ".c";

  if (isC){
    foundCMain_ = true;
    const char* appname = getenv("SSTMAC_APP_NAME");
    if (appname == nullptr){
      llvm::errs() << "Cannot refactor main function unless SSTMAC_APP_NAME environment var is defined\n";
      exit(EXIT_FAILURE);
    }
    std::stringstream sstr;
    sstr << "int sstmac_user_main_" << appname << "(";
    if (mainFxn->getNumParams() == 2){
      sstr << "int argc, char** argv";
    }
    sstr << "){";
    SourceRange rng(mainFxn->getLocStart(), mainFxn->getBody()->getLocStart());
    rewriter_.ReplaceText(rng, sstr.str());

  }
}

bool
ReplGlobalASTVisitor::TraverseFunctionDecl(clang::FunctionDecl* D)
{
  if (D->isMain()){
    replaceMain(D);
  } else if (D->isTemplateInstantiation()){
    return true; //do not visit implicitly instantiated template stuff
  }
  ++insideFxn_;
  TraverseStmt(D->getBody());
  --insideFxn_;
  return true;
}

bool
ReplGlobalASTVisitor::TraverseCXXRecordDecl(CXXRecordDecl *D)
{
  insideClass_++;
  auto end = D->decls_end();
  for (auto iter=D->decls_begin(); iter != end; ++iter){
    TraverseDecl(*iter);
  }
  insideClass_--;
  return true;
}

/**
 * @brief TraverseNamespaceDecl We have to traverse namespaces.
 *        We need pre and post operations. We have to explicitly recurse subnodes.
 * @param D
 * @return
 */
bool
ReplGlobalASTVisitor::TraverseNamespaceDecl(NamespaceDecl* D){
  GlobalVarNamespace* stash = currentNs_;
  GlobalVarNamespace& next = currentNs_->subspaces[D->getNameAsString()];
  next.appendNamespace(currentNs_->ns, D->getNameAsString());

  currentNs_ = &next;
  auto end = D->decls_end();
  for (auto iter=D->decls_begin(); iter != end; ++iter){
    TraverseDecl(*iter);
  }
  currentNs_ = stash;
  return true;
}

bool
ReplGlobalASTVisitor::TraverseFunctionTemplateDecl(FunctionTemplateDecl *D)
{
  if (D->getNameAsString() == "placement_new"){
    return true;
  } else if (D->getNameAsString() == "conditional_new"){
    return true;
  } else {
    TraverseDecl(D->getTemplatedDecl());
    return true;
  }
}

bool
ReplGlobalASTVisitor::TraverseCXXMethodDecl(CXXMethodDecl *D)
{
  //do not traverse this - will mess everything up
  //this got implicitly inserted into AST - has no source location
  if (D->isTemplateInstantiation())
    return true;

  if (CXXConstructorDecl *Ctor = dyn_cast<CXXConstructorDecl>(D)) {
    // Constructor initializers.
    for (auto *I : Ctor->inits()) {
      TraverseConstructorInitializer(I);
    }
  }

  if (D->isThisDeclarationADefinition()) {
    TraverseStmt(D->getBody());
  }

  return true;
}

bool
ReplGlobalASTVisitor::VisitStmt(Stmt *S)
{
  SSTPragma* prg = pragmas_.takeMatch(S);
  if (prg){
    pragma_config_.pragmaDepth++;
    activePragmas_[S] = prg;
    //pragma takes precedence - must occur in pre-visit
    prg->activate(S, rewriter_, pragma_config_);
  }
  return true;
}

bool
ReplGlobalASTVisitor::dataTraverseStmtPre(Stmt* S)
{
  return true;
}

bool
ReplGlobalASTVisitor::dataTraverseStmtPost(Stmt* S)
{
  auto iter = activePragmas_.find(S);
  if (iter != activePragmas_.end()){
    SSTPragma* prg = iter->second;
    prg->deactivate(S, pragma_config_);
    pragma_config_.pragmaDepth--;
  }
  return true;
}


void
ReplGlobalASTVisitor::replGlobal(NamedDecl* decl, SourceRange replRng)
{
  auto iter = globals_.find(decl);
  if (iter != globals_.end()){
    rewriter_.ReplaceText(replRng, iter->second);
  }
}


