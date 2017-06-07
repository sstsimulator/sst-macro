/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include "replAstVisitor.h"
#include <iostream>
#include <fstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

void
ReplGlobalASTVisitor::initConfig()
{
  const char* skelStr = getenv("SSTMAC_SKELETONIZE");
  if (skelStr){
    bool doSkel = atoi(skelStr);
    noSkeletonize_ = !doSkel;
  }
}

void
ReplGlobalASTVisitor::initMPICalls()
{
  mpiCalls_["irecv"] = &ReplGlobalASTVisitor::visitPt2Pt;
  mpiCalls_["isend"] = &ReplGlobalASTVisitor::visitPt2Pt;
  mpiCalls_["recv"] = &ReplGlobalASTVisitor::visitPt2Pt;
  mpiCalls_["send"] = &ReplGlobalASTVisitor::visitPt2Pt;
  mpiCalls_["bcast"] = &ReplGlobalASTVisitor::visitPt2Pt; //bit of a hack
  mpiCalls_["allreduce"] = &ReplGlobalASTVisitor::visitReduce;
  mpiCalls_["reduce"] = &ReplGlobalASTVisitor::visitReduce;
  mpiCalls_["allgather"] = &ReplGlobalASTVisitor::visitCollective;
}

void
ReplGlobalASTVisitor::initReservedNames()
{
  reservedNames_.insert("dont_ignore_this");
  reservedNames_.insert("ignore_for_app_name");
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
ReplGlobalASTVisitor::shouldVisitDecl(VarDecl* D)
{
  if (keepGlobals_ || isa<ParmVarDecl>(D) || D->isImplicit() || insideCxxMethod_){
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
ReplGlobalASTVisitor::VisitCXXNewExpr(CXXNewExpr *expr)
{
  if (noSkeletonize_) return true;

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
      if (expr->hasInitializer()){
        Expr* init = expr->getInitializer();
        if (isa<ParenListExpr>(init)){
          ParenListExpr* pinit = cast<ParenListExpr>(init);
          for (int i=0; i < pinit->getNumExprs(); ++i){
            if (i >= 1) pp.os << ",";
            pp.print(pinit->getExpr(i));
          }
        } else {
          pp.print(init);
        }
      } else {
        const Expr* ctor = expr->getConstructExpr();
        if (ctor){
          pp.print(ctor);
        }
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
            pp.print(expr->getArraySize());
          } else if (expr->hasInitializer()){
            Expr* init = expr->getInitializer();
            if (isa<ParenListExpr>(init)){
              ParenListExpr* pinit = cast<ParenListExpr>(init);
              for (int i=0; i < pinit->getNumExprs(); ++i){
                pp.os << ",";
                pp.print(pinit->getExpr(i));
              }
            } else {
              pp.os << ",";
              pp.print(init);
            }
          } else {
            const Expr* ctor = expr->getConstructExpr();
            if (ctor){
              pp.os << ",";
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
  if (noSkeletonize_) return true;

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
  if (keepGlobals_) return true;

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
ReplGlobalASTVisitor::VisitDeclRefExpr(DeclRefExpr* expr)
{
  NamedDecl* decl =  expr->getFoundDecl();
  replaceGlobalUse(decl, expr->getSourceRange());
  return true;
}

void
ReplGlobalASTVisitor::visitCollective(CallExpr *expr)
{
  if (noSkeletonize_) return;

  //first buffer argument to nullptr
  if (expr->getArg(0)->getType()->isPointerType()){
    //make sure this isn't a shortcut function without buffers
    rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
    rewriter_.ReplaceText(expr->getArg(3)->getSourceRange(), "nullptr");
  }
}

void
ReplGlobalASTVisitor::visitReduce(CallExpr *expr)
{
  if (noSkeletonize_) return;

  //first buffer argument to nullptr
  if (expr->getArg(0)->getType()->isPointerType()){
    //make sure this isn't a shortcut function without buffers
    rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
    rewriter_.ReplaceText(expr->getArg(1)->getSourceRange(), "nullptr");
  }
}

void
ReplGlobalASTVisitor::visitPt2Pt(CallExpr *expr)
{
  if (noSkeletonize_) return;

  //first buffer argument to nullptr
  if (expr->getArg(0)->getType()->isPointerType()){
    //make sure this isn't a shortcut function without buffers
    rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
  }
}

bool
ReplGlobalASTVisitor::VisitCXXMemberCallExpr(CXXMemberCallExpr* expr)
{
  if (noSkeletonize_) return true;

  CXXRecordDecl* cls = expr->getRecordDecl();
  std::string clsName = cls->getNameAsString();
  //std::cout << "got call on " << clsName << std::endl;
  if (clsName == "mpi_api"){
    FunctionDecl* decl = expr->getDirectCallee();
    if (!decl){
      errorAbort(expr->getLocStart(), *ci_, "invalid MPI call");
    }
    auto iter = mpiCalls_.find(decl->getNameAsString());
    if (iter != mpiCalls_.end()){
      MPI_Call call = iter->second;
      (this->*call)(expr);
    }
  }
  return true;
}

bool
ReplGlobalASTVisitor::VisitCallExpr(CallExpr* expr)
{
  if (noSkeletonize_) return true;

  FunctionDecl* callee = expr->getDirectCallee();
  if (callee && pragma_config_.pragmaDepth){
    auto iter = pragma_config_.functionReplacements.find(callee->getNameAsString());
    if (iter != pragma_config_.functionReplacements.end()){
      rewriter_.ReplaceText(expr->getSourceRange(), iter->second);
    }
  }
  return true;
}

void
ReplGlobalASTVisitor::defineGlobalVarInitializers(const std::string& scope_unique_var_name,
   const std::string& var_name, const std::string& init_prefix, llvm::raw_string_ostream& os)
{
  //this has an initial value we need to transfer over
  //log the variable so we can drop replacement info in the static cxx file
  //if static and no init given, then we will drop this initialization code at the instantiation point
  os << "void* __ptr_" << scope_unique_var_name << " = &" << init_prefix << var_name << "; "
     << "int __sizeof_" << scope_unique_var_name << " = sizeof(" << init_prefix << var_name << ");";
}

void
ReplGlobalASTVisitor::declareStaticInitializers(const std::string& scope_unique_var_name,
                                                llvm::raw_string_ostream& os)
{
  //this has an initial value we need to transfer over
  //log the variable so we can drop replacement info in the static cxx file
  //if static and no init given, then we will drop this initialization code at the instantiation point
  currentNs_->replVars.insert(scope_unique_var_name);
  os << "extern void* __ptr_" << scope_unique_var_name << ";"
     << "extern int __sizeof_" << scope_unique_var_name << ";";
}

bool
ReplGlobalASTVisitor::setupGlobalVar(const std::string& scope_prefix,
                                       const std::string& init_prefix,
                                       SourceLocation externVarsInsertLoc,
                                       SourceLocation getRefInsertLoc,
                                       GlobalRedirect_t red_ty,
                                       VarDecl* D)
{

  std::string& varRepl = globals_[D];
  std::string scopeUniqueVarName = scope_prefix + D->getNameAsString();

  // roundabout way to get the type of the variable
  std::string retType;
  const Type* ty  = D->getType().getTypePtr();
  bool isC99array = ty->isArrayType();
  //varRepl will hold the replacement text that we will use in the map
  if (isC99array){
    const ArrayType* aty = ty->getAsArrayTypeUnsafe();
    retType = QualType::getAsString(aty->getElementType().split()) + "*";
    varRepl = "(" + currentNs_->nsPrefix() + "get_" + scopeUniqueVarName + "())";
  } else {
    retType = QualType::getAsString(D->getType().split()) + "*";
    varRepl = "(*" + currentNs_->nsPrefix() + "get_" + scopeUniqueVarName + "())";
  }

  //std::string str;
  {std::string empty;
  llvm::raw_string_ostream os(empty);
  /** Add an extern declaration of all sstmac stack config vars */
  os << "extern int sstmac_global_stacksize; ";
  if (!D->hasExternalStorage()){
    os << "extern int __offset_" << scopeUniqueVarName << "; ";
    currentNs_->replVars.insert(scopeUniqueVarName);
    if (red_ty == Extern){
      defineGlobalVarInitializers(scopeUniqueVarName, D->getNameAsString(), init_prefix, os);
    } else {
      declareStaticInitializers(scopeUniqueVarName, os);
    }
  }
  rewriter_.InsertText(externVarsInsertLoc, os.str());}

  std::string empty;
  llvm::raw_string_ostream os(empty);
  std::string varName = D->getNameAsString();
  bool notDeclared = globalsDeclared_.find(varName) == globalsDeclared_.end();
  if (notDeclared){
    os << "static inline " << retType << " get_" << scopeUniqueVarName << "(){ "
       << " int stack; int* stackPtr = &stack; "
       << " uintptr_t localStorage = ((uintptr_t) stackPtr/sstmac_global_stacksize)*sstmac_global_stacksize; "
       << " char* offsetPtr = *((char**)localStorage) + __offset_" << scopeUniqueVarName << "; "
       << "return (((" << retType << ")((void*)offsetPtr))); "
       << "}";
    globalsDeclared_.insert(varName);
  }
  os << "  ";

  if (getRefInsertLoc.isInvalid()){
    errorAbort(D->getLocStart(), *ci_, "failed replacing global variable declaration");
  }
  rewriter_.InsertText(getRefInsertLoc, os.str());
  return true;
}

clang::SourceLocation
ReplGlobalASTVisitor::getEndLoc(const VarDecl *D)
{
  SourceLocation endLoc = Lexer::findLocationAfterToken(D->getLocEnd(), tok::semi,
                                 ci_->getSourceManager(), ci_->getLangOpts(), false);
  return endLoc;
}

bool
ReplGlobalASTVisitor::checkStaticFileVar(VarDecl* D)
{

  return setupGlobalVar(currentNs_->filePrefix(), "",
                        D->getLocStart(), getEndLoc(D), Extern, D);
}

bool
ReplGlobalASTVisitor::checkGlobalVar(VarDecl* D)
{
  return setupGlobalVar("", "", D->getLocStart(), getEndLoc(D), Extern, D);
}

bool
ReplGlobalASTVisitor::checkStaticFxnVar(VarDecl *D)
{
  if (D->isStaticLocal()){
    FunctionDecl* outerFxn = fxn_contexts_.front();
    SourceLocation insertLoc = outerFxn->getLocStart();
    std::stringstream prefix_sstr; prefix_sstr << "_";
    for (FunctionDecl* fxn : fxn_contexts_){
      prefix_sstr << "_" << fxn->getNameAsString();
    }

    if (D->getType().getTypePtr()->isArrayType()){
      errorAbort(D->getLocStart(), *ci_,
                 "cannot currently refactory array static function variables");
    }

    std::string init_prefix = prefix_sstr.str();
    std::string scope_prefix = currentNs_->filePrefix() + init_prefix;
    /** we have to move the initializer outside the function, which is annoying
     *  to avoid name conflicts, we have to change the name of the initializer to
     *  something unique */
    PrettyPrinter new_init_pp;
    new_init_pp.os << "static " << QualType::getAsString(D->getType().split())
                  << " " << init_prefix << D->getNameAsString();
    if (D->hasInit()){
      new_init_pp.os << " = ";
      new_init_pp.print(D->getInit());
    }
    new_init_pp.os << ";";
    rewriter_.InsertText(insertLoc, new_init_pp.str());
    //now that we have moved the initializer, we can do the replacement
    //but the replacement should point to the new initializer
    setupGlobalVar(scope_prefix, init_prefix,
                   outerFxn->getLocStart(), insertLoc, Extern, D);
    SourceLocation endLoc = getEndLoc(D);
    SourceRange rng(D->getLocStart(), endLoc);
    //while we're at it, might as well get rid of the old initializer
    rewriter_.ReplaceText(rng, "");
  }
  return true;
}

bool
ReplGlobalASTVisitor::checkDeclStaticClassVar(VarDecl *D)
{
  if (D->isStaticDataMember()){
    if (class_contexts_.size() > 1){
      errorAbort(D->getLocStart(), *ci_, "cannot handle static variables in inner classes");
    }
    CXXRecordDecl* outerCls = class_contexts_.front();
    std::stringstream scope_sstr; scope_sstr << "_";
    for (CXXRecordDecl* decl : class_contexts_){
      scope_sstr << "_" << decl->getNameAsString();
    }
    std::string scope_prefix = scope_sstr.str();
    if (!D->hasInit()){
      //no need for special scope prefixes - these are fully scoped within in the class
      setupGlobalVar(scope_prefix, ""/*not used*/, outerCls->getLocStart(),
                     getEndLoc(D), CxxStatic, D);
    } //else this must be a const integer if inited in the header file
      //we don't have to "deglobalize" this
  }
  return true;
}

static void
scopeString(SourceLocation loc, CompilerInstance& ci, DeclContext* ctx,
            std::list<std::string>& scopes){
  while (ctx->getDeclKind() != Decl::TranslationUnit){
    switch (ctx->getDeclKind()){
      case Decl::Namespace: {
        NamespaceDecl* nsDecl = cast<NamespaceDecl>(ctx);
        ctx = nsDecl->getDeclContext();
        scopes.push_front(nsDecl->getNameAsString());
      }
      break;
      case Decl::CXXRecord: {
        CXXRecordDecl* clsDecl = cast<CXXRecordDecl>(ctx);
        ctx = clsDecl->getDeclContext();
        scopes.push_front(clsDecl->getNameAsString());
      }
      break;
    default:
      errorAbort(loc, ci, "bad context type in scope string");
      break;
    }
  }
}

bool
ReplGlobalASTVisitor::checkInstanceStaticClassVar(VarDecl *D)
{
  if (D->isStaticDataMember()){
    //okay - this sucks - I have no idea how this variable is being declared
    //it could be ns::A::x = 10 for a variable in namespace ns, class A
    //it could namespace ns { A::x = 10 }
    //we must the variable declarations in the full namespace - we can't cheat
    DeclContext* lexicalContext = D->getLexicalDeclContext();
    DeclContext* semanticContext = D->getDeclContext();
    CXXRecordDecl* parentCls = cast<CXXRecordDecl>(semanticContext);
    std::list<std::string> lex;
    scopeString(D->getLocStart(), *ci_, lexicalContext, lex);
    std::list<std::string> sem;
    scopeString(D->getLocStart(), *ci_, semanticContext, sem);

    //match the format from checkDeclStaticClassVar
    std::string scope_unique_var_name = "__" + parentCls->getNameAsString() + D->getNameAsString();

    std::string empty;
    llvm::raw_string_ostream os(empty);
    //throw away the class name in the list of scopes
    sem.pop_back();
    auto semBegin = sem.begin();
    for (auto& ignore : lex){ //figure out the overlap between lexical and semantic namespaces
      ++semBegin;
    }
    for (auto iter = semBegin; iter != sem.end(); ++iter){ //I must declare vars in the most enclosing namespace
      os << "namespace " << *iter << " {";
    }

    std::string init_prefix = parentCls->getNameAsString() + "::";
    defineGlobalVarInitializers(scope_unique_var_name, D->getNameAsString(), init_prefix, os);
    for (auto iter = semBegin; iter != sem.end(); ++iter){
      os << "}"; //close namespaces
    }
    rewriter_.InsertText(getEndLoc(D), os.str());
  }
  return true;
}

bool
ReplGlobalASTVisitor::VisitVarDecl(VarDecl* D){

  if (!shouldVisitDecl(D)){
    return true;
  }

  if (reservedNames_.find(D->getNameAsString()) != reservedNames_.end()){
    return true;
  }

  SourceLocation startLoc = D->getLocStart();
  std::string filename = ci_->getSourceManager().getFilename(startLoc).str();

  if (!currentNs_->isPrefixSet){
    currentNs_->setFilePrefix(filename.c_str());
  }

  if (D->getNameAsString() == "sstmac_appname_str"){
    const ImplicitCastExpr* expr1 = cast<const ImplicitCastExpr>(D->getInit());
    const ImplicitCastExpr* expr2 = cast<const ImplicitCastExpr>(expr1->getSubExpr());
    const StringLiteral* lit = cast<StringLiteral>(expr2->getSubExpr());
    mainName_ = lit->getString();
    return true;
  }

  if (insideClass()){
    return checkDeclStaticClassVar(D);
  } else if (insideFxn()){
    return checkStaticFxnVar(D);
  } else if (D->isCXXClassMember()){
    return checkInstanceStaticClassVar(D);
  } else if (D->getStorageClass() == StorageClass::SC_Static){
    return checkStaticFileVar(D);
  } else {
    return checkGlobalVar(D);
  }
}

bool
ReplGlobalASTVisitor::VisitDeclStmt(DeclStmt *S)
{
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
    std::string appname = getAppName();
    if (appname.size() == 0){
      errorAbort(mainFxn->getLocStart(), *ci_,
                 "sstmac_app_name macro not defined before main");
    }
    std::stringstream sstr;
    sstr << "int sstmac_user_main_" << appname << "(";
    if (mainFxn->getNumParams() == 2){
      sstr << "int argc, char** argv";
    }
    sstr << "){";

    SourceRange rng(mainFxn->getLocStart(), mainFxn->getBody()->getLocStart());
    rewriter_.ReplaceText(rng, sstr.str());

  } else {
    errorAbort(mainFxn->getLocStart(), *ci_,
               "sstmac_app_name macro not defined before main");
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

  if (D->isThisDeclarationADefinition())
    VisitDecl(D); //check for pragmas on it

  fxn_contexts_.push_back(D);
  if (!pragma_config_.skipNextStmt){
    TraverseStmt(D->getBody());
  } else {
    pragma_config_.skipNextStmt = false;
  }
  fxn_contexts_.pop_back();
  return true;
}

bool
ReplGlobalASTVisitor::TraverseCXXRecordDecl(CXXRecordDecl *D)
{
  class_contexts_.push_back(D);
  auto end = D->decls_end();
  for (auto iter=D->decls_begin(); iter != end; ++iter){
    TraverseDecl(*iter);
  }
  class_contexts_.pop_back();
  return true;
}

/**
 * @brief TraverseNamespaceDecl We have to traverse namespaces.
 *        We need pre and post operations. We have to explicitly recurse subnodes.
 * @param D
 * @return
 */
bool
ReplGlobalASTVisitor::TraverseNamespaceDecl(NamespaceDecl* D)
{
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
  } else if (D->getNameAsString() == "conditional_array_new"){
    return true;
  } else {
    TraverseDecl(D->getTemplatedDecl());
    return true;
  }
}

bool
ReplGlobalASTVisitor::TraverseCXXConstructorDecl(CXXConstructorDecl *D)
{
  if (D->isTemplateInstantiation())
    return true;

  ++insideCxxMethod_;
  for (auto *I : D->inits()) {
    TraverseConstructorInitializer(I);
  }
  TraverseCXXMethodDecl(D);
  --insideCxxMethod_;
  return true;
}

bool
ReplGlobalASTVisitor::TraverseCXXMethodDecl(CXXMethodDecl *D)
{
  //do not traverse this - will mess everything up
  //this got implicitly inserted into AST - has no source location
  if (D->isTemplateInstantiation())
    return true;

  ++insideCxxMethod_;
  if (D->isThisDeclarationADefinition()) {
    VisitDecl(D); //check for pragmas
    if (!pragma_config_.skipNextStmt){
      TraverseStmt(D->getBody());
    } else {
      pragma_config_.skipNextStmt = false;
    }
  }
  --insideCxxMethod_;
  return true;
}

bool
ReplGlobalASTVisitor::VisitStmt(Stmt *S)
{
  if (noSkeletonize_) return true;

  SSTPragma* prg;
  while ((prg = pragmas_.takeMatch(S))){
    pragma_config_.pragmaDepth++;
    activePragmas_[S] = prg;
    //pragma takes precedence - must occur in pre-visit
    prg->activate(S, rewriter_, pragma_config_);
  }
  return true;
}

bool
ReplGlobalASTVisitor::VisitDecl(Decl *D)
{
  if (noSkeletonize_) return true;

  SSTPragma* prg;
  while ((prg = pragmas_.takeMatch(D))){
    pragma_config_.pragmaDepth++;
    //pragma takes precedence - must occur in pre-visit
    prg->activate(D, rewriter_, pragma_config_);
    pragma_config_.pragmaDepth--;
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
    activePragmas_.erase(iter);
    pragma_config_.pragmaDepth--;
  }
  return true;
}


void
ReplGlobalASTVisitor::replaceGlobalUse(NamedDecl* decl, SourceRange replRng)
{
  if (keepGlobals_) return;

  auto iter = globals_.find(decl);
  if (iter != globals_.end()){
    rewriter_.ReplaceText(replRng, iter->second);
  }
}
