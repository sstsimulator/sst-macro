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

#include "astVisitor.h"
#include "replacePragma.h"
#include <iostream>
#include <fstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

void
SkeletonASTVisitor::initConfig()
{
  const char* skelStr = getenv("SSTMAC_SKELETONIZE");
  if (skelStr){
    bool doSkel = atoi(skelStr);
    noSkeletonize_ = !doSkel;
  }
}

void
SkeletonASTVisitor::initMPICalls()
{
  mpiCalls_["irecv"] = &SkeletonASTVisitor::visitPt2Pt;
  mpiCalls_["isend"] = &SkeletonASTVisitor::visitPt2Pt;
  mpiCalls_["recv"] = &SkeletonASTVisitor::visitPt2Pt;
  mpiCalls_["send"] = &SkeletonASTVisitor::visitPt2Pt;
  mpiCalls_["bcast"] = &SkeletonASTVisitor::visitPt2Pt; //bit of a hack
  mpiCalls_["allreduce"] = &SkeletonASTVisitor::visitReduce;
  mpiCalls_["reduce"] = &SkeletonASTVisitor::visitReduce;
  mpiCalls_["allgather"] = &SkeletonASTVisitor::visitCollective;
}

void
SkeletonASTVisitor::initReservedNames()
{
  reservedNames_.insert("dont_ignore_this");
  reservedNames_.insert("ignore_for_app_name");
}

void
SkeletonASTVisitor::initHeaders()
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
SkeletonASTVisitor::shouldVisitDecl(VarDecl* D)
{
  if (keepGlobals_ || isa<ParmVarDecl>(D) || D->isImplicit() || insideCxxMethod_){
    return false;
  }

  SourceLocation startLoc = D->getLocStart();
  PresumedLoc ploc = ci_->getSourceManager().getPresumedLoc(startLoc);
  SourceLocation headerLoc = ploc.getIncludeLoc();

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
SkeletonASTVisitor::VisitCXXNewExpr(CXXNewExpr *expr)
{
  if (noSkeletonize_) return true;

  if (deletedStmts_.find(expr) != deletedStmts_.end()){
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
SkeletonASTVisitor::VisitCXXDeleteExpr(CXXDeleteExpr* expr)
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
SkeletonASTVisitor::VisitUnaryOperator(UnaryOperator* op)
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
SkeletonASTVisitor::VisitDeclRefExpr(DeclRefExpr* expr)
{
  NamedDecl* decl =  expr->getFoundDecl();
  if (isNullVariable(decl)){
    //we need to delete whatever parent statement this is a part of
    if (stmt_contexts_.empty()){
      if (pragmaConfig_.deletedRefs.find(expr) == pragmaConfig_.deletedRefs.end()){
        errorAbort(expr->getLocStart(), *ci_,
                   "null variable used in statement, but context list is empty");
      } //else this was deleted - so no problem
    } else {
      std::cout << "deleting decl ref " << decl->getNameAsString() << std::endl;
      deleteNullVariableStmt(expr, decl);
    }
  } else {
    replaceGlobalUse(decl, expr->getSourceRange());
  }
  return true;
}

void
SkeletonASTVisitor::visitCollective(CallExpr *expr)
{
  if (noSkeletonize_) return;

  //first buffer argument to nullptr
  if (expr->getArg(0)->getType()->isPointerType()){
    //make sure this isn't a shortcut function without buffers
    rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
    rewriter_.ReplaceText(expr->getArg(3)->getSourceRange(), "nullptr");
    deletedStmts_.insert(expr->getArg(0));
    deletedStmts_.insert(expr->getArg(3));
  }
}

void
SkeletonASTVisitor::visitReduce(CallExpr *expr)
{
  if (noSkeletonize_) return;

  //first buffer argument to nullptr
  if (expr->getArg(0)->getType()->isPointerType()){
    //make sure this isn't a shortcut function without buffers
    rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
    rewriter_.ReplaceText(expr->getArg(1)->getSourceRange(), "nullptr");
    deletedStmts_.insert(expr->getArg(0));
    deletedStmts_.insert(expr->getArg(1));
  }
}

void
SkeletonASTVisitor::visitPt2Pt(CallExpr *expr)
{
  if (noSkeletonize_) return;

  //first buffer argument to nullptr
  if (expr->getArg(0)->getType()->isPointerType()){
    //make sure this isn't a shortcut function without buffers
    rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
    deletedStmts_.insert(expr->getArg(0));
  }
}

bool
SkeletonASTVisitor::TraverseCXXMemberCallExpr(CXXMemberCallExpr* expr, DataRecursionQueue* queue)
{
  bool skipVisit = noSkeletonize_ ? false : activatePragmasForStmt(expr);
  if (skipVisit){
    return true;
  }

  if (!pragmaConfig_.makeNoChanges){
    CXXRecordDecl* cls = expr->getRecordDecl();
    std::string clsName = cls->getNameAsString();
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
  }
  stmt_contexts_.push_back(expr);
  //TraverseStmt(expr->getImplicitObjectArgument());
  TraverseStmt(expr->getCallee()); //this will visit member expr that also visit implicit this
  for (int i=0; i < expr->getNumArgs(); ++i){
    Expr* arg = expr->getArg(i);
    //this did not get modified
    if (deletedStmts_.find(arg) == deletedStmts_.end()){
      TraverseStmt(expr->getArg(i));
    }
  }
  stmt_contexts_.pop_back();

  return true;
}

bool
SkeletonASTVisitor::TraverseMemberExpr(MemberExpr *expr, DataRecursionQueue* queue)
{
  ValueDecl* vd = expr->getMemberDecl();

  if (isNullVariable(vd)){
    SSTNullVariablePragma* prg = getNullVariable(vd);
    if (prg->noExceptions()){
      //we delete all uses of this variable
      deleteNullVariableStmt(expr, vd);
      return true;
    }
    //else depends on which members of this member are used
  } else {
    Expr* base = getUnderlyingExpr(expr->getBase());
    if (base->getStmtClass() == Stmt::DeclRefExprClass){
      DeclRefExpr* dref = cast<DeclRefExpr>(base);
      SSTNullVariablePragma* prg = getNullVariable(dref->getFoundDecl());
      if (prg){
        bool deleteStmt = true;
        if (prg->hasExceptions()){
          deleteStmt = !prg->isException(vd); //this might be a thing we dont delete
        } else if (prg->hasOnly()){
          deleteStmt = prg->isOnly(vd); //this might be a thing we do delete
        }

        if (deleteStmt){
          deleteNullVariableStmt(expr, vd);
        } else {
          if (prg->isNullifiedNew(vd)){
            //make damn sure we allocate no memory
            //even though we keep the statement
            SSTNewPragma::defaultAct(stmt_contexts_.front(),rewriter_,*ci_,false,false,true);
          }
        }
        return true;
      }
    }
  }



  TraverseStmt(expr->getBase());
  return true;
}

bool
SkeletonASTVisitor::TraverseCallExpr(CallExpr* expr, DataRecursionQueue* queue)
{
  bool skipVisit = noSkeletonize_ ? false : activatePragmasForStmt(expr);
  if (skipVisit){
    return true;
  }

  if (!noSkeletonize_ && !pragmaConfig_.replacePragmas.empty()){
    Expr* fxn = getUnderlyingExpr(const_cast<Expr*>(expr->getCallee()));
    if (fxn->getStmtClass() == Stmt::DeclRefExprClass){
      //this is a basic function call
      DeclRefExpr* dref = cast<DeclRefExpr>(fxn);
      std::string fxnName = dref->getFoundDecl()->getNameAsString();
      for (auto& pair : pragmaConfig_.replacePragmas){
        SSTReplacePragma* replPrg = static_cast<SSTReplacePragma*>(pair.second);
        if (replPrg->fxn() == fxnName){
          replPrg->run(expr, rewriter_);
          return true;
        }
      }
    }
  }

  stmt_contexts_.push_back(expr);
  TraverseStmt(expr->getCallee());
  for (int i=0; i < expr->getNumArgs(); ++i){
    TraverseStmt(expr->getArg(i));
  }
  stmt_contexts_.pop_back();


  return true;
}

void
SkeletonASTVisitor::defineGlobalVarInitializers(const std::string& scope_unique_var_name,
   const std::string& var_name, const std::string& init_prefix, llvm::raw_string_ostream& os)
{
  //this has an initial value we need to transfer over
  //log the variable so we can drop replacement info in the static cxx file
  //if static and no init given, then we will drop this initialization code at the instantiation point
  os << "void* __ptr_" << scope_unique_var_name << " = &" << init_prefix << var_name << "; "
     << "int __sizeof_" << scope_unique_var_name << " = sizeof(" << init_prefix << var_name << ");";
}

void
SkeletonASTVisitor::declareStaticInitializers(const std::string& scope_unique_var_name,
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
SkeletonASTVisitor::setupGlobalVar(const std::string& scope_prefix,
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
  os << "extern int sstmac_global_stacksize; "
     << "extern int __offset_" << scopeUniqueVarName << "; ";
  if (!D->hasExternalStorage()){
    currentNs_->replVars.insert(scopeUniqueVarName);
    if (red_ty == Extern){
      defineGlobalVarInitializers(scopeUniqueVarName, D->getNameAsString(), init_prefix, os);
    } else {
      declareStaticInitializers(scopeUniqueVarName, os);
    }
  }
  if (externVarsInsertLoc.isInvalid()){
    errorAbort(D->getLocStart(), *ci_, "got bad global variable source location");
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
SkeletonASTVisitor::getEndLoc(const VarDecl *D)
{
  SourceLocation endLoc = Lexer::findLocationAfterToken(D->getLocEnd(), tok::semi,
                                 ci_->getSourceManager(), ci_->getLangOpts(), false);
  return endLoc;
}

bool
SkeletonASTVisitor::checkStaticFileVar(VarDecl* D)
{
  SourceLocation end = getEndLoc(D);
  return setupGlobalVar(currentNs_->filePrefix(), "", end, end, Extern, D);
}

bool
SkeletonASTVisitor::checkGlobalVar(VarDecl* D)
{
  SourceLocation end = getEndLoc(D);
  if (end.isInvalid()){
    errorAbort(D->getLocStart(), *ci_,
               "Multi-variable declrations for global variables not allowed");
  }
  return setupGlobalVar("", "", end, end, Extern, D);
}

void
SkeletonASTVisitor::deleteStmt(Stmt *s)
{
  SourceRange rng(s->getLocStart(), s->getLocEnd());
  rewriter_.ReplaceText(rng,"");
}

bool
SkeletonASTVisitor::checkStaticFxnVar(VarDecl *D)
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
SkeletonASTVisitor::checkDeclStaticClassVar(VarDecl *D)
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
SkeletonASTVisitor::checkInstanceStaticClassVar(VarDecl *D)
{
  if (D->isStaticDataMember()){
    //okay - this sucks - I have no idea how this variable is being declared
    //it could be ns::A::x = 10 for a variable in namespace ns, class A
    //it could namespace ns { A::x = 10 }
    //we must the variable declarations in the full namespace - we can't cheat
    DeclContext* lexicalContext = D->getLexicalDeclContext();
    DeclContext* semanticContext = D->getDeclContext();
    if (!isa<CXXRecordDecl>(semanticContext)){
      std::string error = "variable " + D->getNameAsString() + " does not have class semantic context";
      errorAbort(D->getLocStart(), *ci_, error);
    }
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
SkeletonASTVisitor::TraverseVarDecl(VarDecl* D)
{
  if (D->getMemberSpecializationInfo() && D->getTemplateInstantiationPattern()){
    return true;
  }

  visitVarDecl(D);

  if (D->hasInit()){
    TraverseStmt(D->getInit());
  }

  return true;
}

bool
SkeletonASTVisitor::visitVarDecl(VarDecl* D)
{
  if (!shouldVisitDecl(D)){
    return true;
  }

  //we need do nothing with this
  if (D->isConstexpr()){
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
  } else if (visitingGlobal_){
    return checkGlobalVar(D);
  }
  return true;
}

void
SkeletonASTVisitor::replaceMain(clang::FunctionDecl* mainFxn)
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
SkeletonASTVisitor::TraverseFunctionDecl(clang::FunctionDecl* D)
{
  if (D->isMain()){
    replaceMain(D);
  } else if (D->isTemplateInstantiation()){
    return true; //do not visit implicitly instantiated template stuff
  }

  bool skipVisit = false;
  if (D->isThisDeclarationADefinition()){
    skipVisit = noSkeletonize_ ? false : activatePragmasForDecl(D);
  }

  fxn_contexts_.push_back(D);
  if (!skipVisit){
    TraverseStmt(D->getBody());
  }
  fxn_contexts_.pop_back();
  return true;
}

bool
SkeletonASTVisitor::TraverseCXXRecordDecl(CXXRecordDecl *D)
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
SkeletonASTVisitor::TraverseNamespaceDecl(NamespaceDecl* D)
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
SkeletonASTVisitor::TraverseFunctionTemplateDecl(FunctionTemplateDecl *D)
{
  static const std::set<std::string> skip_set = {
    "placement_new",
    "conditional_new",
    "conditional_array_new",
    "conditional_delete",
    "conditional_delete_array",
  };

  if (skip_set.find(D->getNameAsString()) != skip_set.end()){
    return true;
  } else {
    TraverseDecl(D->getTemplatedDecl());
    return true;
  }
}

bool
SkeletonASTVisitor::TraverseCXXConstructorDecl(CXXConstructorDecl *D)
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
SkeletonASTVisitor::TraverseCXXMethodDecl(CXXMethodDecl *D)
{
  //do not traverse this - will mess everything up
  //this got implicitly inserted into AST - has no source location
  if (D->isTemplateInstantiation())
    return true;

  ++insideCxxMethod_;
  if (D->isThisDeclarationADefinition()) {
    bool skipVisit = noSkeletonize_ ? false : activatePragmasForDecl(D);
    if (!skipVisit){
      TraverseStmt(D->getBody());
    }
  }
  --insideCxxMethod_;
  return true;
}

bool
SkeletonASTVisitor::TraverseCompoundStmt(CompoundStmt* stmt, DataRecursionQueue* queue)
{
  bool skipVisit = noSkeletonize_ ? false : activatePragmasForStmt(stmt);

  if (!skipVisit){
    auto end = stmt->body_end();
    for (auto iter=stmt->body_begin(); iter != end; ++iter){
      TraverseStmt(*iter);
    }
  }

  return true;
}

bool
SkeletonASTVisitor::TraverseUnaryOperator(UnaryOperator* op, DataRecursionQueue* queue)
{
  VisitStmt(op);
  stmt_contexts_.push_back(op);
  TraverseStmt(op->getSubExpr());
  stmt_contexts_.pop_back();
  return true;
}

bool
SkeletonASTVisitor::TraverseBinaryOperator(BinaryOperator* op, DataRecursionQueue* queue)
{
  bool skipVisit = noSkeletonize_ ? false : activatePragmasForStmt(op);
  stmt_contexts_.push_back(op);
  if (skipVisit){
    deletedStmts_.insert(op);
  } else {
    TraverseStmt(op->getLHS());
    TraverseStmt(op->getRHS());
  }
  stmt_contexts_.pop_back();
  return true;
}

bool
SkeletonASTVisitor::TraverseIfStmt(IfStmt* stmt, DataRecursionQueue* queue)
{
  VisitStmt(stmt);
  stmt_contexts_.push_back(stmt);
  TraverseStmt(stmt->getCond());
  TraverseStmt(stmt->getThen());
  if (stmt->getElse()) TraverseStmt(stmt->getElse());
  stmt_contexts_.pop_back();
  return true;
}

bool
SkeletonASTVisitor::TraverseDeclStmt(DeclStmt* stmt, DataRecursionQueue* queue)
{
  bool skipVisit = noSkeletonize_ ? false : activatePragmasForStmt(stmt);
  if (skipVisit){
    deletedStmts_.insert(stmt);
  } else {
    stmt_contexts_.push_back(stmt);
    if (stmt->isSingleDecl()){
      Decl* D = stmt->getSingleDecl();
      SSTNullVariablePragma* prg = getNullVariable(D);
      if (prg){
        if (prg->keepCtor()){
          SSTNewPragma::defaultAct(stmt_contexts_.front(),rewriter_,*ci_,false,true,false);
        } else {
          deleteStmt(stmt);
        }
      } else {
        TraverseDecl(D);
      }
    } else {
      for (auto* D : stmt->decls()){
        TraverseDecl(D);
      }
    }
    stmt_contexts_.pop_back();
  }


  return true;
}

bool
SkeletonASTVisitor::TraverseForStmt(ForStmt *S, DataRecursionQueue* queue)
{
  loop_contexts_.push_back(S);
  bool skipVisit = noSkeletonize_ ? false : activatePragmasForStmt(S);
  if (!skipVisit){
    if (S->getInit()) TraverseStmt(S->getInit());
    if (S->getCond()) TraverseStmt(S->getCond());
    if (S->getInc()) TraverseStmt(S->getInc());
    if (S->getBody()) TraverseStmt(S->getBody());
  }
  loop_contexts_.pop_back();
  return true;
}

bool
SkeletonASTVisitor::VisitStmt(Stmt *S)
{
  if (noSkeletonize_) return true;

  activatePragmasForStmt(S);

  return true;
}

bool
SkeletonASTVisitor::VisitDecl(Decl *D)
{
  if (noSkeletonize_) return true;

  for (SSTPragma* prg : pragmas_.getMatches(D)){
    pragmaConfig_.pragmaDepth++;
    //pragma takes precedence - must occur in pre-visit
    prg->activate(D, rewriter_, pragmaConfig_);
    pragmaConfig_.pragmaDepth--;
  }

  return true;
}

bool
SkeletonASTVisitor::dataTraverseStmtPre(Stmt* S)
{
  switch(S->getStmtClass()){
    case Stmt::ForStmtClass:
    case Stmt::BinaryOperatorClass:
    case Stmt::CompoundAssignOperatorClass:
    case Stmt::CallExprClass:
      stmt_contexts_.push_back(S);
      break;
    default:
      break;
  }

  return true;
}

bool
SkeletonASTVisitor::dataTraverseStmtPost(Stmt* S)
{
  switch(S->getStmtClass()){
    case Stmt::ForStmtClass:
    case Stmt::BinaryOperatorClass:
    case Stmt::CompoundAssignOperatorClass:
    case Stmt::CallExprClass:
      stmt_contexts_.pop_back();
      break;
    default:
      break;
  }

  auto iter = activePragmas_.find(S);
  if (iter != activePragmas_.end()){
    SSTPragma* prg = iter->second;
    prg->deactivate(S, pragmaConfig_);
    activePragmas_.erase(iter);
    pragmaConfig_.pragmaDepth--;
  }
  return true;
}

void
SkeletonASTVisitor::deleteNullVariableStmt(Stmt* use_stmt, Decl* d)
{
  if (stmt_contexts_.empty()){
    errorAbort(use_stmt->getLocStart(), *ci_,
               "null variable used in statement, but context list is empty");
  }
  Stmt* s = stmt_contexts_.front(); //delete the outermost stmt
  if (!loop_contexts_.empty()){
    errorAbort(use_stmt->getLocStart(), *ci_,
               "null variable used inside loop - loop skeletonization "
               "must be indicated with #pragma sst compute outside loop");
  }
  deleteStmt(s);
}

bool
SkeletonASTVisitor::activatePragmasForStmt(Stmt* S)
{
  bool skipVisit = false;
  for (SSTPragma* prg : pragmas_.getMatches(S)){
    if (skipVisit){
      errorAbort(S->getLocStart(), *ci_,
           "code block deleted by pragma - invalid pragma combination");
    }
    pragmaConfig_.pragmaDepth++;
    activePragmas_[S] = prg;
    //pragma takes precedence - must occur in pre-visit
    prg->activate(S, rewriter_, pragmaConfig_);
    //a compute pragma totally deletes the block
    bool blockDeleted = false;
    switch (prg->cls){
      case SSTPragma::Compute:
      case SSTPragma::Delete:
      case SSTPragma::Init:
        blockDeleted = true;
        break;
      default: break;
    }
    skipVisit = skipVisit || blockDeleted;
  }
  return skipVisit;
}

bool
SkeletonASTVisitor::activatePragmasForDecl(Decl* D)
{
  bool skipVisit = false;
  for (SSTPragma* prg : pragmas_.getMatches(D)){
    if (skipVisit){
      errorAbort(D->getLocStart(), *ci_,
           "code block deleted by pragma - invalid pragma combination");
    }
    pragmaConfig_.pragmaDepth++;
    //pragma takes precedence - must occur in pre-visit
    prg->activate(D, rewriter_, pragmaConfig_);
    //a compute pragma totally deletes the block
    bool blockDeleted = prg->cls == SSTPragma::Compute || prg->cls == SSTPragma::Delete;
    skipVisit = skipVisit || blockDeleted;
  }
  return skipVisit;
}

Expr*
SkeletonASTVisitor::getUnderlyingExpr(Expr *e)
{
#define sub_case(e,cls) \
  case Stmt::cls##Class: \
    e = cast<cls>(e)->getSubExpr(); break
  while (1){
    switch(e->getStmtClass()){
    sub_case(e,ParenExpr);
    sub_case(e,CStyleCastExpr);
    sub_case(e,ImplicitCastExpr);
    default:
      return e;
    }
  }
#undef sub_case
}

void
SkeletonASTVisitor::replaceGlobalUse(NamedDecl* decl, SourceRange replRng)
{
  if (keepGlobals_) return;

  auto iter = globals_.find(decl);
  if (iter != globals_.end()){
    rewriter_.ReplaceText(replRng, iter->second);
  }
}
