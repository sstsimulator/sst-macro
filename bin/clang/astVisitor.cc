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
  mpiCalls_["sstmac_irecv"] = &SkeletonASTVisitor::visitPt2Pt;
  mpiCalls_["sstmac_isend"] = &SkeletonASTVisitor::visitPt2Pt;
  mpiCalls_["sstmac_recv"] = &SkeletonASTVisitor::visitPt2Pt;
  mpiCalls_["sstmac_send"] = &SkeletonASTVisitor::visitPt2Pt;
  mpiCalls_["sstmac_bcast"] = &SkeletonASTVisitor::visitPt2Pt; //bit of a hack
  mpiCalls_["sstmac_allreduce"] = &SkeletonASTVisitor::visitReduce;
  mpiCalls_["sstmac_reduce"] = &SkeletonASTVisitor::visitReduce;
  mpiCalls_["sstmac_allgather"] = &SkeletonASTVisitor::visitCollective;

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
  reservedNames_.insert("nullptr");
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
  while (ifs.good()){
    std::getline(ifs, line);
    validHeaders_.insert(line);
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
  if (headerLoc.isValid() && !useAllHeaders){
    //we are inside a header
    char fullpathBuffer[1024];
    const char* fullpath = realpath(ploc.getFilename(), fullpathBuffer);
    if (fullpath){
      //is this in the list of valid headers
      return validHeaders_.find(fullpath) != validHeaders_.end();
    } else {
      warn(startLoc, *ci_,
           "bad header path location, you probably abused and misused #line in your file");
      return false;
    }
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

  return true; //don't do this anymore - but keep code around

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
    replace(expr, rewriter_, pp.os.str(), *ci_);
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
          replace(expr, rewriter_, pp.os.str(), *ci_);
        }
      }
      default:
        break;
    }
  }
  return true;
}


bool
SkeletonASTVisitor::TraverseCXXDeleteExpr(CXXDeleteExpr* expr, DataRecursionQueue* queue)
{
  if (noSkeletonize_) return true;

  stmt_contexts_.push_back(expr);
  TraverseStmt(expr->getArgument());
  stmt_contexts_.pop_back();
  return true; //don't do this anymore

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
  replace(expr, rewriter_, pp.os.str(), *ci_);
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
SkeletonASTVisitor::VisitCXXOperatorCallExpr(CXXOperatorCallExpr* expr)
{
  Expr* implicitThis = expr->getArg(0);
  if (isa<MemberExpr>(implicitThis)){
    MemberExpr* me = cast<MemberExpr>(implicitThis);
    if (isNullVariable(me->getMemberDecl())){
      errorAbort(expr->getLocStart(), *ci_,
                 "operator used on null variable");
    }
  }
  return true;
}

bool
SkeletonASTVisitor::VisitDeclRefExpr(DeclRefExpr* expr)
{
  Decl* decl =  expr->getFoundDecl()->getCanonicalDecl();
  if (isNullVariable(decl)){
    //we need to delete whatever parent statement this is a part of
    if (stmt_contexts_.empty()){
      if (pragmaConfig_.deletedRefs.find(expr) == pragmaConfig_.deletedRefs.end()){
        errorAbort(expr->getLocStart(), *ci_,
                   "null variable used in statement, but context list is empty");
      } //else this was deleted - so no problem
    } else {
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
    replace(expr->getArg(0), rewriter_, "nullptr", *ci_);
    replace(expr->getArg(3), rewriter_, "nullptr", *ci_);
    //rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
    //rewriter_.ReplaceText(expr->getArg(3)->getSourceRange(), "nullptr");
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
    replace(expr->getArg(0), rewriter_, "nullptr", *ci_);
    replace(expr->getArg(1), rewriter_, "nullptr", *ci_);
    //rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
    //rewriter_.ReplaceText(expr->getArg(1)->getSourceRange(), "nullptr");
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
    replace(expr->getArg(0), rewriter_, "nullptr", *ci_);
    //rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
    deletedStmts_.insert(expr->getArg(0));
  }
}
bool 
SkeletonASTVisitor::TraverseReturnStmt(clang::ReturnStmt* stmt, DataRecursionQueue* queue)
{
  bool skipVisit = noSkeletonize_ ? false : activatePragmasForStmt(stmt);
  if (skipVisit){
    return true;
  }
  
  TraverseStmt(stmt->getRetValue());

  return true;
}

bool
SkeletonASTVisitor::TraverseCXXMemberCallExpr(CXXMemberCallExpr* expr, DataRecursionQueue* queue)
{
  bool skipVisit = noSkeletonize_ ? false : activatePragmasForStmt(expr);
  if (skipVisit){
    return true;
  }
  if (pragmaConfig_.makeNoChanges){
    pragmaConfig_.makeNoChanges = false; //turn off for next guy
  } else {
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
  //this will visit member expr that also visits implicit this
  TraverseStmt(expr->getCallee());
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
    SSTNullVariablePragma* prg = nullptr;
    if (base->getStmtClass() == Stmt::DeclRefExprClass){
      DeclRefExpr* dref = cast<DeclRefExpr>(base);
      prg = getNullVariable(dref->getFoundDecl());
    } else if (base->getStmtClass() == Stmt::MemberExprClass){
      MemberExpr* memExpr = cast<MemberExpr>(base);
      prg = getNullVariable(memExpr->getMemberDecl());
    }

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
        //boy - i really hope this doesn't allocate memory
      }
      return true;
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

  DeclRefExpr* baseFxn = nullptr;
  if (!noSkeletonize_ && !pragmaConfig_.replacePragmas.empty()){
    Expr* fxn = getUnderlyingExpr(const_cast<Expr*>(expr->getCallee()));
    if (fxn->getStmtClass() == Stmt::DeclRefExprClass){
      //this is a basic function call
      baseFxn = cast<DeclRefExpr>(fxn);
      std::string fxnName = baseFxn->getFoundDecl()->getNameAsString();
      for (auto& pair : pragmaConfig_.replacePragmas){
        SSTReplacePragma* replPrg = static_cast<SSTReplacePragma*>(pair.second);
        if (replPrg->fxn() == fxnName){
          replPrg->run(expr, rewriter_);
          return true;
        }
      }
    }
  } else if (!noSkeletonize_) {
    Expr* fxn = getUnderlyingExpr(const_cast<Expr*>(expr->getCallee()));
    if (fxn->getStmtClass() == Stmt::DeclRefExprClass){
      DeclRefExpr* dref = cast<DeclRefExpr>(fxn);
      std::string fxnName = dref->getFoundDecl()->getNameAsString();
      auto iter = mpiCalls_.find(fxnName);
      if (iter != mpiCalls_.end()){
        MPI_Call call = iter->second;
        (this->*call)(expr);
      }
    }
  }

  if (baseFxn){
    //we have a set of standard replacements
  }

  stmt_contexts_.push_back(expr);
  TraverseStmt(expr->getCallee());
  for (int i=0; i < expr->getNumArgs(); ++i){
    Expr* arg = expr->getArg(i);
    if (deletedStmts_.find(arg) == deletedStmts_.end()){
      TraverseStmt(arg);
    }
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

SkeletonASTVisitor::ArrayInfo*
SkeletonASTVisitor::checkArray(VarDecl* D, ArrayInfo* info)
{
  const Type* ty  = D->getType().getTypePtr();
  bool isC99array = ty->isArrayType();
  bool isConstC99array = ty->isConstantArrayType();

  if (isConstC99array){
    const ConstantArrayType* aty = static_cast<const ConstantArrayType*>(ty->getAsArrayTypeUnsafe());
    info->typedefName = "type" + D->getNameAsString();
    std::stringstream sstr;
    sstr << "typedef " << QualType::getAsString(aty->getElementType().split())
         << " " << info->typedefName
         << "[" << aty->getSize().getZExtValue() << "]";
    info->typedefString = sstr.str();
    info->isConstSize = true;
    info->size = aty->getSize().getZExtValue();
    info->retType = "type" + D->getNameAsString() + "*";
    return info;
  } else if (isC99array){
    const ArrayType* aty = ty->getAsArrayTypeUnsafe();
    info->retType = QualType::getAsString(aty->getElementType().split()) + "**";
    info->isConstSize = false;
    return info;
  } else {
    return nullptr;
  }
}

RecordDecl*
SkeletonASTVisitor::checkCombinedStructVarDecl(VarDecl* D)
{
  auto ty = D->getType().getTypePtr();
  if (ty->isStructureType()){
    RecordDecl* recDecl = ty->getAsStructureType()->getDecl();
    if (recDecl->getLocStart() == D->getLocStart()){
      //oh, well, they are both from the same spot
      //so, yeah, combined c-style var and struct decl
      return recDecl;
    }
  }
  return nullptr;
}

SkeletonASTVisitor::AnonRecord*
SkeletonASTVisitor::checkAnonStruct(VarDecl* D, AnonRecord* rec)
{
  auto ty = D->getType().getTypePtr();
  const char* prefix;
  RecordDecl* recDecl;
  if (ty->isStructureType()){
    recDecl = ty->getAsStructureType()->getDecl();
    prefix = "struct";
  } else if (ty->isUnionType()){
    recDecl = ty->getAsUnionType()->getDecl();
    prefix = "union";
  } else {
    return nullptr;
  }

  bool typedefd = typedef_structs_.find(recDecl) != typedef_structs_.end();
  if (!typedefd && recDecl->getNameAsString() == ""){
    rec->decl = recDecl;
    rec->structType = prefix;
    rec->typeName = D->getNameAsString() + "_anonymous_type";
    rec->retType = rec->structType + " "  + rec->typeName + "*";
    return rec;
  } else {
    return nullptr;
  }
}

void
SkeletonASTVisitor::setupGlobalVar(const std::string& scope_prefix,
                                   const std::string& init_prefix,
                                   const std::string& var_repl_prefix,
                                   SourceLocation externVarsInsertLoc,
                                   SourceLocation getRefInsertLoc,
                                   GlobalRedirect_t red_ty,
                                   VarDecl* D,
                                   AnonRecord* anonRecord,
                                   ArrayInfo* arrayInfo)
{  
  //const global variables can't change... so....
  //no reason to do any work tracking them
  if (D->getType().isConstQualified()){
    errorAbort(D->getLocStart(), *ci_,
               "internal compiler error: trying to refactor const global variable");
  }

  std::string& varRepl = globals_[D];
  std::string scopeUniqueVarName = scope_prefix + D->getNameAsString();

  // roundabout way to get the type of the variable
  std::string retType;
  if (arrayInfo){
    retType = arrayInfo->retType;
  } else {
    retType = QualType::getAsString(D->getType().split()) + "*";
  }
  varRepl = "(*" + currentNs_->nsPrefix() + var_repl_prefix + "get_" + scopeUniqueVarName + "())";

  if (anonRecord){
    retType = anonRecord->retType;
    if (!anonRecord->isFxnStatic){
      SourceLocation openBrace = anonRecord->decl->getBraceRange().getBegin();
      rewriter_.InsertText(openBrace, "  " + anonRecord->typeName, false, false);
    }
  }

  //std::string str;
  SourceLocation externStackDeclLoc = externVarsInsertLoc;

  std::string extern_empty;
  llvm::raw_string_ostream extern_var_os(extern_empty);

  NamespaceDecl* outerNsDecl = getOuterNamespace(D);
  if (outerNsDecl){
    externStackDeclLoc = outerNsDecl->getLocStart();
  }

  if (externStackDeclLoc.isInvalid()){
    errorAbort(D->getLocStart(), *ci_, "computed incorrect replacement location for global variable");
  }
  rewriter_.InsertText(externStackDeclLoc, "extern int sstmac_global_stacksize; ");

  extern_var_os << "extern int __offset_" << scopeUniqueVarName << "; ";
  if (!D->hasExternalStorage()){
    currentNs_->replVars.insert(scopeUniqueVarName);
    if (red_ty == Extern){
      defineGlobalVarInitializers(scopeUniqueVarName, D->getNameAsString(),
                                  init_prefix, extern_var_os);
    } else {
      declareStaticInitializers(scopeUniqueVarName, extern_var_os);
    }
  }
  if (externVarsInsertLoc.isInvalid()){
    errorAbort(D->getLocStart(), *ci_,
               "got bad global variable source location - maybe this is a multi-declaration? "
               "this breaks current source-2-source");
  }
  rewriter_.InsertText(externVarsInsertLoc, extern_var_os.str());

  std::string empty;
  llvm::raw_string_ostream os(empty);
  std::string varName = currentNs_->nsPrefix() + D->getNameAsString();
  std::string checkName = currentNs_->nsPrefix() + scopeUniqueVarName;
  bool notDeclared = globalsDeclared_.find(checkName) == globalsDeclared_.end();

  static const char* globalOffsetString = "sizeof(int)";
  if (notDeclared){
    if (arrayInfo && !arrayInfo->isFxnStatic && arrayInfo->isConstSize){
      os << arrayInfo->typedefString << "; ";
    }
    os << "static inline " << retType << " get_" << scopeUniqueVarName << "(){ "
       << " int stack; int* stackPtr = &stack; "
       << " uintptr_t localStorage = ((uintptr_t) stackPtr/sstmac_global_stacksize)*sstmac_global_stacksize; "
       << " char** globalMapPtr = (char**)(localStorage+" << globalOffsetString << "); "
       << " char* offsetPtr = *globalMapPtr + __offset_" << scopeUniqueVarName << ";"
       << "return (((" << retType << ")((void*)offsetPtr))); "
       << "}";
    globalsDeclared_.insert(checkName);
  }
  os << "  ";

  if (getRefInsertLoc.isInvalid()){
    errorAbort(D->getLocStart(), *ci_, "failed replacing global variable declaration");
  }
  rewriter_.InsertText(getRefInsertLoc, os.str());
}

clang::SourceLocation
SkeletonASTVisitor::getEndLoc(const VarDecl *D)
{
  SourceLocation endLoc = Lexer::findLocationAfterToken(D->getLocEnd(), tok::semi,
                                 ci_->getSourceManager(), ci_->getLangOpts(), false);
  int numTries = 1;
  while (endLoc.isInvalid() && numTries < 10){
    SourceLocation newLoc = D->getLocEnd().getLocWithOffset(numTries);
    endLoc = Lexer::findLocationAfterToken(newLoc, tok::semi,
                             ci_->getSourceManager(), ci_->getLangOpts(), false);
    ++numTries;
  }
  return endLoc;
}

bool
SkeletonASTVisitor::checkStaticFileVar(VarDecl* D)
{
  SourceLocation end = getEndLoc(D);
  AnonRecord rec;
  AnonRecord* recPtr = checkAnonStruct(D, &rec);
  ArrayInfo info;
  ArrayInfo* infoPtr = checkArray(D, &info);
  setupGlobalVar(currentNs_->filePrefix(), "", "",
                 end, end, Extern, D, recPtr, infoPtr);
  return true;
}

bool
SkeletonASTVisitor::checkGlobalVar(VarDecl* D)
{
  AnonRecord rec;
  AnonRecord* recPtr = checkAnonStruct(D, &rec);
  ArrayInfo info;
  ArrayInfo* infoPtr = checkArray(D, &info);
  SourceLocation end = getEndLoc(D);
  if (end.isInvalid()){
    errorAbort(D->getLocStart(), *ci_,
               "Multi-variable declarations for global variables not allowed");
  }
  setupGlobalVar("", "", "", end, end, Extern, D, recPtr, infoPtr);
  return true;
}

void
SkeletonASTVisitor::deleteStmt(Stmt *s)
{
  replace(s, rewriter_, "", *ci_);
}

bool
SkeletonASTVisitor::checkStaticFxnVar(VarDecl *D)
{
  FunctionDecl* outerFxn = fxn_contexts_.front();
  SourceLocation insertLoc = outerFxn->getLocStart();
  std::stringstream prefix_sstr; prefix_sstr << "_";
  for (FunctionDecl* fxn : fxn_contexts_){
    prefix_sstr << "_" << fxn->getNameAsString();
  }

  AnonRecord anonRec;
  AnonRecord* anonRecPtr = checkAnonStruct(D, &anonRec);
  ArrayInfo arrayInfo;
  ArrayInfo* arrayInfoPtr = checkArray(D, &arrayInfo);

  std::string init_prefix = prefix_sstr.str();
  std::string scope_prefix = currentNs_->filePrefix() + init_prefix;
  /** we have to move the initializer outside the function, which is annoying
   *  to avoid name conflicts, we have to change the name of the initializer to
   *  something unique */
  PrettyPrinter new_init_pp;
  if (anonRecPtr){
    if (anonRecPtr->structType.size() == 0){
      errorAbort(D->getLocStart(), *ci_, "failed to compute type of anonymous struct");
    }
    new_init_pp.os << anonRecPtr->structType
                   << " " << anonRecPtr->typeName
                   << "{";
    auto end = anonRecPtr->decl->field_end();
    for (auto iter=anonRecPtr->decl->field_begin(); iter != end; ++iter){
      FieldDecl* fdec = *iter;
      new_init_pp.print(fdec);
      new_init_pp.os << "; ";
    }
    new_init_pp.os << "}; ";
    if (anonRecPtr->structType.size() == 0){
      errorAbort(D->getLocStart(), *ci_, "failed to compute type of anonymous struct");
    }
    new_init_pp.os << "static " << anonRecPtr->structType
                   << " " << anonRecPtr->typeName;
  } else if (arrayInfoPtr) {
    new_init_pp.os << arrayInfoPtr->typedefString << "; ";
    new_init_pp.os << "static " << arrayInfoPtr->typedefName;
    arrayInfoPtr->isFxnStatic = true;
  } else {
    new_init_pp.os << "static " << QualType::getAsString(D->getType().split());
  }
  new_init_pp.os << " " << init_prefix << D->getNameAsString();
  if (D->hasInit()){
    new_init_pp.os << " = ";
    new_init_pp.print(D->getInit());
  }
  new_init_pp.os << ";";
  rewriter_.InsertText(insertLoc, new_init_pp.str());
  //now that we have moved the initializer, we can do the replacement
  //but the replacement should point to the new initializer
  setupGlobalVar(scope_prefix, init_prefix, "",
                 outerFxn->getLocStart(), insertLoc,
                 Extern, D, anonRecPtr, arrayInfoPtr);
  SourceLocation endLoc = getEndLoc(D);
  SourceRange rng(D->getLocStart(), endLoc);
  //while we're at it, might as well get rid of the old initializer
  RecordDecl* combinedDecl = checkCombinedStructVarDecl(D);
  if (!combinedDecl){
    //replace(rng, rewriter_, "", *ci_);
  }
  return true;
}

NamespaceDecl*
SkeletonASTVisitor::getOuterNamespace(Decl *D)
{
  DeclContext* nsCtx = nullptr;
  DeclContext* next = D->getDeclContext();;
  DeclContext* ctx = nullptr;
  while (next && ctx != next){
    ctx = next;
    if (ctx->getDeclKind() == Decl::Namespace){
      nsCtx = ctx;
    }
    next = next->getParent();
  }

  if (nsCtx) return cast<NamespaceDecl>(nsCtx);
  else return nullptr;
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
    std::stringstream var_repl_sstr;
    for (CXXRecordDecl* decl : class_contexts_){
      scope_sstr << "_" << decl->getNameAsString();
      var_repl_sstr << decl->getNameAsString() << "::";
    }
    std::string scope_prefix = scope_sstr.str();
    if (!D->hasInit()){
      //no need for special scope prefixes - these are fully scoped within in the class
      setupGlobalVar(scope_prefix, "", var_repl_sstr.str(),
                     outerCls->getLocStart(), getEndLoc(D),
                     CxxStatic, D, nullptr, nullptr);
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
  for (SSTPragma* prg : pragmas_.getMatches(D)){
    pragmaConfig_.pragmaDepth++;
    //pragma takes precedence - must occur in pre-visit
    prg->activate(D, rewriter_, pragmaConfig_);
    pragmaConfig_.pragmaDepth--;
  }

  if (pragmaConfig_.makeNoChanges){
    pragmaConfig_.makeNoChanges = false;
    return true;
  }

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
  } else if (D->getTSCSpec() != TSCS_unspecified){
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

  //do not replace const global variables
  if (D->getType().isConstQualified()){
    return true;
  }

  bool isGlobal = true;
  bool rc = true;
  if (insideClass()){
    rc = checkDeclStaticClassVar(D);
  } else if (insideFxn() && D->isStaticLocal()){
    rc = checkStaticFxnVar(D);
  } else if (D->isCXXClassMember()){
    rc = checkInstanceStaticClassVar(D);
  } else if (D->getStorageClass() == StorageClass::SC_Static){
    rc = checkStaticFileVar(D);
  } else if (!D->isLocalVarDeclOrParm()){
    rc = checkGlobalVar(D);
  } else {
    isGlobal = false;
  }

  if (isGlobal && D->getType().getTypePtr()->isFunctionPointerType()){
      errorAbort(D->getLocStart(), *ci_,
                 "global variable function pointers not compatible with auto-skeletonizer - use #pragma sst keep if possible");
  }

  return true;
}

void
SkeletonASTVisitor::replaceMain(clang::FunctionDecl* mainFxn)
{
  if (!mainFxn->getDefinition()) return;

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
    replace(rng, rewriter_, sstr.str(), *ci_);
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

  if (D->isLocalExternDecl()){
    //weird extern declaration - skip it
    return true;
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

  bool skipVisit = noSkeletonize_ ? false : activatePragmasForDecl(D);

  if (D->isThisDeclarationADefinition()) {
    ++insideCxxMethod_;
    if (!skipVisit){
      TraverseStmt(D->getBody());
    }
    --insideCxxMethod_;
  }
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
SkeletonASTVisitor::TraverseCompoundAssignOperator(CompoundAssignOperator *op, DataRecursionQueue *queue)
{
  //nothing special for now
  return TraverseBinaryOperator(op,queue);
}

bool
SkeletonASTVisitor::TraverseBinaryOperator(BinaryOperator* op, DataRecursionQueue* queue)
{
  bool skipVisit = noSkeletonize_ ? false : activatePragmasForStmt(op);
  stmt_contexts_.push_back(op);
  if (skipVisit){
    deletedStmts_.insert(op);
  } else {
    sides_.push_back(BinOpSide::LHS);
    TraverseStmt(op->getLHS());
    sides_.pop_back();
    sides_.push_back(BinOpSide::RHS);
    TraverseStmt(op->getRHS());
    sides_.pop_back();
  }
  stmt_contexts_.pop_back();
  return true;
}

bool
SkeletonASTVisitor::TraverseIfStmt(IfStmt* stmt, DataRecursionQueue* queue)
{
  bool skipVisit = noSkeletonize_ ? false : activatePragmasForStmt(stmt);
  if (skipVisit){
    return true;
  }

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
          //boy, I really hope this doesn't allocate memory
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
SkeletonASTVisitor::VisitArraySubscriptExpr(ArraySubscriptExpr* expr)
{
  Expr* base = getUnderlyingExpr(expr->getBase());
  if (base->getStmtClass() == Expr::DeclRefExprClass){
    DeclRefExpr* dref = cast<DeclRefExpr>(base);
    if (isNullVariable(dref->getFoundDecl())){
      if (stmt_contexts_.empty()){
        errorAbort(expr->getLocStart(), *ci_,
                   "array subscript applied to null variable");
      } else {
        deleteStmt(stmt_contexts_.front());
      }
    }
  }
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
SkeletonASTVisitor::VisitTypedefDecl(TypedefDecl* D)
{
  auto ty = D->getUnderlyingType().getTypePtr();
  if (ty->isStructureType()){
    auto str_ty = ty->getAsStructureType();
    if (!str_ty){
      errorAbort(D->getLocStart(), *ci_, "structure type did not return a record declaration");
    }
    typedef_structs_[str_ty->getDecl()] = D;
  }
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
  return true;
}

bool
SkeletonASTVisitor::dataTraverseStmtPost(Stmt* S)
{
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
  FunctionDecl* fxnCalled = nullptr;
  if (s->getStmtClass() == Stmt::IfStmtClass){
    //oooooh, not good - I could really foobar things here
    //crash and burn and tell programmer to fix it
    warn(use_stmt->getLocStart(), *ci_,
         "null variables used as predicate in if-statement - "
         "this could produce undefined behavior - forcing always false");
    IfStmt* ifs = cast<IfStmt>(s);
    replace(ifs->getCond(), rewriter_, "0", *ci_);
    return;
  } else if (s->getStmtClass() == Stmt::CallExprClass){
    CallExpr* call = cast<CallExpr>(s);
    fxnCalled = call->getDirectCallee();
  } else if (s->getStmtClass() == Stmt::CXXMemberCallExprClass){
    CXXMemberCallExpr* call = cast<CXXMemberCallExpr>(s);
    fxnCalled = call->getMethodDecl();
  }

  if (fxnCalled){
    if (keepWithNullArgs(fxnCalled)){
      return;
    } else if (fxnCalled->getNumParams() == 0 || deleteWithNullArgs(fxnCalled)){
      //if the function takes no parameters - safe to delete because
      //the callee is the null variable
      //OR if explicitly marked safe to delete
    } else {
      std::stringstream sstr;
      sstr << "call expression '" << fxnCalled->getNameAsString()
            << "' has null argument, but I don't know what to do with it\n"
            << " function should be marked with either "
            << " pragma sst keep_if_null_args or delete_if_null_args";
      warn(use_stmt->getLocStart(), *ci_, sstr.str());
    }
  }

  if (!loop_contexts_.empty()){
    bool justWarn = false;
    if (d->getKind() == Decl::Var){
      auto cls = s->getStmtClass();
      if (cls == Stmt::BinaryOperatorClass || cls == Stmt::CompoundAssignOperatorClass){
        if (sides_.back() == LHS){
          //well, this is on the left hand side
          VarDecl* vd = cast<VarDecl>(d);
          if (vd->getType()->isPointerType()){
            //whatever - pointer arithmetic, let it stay
            justWarn = true;
          }
        }
      }
    }
    if (justWarn){
      warn(use_stmt->getLocStart(), *ci_,
           "null variable used inside loop - loop skeletonization "
           "must be indicated with #pragma sst compute outside loop");
    } else {
      errorAbort(use_stmt->getLocStart(), *ci_,
                 "null variable used inside loop - loop skeletonization "
                 "must be indicated with #pragma sst compute outside loop");
    }
  }

  if (s->getStmtClass() == Stmt::DeclStmtClass){
    DeclStmt* ds = cast<DeclStmt>(s);
    Decl* lhs = ds->getSingleDecl();
    if (lhs && lhs != d){
      //well, crap, the nullness might propagate to a new variable
      if (lhs->getKind() == Decl::Var){
        //yep, it does
        VarDecl* vd = cast<VarDecl>(lhs);
        //propagate the null-ness to this new variable
        SSTNullVariablePragma* fakePragma = new SSTNullVariablePragma;
        pragmaConfig_.nullVariables[vd] = fakePragma;
      }
    }
  }
  deleteStmt(s); //just delete it
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
      case SSTPragma::Instead:
      case SSTPragma::Return:
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
SkeletonASTVisitor::replaceGlobalUse(Decl* decl, SourceRange replRng)
{
  if (keepGlobals_) return;

  auto iter = globals_.find(decl);
  if (iter != globals_.end()){
    //if (decl->getNameAsString() == "ncptl_cycles_per_usec") abort();
    replace(replRng, rewriter_, iter->second, *ci_);
  }
}

bool
FirstPassASTVistor::VisitDecl(Decl *d)
{
  if (noSkeletonize_) return true;

  std::list<SSTPragma*> pragmas = pragmas_.getMatches(d,true);
  for (SSTPragma* prg : pragmas){
    prg->activate(d, rewriter_, pragmaConfig_);
    pragmas_.erase(prg);
  }
  return true;
}

bool
FirstPassASTVistor::VisitStmt(Stmt *s)
{
  if (noSkeletonize_) return true;

  std::list<SSTPragma*> pragmas = pragmas_.getMatches(s,true);
  for (SSTPragma* prg : pragmas){
    prg->activate(s, rewriter_, pragmaConfig_);
    pragmas_.erase(prg);
  }
  return true;
}

FirstPassASTVistor::FirstPassASTVistor(SSTPragmaList& prgs, clang::Rewriter& rw,
                   PragmaConfig& cfg) :
  pragmas_(prgs), rewriter_(rw), pragmaConfig_(cfg)
{
  const char* skelStr = getenv("SSTMAC_SKELETONIZE");
  if (skelStr){
    bool doSkel = atoi(skelStr);
    noSkeletonize_ = !doSkel;
  }
}
