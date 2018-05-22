/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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
#include "computePragma.h"
#include <iostream>
#include <fstream>
#include <sstmac/common/sstmac_config.h>

clang::LangOptions Printing::langOpts;
clang::PrintingPolicy Printing::policy(Printing::langOpts);

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

static const Type*
getBaseType(VarDecl* D){
  auto ty = D->getType().getTypePtr();
  while (ty->isPointerType() && !(ty->isFunctionPointerType() || ty->isFunctionProtoType())){
    ty = ty->getPointeeType().getTypePtr();
  }
  return ty;
}

static std::string appendText(clang::Expr* expr, const std::string& toAppend)
{
  PrettyPrinter pp;
  pp.print(expr);
  pp.os << toAppend;
  return pp.str();
}

void
SkeletonASTVisitor::initConfig()
{
  const char* skelStr = getenv("SSTMAC_SKELETONIZE");
  if (skelStr){
    bool doSkel = atoi(skelStr);
    noSkeletonize_ = !doSkel;
  }

  const char* mainStr = getenv("SSTMAC_REFACTOR_MAIN");
  if (mainStr){
    refactorMain_ = atoi(mainStr);
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
  reservedNames_.insert("_keywords_");
  reservedNames_.insert("_keyword_register_");

  ignoredHeaders_.insert("chrono");
  ignoredHeaders_.insert("thread_safe_new.h");
  ignoredHeaders_.insert("_ctype.h");
  ignoredHeaders_.insert("__locale");
  ignoredHeaders_.insert("ostream");
  ignoredHeaders_.insert("istream");
  ignoredHeaders_.insert("iostream");
  ignoredHeaders_.insert("fstream");
  ignoredHeaders_.insert("locale");
  ignoredHeaders_.insert("iterator");
  ignoredHeaders_.insert("ios");
  ignoredHeaders_.insert("algorithm");
  ignoredHeaders_.insert("__debug");
  ignoredHeaders_.insert("ios_base.h"); //gcc6 mac
  ignoredHeaders_.insert("functional");
  ignoredHeaders_.insert("codecvt.h"); //gcc6 mac
  ignoredHeaders_.insert("locale_classes.h"); //gcc6 mac
  ignoredHeaders_.insert("locale_facets.tcc"); //gcc6 mac
  ignoredHeaders_.insert("locale_facets.h"); //gcc6 mac
  ignoredHeaders_.insert("localefwd.h"); //gcc6 mac
  ignoredHeaders_.insert("stl_algobase.h"); //gcc6 mac
  ignoredHeaders_.insert("basic_string.h"); //gcc49 mac

  globalVarWhitelist_.insert("_DefaultRuneLocale");
  globalVarWhitelist_.insert("__stdoutp");
  globalVarWhitelist_.insert("optarg");
  globalVarWhitelist_.insert("optind");
  globalVarWhitelist_.insert("opterr");
  globalVarWhitelist_.insert("daylight");
  globalVarWhitelist_.insert("tzname");
  globalVarWhitelist_.insert("timezone");
  globalVarWhitelist_.insert("optreset");
  globalVarWhitelist_.insert("suboptarg");
  globalVarWhitelist_.insert("getdate_err");
  globalVarWhitelist_.insert("optopt");
  globalVarWhitelist_.insert("__mb_cur_max");
  globalVarWhitelist_.insert("sstmac_global_stacksize");
  globalVarWhitelist_.insert("__stderrp");
  globalVarWhitelist_.insert("__stdinp");

  sstmacFxnPrepends_.insert("free");
}

void
SkeletonASTVisitor::registerNewKeywords(std::ostream& os)
{
  if (pragmaConfig_.newParams.empty()){
    return;
  }

  os << "#include <sprockit/keyword_registration.h>"
     << "\nRegisterKeywords(";
  for (auto& str : pragmaConfig_.newParams){
    os << "\n{\"" << str << "\", \"new keyword\" },";
  }
  os << "\n);";
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

  //ignore eli variables
  std::string varName = D->getNameAsString();
  if (varName.size() >= 5){
    std::string last4 = varName.substr(varName.size() - 4);
    if (last4 == "_eli"){
      return false;
    }
  }

  bool useAllHeaders = false;
  if (headerLoc.isValid() && !useAllHeaders){
    //we are inside a header
    char fullpathBuffer[1024];
    const char* fullpath = realpath(ploc.getFilename(), fullpathBuffer);
    if (fullpath){
      //is this in the list of valid headers
      bool valid = validHeaders_.find(fullpath) != validHeaders_.end();
      if (!valid){
        std::string path(fullpath);
        auto slashPos = path.find_last_of('/');
        if (slashPos != std::string::npos){
          path = path.substr(slashPos);
        }
        auto dotPos = path.find_last_of('.');
        if (dotPos == std::string::npos){
          return valid; //no suffix - assume valid c++ header with no suffix, e.g. <cstdlib>
        }
        std::string suffix = path.substr(dotPos+1);
        if (suffix != "h" && suffix != "hpp" && suffix != "tcc"){
          std::string error = std::string("got included global variable ") + D->getNameAsString()
              + "in unknown file " + fullpath + " with suffix ." + suffix;
          errorAbort(D->getLocStart(), *ci_, error);
        }
      }
      return valid;
    } else {
      warn(startLoc, *ci_,
           "bad header path location, you probably abused and misused #line in your file");
      return false;
    }
  }

  /**
  if (headerLoc.isValid()){
    char fullpathBuffer[1024];
    const char* fullpath = realpath(ploc.getFilename(), fullpathBuffer);
    if (fullpath){
      std::string includeName(fullpath);
      auto checkSst = includeName.find("sstmac");
      if (checkSst != std::string::npos){
        return false; //never do this on sstmac headers
      }

      auto checkSumi = includeName.find("sumi");
      if (checkSumi != std::string::npos){
        return false; //never do this on sumi headers
      }

      auto checkSpkt = includeName.find("sprockit");
      if (checkSpkt != std::string::npos){
        return false; //never do this on sprockit headers
      }

      auto slashPos = includeName.find_last_of('/');
      if (slashPos != std::string::npos){
        includeName = includeName.substr(slashPos+1);
      }

      auto iter = ignoredHeaders_.find(includeName);
      if (iter != ignoredHeaders_.end()){
        //this is in a header that we know to ignore
        return false;
      }
    }

    auto iter = globalVarWhitelist_.find(D->getNameAsString());
    if (iter != globalVarWhitelist_.end()){
      //this variable is whitelisted from de-globalization
      return false;
    }

  }
  */


  //not a header - good to go
  return true;
}


bool
SkeletonASTVisitor::VisitCXXNewExpr(CXXNewExpr *expr)
{
  return true; //don't do this anymore - but keep code around

  if (noSkeletonize_) return true;
  if (deletedStmts_.find(expr) != deletedStmts_.end()){
    //already deleted - do nothing here
    return true;
  }
}


bool
SkeletonASTVisitor::TraverseCXXDeleteExpr(CXXDeleteExpr* expr, DataRecursionQueue* queue)
{
  activeBinOpIdx_ = IndexResetter;

  if (noSkeletonize_) return true;

  goIntoContext(expr, [&]{
    TraverseStmt(expr->getArgument());
  });

  return true;
}

bool
SkeletonASTVisitor::TraverseInitListExpr(InitListExpr* expr, DataRecursionQueue* queue)
{
  if (visitingGlobal_){
    QualType qty = expr->getType();
    if (qty->isStructureType()){
      const RecordType* ty = qty->getAsStructureType();
      RecordDecl* rd = ty->getDecl();
      auto iter = rd->field_begin();
      for (int i=0; i < expr->getNumInits(); ++i, ++iter){
        Expr* ie = const_cast<Expr*>(expr->getInit(i));
        Expr* base_ie = getUnderlyingExpr(ie);
        if (base_ie->getStmtClass() == Stmt::DeclRefExprClass){
          DeclRefExpr* dr = cast<DeclRefExpr>(base_ie);
          if (isGlobal(dr)){
            //init expr must be compile-time constants - which means not refactoring this global
            continue;
          }
        }
        FieldDecl* fd = *iter;
        activeFieldDecls_.push_back(fd);
        activeInits_.push_back(getUnderlyingExpr(ie));
        TraverseStmt(ie);
        activeFieldDecls_.pop_back();
        activeInits_.pop_back();
      }
    } else {
      bool isArray = expr->getType()->isConstantArrayType();
      for (int i=0; i < expr->getNumInits(); ++i){
        if (isArray) initIndices_.push_back(i);
        Expr* ie = const_cast<Expr*>(expr->getInit(i));
        activeInits_.push_back(getUnderlyingExpr(ie));
        TraverseStmt(ie);
        activeInits_.pop_back();
        if (isArray) initIndices_.pop_back();
      }
    }
  } else {
    for (int i=0; i < expr->getNumInits(); ++i){
      Expr* ie = const_cast<Expr*>(expr->getInit(i));
      TraverseStmt(ie);
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

void
SkeletonASTVisitor::executeCurrentReplacements()
{
  for (auto& pair : stmtReplacements_.back()){
    ::replace(pair.first, rewriter_, pair.second, *ci_);
  }
}

void
SkeletonASTVisitor::replace(SourceRange rng, const std::string& repl)
{
  if (stmtContexts_.empty()){
    //go ahead an immediately do the replacement
    ::replace(rng, rewriter_, repl, *ci_);
  } else {
    //we can foobar things if we do the replacement immediately
    //in traversing, a null variable or other trigger might delete the statement we are working on
    //create a delayed replacement to execute later if needed
    stmtReplacements_.back().emplace_back(rng, repl);
  }
}

Stmt*
SkeletonASTVisitor::checkNullAssignments(clang::NamedDecl* src, bool hasReplacement)
{
  //first check for DeclStmts
  for (Stmt* s : stmtContexts_){
    if (s->getStmtClass() == Stmt::DeclStmtClass){
      DeclStmt* ds = cast<DeclStmt>(s);
      Decl* lhs = ds->getSingleDecl();
      if (lhs && lhs->getKind() == Decl::Var){
        propagateNullness(lhs, src);
        return s;
      }
      break;
    }
  }

  Expr* outermost = nullptr;
  for (int i=activeBinOpIdx_; i < binOps_.size(); ++i){
    auto& pair = binOps_[i];
    BinaryOperator* binOp = pair.first;
    BinOpSide side = pair.second;

    //we are assigning to something on the LHS of this
    if (binOp->getOpcode() == BO_Assign && side == RHS){
      Expr* lhs = getUnderlyingExpr(binOp->getLHS());
      if (lhs->getStmtClass() == Stmt::MemberExprClass){
        if (!outermost) outermost = binOp;
        MemberExpr* expr = cast<MemberExpr>(lhs);
        NamedDecl* member = expr->getMemberDecl();
        if (!isNullVariable(member) && !hasReplacement){
          //oooh - not good
          std::string error = "member " + member->getNameAsString()
              + " is assigned to null_variable, but is not a null_variable itself";
          errorAbort(expr->getLocStart(), *ci_, error);
        }
      } else if (lhs->getStmtClass() == Stmt::DeclRefExprClass){
        if (!outermost)  outermost = binOp;
        DeclRefExpr* dref = cast<DeclRefExpr>(lhs);
        propagateNullness(dref->getFoundDecl(), src);
      }
    }
  }
  return outermost;
}

void
SkeletonASTVisitor::replaceNullVariableConnectedContext(Expr* expr, const std::string& repl)
{
  //don't actually delete the expression it is a part of - but replace it
  //the expression should be a pointer expression involving pointer math
  if (activeBinOpIdx_ >= 0){
    auto& pair = binOps_[activeBinOpIdx_];
    BinaryOperator* binOp = pair.first;
    BinOpSide side = pair.second;
    Expr* toDel = nullptr;
    if (binOp->getOpcode() == BO_Assign){
      if (side == LHS){
        //replace the whole thing - don't ever assign to null value
        toDel = binOp;
      } else if (!repl.empty()){
        //it is the right hand side that needs replacing and I have a repl
        //this should have happened already - propagateNullness(binOp, nd);
        toDel = getUnderlyingExpr(binOp->getRHS());
      } else {
        toDel = binOp;
      }
    } else {
      toDel = binOp;
    }
    ::replace(toDel, rewriter_, repl, *ci_);
    //pass the delete up to the owner
    throw StmtDeleteException(toDel);
  }

  //we are part of a member expr if either we are a member expr
  //or the base of another member expr
  bool isMemberExpr = expr->getStmtClass() == Stmt::MemberExprClass
      || !memberAccesses_.empty();

  //figure out the outermost stmt we are connected to
  Stmt* outermost = nullptr;
  for (Stmt* s : stmtContexts_){
    switch(s->getStmtClass()){
    case Stmt::CallExprClass:
    case Stmt::ForStmtClass:
    case Stmt::WhileStmtClass: //clear
      outermost = nullptr;
      break;
    case Stmt::CXXMemberCallExprClass: //members connect to members
    case Stmt::CXXOperatorCallExprClass:
    case Stmt::MemberExprClass:
      if (isMemberExpr){ //we can connect through implicit this
        if (!outermost) outermost = s;
      } else {
        outermost = nullptr;
      }
      break;
    case Stmt::BinaryOperatorClass:
    case Stmt::CXXDeleteExprClass:
    case Stmt::UnaryOperatorClass:
      if (!outermost) outermost = s; //connect
      break;
    default:
      break; //neither break nor add connections
    }
  }

  if (outermost){
    //we need to delet the statement we are connected to
    replaceNullVariableStmt(outermost, repl);
  } else {
    //just replace the expr outright
    replaceNullVariableStmt(expr, repl);
  }
}

void
SkeletonASTVisitor::nullDereferenceError(Expr* expr, const std::string& varName)
{
  std::stringstream sstr;
  for (Stmt* s : loopContexts_){
    sstr << "consider skeletonizing with pragma sst compute here: "
         << s->getLocStart().printToString(ci_->getSourceManager()) << "\n";
  }
  sstr << "null_variable " << varName << " used in dereference";
  errorAbort(expr->getLocStart(), *ci_, sstr.str());
}

void
SkeletonASTVisitor::addTransitiveNullInformation(NamedDecl* decl, std::ostream& os, SSTNullVariablePragma* prg)
{
  os << "\nvariable " << decl->getNameAsString() << " is transitively null ";
  SSTNullVariablePragma* transPrg = prg->getTransitive();
  NamedDecl* nd = transPrg->getAppliedDecl();
  if (nd) os << "from " << nd->getNameAsString();
  transPrg = transPrg->getTransitive();
  while (transPrg){
    nd = transPrg->getAppliedDecl();
    if (nd) os << "->" << nd->getNameAsString();
  }
}

void
SkeletonASTVisitor::visitNullVariable(Expr* expr, NamedDecl* nd)
{
  SSTNullVariablePragma* nullVarPrg = getNullVariable(nd->getCanonicalDecl());

  if (!nullVarPrg->deleteAll() && !activeDerefs_.empty()){
    if (nullVarPrg->skeletonizeCompute()){
      //oh, okay - auto-skeletonize computes involving this
      if (!loopContexts_.empty()){
        //skeletonize ALL containing loops
        Stmt* loop = loopContexts_.front();
        if (loop->getStmtClass() == Stmt::ForStmtClass){
          ForStmt* forLoop = cast<ForStmt>(loop);
          SSTComputePragma::replaceForStmt(forLoop, *ci_, pragmas_, rewriter_,
                                           pragmaConfig_, this, "");
        } else {
          nullDereferenceError(expr, nd->getNameAsString());
        }
        throw StmtDeleteException(loop);
      }
    }
    nullDereferenceError(expr, nd->getNameAsString());
  }

  bool nullSafeFunctionCall = false;
  if (!nullVarPrg->deleteAll() && !activeFxnParams_.empty()){
    //we have "IPA" to deal with here
    //a null variable MUST map into a null variable - otherwise this is an error
    ParmVarDecl* pvd = activeFxnParams_.front();
    if (!isNullVariable(pvd)){
      const DeclContext* dc = pvd->getParentFunctionOrMethod();
      if (!isNullSafeFunction(dc)){
        std::string fxnName;
        SourceLocation fxnDeclLoc;
        if (dc->getDeclKind() == Decl::Function){
          const FunctionDecl* fd = cast<const FunctionDecl>(dc);
          fxnName = fd->getNameAsString();
          fxnDeclLoc = fd->getLocStart();
        }
        std::stringstream sstr;
        sstr << "null variable " << nd->getNameAsString()
            << " mapped into non-null parameter " << pvd->getNameAsString()
            << " in function ";
        if (!fxnName.empty()){
          sstr << fxnName << " declared at "
               << fxnDeclLoc.printToString(ci_->getSourceManager());
        }
        if (nullVarPrg->isTransitive())
          addTransitiveNullInformation(nd, sstr, nullVarPrg);
        errorAbort(expr->getLocStart(), *ci_, sstr.str());
      }
    }
    nullSafeFunctionCall = true;
  }

  bool hasRepl = nullVarPrg->hasReplacement();
  Stmt* outerMostAssignment = checkNullAssignments(nd, hasRepl);
  if (hasRepl){
    replaceNullVariableConnectedContext(expr, nullVarPrg->getReplacement());
  } else if (!activeIfs_.empty()){
    if (activeIfs_.size() > 1){
      errorAbort(activeIfs_.back()->getLocStart(), *ci_,
                 "internal error: cannot handle nested if-stmts with null variables");
    }
    IfStmt* s  = activeIfs_.back();
    nullifyIfStmt(s,nd);
  } else if (nullVarPrg->deleteAll() && !stmtContexts_.empty()){
    deleteNullVariableStmt(stmtContexts_.front());
  } else if (outerMostAssignment){
    //okay, a null is propagating to stuff
    //however, I have no replacement and so must delete completely
    if (outerMostAssignment->getStmtClass() == Stmt::BinaryOperatorClass){
      BinaryOperator* bop = cast<BinaryOperator>(outerMostAssignment);
      Expr* lhs = getUnderlyingExpr(bop->getLHS());
      Expr* rhs = getUnderlyingExpr(bop->getRHS());
      if (lhs == expr && rhs->getStmtClass() == Stmt::DeclRefExprClass){
        DeclRefExpr* dref = cast<DeclRefExpr>(rhs);
        NamedDecl* rnd = dref->getFoundDecl();
        if (!isNullVariable(rnd->getCanonicalDecl())){
          std::string warning = "assigning to null " + nd->getNameAsString()
              + " directly from a non-null variable - skeletonization might be missed";
          warn(bop->getLocStart(), *ci_, warning);
        }
      }
    }
    deleteNullVariableStmt(outerMostAssignment);
  } else {
    //I am not assigning to anyone nor am I passed along as
    //a function parameter - but I have not been given permission to delete all
    //that's okay - I'm a meaningless standalone statement - delete me
    if (!activeFxnParams_.empty()){
      //but I can't just delete if I am being passed off to another expression
      std::stringstream sstr;
      sstr << "variable " << nd->getNameAsString() << " is a null_variable "
           << " and passed to a function with no replacement specified ";
      if (nullVarPrg->isTransitive())
        addTransitiveNullInformation(nd, sstr, nullVarPrg);
      errorAbort(expr->getLocStart(), *ci_, sstr.str());
    }
    replaceNullVariableConnectedContext(expr, "");
  }
}

bool
SkeletonASTVisitor::TraverseDecltypeTypeLoc(DecltypeTypeLoc loc)
{
  //don't visit decltype expressions... just let them go
  return true;
}

bool
SkeletonASTVisitor::VisitDeclRefExpr(DeclRefExpr* expr)
{
  clang::NamedDecl* nd = expr->getFoundDecl();

  if (isNullVariable(nd->getCanonicalDecl())){
    bool shouldVisitNull = true;
    if (!memberAccesses_.empty()){
      //the null is the base of a member expr
      shouldVisitNull = deleteNullVariableMember(nd, memberAccesses_.back());
    }
    if (shouldVisitNull){
      try {
        visitNullVariable(expr, nd);
      } catch (StmtDeleteException& e){
        if (e.deleted != expr) throw e;
      }
      return true;
    }
  }
  //not a null variable
  maybeReplaceGlobalUse(expr, expr->getSourceRange());
  return true;
}

void
SkeletonASTVisitor::visitCollective(CallExpr *expr)
{
  if (noSkeletonize_) return;

  //first buffer argument to nullptr
  if (expr->getArg(0)->getType()->isPointerType()){
    //make sure this isn't a shortcut function without buffers
    replace(expr->getArg(0), "nullptr");
    replace(expr->getArg(3), "nullptr");
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
    replace(expr->getArg(0), "nullptr");
    replace(expr->getArg(1), "nullptr");
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
    replace(expr->getArg(0), "nullptr");
    //rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
    deletedStmts_.insert(expr->getArg(0));
  }
}
bool 
SkeletonASTVisitor::TraverseReturnStmt(clang::ReturnStmt* stmt, DataRecursionQueue* queue)
{
  PragmaActivateGuard pag(stmt, this);
  if (pag.skipVisit()) return true;
  
  TraverseStmt(stmt->getRetValue());

  return true;
}

bool
SkeletonASTVisitor::TraverseCXXMemberCallExpr(CXXMemberCallExpr* expr, DataRecursionQueue* queue)
{
  PragmaActivateGuard pag(expr, this);
  if (pag.skipVisit()) return true;

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

  goIntoContext(expr, [&]{
    Expr* ue = getUnderlyingExpr(const_cast<Expr*>(expr->getCallee()));
    if (ue->getStmtClass() != Stmt::MemberExprClass){
      internalError(expr->getLocStart(), *ci_,
                    "base of CXXMemberCallExpr is not a MemberExpr");
    }
    InsertGuard<Stmt,Stmt> ig(extendedReplacements_, ue, expr);
    //TraverseStmt(expr->getImplicitObjectArgument());
    //this will visit member expr that also visits implicit this
    TraverseStmt(expr->getCallee());
    for (int i=0; i < expr->getNumArgs(); ++i){
      //once I start parsing args, I have "broken" all connections with previous binops
      activeBinOpIdx_ = IndexResetter;
      Expr* arg = expr->getArg(i);
      //this did not get modified
      if (deletedStmts_.find(arg) == deletedStmts_.end()){
        TraverseStmt(expr->getArg(i));
      }
    }
  });

  return true;
}

void
SkeletonASTVisitor::replaceNullWithEmptyType(QualType type, Expr* toRepl)
{
  if (type->isVoidType()){
    deleteStmt(toRepl);
    throw StmtDeleteException(toRepl);
  }

  if (type->isReferenceType()){
    errorAbort(toRepl->getLocStart(), *ci_,
               "cannot create empty reference for null_variable");
  }

  std::string repl;
  if (type->isPointerType()){
    repl = "nullptr";
  } else {
    repl = GetAsString(type) + "()";
  }

  ::replace(toRepl, rewriter_, repl, *ci_);
}

void
SkeletonASTVisitor::tryVisitNullVariable(Expr* expr, NamedDecl* decl)
{
  try {
    visitNullVariable(expr, decl);
  } catch (StmtDeleteException& e) {
    if (e.deleted != expr){
      throw e;
    }
  }
}

bool
SkeletonASTVisitor::deleteNullVariableMember(NamedDecl* nullVarDecl, MemberExpr* expr)
{
  ValueDecl* member = expr->getMemberDecl();
  //the class itself is a null variable
  SSTNullVariablePragma* prg = getNullVariable(nullVarDecl);
  if (prg->hasExceptions()){
    return !prg->isException(member); //this might be a thing we dont delete
  } else if (prg->hasOnly()){
    return prg->isOnly(member); //this might be a thing we do delete
  } else return true; //yep, delete
}

bool
SkeletonASTVisitor::TraverseMemberExpr(MemberExpr *expr, DataRecursionQueue* queue)
{
  ValueDecl* member = expr->getMemberDecl();
  if (isNullVariable(member)){
    //the member variable itself is declared null
    if (!memberAccesses_.empty()){
      //the member might be part of another member expr
      MemberExpr* activeMember = memberAccesses_.back();
      bool del = deleteNullVariableMember(member, activeMember);
      if (del){
        //call exception-safe version
        tryVisitNullVariable(activeMember, member);
      }
    } else {
      //nope, standalone member
      tryVisitNullVariable(expr, member);
    }
  }

  if (expr->getMemberDecl()->getKind() == Decl::Var){
    VarDecl* vd = cast<VarDecl>(expr->getMemberDecl());
    const Decl* md = getOriginalDeclaration(vd);
    auto iter = globals_.find(md);
    if (iter != globals_.end()){
      GlobalReplacement& gr = iter->second;
      if (globalsTouched_.empty() || !ctorContexts_.empty()){
        //one-off access
        replace(expr, gr.oneOffText);
      } else {
        //I hate that clang makes me do this
        //source locations are all messed up and I can't just append
        std::string replText = gr.append ? appendText(expr, gr.oneOffText) : gr.reusableText;
        replace(expr, replText);
      }
    }
  }

  PushGuard<MemberExpr*> pg(memberAccesses_, expr);
  TraverseStmt(expr->getBase());
  return true;
}

bool
SkeletonASTVisitor::TraverseCallExpr(CallExpr* expr, DataRecursionQueue* queue)
{
  //this "breaks" connections for null variable propagation
  activeBinOpIdx_ = IndexResetter;

  PragmaActivateGuard pag(expr, this);
  if (pag.skipVisit()) return true;

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
      if (sstmacFxnPrepends_.find(fxnName) != sstmacFxnPrepends_.end()){
        rewriter_.InsertText(expr->getCallee()->getLocStart(), "sstmac_", false);
      }

      auto iter = mpiCalls_.find(fxnName);
      if (iter != mpiCalls_.end()){
        MPI_Call call = iter->second;
        (this->*call)(expr);
      }
    }
  }

  FunctionDecl* fd = expr->getDirectCallee();
  goIntoContext(expr, [&]{
    TraverseStmt(expr->getCallee());
    for (int i=0; i < expr->getNumArgs(); ++i){
      //va_args really foobars things here...
      //call site can have more args than params in declaration
      if (fd && i < fd->getNumParams()){
        PushGuard<ParmVarDecl*> pg(activeFxnParams_, fd->getParamDecl(i));
        Expr* arg = expr->getArg(i);
        if (deletedStmts_.find(arg) == deletedStmts_.end()){
          TraverseStmt(arg);
        }
      } else {
        Expr* arg = expr->getArg(i);
        if (deletedStmts_.find(arg) == deletedStmts_.end()){
          TraverseStmt(arg);
        }
      }
    }
  });

  return true;
}

void
SkeletonASTVisitor::setFundamentalTypes(QualType qt, cArrayConfig& cfg)
{
  cfg.fundamentalTypeString = GetAsString(qt);
  auto pos = cfg.fundamentalTypeString.find("anonymous struct");
  if (pos != std::string::npos){
    cfg.fundamentalTypeString = "anon";
  }
  cfg.fundamentalType = qt;
}

void
SkeletonASTVisitor::getArrayType(const Type* ty, cArrayConfig& cfg)
{
  const ConstantArrayType* aty = static_cast<const ConstantArrayType*>(ty->getAsArrayTypeUnsafe());
  QualType qt = aty->getElementType();
  bool isConstC99array = qt.getTypePtr()->isConstantArrayType();
  cfg.arrayIndices << "[" << aty->getSize().getZExtValue() << "]";
  if (isConstC99array){
    const ConstantArrayType* next = static_cast<const ConstantArrayType*>(qt.getTypePtr()->getAsArrayTypeUnsafe());
    getArrayType(next, cfg);
  } else {
    setFundamentalTypes(qt, cfg);
  }
}

void
SkeletonASTVisitor::addRecordField(SourceLocation typeDeclCutoff,
                                   const RecordDecl* rd, ReconstructedType& rt,
                                   std::map<const RecordDecl*, ReconstructedType>& newTypes)
{
  if (rd->getLocStart() < typeDeclCutoff){
    //oh, cool, we can just directly put this in the specifier
    rt.classFieldTypes.push_back(rd);
  } else {
    //ugh - static struct decl or something
    //we have to unroll this
    rt.newClassFieldTypes.push_back(rd);
    rt.structDependencies.insert(rd);
    auto iter = newTypes.find(rd);
    if (iter == newTypes.end()){
      ReconstructedType& newRt = newTypes[rd];
      reconstructType(typeDeclCutoff, rd, newRt, newTypes);
    }
  }
}

static RecordDecl* getRecordDeclForType(QualType qt)
{
  if (qt->isStructureType() || qt->isClassType()){
    const RecordType* rt = qt->getAsStructureType();
    return rt->getDecl();
  } else if (qt->isUnionType()){
    const RecordType* rt = qt->getAsUnionType();
    return rt->getDecl();
  } else {
    return nullptr;
  }
}

void
SkeletonASTVisitor::reconstructType(SourceLocation typeDeclCutoff,
                                    QualType qt, ReconstructedType& rt,
                                    std::map<const RecordDecl*, ReconstructedType>& newTypes)
{
  auto ty = qt.getTypePtr();
  if (qt->isConstantArrayType()){
    cArrayConfig cfg;
    getArrayType(ty, cfg);
    std::string elementType = getTypeNameForSizing(typeDeclCutoff, cfg.fundamentalType, newTypes);
    RecordDecl* rd = getRecordDeclForType(cfg.fundamentalType);
    if (rd){
      rt.structDependencies.insert(rd);
    }
    rt.arrayTypes.emplace_back(elementType, cfg.arrayIndices.str());
  } else if (qt->isFundamentalType() || qt->isPointerType() || qt->isArrayType()){
    rt.fundamentalFieldTypes.push_back(qt);
  } else {
    RecordDecl* rd = getRecordDeclForType(qt);
    if (rd){
     addRecordField(typeDeclCutoff, rd, rt, newTypes);
    }
  }
}

void
SkeletonASTVisitor::reconstructType(SourceLocation typeDeclCutoff,
                                    const RecordDecl* decl, ReconstructedType& rt,
                                    std::map<const RecordDecl*, ReconstructedType>& newTypes)
{
  rt.typeIndex = reconstructCount_++;

  int numDecls = 0;
  for (auto iter=decl->decls_begin(); iter != decl->decls_end(); ++iter){
    Decl* d = *iter;
    switch (d->getKind()){
    case Decl::Var:
    {
      VarDecl* vd = cast<VarDecl>(d);
      reconstructType(typeDeclCutoff, vd->getType(), rt, newTypes);
      ++numDecls;
      break;
    }
    case Decl::Field:
    {
      FieldDecl* vd = cast<FieldDecl>(d);
      reconstructType(typeDeclCutoff, vd->getType(), rt, newTypes);
      ++numDecls;
      break;
    }
    default:
      break; //do nothing
    }
  }
  if (numDecls == 0){
    decl->dump();
    errorAbort(decl->getLocStart(), *ci_,
               "failed sizing struct - is this actually empty?");
  }
}

std::string
SkeletonASTVisitor::getRecordTypeName(const RecordDecl* rd){
  std::string name = GetAsString(rd->getTypeForDecl());
  if (name.empty()){
    errorAbort(rd->getLocStart(), *ci_,
               "got back empty name for struct type");
  }
  return name;
}

void
SkeletonASTVisitor::addTypeReconstructionText(const RecordDecl* rd, ReconstructedType& rt,
                                              std::map<const RecordDecl*, ReconstructedType>& newTypes,
                                              std::set<const RecordDecl*>& alreadyDone,
                                              std::ostream& os)
{
  if (alreadyDone.find(rd) != alreadyDone.end()){
    return;
  }
  alreadyDone.insert(rd);

  for (const RecordDecl* next : rt.structDependencies){
    auto& nextRt = newTypes[next];
    addTypeReconstructionText(next,nextRt,newTypes,alreadyDone,os);
  }

  os << "struct sstTmpStructType" << rt.typeIndex << " {";
  int varCount = 0;
  for (QualType& qt : rt.fundamentalFieldTypes){
    if (qt->isFundamentalType()){
      os << GetAsString(qt) << " var" << varCount++ << "; ";
    } else if (qt->isPointerType() || qt->isArrayType()){
      os << "void* var" << varCount++ << "; ";
    } else {
      errorAbort(rd->getLocStart(), *ci_,
                 "internal error - got bad type in reconstruction");
    }
  }

  for (const RecordDecl* rd : rt.classFieldTypes){
    os << getRecordTypeName(rd) << " var" << varCount++ << "; ";
  }

  for (const RecordDecl* rd : rt.newClassFieldTypes){
    auto& newRt = newTypes[rd];
    //this better have been visited
    if (newRt.typeIndex == 0){
      internalError(rd->getLocStart(), *ci_,
                    "unassigned type index");
    }
    os << "sstTmpStructType" << newRt.typeIndex << " var" << varCount++ <<"; ";
  }

  for (auto& pair : rt.arrayTypes){
    //first = element type
    //second = array indices
    os << pair.first << " var" << varCount++ << pair.second << "; ";
  }

  os << "}; ";

}

std::string
SkeletonASTVisitor::getTypeNameForSizing(SourceLocation typeDeclCutoff, QualType qt,
                                         std::map<const RecordDecl*, ReconstructedType>& newTypes)
{
  if (qt->isConstantArrayType() || qt->isArrayType()){
    cArrayConfig cfg;
    getArrayType(qt.getTypePtr(), cfg);
    std::string retType = getTypeNameForSizing(typeDeclCutoff, cfg.fundamentalType, newTypes);
    return retType + cfg.arrayIndices.str();
  } else if (qt->isFundamentalType() || qt->isEnumeralType()){
    //easy peasy
    return GetAsString(qt);
  } else if (qt->isPointerType()){
    return "void*";
  } else if (qt->isStructureType() || qt->isClassType() || qt->isUnionType()){
    const RecordDecl* rd;
    if (qt->isUnionType()){
      rd = qt->getAsUnionType()->getDecl();
    } else {
      rd = qt->getAsStructureType()->getDecl();
    }

    if (rd->getLocStart() < typeDeclCutoff){
      //also easy peasy
      return getRecordTypeName(rd);
    }

    auto& rt = newTypes[rd];
    reconstructType(typeDeclCutoff, rd, rt, newTypes);

    std::stringstream sstr;
    sstr << "struct sstTmpStructType" << rt.typeIndex;
    return sstr.str();
  } else if (isa<TypedefType>(qt.getTypePtr())){
    const TypedefType* tdef = cast<const TypedefType>(qt.getTypePtr());
    TypedefNameDecl* decl = tdef->getDecl();
    const Type* next = decl->getTypeForDecl();
    return getTypeNameForSizing(typeDeclCutoff, QualType(next, 0), newTypes);
  } else {
    std::string error = "bad type for static function variable: " + GetTypeString(qt.split());
    errorAbort(typeDeclCutoff, *ci_, error);
    return "";
  }
}

void
SkeletonASTVisitor::arrayFxnPointerTypedef(VarDecl* D, SkeletonASTVisitor::ArrayInfo* info,
                                   std::stringstream& sstr)
{
  std::string typeStr = GetAsString(D->getType());
  auto replPos = typeStr.find("*[");
  std::string typedefText = typeStr.insert(replPos+1, info->typedefName);
  sstr << "typedef " << typedefText;
}

static bool isFxnPointerForm(QualType qt)
{
  //typedefs need no special treatment
  bool isTypeDef = isa<TypedefType>(qt.getTypePtr());
  if (isTypeDef) return false;

  return qt->isFunctionPointerType() || qt->isFunctionProtoType();
}



SkeletonASTVisitor::ArrayInfo*
SkeletonASTVisitor::checkArray(VarDecl* D, ArrayInfo* info)
{
  const Type* ty  = D->getType().getTypePtr();
  bool isC99array = ty->isArrayType();
  bool isConstC99array = ty->isConstantArrayType();

  if (isConstC99array){
    info->typedefName = "array_type_" + D->getNameAsString();
    //something of the form type x[N][M];
    //we possible need to first: typedef type tx[N][M];
    //replacement global will be tx* xRepl = &(sstmac_global_data + offset)
    const ArrayType* aty = ty->getAsArrayTypeUnsafe();
    std::stringstream sstr;
    cArrayConfig cfg;
    getArrayType(aty, cfg);
    if (isFxnPointerForm(cfg.fundamentalType)){
      arrayFxnPointerTypedef(D, info, sstr);
    } else {
      sstr << "typedef " << cfg.fundamentalTypeString
           << " " << info->typedefName
           << cfg.arrayIndices.str();

      if (cfg.fundamentalTypeString == "anon"){
        errorAbort(D->getLocStart(), *ci_,
                   "anonymous struct used in array declaration - cannot refactor");
      }
    }

    info->typedefString = sstr.str();
    info->retType = info->typedefName + "*";
    info->needsDeref = false;
    return info;
  } else if (isC99array){
    const ArrayType* aty = ty->getAsArrayTypeUnsafe();
    QualType ety = aty->getElementType();
    //if the element type of the array is itself a constant array
    if (ety->isConstantArrayType()){
      //something of the form type x[][M]
      //we need to first: typedef type tx[][M];
      //replacement global will be tx* xRepl = &(sstmac_global_data + offset)
      info->typedefName = "type_" + D->getNameAsString();
      std::stringstream sstr;
      cArrayConfig cfg;
      getArrayType(ety.getTypePtr(), cfg);
      if (isFxnPointerForm(cfg.fundamentalType)){
        arrayFxnPointerTypedef(D, info, sstr);
      } else {
        if (cfg.fundamentalTypeString == "anon"){
          errorAbort(D->getLocStart(), *ci_,
                     "anonymous struct used in array declaration - cannot refactor");
        }
        sstr << "typedef " << cfg.fundamentalTypeString
             << " " << info->typedefName
             << "[]"  //don't do this anymore
             << cfg.arrayIndices.str();
      }
      info->typedefString = sstr.str();
      info->retType = info->typedefName + "*";
      info->needsDeref = false;
    } else {
      //something of the form type x[]
      //replacement global will type* xRepl;
      info->typedefName = "type_" + D->getNameAsString();
      std::stringstream sstr;
      QualType ety = aty->getElementType();
      if (isFxnPointerForm(ety)){
        arrayFxnPointerTypedef(D, info, sstr);
      } else {
        sstr << "typedef " << GetAsString(ety)
             << " " << info->typedefName << "[]";
      }
      info->typedefString = sstr.str();
      info->retType = info->typedefName + "*";
      info->needsDeref = false;
    }
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

  bool typedefd = typedefStructs_.find(recDecl) != typedefStructs_.end();
  if (!typedefd && !(recDecl->getKind() == Decl::CXXRecord)){
    //if this is a combined struct and variable declaration
    if (recDecl->getNameAsString() == "" || recDecl->getLocStart() == D->getLocStart()){
      //actually anonymous - no name given to it
      rec->decl = recDecl;
      rec->structType = prefix;
      rec->typeName = D->getNameAsString() + "_anonymous_type";
      rec->retType = rec->structType + " "  + rec->typeName + "*";
      return rec;
    }
  }
  return nullptr;
}

static std::string getExprString(Expr* e)
{
  PrettyPrinter pp;
  pp.print(e);
  return pp.os.str();
}

void
SkeletonASTVisitor::addCppGlobalCallExprString(PrettyPrinter& pp, CallExpr* expr, QualType ty)
{
  ty.removeLocalConst();
  pp.os << "std::function<void(void*)>([](void* ptr){"
        << "  new (ptr) (" << GetAsString(ty) << ")(";
  pp.print(expr); pp.os << ");";
  pp.os << "})";
}

void
SkeletonASTVisitor::addCppGlobalCtorString(PrettyPrinter& pp, CXXConstructExpr* ctor, bool leadingComma)
{
  if (ctor->getNumArgs() > 0){
    if (leadingComma) pp.os << ",";
    Expr* mainArg = getUnderlyingExpr(ctor->getArg(0));
    if (ctor->getNumArgs() == 1 && mainArg->getStmtClass() == Stmt::CallExprClass){
      //ugh, well, I know this is C++ - but the call expression needs to happen at ctor time
      //which might not happen for a little while longer
      //I need to create a lambda that will execute the ctor when needed
      //cast the lambda to an std::function to make partial template specialization work
      addCppGlobalCallExprString(pp, cast<CallExpr>(mainArg), ctor->getType());
    } else {
      for (int i=0; i < ctor->getNumArgs(); ++i){
        if (i > 0) pp.os << ",";
        Expr* e = getUnderlyingExpr(ctor->getArg(i));
        pp.print(e);
      }
    }
  }
}

bool
SkeletonASTVisitor::addInitializers(VarDecl *D, std::ostream &os, bool leadingComma)
{
  if (!D->hasInit()){
    return false;
  }

  Expr* ue = getUnderlyingExpr(D->getInit());

  switch (ue->getStmtClass()){
  case Stmt::CXXConstructExprClass: {
    PrettyPrinter pp;
    CXXConstructExpr* ctor = cast<CXXConstructExpr>(ue);
    if (ctor->getNumArgs() == 0) return false;
    addCppGlobalCtorString(pp, ctor, leadingComma);
    os << pp.str();
    return false;
  }
  case Stmt::InitListExprClass: {
    InitListExpr* init = cast<InitListExpr>(ue);
    if (init->getNumInits() == 0) return false;
    for (int i=0; i < init->getNumInits(); ++i){
      //for now just print the expression
      if (i > 0 || leadingComma) os << ",";
      os << getExprString(init->getInit(i));
    }
    return false;
  }
  case Stmt::CallExprClass: {
    PrettyPrinter pp;
    if (leadingComma) os << ",";
    addCppGlobalCallExprString(pp, cast<CallExpr>(ue), D->getType());
    os << pp.str();
    return true;
  }
  default: {
    //for now just print the expression
    std::string str = getExprString(ue);
    if (!str.empty() && leadingComma) os << ",";
    os << str;
    return false;
  }
  }



  /**
  Expr* initExpr = D->getInit();
  PrettyPrinter pp;
  switch (initExpr->getStmtClass()){
    case Stmt::ParenListExprClass: {
      ParenListExpr* pe = cast<ParenListExpr>(initExpr);
      for (int i=0; i < pe->getNumExprs(); ++i){
        pp.os << i > 0 ? "," : "";
        pp.print(pe->getExpr(i));
      }
      break;
    }
    default:
  }
  */
}

CXXConstructExpr*
SkeletonASTVisitor::getCtor(VarDecl *vd)
{
  clang::Expr* expr = vd->getInit();
  while (1) {
    switch(expr->getStmtClass()){
    case Stmt::CXXConstructExprClass:
      return cast<CXXConstructExpr>(expr);
    case Stmt::ExprWithCleanupsClass: {
      expr = cast<ExprWithCleanups>(expr)->getSubExpr();
      break;
    }
    default:
      return nullptr;
    }
  }
}



bool
SkeletonASTVisitor::setupGlobalVar(const std::string& varnameScopeprefix,
                                   const std::string& clsScopePrefix,
                                   SourceLocation sstmacExternVarsLoc,
                                   SourceLocation varSizeOfInsertLoc,
                                   SourceLocation sstmacOffsetLoc,
                                   bool insertOffsetAfter,
                                   GlobalVariable_t global_var_ty,
                                   VarDecl* D,
                                   SourceLocation declEnd)
{
  if (D->getType().isConstQualified()){
    return true; //don't refactor const variables
  } else if (D->getType()->isPointerType()){
    if (D->getType()->getPointeeType().isConstQualified()){
      return true;
    }
  }

  bool threadLocal = isThreadLocal(D);
  if (threadLocal){
    errorAbort(D->getLocStart(), *ci_,
               "thread local variables not yet allowed");
  }

  bool skipInit = false;

  AnonRecord rec;
  AnonRecord* anonRecord = checkAnonStruct(D, &rec);
  ArrayInfo info;
  ArrayInfo* arrayInfo = checkArray(D, &info);

  if (declEnd.isInvalid()) declEnd = getEndLoc(D->getLocEnd());

  if (declEnd.isInvalid()){
    D->dump();
    errorAbort(D->getLocStart(), *ci_,
               "unable to locate end of variable declaration");
  }

  if (sstmacExternVarsLoc.isInvalid()) sstmacExternVarsLoc = declEnd;
  if (varSizeOfInsertLoc.isInvalid()) varSizeOfInsertLoc = declEnd;

  //const global variables can't change... so....
  //no reason to do any work tracking them
  if (D->getType().isConstQualified()){
    errorAbort(D->getLocStart(), *ci_,
               "internal compiler error: trying to refactor const global variable");
  }

  // roundabout way to get the type of the variable
  std::string retType;
  bool deref = true;
  if (arrayInfo) {
    retType = arrayInfo->retType;
    deref = arrayInfo->needsDeref;
  }  else if (anonRecord){
    retType = anonRecord->retType;
    if (!anonRecord->typeNameAdded){
      anonRecord->typeNameAdded = true;
      SourceLocation openBrace = anonRecord->decl->getBraceRange().getBegin();
      rewriter_.InsertText(openBrace, "  " + anonRecord->typeName, false, false);
    }
  } else if (D->getType()->isBooleanType()) {
    retType = "bool*";
  } else if (isCxx() && D->getType()->isStructureOrClassType()) {
    //it's effing obnoxious that I have to do this
    //the problem is that Clang in its stupidity doesn't print this type with correct namespacing
    //this produces obnoxious incomplete type or unknown type errors later
    //rather than directly calling D->getType()....
    std::stringstream sstr;
    sstr << "decltype(" << currentNs_->nsPrefix();
    if (D->isStaticDataMember()){
      CXXRecordDecl* crd = cast<CXXRecordDecl>(D->getDeclContext());
      sstr << crd->getNameAsString() << "::";
    }
    sstr << D->getNameAsString() << ")*";
    retType = sstr.str();
  } else {
    retType = GetAsString(D->getType()) + "*";
  }


  NamespaceDecl* outerNsDecl = getOuterNamespace(D);
  if (outerNsDecl){
    //this is just the position that we extern declare sstmac_global_stacksize
    sstmacExternVarsLoc = outerNsDecl->getLocStart();
  }

  if (sstmacExternVarsLoc.isInvalid()){
    errorAbort(D->getLocStart(), *ci_,
               "computed incorrect replacement location for global variable - "
               "probably this a multi-declaration that confused the source-to-source");
  }


  std::string scopeUniqueVarName = varnameScopeprefix + D->getNameAsString();
  setActiveGlobalScopedName(scopeUniqueVarName);

  scopedNames_[mainDecl(D)] = scopeUniqueVarName;

  /** Whether an extern type_t __offset_X declaration is needed */
  bool needExternalDecls = true;
  bool checkCxxCtor = true;
  bool needClsScope = true;
  bool useAccessor = false;
  if (!D->hasExternalStorage()){
    std::stringstream os;
    bool isVolatile = D->getType().isVolatileQualified();
    GlobalVarNamespace::Variable var;
    var.isFxnStatic = false;
    var.isThreadLocal = threadLocal;
    switch (global_var_ty){
      case Global:
      case FileStatic:
        if (isVolatile) os << "volatile ";
        os << "void* __ptr_" << scopeUniqueVarName << " = &" << D->getNameAsString() << "; "
           << "int __sizeof_" << scopeUniqueVarName << " = sizeof(" << D->getNameAsString() << ");";
        break;
      case FxnStatic:
        if (insideTemplateFxn()){
          //okay, this is a little bit weird
          //we have a template parameter
          needExternalDecls = false;
          checkCxxCtor = false;
          needClsScope = false;
          //the offset declaration actually goes here
          //if (innerStructTagsDeclared_.find(D->getDeclContext()) == innerStructTagsDeclared_.end()){
            //keep from declaring this more than once per decl context
          os << "struct inner_" << scopeUniqueVarName << "{};";
          //  innerStructTagsDeclared_.insert(D->getDeclContext());
          //}
          os << "static int __offset_" << scopeUniqueVarName
             << " = sstmac::inplace_cpp_global<"
             << "inner_" << scopeUniqueVarName
             << "," << getCleanTypeName(D->getType())
             << "," << std::boolalpha << threadLocal
             << ">(";
          addInitializers(D, os, false); //no leading comma
          os << ");";
        } else {
          os << "static int sstmac_inited_" << D->getNameAsString() << " = 0;"
            << "if (!sstmac_inited_" << D->getNameAsString() << "){"
            << "  sstmac_init_global_space(&" << D->getNameAsString()
               << "," << "__sizeof_" << scopeUniqueVarName
               << "," << "__offset_" << scopeUniqueVarName
               << "," << (threadLocal ? "1" : "0") //because of C, can't guarantee bools defined
               << ");"
            << "  sstmac_inited_" << D->getNameAsString() << " = 1; "
            << "}";
          std::stringstream size_os;
          std::map<const RecordDecl*, ReconstructedType> newTypes;
          std::string typeNameToSize = getTypeNameForSizing(fxnContexts_.front()->getLocStart(),
                                                            D->getType(), newTypes);
          if (typeNameToSize.empty()){
            errorAbort(D->getLocStart(), *ci_,
                       "internal error: empty type name for variable");
          }
          std::set<const RecordDecl*> alreadyDone;
          for (auto& pair : newTypes){
            addTypeReconstructionText(pair.first, pair.second, newTypes, alreadyDone, size_os);
          }
          size_os << "int __sizeof_" << scopeUniqueVarName << " = "
                  << "sizeof(" << typeNameToSize << "); ";
          rewriter_.InsertText(varSizeOfInsertLoc, size_os.str());
        }
        var.isFxnStatic = true;
        break;
      case CxxStatic:
        needExternalDecls = false;
        checkCxxCtor = false;
        //create an offset for the global variable
        //also, crap, this has to be public
        os << " public: "
           << "static int __offset_" << D->getNameAsString() << ";";

        if (D->getDeclContext()->isDependentContext()){
          dependentStaticMembers_[D->getNameAsString()] = D;
          //don't privatize this anymore - it breaks all sorts of things like decltype
          //we used to privatize variables to force compiler errors

          //oh, templates
          //this should never be accessed directly - only through new accessor method
          needClsScope = false;
          //create an accessor method for
          //use decltype for return... this can create weirdness
          os << "static decltype(" << D->getNameAsString() << ")& "
              << D->getNameAsString() << "_getter(){ "
              << " char* data = " << (threadLocal ? "get_sstmac_tls_data(); " : "get_sstmac_global_data(); ")
              << " void* offsetPtr = data + __offset_" << D->getNameAsString() << ";"
              << "return *((decltype(" << D->getNameAsString() << ")*)(offsetPtr));"
              << "}";
          useAccessor = true;
        }
        break;
    }
    //all of this text goes immediately after the variable
    rewriter_.InsertText(declEnd, os.str());
    if (needExternalDecls) currentNs_->replVars[scopeUniqueVarName] = var;
  }

  if (needExternalDecls){
    std::stringstream offset_os;
    offset_os << " extern int __offset_" << scopeUniqueVarName << "; ";
    if (sstmacOffsetLoc.isInvalid()){
      sstmacOffsetLoc = declEnd;
    }
    rewriter_.InsertText(sstmacOffsetLoc, offset_os.str(), insertOffsetAfter);
  }


  if (arrayInfo && arrayInfo->needsTypedef()){
    rewriter_.InsertText(declEnd, arrayInfo->typedefString + ";");
  } else if (D->getType().getTypePtr()->isFunctionPointerType()){
    bool isTypeDef = isa<TypedefType>(D->getType().getTypePtr());
    if (!isTypeDef){
      //see if there is extern
      PrettyPrinter pp;
      pp.print(D->getCanonicalDecl());
      std::string declStr = pp.os.str();
      auto pos = declStr.find("extern");
      if (pos != std::string::npos){
        declStr = declStr.replace(pos, 6, "");
      }

      pos = declStr.find("__attribute__");
      if (pos != std::string::npos){
        declStr = declStr.substr(0, pos);
      }

      pos = declStr.find("static");
      if (pos != std::string::npos){
        declStr = declStr.replace(pos, 6, "");
      }

      pos = declStr.find("=");
      if (pos != std::string::npos){
        declStr = declStr.substr(0, pos);
      }

      auto varName = D->getNameAsString();
      pos = declStr.find(varName);
      auto replName = varName + "_sstmac_fxn_typedef";
      declStr = "typedef " + declStr.replace(pos, varName.size(), replName) + ";";
      rewriter_.InsertText(declEnd, declStr);
      retType = replName + "*";
    }
  }

  if (!D->hasExternalStorage() && checkCxxCtor){
    RecordDecl* rd = D->getType().getTypePtr()->getAsCXXRecordDecl();
    if (rd){
      //well, crap, we have to register a constructor to call
      PrettyPrinter pp;
      if (D->getStorageClass() == SC_Static) pp.os << "static ";
      pp.os << "sstmac::CppGlobal* " << D->getNameAsString() << "_sstmac_ctor"
           << " = sstmac::new_cpp_global<"
           << GetAsString(D->getType())
           << "," << (threadLocal ? "true" : "false")
           << ">(" << "__offset_" << scopeUniqueVarName;

      CXXConstructExpr* ctor = getCtor(D);
      if (ctor){
        //need leading comma if there are arguments
        addCppGlobalCtorString(pp, ctor, true);
      }

      pp.os << "); ";

      std::string str = pp.os.str();
      auto pos = str.find("struct "); //clang, why do you put struct everywhere
      if (pos != std::string::npos){
        str = eraseAllStructQualifiers(str);
      }

      rewriter_.InsertText(declEnd, str);
    }
  }

  if (varSizeOfInsertLoc.isInvalid()){
    errorAbort(D->getLocStart(), *ci_, "failed replacing global variable declaration");
  }

  const Decl* md = mainDecl(D);

  if (useAccessor){
    globals_.insert({md, GlobalReplacement("", "_getter()", true)});
  } else {
    std::string standinName = "sstmac_";
    if (needClsScope){
      std::string uniqueNsPrefix = currentNs_->nsPrefix() + clsScopePrefix;
      for (int i=0; i < uniqueNsPrefix.size(); ++i){
        char& next = uniqueNsPrefix.at(i);
        if (next == ':') next = '_';
      }
      standinName += uniqueNsPrefix;
    }
    standinName += scopeUniqueVarName;

    //fxn pointers mess up everything
    std::string::size_type fxnPtrPos = std::string::npos;
    if (D->getType()->isPointerType()){
      const Type* subTy = getBaseType(D);
      if (subTy->isFunctionPointerType() || subTy->isFunctionProtoType()){
        bool isTypeDef = isa<TypedefType>(subTy);
        if (!isTypeDef){
          fxnPtrPos = retType.find("**");
        }
      }
    }

    std::stringstream standinSstr;
    std::stringstream oneOffSstr;
    if (fxnPtrPos != std::string::npos){
      //pointer to a function pointer
      //the substr chops off a trailing "*" that was added up above
      //instead the * needs to go inside parenthesis for function pointer
      retType = retType.substr(0, retType.size() - 1);
      //variable declaration has to look like void(***varname)(int)
      std::string fullDecl = retType; //have to copy this
      std::string repl = "***" + standinName;
      fullDecl = fullDecl.replace(fxnPtrPos, 2, repl);
      standinSstr << fullDecl;
      //and the return type needs to be void(***)(int)
      retType = retType.replace(fxnPtrPos, 2, "***");
    } else {
      standinSstr << retType << " " << standinName;
      oneOffSstr << "(*((" << retType << ")(";
    }
    standinSstr << "=(" << retType << ")";
    if (threadLocal){
      standinSstr << "(sstmac_tls_data + ";
      oneOffSstr << "get_sstmac_tls_data() + ";
    } else {
      standinSstr << "(sstmac_global_data + ";
      oneOffSstr << "get_sstmac_global_data() + ";
    }
    if (needClsScope){
      standinSstr << currentNs_->nsPrefix()
                  << clsScopePrefix;
      oneOffSstr << currentNs_->nsPrefix()
                  << clsScopePrefix;
    }
    standinSstr << "__offset_" << scopeUniqueVarName << ")";
    oneOffSstr << "__offset_" << scopeUniqueVarName << ")))";

    GlobalStandin& newStandin = globalStandins_[md];
    newStandin.replText = standinSstr.str();
    newStandin.threadLocal = threadLocal;

    if (global_var_ty == FxnStatic){
      newStandin.fxnStatic = true;
      //we need to put the global standin text here!
      std::string newText = newStandin.replText + "; ";
      rewriter_.InsertText(declEnd, newText);
      //it's possible this never gets referenced because of silly code
      //make sure this gets added to globals touched lists
      //regardless of whether it shows up in a DeclRefExpr
      globalsTouched_.back().insert(md);
    }

    std::stringstream replSstr;
    replSstr << "(*" << standinName << ")";
    globals_.insert({md, GlobalReplacement(replSstr.str(), oneOffSstr.str(), false)});
  }


  return skipInit;
}

bool
SkeletonASTVisitor::TraverseLambdaExpr(LambdaExpr* expr)
{
  LambdaCaptureDefault def = expr->getCaptureDefault();
  switch (expr->getCaptureDefault()){
    case LCD_None: {
      EmplaceGuard<std::set<const clang::Decl*>> eg(globalsTouched_);
      bool flag = RecursiveASTVisitor<SkeletonASTVisitor>::TraverseLambdaExpr(expr);
      if (!flag) return flag;
      for (auto iter = expr->explicit_capture_begin();
        iter != expr->explicit_capture_end(); ++iter){
        const LambdaCapture& cap = *iter;
        if (cap.getCaptureKind() == LCK_ByRef){
          //reference doesn't cause any headaches
          continue;
        }

        //these globals should not be declared inside the lambda
        if (!cap.capturesVariable()){
          continue;
          //errorAbort(cap.getLocation(), *ci_,
          //           "variable does not capture anything");
        }

        VarDecl* vd = cap.getCapturedVar();
        Expr* needed = vd->getInit();
        if (!needed){
          continue; //there is no init to worry about, I guess
        }

        bool cont = true;
        while (cont && needed->getStmtClass() != Stmt::DeclRefExprClass){
          switch (needed->getStmtClass()){
            case Stmt::CXXConstructExprClass: {
              CXXConstructExpr* next = cast<CXXConstructExpr>(needed);
              needed = next->getArg(0);
              break;
            }
            case Stmt::ImplicitCastExprClass: {
              ImplicitCastExpr* next = cast<ImplicitCastExpr>(needed);
              needed = next->getSubExpr();
              break;
            }
            case Stmt::CXXDependentScopeMemberExprClass: {
              CXXDependentScopeMemberExpr* next = cast<CXXDependentScopeMemberExpr>(needed);
              needed = next->getBase();
              break;
            }
            case Stmt::CallExprClass:
            case Stmt::MemberExprClass:
            case Stmt::CXXBoolLiteralExprClass:
            case Stmt::ExprWithCleanupsClass:
            case Stmt::CXXMemberCallExprClass:
              cont = false;
              break; //do nothing
            default: {
              vd->dump();
              needed->dump();
              std::string error = "finding capture target of "
                  + vd->getNameAsString() + " lead to bad expression type "
                  + needed->getStmtClassName();
              errorAbort(vd->getLocStart(), *ci_, error);
            }
          }
        }
        if (cont){
          DeclRefExpr* dref = cast<DeclRefExpr>(needed);
          globalsTouched_.back().erase(dref->getDecl()->getCanonicalDecl());
        }
      }
      addInContextGlobalDeclarations(expr->getBody());
      break;
    }
    case LCD_ByCopy:
      //this can be confusing for global variables
      //but an [=] capture of a global variable is "by reference"
      //so no special work is need for this case
    case LCD_ByRef:
      return RecursiveASTVisitor<SkeletonASTVisitor>::TraverseLambdaExpr(expr);
  }
  return true;
}

clang::SourceLocation
SkeletonASTVisitor::getEndLoc(SourceLocation searchStart)
{
  int numTries = 0;
  SourceLocation newLoc = searchStart;
  while (numTries < 1000){
    Token res;
    Lexer::getRawToken(newLoc, res, ci_->getSourceManager(), ci_->getLangOpts());
    if (res.getKind() == tok::semi){
      return newLoc.getLocWithOffset(1);
    } else {
      //JJW 1/22/2018
      //Need to change it to do this - D->getEndLoc() is incorrect for string literal inits
      SourceLocation next = Lexer::getLocForEndOfToken(newLoc, 1, ci_->getSourceManager(), Printing::langOpts);
      if (next == newLoc){
        newLoc = newLoc.getLocWithOffset(1);
      } else {
        newLoc = next;
      }
    }
    ++numTries;
  }
  errorAbort(searchStart, *ci_,
    "unable to locate end of variable declaration");
  return SourceLocation();
}

bool
SkeletonASTVisitor::checkFileVar(const std::string& filePrefix, VarDecl* D)
{
  return setupGlobalVar(filePrefix, "", SourceLocation(),
                        SourceLocation(),
                        SourceLocation(), true, //insert after end of decl
                        FileStatic, D);
}

bool
SkeletonASTVisitor::checkStaticFileVar(VarDecl* D)
{
  return checkFileVar(currentNs_->filePrefix(ci_, D->getLocStart()), D);
}

bool
SkeletonASTVisitor::checkGlobalVar(VarDecl* D)
{
  return checkFileVar("", D);
}

void
SkeletonASTVisitor::deleteStmt(Stmt *s)
{
  //go straight to replace, don't delay this
  ::replace(s, rewriter_, "", *ci_);
}

static bool isCombinedDecl(VarDecl* vD, RecordDecl* rD)
{
  return vD->getLocStart() <= rD->getLocStart() && rD->getLocEnd() <= vD->getLocEnd();
}

bool
SkeletonASTVisitor::checkStaticFxnVar(VarDecl *D)
{
  FunctionDecl* outerFxn = fxnContexts_.front();
  std::stringstream prefix_sstr;
  prefix_sstr << "_" << outerFxn->getNameAsString();

  auto& staticVars = staticFxnVarCounts_[outerFxn];
  int& cnt = staticVars[D->getNameAsString()];
  //due to scoping, we might have several static fxn vars
  //all with the same name
  if (cnt != 0){
    prefix_sstr << "_" << cnt;
  }
  ++cnt;

  std::string scope_prefix = currentNs_->filePrefix(ci_, D->getLocStart()) + prefix_sstr.str();

  return setupGlobalVar(scope_prefix, "",
                 outerFxn->getLocStart(),
                 outerFxn->getLocStart(),
                 outerFxn->getLocStart(), false, //insert after end of decl
                 FxnStatic, D);
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
  if (classContexts_.size() > 1){
    errorAbort(D->getLocStart(), *ci_, "cannot handle static variables in inner classes");
  }

  CXXRecordDecl* outerCls = classContexts_.front();
  std::stringstream varname_scope_sstr; varname_scope_sstr << "_";
  std::stringstream cls_scope_sstr;
  for (CXXRecordDecl* decl : classContexts_){
    varname_scope_sstr << "_" << decl->getNameAsString();
    cls_scope_sstr << decl->getNameAsString() << "::";
  }

  if (!D->hasInit()){
    //no need for special scope prefixes - these are fully scoped within in the class
    //setupGlobalVar(varname_scope_sstr.str(), cls_scope_sstr.str(),
    setupGlobalVar("", cls_scope_sstr.str(),
                  outerCls->getLocStart(), SourceLocation(),
                   outerCls->getLocStart(), false, //put at beginning of class
                   CxxStatic, D);
  } //else this must be a const integer if inited in the header file
    //we don't have to "deglobalize" this

  return false;
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

void
SkeletonASTVisitor::getTemplatePrefixString(std::ostream& os, TemplateParameterList* theList)
{
  os << "template <"; //this gets the prefix <class T, class U>
  int numParams = theList->size();
  for (int i=0; i < numParams; ++i){
    NamedDecl* nd = theList->getParam(i);
    if (i > 0){
      os << ",";
    }
    if (nd->getKind() == Decl::TemplateTypeParm){
      os << "class";
      if (nd->isTemplateParameterPack()){
        os << "...";
      }
      os << " " << nd->getNameAsString();
    } else if (nd->getKind() == Decl::NonTypeTemplateParm){
      NonTypeTemplateParmDecl* pd = cast<NonTypeTemplateParmDecl>(nd);
      os << GetAsString(pd->getType()) << " " << nd->getNameAsString();
    } else if (nd->getKind() == Decl::TemplateTemplateParm){
      TemplateTemplateParmDecl* ttpd = cast<TemplateTemplateParmDecl>(nd);
      PrettyPrinter pp;
      pp.print(ttpd);
      os << pp.os.str();
    } else {
      PrettyPrinter pp;
      pp.os << "got bad template parameter - has incorrect Clang Decl kind: "
          << nd->getDeclKindName() << "\n";
      pp.print(nd);
      internalError(nd->getLocStart(), *ci_, pp.os.str());
    }
  }
  os << "> ";
}

void
SkeletonASTVisitor::getTemplateParamsString(std::ostream& os, TemplateParameterList* theList)
{
  int numParams = theList->size();
  for (int i=0; i < numParams; ++i){
    NamedDecl* nd = theList->getParam(i);
    if (i > 0){
      os << ",";
    }
    os << nd->getNameAsString();
    if (nd->isTemplateParameterPack() || nd->isParameterPack()){
      os << "...";
    }
  }
  os << ">";
}

std::string
SkeletonASTVisitor::eraseAllStructQualifiers(const std::string& name)
{
  auto pos = name.find("struct ");
  std::string ret = name;
  while (pos != std::string::npos){
    ret = ret.replace(pos, 6, " ");
    pos = ret.find("struct ");
  }
  return ret;
}

std::string
SkeletonASTVisitor::getCleanName(const std::string& name)
{
  auto pos = name.find("struct");
  if (pos != std::string::npos){
    return name.substr(pos + 6);
  }

  pos = name.find("class");
  if (pos != std::string::npos){
    return name.substr(pos + 5);
  }

  return name;
}

std::string
SkeletonASTVisitor::getCleanTypeName(QualType ty)
{
  ty.removeLocalConst();
  return getCleanName(GetAsString(ty));
  //this is a nightmare
  //trying to do this more elegantly directly from type info
  //forget it, just do string replace

  /**
  if (ty->isStructureType() || ty->isClassType()){
    CXXRecordDecl* crd = ty->getAsCXXRecordDecl();
    if (crd){
      if (crd->isDependentContext()){
        return GetAsString(ty);
      } else if (crd->getTemplateInstantiationPattern()){
        if (isa<TemplateSpecializationType>(ty.getTypePtr())){
          const TemplateSpecializationType* tySpec = cast<const TemplateSpecializationType>(ty.getTypePtr());
          if (tySpec->isAliasType()){
            return ty
          } else {

          }
        } else if (isa<TypedefType>(ty.getTypePtr())){
          //typedefd name with no template parameters
          const TypedefType* tyDef = cast<TypedefType>(ty);
          TypedefNameDecl* decl = tyDef->getDecl();
          return decl->getNameAsString();
        } else {
          //not a template context, but a template instantiation
          return GetAsString(ty);
        }
      } else {
        return crd->getNameAsString();
      }
    }

    const RecordType* rt = ty->getAsStructureType();
    if (rt){
      RecordDecl* rd = rt->getDecl();
      if (rd->isDependentContext()){
        return GetAsString(ty);
      } else {
        return rd->getNameAsString();
      }
    } else {
      std::string error = "type " + GetAsString(ty) + " is a structure type, but RecordType is null";
      internalError(SourceLocation(), *ci_, error);
    }
  } else {
    return GetAsString(ty);
  }
  */
}

SourceLocation
SkeletonASTVisitor::getVariableNameLocationEnd(VarDecl* D)
{
  SourceLocation loc = D->getLocStart();
  int numTries = 0;
  while (numTries < 100000){
    Token res;
    Lexer::getRawToken(loc, res, ci_->getSourceManager(), ci_->getLangOpts());
    std::string next;
    if (res.getKind() == tok::identifier){
      next = res.getIdentifierInfo()->getName().str();
    } else if (res.getKind() == tok::raw_identifier) {
      next = res.getRawIdentifier().str();
    }
    if (!next.empty() && next == D->getNameAsString()){
      return Lexer::getLocForEndOfToken(loc, 0, ci_->getSourceManager(), Printing::langOpts);
    }
    //super annoying that I have to do this
    SourceLocation nextLoc = Lexer::getLocForEndOfToken(loc, 1, ci_->getSourceManager(), Printing::langOpts);
    if (nextLoc == loc){
      loc = loc.getLocWithOffset(1);
    } else {
      loc = nextLoc;
    }
    ++numTries;
  }
  internalError(D->getLocStart(), *ci_,
    "unable to locate variable name");
  return SourceLocation();
}


bool
SkeletonASTVisitor::checkInstanceStaticClassVar(VarDecl *D)
{
  auto iter = globals_.find(mainDecl(D));
  if (iter == globals_.end()){
    //hmm, this must be const or something
    //because the main decl didn't end up in the globals
    return true;
  }

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

  std::stringstream os;
  //throw away the class name in the list of scopes
  sem.pop_back();
  auto semBegin = sem.begin();
  for (auto& ignore : lex){ //figure out the overlap between lexical and semantic namespaces
    ++semBegin;
  }
  for (auto iter = semBegin; iter != sem.end(); ++iter){ //I must init/declare vars in the most enclosing namespace
    os << "namespace " << *iter << " {";
  }

  std::string clsName = parentCls->getNameAsString();
  if (parentCls->isDependentContext()){
    std::stringstream tmplSstr; //the name A<T,U>
    tmplSstr << clsName << "<";
    int numLists = D->getNumTemplateParameterLists();
    if (numLists > 1){
      internalError(D->getLocStart(), *ci_,
          "cannot handle nested template declarations");
    }
    TemplateParameterList* theList = D->getTemplateParameterList(0);
    getTemplatePrefixString(os, theList);
    getTemplateParamsString(tmplSstr, theList);
    clsName = tmplSstr.str();
  }

  os << "int " << clsName
     << "::__offset_" << D->getNameAsString()
     << " = sstmac::inplace_cpp_global<"
     << clsName
     << "," << getCleanTypeName(D->getType())
     << "," << std::boolalpha << isThreadLocal(D)
     << ">(";
  bool isCallExpr = addInitializers(D, os, false); //no leading comma
  os << ");";

  /** don't need this anymore
  std::string init_prefix = parentCls->getNameAsString() + "::";
  //this has an initial value we need to transfer over
  //log the variable so we can drop replacement info in the static cxx file
  //if static and no init given, then we will drop this initialization code at the instantiation point
  if (D->getType().isVolatileQualified()) os << "volatile ";
  os << "void* __ptr_" << scope_unique_var_name << " = &"
     << init_prefix << D->getNameAsString() << "; "
     << "int __sizeof_" << scope_unique_var_name << " = sizeof("
     << init_prefix << D->getNameAsString() << ");";
  */

  for (auto iter = semBegin; iter != sem.end(); ++iter){
    os << "}"; //close namespaces
  }

  rewriter_.InsertText(getEndLoc(D->getLocEnd()), os.str());

  return true;
}

bool
SkeletonASTVisitor::TraverseUnresolvedLookupExpr(clang::UnresolvedLookupExpr* expr,
                                                 DataRecursionQueue* queue)
{
  for (auto iter=expr->decls_begin(); iter != expr->decls_end(); ++iter){
    NamedDecl* nd = *iter;
    if (nd->getKind() == Decl::VarTemplate){
      VarTemplateDecl* vtd = cast<VarTemplateDecl>(nd);
      VarDecl* vd = vtd->getCanonicalDecl()->getTemplatedDecl();
      if (variableTemplates_.find(vd) != variableTemplates_.end()){
        rewriter_.InsertText(expr->getLocEnd().getLocWithOffset(1), "()", false);
        return true;
      }
    }
  }
  return RecursiveASTVisitor<SkeletonASTVisitor>::TraverseUnresolvedLookupExpr(expr);
}

bool
SkeletonASTVisitor::TraverseVarDecl(VarDecl* D)
{
  try {

  PragmaActivateGuard pag(D, this);
  if (pag.skipVisit()) return true;

  if (pragmaConfig_.makeNoChanges){
    pragmaConfig_.makeNoChanges = false;
    return true;
  }

  if (pragmaConfig_.nullifyDeclarationsPragma){
    SSTNullVariablePragma* prg = pragmaConfig_.nullifyDeclarationsPragma->generate(D, *ci_);
    prg->CI = ci_;
    pag.reactivate(D, prg);
  }

  if (D->getDescribedVarTemplate()){
    if (D->isStaticDataMember()){
      const CXXRecordDecl* crd = cast<const CXXRecordDecl>(D->getDeclContext());
      VarTemplateDecl* vtd = D->getDescribedVarTemplate();
      TemplateParameterList* theList = vtd->getTemplateParameters();
      TemplateParameterList* clsList = nullptr;

      std::string clsName = crd->getNameAsString();
      if (crd->isDependentContext()){ //class is also a template
        ClassTemplateDecl* decl = crd->getDescribedClassTemplate();
        if (!decl){
          internalError(crd->getLocStart(), *ci_,
                     "template class has no template lists");
        }
        clsList = decl->getTemplateParameters();
        clsName = GetAsString(crd->getTypeForDecl());
      }

      std::stringstream sstTypeOs;
      sstTypeOs << " sstmac::CppVarTemplate<" << clsName
           << "," << GetAsString(D->getType())
           << "," << std::boolalpha << isThreadLocal(D)
           << "> ";

      if (D->isThisDeclarationADefinition() == VarDecl::DeclarationOnly){
        //this is the declaration inside the class of a class static template variable
        //template <class T> static T member; e.g.
        std::stringstream sstr;
        VarTemplateDecl* vtd = D->getDescribedVarTemplate();
        TemplateParameterList* theList = vtd->getTemplateParameters();
        getTemplatePrefixString(sstr, theList);
        sstr << " static " << sstTypeOs.str()
             << " " << D->getNameAsString() << ";";
        replace(vtd, sstr.str());
        variableTemplates_.insert(D);
        return true;
      } else {
        //this is the definition of a class static template variable
        //template <class T> static T member; e.g.
        std::stringstream sstr;
        if (clsList){
          //I think I get this for free
          //getTemplatePrefixString(sstr, clsList);
          //sstr << " ";
        }
        getTemplatePrefixString(sstr, theList);
        sstr << sstTypeOs.str() << " " << clsName  << "::"
             << D->getNameAsString();
        if (D->hasInit()){
          PrettyPrinter pp;
          pp.print(D->getInit());
          sstr << pp.os.str();
        }
        sstr << ";";
        replace(vtd, sstr.str());
      }
    } else {
      std::stringstream sstr;
      sstr << "struct sstmacTagClass" << D->getNameAsString() << "{}; ";
      VarTemplateDecl* vtd = D->getDescribedVarTemplate();
      TemplateParameterList* theList = vtd->getTemplateParameters();
      getTemplatePrefixString(sstr, theList);
      if (D->isStaticLocal()){
        sstr << " static";
      }
      sstr << " sstmac::CppVarTemplate<sstmacTagClass" << D->getNameAsString()
           << "," << GetAsString(D->getType())
           << "," << std::boolalpha << isThreadLocal(D)
           << "> " << D->getNameAsString();
      if (D->hasInit()){
        PrettyPrinter pp;
        pp.print(D->getInit());
        sstr << pp.os.str();
      }
      sstr << ";";
      replace(vtd, sstr.str());
      variableTemplates_.insert(D);
      return true;
    }
  }

  if (D->getMemberSpecializationInfo() && D->getTemplateInstantiationPattern()){
    return true;
  }

  if (isNullVariable(D)){
    if (D->getType()->isPointerType()){
      if (D->hasInit()){
        replace(D->getInit(), "nullptr");
      }
    } else {
      errorAbort(D->getLocStart(), *ci_,
                 "null_variable can only be applied to pointer");
    }
    return true;
  }

  //don't skip init on non-globals
  bool skipInit = visitVarDecl(D);

  if (D->hasInit() && !skipInit){
    if (visitingGlobal_){
      PushGuard<VarDecl*> pgD(activeDecls_, D);
      PushGuard<Expr*> pgI(activeInits_, getUnderlyingExpr(D->getInit()));
      TraverseStmt(D->getInit());
    } else {
      TraverseStmt(D->getInit());
    }
  }

  clearActiveGlobal();

  } catch (DeclDeleteException& e){
    if (e.deleted != D) throw e;
  }

  return true;
}

bool
SkeletonASTVisitor::visitVarDecl(VarDecl* D)
{
  //we need do nothing with this
  if (D->isConstexpr()){
    return false;
  }

  if (reservedNames_.find(D->getNameAsString()) != reservedNames_.end()){
    return false;
  }

  if (D->getNameAsString() == "sstmac_appname_str"){
    StringLiteral* lit = cast<StringLiteral>(getUnderlyingExpr(D->getInit()));
    mainName_ = lit->getString();
    return false;
  }

  bool skipInit = false;
  if (insideClass() && D->isStaticDataMember() && shouldVisitDecl(D)){
    skipInit = checkDeclStaticClassVar(D);
  } else if (insideFxn() && D->isStaticLocal() && shouldVisitDecl(D)){
    skipInit = checkStaticFxnVar(D);
  } else if (D->isCXXClassMember() && D->isStaticDataMember() && shouldVisitDecl(D)){
    skipInit = checkInstanceStaticClassVar(D);
  } else if (D->getStorageClass() == StorageClass::SC_Static && shouldVisitDecl(D)){
    skipInit = checkStaticFileVar(D);
  } else if (!D->isLocalVarDeclOrParm() && shouldVisitDecl(D)){
    skipInit = checkGlobalVar(D);
  }

  return skipInit;
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
    replace(rng, sstr.str());
  } else {
    errorAbort(mainFxn->getLocStart(), *ci_,
               "sstmac_app_name macro not defined before main");
  }
}

void
SkeletonASTVisitor::addInContextGlobalDeclarations(clang::Stmt* body)
{
  auto& currentGlobals = globalsTouched_.back();
  if (!currentGlobals.empty()){
    bool needGlobalData = false;
    bool needTlsData = false;
    for (auto d : currentGlobals){
      GlobalStandin& gs = globalStandins_[d];
      needGlobalData = !gs.threadLocal || needGlobalData;
      needTlsData = gs.threadLocal || needTlsData;
    }

    std::stringstream sstr;
    sstr << "{ ";
    if (needGlobalData) sstr << "char* sstmac_global_data = get_sstmac_global_data();";
    if (needTlsData) sstr << "char* sstmac_tls_data = get_sstmac_tls_data();";
    for (auto d : currentGlobals){
      GlobalStandin& gs = globalStandins_[d];
      if (!gs.fxnStatic){
        sstr << gs.replText << "; ";
      }
    }
    rewriter_.InsertText(body->getLocStart(), sstr.str(), false);
    rewriter_.InsertText(body->getLocEnd(), " }", true);
  }
}

void
SkeletonASTVisitor::traverseFunctionBody(clang::Stmt* s)
{
  EmplaceGuard<std::set<const clang::Decl*>> eg(globalsTouched_);
  TraverseStmt(s);
  addInContextGlobalDeclarations(s);
}

bool
SkeletonASTVisitor::TraverseFunctionDecl(clang::FunctionDecl* D)
{

  if (D->isMain() && refactorMain_){
    replaceMain(D);
  } else if (D->isTemplateInstantiation()){
    return true; //do not visit implicitly instantiated template stuff
  } else if (!D->isThisDeclarationADefinition()){
    //clang is weird... this can be just a declaration
    //but it gets its definition filled in...
    return true;
  }

  if (D->hasAttrs()){
    for (Attr* attr : D->getAttrs()){
      if (attr->getKind() == attr::AlwaysInline){
        if (!D->isInlined()){
          SourceLocation start = D->getReturnTypeSourceRange().getBegin();
          if (start.isValid()){
            rewriter_.InsertText(start, " inline ", false);
          }
        }
      }
    }
  }


  if (D->isLocalExternDecl()){
    //weird extern declaration - skip it
    return true;
  }

  PragmaActivateGuard pag(D, this, D->isThisDeclarationADefinition());
  if (!pag.skipVisit() && D->getBody()){
    PushGuard<FunctionDecl*> pg(fxnContexts_, D);
    traverseFunctionBody(D->getBody());
  }
  return true;
}

bool
SkeletonASTVisitor::TraverseCXXRecordDecl(CXXRecordDecl *D)
{
  PushGuard<CXXRecordDecl*> pg(classContexts_, D);
  auto end = D->decls_end();
  for (auto iter=D->decls_begin(); iter != end; ++iter){
    Decl* d = *iter;
    switch (d->getKind()){
      case Decl::Var:
        //so this is wonky - but
        //I need to know about all variables fist
        TraverseDecl(d);
        break;
      default:
        break;
    }
  }
  for (auto iter=D->decls_begin(); iter != end; ++iter){
    Decl* d = *iter;
    switch (d->getKind()){
      case Decl::Var:
        break;
      default:
        //now I can visit everything else
        TraverseDecl(d);
        break;
    }
  }
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
  EmplaceGuard<std::set<const clang::Decl*>> eg(globalsTouched_);
  TraverseDecl(D->getTemplatedDecl());
  return true;
}

bool
SkeletonASTVisitor::TraverseCXXConstructorDecl(CXXConstructorDecl *D)
{
  if (D->isTemplateInstantiation())
    return true;

  IncrementGuard ig(insideCxxMethod_);
  PushGuard<CXXConstructorDecl*> pg(ctorContexts_, D);
  for (auto *I : D->inits()) {
    TraverseConstructorInitializer(I);
  }
  TraverseCXXMethodDecl(D);
  return true;
}

bool
SkeletonASTVisitor::TraverseCXXMethodDecl(CXXMethodDecl *D)
{
  //do not traverse this - will mess everything up
  //this got implicitly inserted into AST - has no source location
  if (D->isTemplateInstantiation())
    return true;



  PragmaActivateGuard pag(D, this);
  if (D->isThisDeclarationADefinition() && !pag.skipVisit()) {
    IncrementGuard ig(insideCxxMethod_);
    traverseFunctionBody(D->getBody());
  }
  return true;
}

bool
SkeletonASTVisitor::VisitDependentScopeDeclRefExpr(DependentScopeDeclRefExpr* expr)
{
  if (!pragmaConfig_.dependentScopeGlobal.empty()){
    //we have been told about a global variable that cannot be recongized
    //because it is a dependent scope expression
    std::string memberName = expr->getDeclName().getAsString();
    if (memberName == pragmaConfig_.dependentScopeGlobal){
      auto iter = dependentStaticMembers_.find(memberName);
      if (iter == dependentStaticMembers_.end()){
        std::string warning = "variable " + memberName
            + " tagged as global, but there are no know globals with that name";
        warn(expr->getLocStart(), *ci_, warning);
      }
      std::string repl = appendText(expr, "_getter()");
      ::replace(expr, rewriter_, repl, *ci_);
      pragmaConfig_.dependentScopeGlobal.clear();
      return true; //skip checks below
      //std::string error = "pragma gloçbal name " + pragmaConfig_.dependentScopeGlobal
      //    + " does not match found member name " + memberName;
      //errorAbort(expr->getLocStart(), *ci_, error);
    }
  }

  auto iter = dependentStaticMembers_.find(expr->getDeclName().getAsString());
  if (iter != dependentStaticMembers_.end()){
    clang::VarDecl* vd = iter->second;
    std::string warning = "member " + vd->getNameAsString()
        + " used in dependent scope matches global variable at "
        + vd->getLocStart().printToString(ci_->getSourceManager())
        + ". I can't tell if this is a global variable. If it is "
        "please let me know by using #pragma sst global " + vd->getNameAsString();
    warn(expr->getLocStart(), *ci_, warning);
  }


  return true;
}

bool
SkeletonASTVisitor::VisitCXXDependentScopeMemberExpr(clang::CXXDependentScopeMemberExpr* expr)
{
  if (!pragmaConfig_.dependentScopeGlobal.empty()){
    //we have been told about a global variable that cannot be recongized
    //because it is a dependent scope expression
    std::string memberName = expr->getMember().getAsString();
    if (memberName == pragmaConfig_.dependentScopeGlobal){
      auto iter = dependentStaticMembers_.find(memberName);
      if (iter == dependentStaticMembers_.end()){
        std::string warning = "variable " + memberName
            + " tagged as global, but there are no know globals with that name";
        warn(expr->getLocStart(), *ci_, warning);
      }
      std::string repl = appendText(expr, "_getter()");
      ::replace(expr, rewriter_, repl, *ci_);
      pragmaConfig_.dependentScopeGlobal.clear();
      return true; //skip checks below
      //std::string error = "pragma gloçbal name " + pragmaConfig_.dependentScopeGlobal
      //    + " does not match found member name " + memberName;
      //errorAbort(expr->getLocStart(), *ci_, error);
    }
  }

  auto iter = dependentStaticMembers_.find(expr->getMember().getAsString());
  if (iter != dependentStaticMembers_.end()){
    clang::VarDecl* vd = iter->second;
    std::string warning = "member " + vd->getNameAsString()
        + " used in dependent scope matches global variable at "
        + vd->getLocStart().printToString(ci_->getSourceManager())
        + ". I can't tell if this is a global variable. If it is "
        "please let me know by using #pragma sst global " + vd->getNameAsString();
    warn(expr->getLocStart(), *ci_, warning);
  }


  return true;
}

bool
SkeletonASTVisitor::TraverseCompoundStmt(CompoundStmt* stmt, DataRecursionQueue* queue)
{
  try {
    PragmaActivateGuard pag(stmt, this);
    if (!pag.skipVisit()){
      auto end = stmt->body_end();
      for (auto iter=stmt->body_begin(); iter != end; ++iter){
        TraverseStmt(*iter);
      }
    }
  } catch (StmtDeleteException& e) {
    if (e.deleted != stmt) throw e;
  }

  return true;
}

bool
SkeletonASTVisitor::TraverseFieldDecl(clang::FieldDecl* fd, DataRecursionQueue* queue)
{
  try {
    PragmaActivateGuard pag(fd, this);
    if (!pag.skipVisit()){
      PushGuard<FieldDecl*> pg(activeFieldDecls_, fd);
      TraverseStmt(fd->getBody());
    }
  } catch (DeclDeleteException& e) {
    if (e.deleted != fd) throw e;
  }

  return true;
}

void
SkeletonASTVisitor::addRelocation(UnaryOperator* op, DeclRefExpr* dr, ValueDecl* member)
{
  if (activeInits_.empty()){
    errorAbort(op->getLocStart(), *ci_,
               "unable to parse global variable initialization");
  }

  Expr* init = activeInits_.back();
  if (op != init){
    errorAbort(op->getLocStart(), *ci_,
               "pointer to global variable used in initialization of global variable "
               " - deglobalization cannot create relocation pointer");
  }

  std::string dstFieldOffsetPtrName;
  std::string srcFieldOffsetPtrName;
  std::stringstream ptr_str;
  std::stringstream cpp_str;
  VarDecl* vd = activeDecls_.back();
  if (!activeFieldDecls_.empty()){
    std::stringstream sstr;
    std::string varScopedName = scopedNames_[mainDecl(vd)];
    sstr << "__field_offset_ptr_" << varScopedName;
    for (FieldDecl* fd : activeFieldDecls_){
      sstr << "_" << fd->getNameAsString();
    }
    dstFieldOffsetPtrName = sstr.str();
    if (!currentNs_->relocationOffsetDeclared(dstFieldOffsetPtrName)){
      ptr_str << "void* " << dstFieldOffsetPtrName << " = "
           << "&" << vd->getNameAsString();
      if (!initIndices_.empty()){
        for (auto& idx : initIndices_){
          ptr_str << "[" << idx << "]";
        }
      }
      for (FieldDecl* fd : activeFieldDecls_){
        ptr_str << "." << fd->getNameAsString();
      }
      currentNs_->setRelocationOffsetDeclared(dstFieldOffsetPtrName);
    }
    ptr_str << "; ";
    cpp_str << "extern void* " << dstFieldOffsetPtrName << "; ";
  }

  if (member){
    std::stringstream sstr;
    sstr << "__field_offset_ptr_" << dr->getDecl()->getNameAsString() << "_" << member->getNameAsString();
    srcFieldOffsetPtrName = sstr.str();
    if (!currentNs_->relocationOffsetDeclared(srcFieldOffsetPtrName)){
      ptr_str << "void* " << srcFieldOffsetPtrName << " = "
           << "&" << dr->getDecl()->getNameAsString()
           << "." << member->getNameAsString() << "; ";
      currentNs_->setRelocationOffsetDeclared(srcFieldOffsetPtrName);
    }
    cpp_str << "extern void* " << srcFieldOffsetPtrName << "; ";
  }

  std::string ptr_decls = ptr_str.str();
  if (!ptr_decls.empty()){
    SourceLocation end = getEndLoc(vd->getLocEnd());
    rewriter_.InsertText(end, ptr_decls);
  }



  std::string srcScopedName = scopedNames_[mainDecl(dr)];
  if (srcScopedName.empty()){
    errorAbort(dr->getLocStart(), *ci_,
               "failed configuring global variable relocation");
  }

  if (!currentNs_->variableDefined(srcScopedName)){
    cpp_str << "extern void* __ptr_" << srcScopedName << ";\n"
            << "extern int __offset_" << srcScopedName << ";\n";
  }

  cpp_str << "sstmac::RelocationPointer r" << currentNs_->filePrefix(ci_, op->getLocStart())
      << numRelocations_++ << "(";

  if (member){
    if (srcFieldOffsetPtrName.empty()){
      errorAbort(dr->getLocStart(), *ci_,
                 "failed configuring global variable relocation");
    }
    cpp_str << srcFieldOffsetPtrName;
  } else {
    cpp_str << "__ptr_" << srcScopedName;
  }
  cpp_str << ",__ptr_" << srcScopedName
       << ",__offset_" << srcScopedName
       << ",";
  if (activeFieldDecls_.empty()){
    cpp_str << "__ptr_" << activeGlobalScopedName();
  } else {
    cpp_str << dstFieldOffsetPtrName;
  }

  cpp_str << ",__ptr_" << activeGlobalScopedName()
       << ",__offset_" << activeGlobalScopedName()
       << ");";

  currentNs_->relocations.push_back(cpp_str.str());
}

bool
SkeletonASTVisitor::TraverseUnaryOperator(UnaryOperator* op, DataRecursionQueue* queue)
{
  //VisitStmt(op);
  if (activeGlobal() && op->getOpcode() == UO_AddrOf){
    Expr* e = getUnderlyingExpr(op->getSubExpr());
    if (e->getStmtClass() == Stmt::DeclRefExprClass){
      DeclRefExpr* dr = cast<DeclRefExpr>(e);
      if (isGlobal(dr)){
        addRelocation(op, dr);
        return true; //don't refactor global variable
      }
    } else if (e->getStmtClass() == Stmt::MemberExprClass){
      MemberExpr* m = cast<MemberExpr>(e);
      Expr* base = getUnderlyingExpr(m->getBase());
      if (base->getStmtClass() == Stmt::DeclRefExprClass){
        DeclRefExpr* dr = cast<DeclRefExpr>(base);
        if (isGlobal(dr)){
          //oh my science - why - pointer to a field of a global struct
          ValueDecl* member = m->getMemberDecl();
          addRelocation(op, dr, member);
          return true;
        }
      }
    }
  }

  switch(op->getOpcode()){
    case UO_Deref: {
      PushGuard<Expr*> pg(activeDerefs_, op);
      goIntoContext(op, [&]{
        TraverseStmt(op->getSubExpr());
      });
      break;
    }
    default:
      goIntoContext(op, [&]{
        TraverseStmt(op->getSubExpr());
      });
      break;
  }



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
  try {

  PragmaActivateGuard pag(op, this);
  if (pag.skipVisit()) return true;


  auto toExec = [&]{
    VectorPushGuard<std::pair<BinaryOperator*,BinOpSide>>
        pg(binOps_, op, BinOpSide::LHS);
    TraverseStmt(op->getLHS());
    pg.swap(op, BinOpSide::RHS);
    TraverseStmt(op->getRHS());
  };

  switch (op->getOpcode()){
    case BO_Mul:
    case BO_Add:
    case BO_Rem:
    case BO_Div:
    case BO_Assign:
    case BO_MulAssign:
    case BO_DivAssign:
    case BO_RemAssign:
    case BO_AddAssign:
    case BO_SubAssign: {
      IndexGuard ig(activeBinOpIdx_, binOps_.size());
      goIntoContext(op, toExec);
      break;
    }
    default:
      goIntoContext(op, toExec);
      break;
  }

  } catch (StmtDeleteException& e){
    if (e.deleted != op) throw e;
    else return true; //deleted, don't visit anything else
  }

  return true;
}

bool
SkeletonASTVisitor::TraverseIfStmt(IfStmt* stmt, DataRecursionQueue* queue)
{
  try {
    PragmaActivateGuard pag(stmt, this);
    if (pag.skipVisit()) return true;

    //only make this an "active" statement on the conditional
    //once we decide to visit the bodies, ignore the if
    goIntoContext(stmt, [&]{
      PushGuard<IfStmt*> pg(activeIfs_, stmt);
      TraverseStmt(stmt->getCond());
    });

    TraverseStmt(stmt->getThen());
    if (stmt->getElse()) TraverseStmt(stmt->getElse());
  } catch (StmtDeleteException& e) {
    if (e.deleted != stmt) throw e;
  }

  return true;
}

bool
SkeletonASTVisitor::TraverseDeclStmt(DeclStmt* stmt, DataRecursionQueue* queue)
{
  try {

  PragmaActivateGuard pag(stmt, this);

  goIntoContext(stmt, [&]{
    if (stmt->isSingleDecl()){
      Decl* D = stmt->getSingleDecl();
      SSTNullVariablePragma* prg = getNullVariable(D);
      if (prg){
        if (!prg->keepCtor()){
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
  });

  } catch (StmtDeleteException& e) {
    if (e.deleted != stmt) throw e;
    else return true;
  }

  return true;
}

bool
SkeletonASTVisitor::TraverseDoStmt(DoStmt* S, DataRecursionQueue* queue)
{
  try {
    PragmaActivateGuard pag(S, this);
    if (!pag.skipVisit()){
     //PushGuard<Stmt*> pg(loopContexts_, S);
      if (S->getBody()) TraverseStmt(S->getBody());
      if (S->getCond()) TraverseStmt(S->getCond());
    }
  } catch (StmtDeleteException& e) {
    if (e.deleted != S) throw e;
  }
  return true;
}

bool
SkeletonASTVisitor::TraverseWhileStmt(WhileStmt* S, DataRecursionQueue* queue)
{
  try {
    PragmaActivateGuard pag(S, this);
    if (!pag.skipVisit()){
      PushGuard<Stmt*> pg(loopContexts_, S);
      if (S->getCond()) TraverseStmt(S->getCond());
      if (S->getBody()) TraverseStmt(S->getBody());
    }
  } catch (StmtDeleteException& e) {
    if (e.deleted != S) throw e;
  }
  return true;
}

bool
SkeletonASTVisitor::TraverseForStmt(ForStmt *S, DataRecursionQueue* queue)
{
  try {
    PragmaActivateGuard pag(S, this);
    if (pag.skipVisit()) return true;

    PushGuard<Stmt*> pg(loopContexts_, S);
    if (S->getInit()) TraverseStmt(S->getInit());
    if (S->getCond()) TraverseStmt(S->getCond());
    if (S->getInc()) TraverseStmt(S->getInc());
    if (S->getBody()) TraverseStmt(S->getBody());

  } catch (StmtDeleteException& e) {
    if (e.deleted != S) throw e;
  }

  return true;
}

bool
SkeletonASTVisitor::TraverseArraySubscriptExpr(ArraySubscriptExpr* expr, DataRecursionQueue* queue)
{
  /**
  Expr* base = getUnderlyingExpr(expr->getBase());
  if (base->getStmtClass() == Expr::DeclRefExprClass){
    DeclRefExpr* dref = cast<DeclRefExpr>(base);
    if (isNullVariable(dref->getFoundDecl())){
      if (stmt_contexts_.empty()){
        errorAbort(expr->getLocStart(), *ci_,
                   "array subscript applied to null variable");
      } else {
        Stmt* toDel = stmt_contexts_.front();
        deleteStmt(toDel);
        //break out, stmt is gone
        throw StmtDeleteException(toDel);
      }
    }
  }
  */

  PushGuard<Expr*> pg(activeDerefs_, expr);
  TraverseStmt(expr->getBase());
  TraverseStmt(expr->getIdx());

  return true;
}

bool
SkeletonASTVisitor::VisitStmt(Stmt *S)
{
  if (noSkeletonize_) return true;

  try {
    PragmaActivateGuard pag(S, this);
  } catch (StmtDeleteException& e) {
    if (e.deleted != S) throw e;
  }

  return true;
}

bool
SkeletonASTVisitor::VisitTypedefDecl(TypedefDecl* D)
{
  auto ty = D->getUnderlyingType().getTypePtr();
  if (ty->isStructureType()){
    auto str_ty = ty->getAsStructureType();
    if (!str_ty){
      errorAbort(D->getLocStart(), *ci_,
                 "structure type did not return a record declaration");
    }
    typedefStructs_[str_ty->getDecl()] = D;
  } else if (ty->isUnionType()){
    auto un_ty = ty->getAsUnionType();
    if (!un_ty){
      errorAbort(D->getLocStart(), *ci_,
                 "union type did not return a record declaration");
    }
    typedefStructs_[un_ty->getDecl()] = D;
  }
  return true;
}

bool
SkeletonASTVisitor::TraverseDecl(Decl *D)
{
  if (!D) return true;

  try {
    PragmaActivateGuard pag(D, this);
    if (pag.skipVisit()) return true;

    RecursiveASTVisitor<SkeletonASTVisitor>::TraverseDecl(D);
  } catch (DeclDeleteException& e) {
    if (e.deleted != D) throw e;
  }
  return true;
}


void
SkeletonASTVisitor::PragmaActivateGuard::init()
{
  skipVisit_ = false;
  auto iter = myPragmas_.begin();
  auto end = myPragmas_.end();
  while (iter != end){
    auto tmp = iter++;
    SSTPragma* prg = *tmp;
    bool activate = !visitor_->noSkeletonize();

    //a compute pragma totally deletes the block
    bool blockDeleted = false;
    switch (prg->cls){
      case SSTPragma::GlobalVariable: //always - regardless of skeletonization
      case SSTPragma::Overhead:
        skipVisit_ = false;
        activate = true;
        break;
      case SSTPragma::Keep:
        skipVisit_ = true;
        activate = true; //always - regardless of skeletonization
        break;
      case SSTPragma::AlwaysCompute:
        blockDeleted = true;
        activate = true;
        break;
      case SSTPragma::Compute:
      case SSTPragma::Delete:
      case SSTPragma::Init:
      case SSTPragma::Instead:
      case SSTPragma::Return:
        blockDeleted = true;
        break;
      default: break;
    }
    skipVisit_ = skipVisit_ || blockDeleted;

    if (!activate){
      myPragmas_.erase(tmp);
    }
  }

  if (visitor_->pragmaConfig_.makeNoChanges){
    skipVisit_ = true;
    visitor_->pragmaConfig_.makeNoChanges = false;
  }
}

SkeletonASTVisitor::PragmaActivateGuard::~PragmaActivateGuard()
{
  for (SSTPragma* prg : activePragmas_){
    prg->deactivate(visitor_->pragmaConfig_);
  }
  visitor_->pragmaConfig_.pragmaDepth--;
}

void
SkeletonASTVisitor::propagateNullness(Decl* target, Decl* src)
{
  //yep, it does
  if (target->getKind() != Decl::Var){
    errorAbort(target->getLocStart(), *ci_,
          "propagate nullness to declaration that isn't a variable");
  }
  VarDecl* vd = cast<VarDecl>(target);
  //VarDecl* svd = cast<VarDecl>(src);
  //propagate the null-ness to this new variable
  SSTNullVariablePragma* oldPragma = getNullVariable(src);
  SSTNullVariablePragma*& existing = pragmaConfig_.nullVariables[vd];
  if (!existing){
    existing = oldPragma->clone();
    existing->setTransitive(oldPragma);
  }
}

void
SkeletonASTVisitor::nullifyIfStmt(IfStmt* if_stmt, Decl* d)
{
  //oooooh, not good - I could really foobar things here
  //crash and burn and tell programmer to fix it
  warn(if_stmt->getLocStart(), *ci_,
       "null variables used as predicate in if-statement - "
       "this could produce undefined behavior - forcing always false");
  IfStmt* ifs = cast<IfStmt>(if_stmt);
  //force the replacement
  ::replace(ifs->getCond(), rewriter_, "0", *ci_);
  throw StmtDeleteException(ifs);
}

Stmt*
SkeletonASTVisitor::replaceNullVariableStmt(Stmt* s, const std::string& repl)
{
  Stmt* toRepl = s;
  auto iter = extendedReplacements_.find(s);
  if (iter != extendedReplacements_.end()){
    toRepl = iter->second;
    extendedReplacements_.erase(iter);
  }
  ::replace(toRepl->getSourceRange(), rewriter_, repl, *ci_);
  return toRepl;
}

void
SkeletonASTVisitor::deleteNullVariableStmt(Stmt* s)
{
  Stmt* actuallyReplaced = replaceNullVariableStmt(s, "");
  throw StmtDeleteException(actuallyReplaced);
}



Expr*
SkeletonASTVisitor::getFinalExpr(Expr* e)
{
#define sub_case(e,cls) \
  case Stmt::cls##Class: \
    e = cast<cls>(e)->getSubExpr(); break
  while (1){
    switch(e->getStmtClass()){
    sub_case(e,UnaryOperator);
    sub_case(e,ParenExpr);
    sub_case(e,CStyleCastExpr);
    sub_case(e,ImplicitCastExpr);
    default:
      return e;
    }
  }
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
    sub_case(e,CXXBindTemporaryExpr);
    sub_case(e,ExprWithCleanups);
    case Stmt::MaterializeTemporaryExprClass: {
      MaterializeTemporaryExpr* mte = cast<MaterializeTemporaryExpr>(e);
      e = mte->GetTemporaryExpr();
      break;
    }
    default:
      return e;
    }
  }
#undef sub_case
}

const Decl*
SkeletonASTVisitor::getOriginalDeclaration(VarDecl* vd)
{
  MemberSpecializationInfo* msi = nullptr;
  ClassTemplateSpecializationDecl* templDecl = nullptr;
  if (vd->isStaticDataMember()){
    msi = vd->getMemberSpecializationInfo();
    if (msi){
      return msi->getInstantiatedFrom();
      templDecl = cast<ClassTemplateSpecializationDecl>(vd->getDeclContext());
    } else {
      return mainDecl(vd);
    }
  } else {
    return mainDecl(vd);
  }
}

void
SkeletonASTVisitor::maybeReplaceGlobalUse(DeclRefExpr* expr, SourceRange replRng)
{
  if (keepGlobals_) return;

  switch (expr->getDecl()->getKind()){
  case Decl::Var: {
    VarDecl* vd = cast<VarDecl>(expr->getDecl());

    const Decl* md = getOriginalDeclaration(vd);

    auto iter = globals_.find(md);
    if (iter != globals_.end()){
      GlobalReplacement& repl = iter->second;
      if (globalsTouched_.empty() || !ctorContexts_.empty()){
        replace(replRng, repl.oneOffText);
        return;
      }

      /** Don't do this check anymore, redirect to an accessor method
      if (msi){ //for now, we have to check if this outside class
        DeclContext* dc = vd->getDeclContext();
        bool bad = true;
        for (DeclContext* test : classContexts_){
          if (test == dc){
            bad = false;
            break;
          }
        }
        if (bad){
          errorAbort(expr->getLocStart(), *ci_,
              "template static variables cannot currently be accessed outside of class");
        }
      }
      */


      globalsTouched_.back().insert(md);
      std::string replText = repl.append ? appendText(expr, repl.oneOffText) : repl.reusableText;
      //there is a bug in Clang I can't quite track down
      //it is erroneously causing DeclRefExpr to get visited twice
      //when they occur inside a struct decl
      auto done = alreadyReplaced_.find(expr);
      if (done == alreadyReplaced_.end()){
        replace(replRng, replText);
        alreadyReplaced_.insert(expr);
      }
    }
    break;
  }
  case Decl::VarTemplateSpecialization: {
    VarTemplateSpecializationDecl* vtsd = cast<VarTemplateSpecializationDecl>(expr->getDecl());
    MemberSpecializationInfo* msi = vtsd->getMemberSpecializationInfo();
    VarDecl* staticDecl = vtsd->getInstantiatedFromStaticDataMember();
    VarDecl* search = msi ? vtsd->getTemplateInstantiationPattern()->getCanonicalDecl()
         : (staticDecl ? staticDecl : vtsd->getTemplateInstantiationPattern());
    if (variableTemplates_.find(search) != variableTemplates_.end()){
      //convert access to a call operator
      //really weird that I need to do this + 1
      rewriter_.InsertText(expr->getLocEnd().getLocWithOffset(1), "()", false);
    } else if (vtsd->isStaticDataMember()) {
      internalError(expr->getLocStart(), *ci_,
                 "failed replacing static template member");
    }
    break; //proceed
  }
  default:
    break; //nooo!
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
  pragmas_(prgs), rewriter_(rw), pragmaConfig_(cfg), noSkeletonize_(false)
{
  const char* skelStr = getenv("SSTMAC_SKELETONIZE");
  if (skelStr){
    bool doSkel = atoi(skelStr);
    noSkeletonize_ = !doSkel;
  }
}
