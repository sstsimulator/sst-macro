/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

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
#include <sys/time.h>
#include <iostream>
#include <fstream>
#include <sstmac/common/sstmac_config.h>

clang::LangOptions Printing::langOpts;
clang::PrintingPolicy Printing::policy(Printing::langOpts);

llvm::cl::OptionCategory CompilerGlobals::sstmacCategoryOpt("SST/Macro options");
llvm::cl::opt<std::string> CompilerGlobals::includeListOpt("system-includes",
  llvm::cl::desc("Give the list of default system include paths for the pre-processing compiler"),
  llvm::cl::value_desc("list:of:paths"),
  llvm::cl::cat(sstmacCategoryOpt),
  llvm::cl::ValueRequired);
llvm::cl::opt<bool> CompilerGlobals::skeletonizeOpt("skeletonize",
  llvm::cl::desc("Run skeletonization source-to-source"),
  llvm::cl::cat(sstmacCategoryOpt));
llvm::cl::opt<bool> CompilerGlobals::memoizeOpt("memoize",
  llvm::cl::desc("Run memoization source-to-source"),
  llvm::cl::cat(sstmacCategoryOpt));
llvm::cl::opt<bool> CompilerGlobals::encapsulateOpt("encapsulate",
  llvm::cl::desc("Run skeletonization source-to-source"),
  llvm::cl::cat(sstmacCategoryOpt));
llvm::cl::opt<bool> CompilerGlobals::shadowizeOpt("shadowize",
  llvm::cl::desc("Run memoization source-to-source"),
  llvm::cl::cat(sstmacCategoryOpt));
llvm::cl::opt<bool> CompilerGlobals::puppetizeOpt("puppetize",
  llvm::cl::desc("Run skeletonization source-to-source"),
  llvm::cl::cat(sstmacCategoryOpt));
llvm::cl::opt<bool> CompilerGlobals::verboseOpt("verbose",
  llvm::cl::desc("Print verbose source-to-source output"),
  llvm::cl::cat(sstmacCategoryOpt));
static llvm::cl::alias verboseAliasOpt("v",
  llvm::cl::aliasopt(CompilerGlobals::verboseOpt),
  llvm::cl::cat(CompilerGlobals::sstmacCategoryOpt));
llvm::cl::opt<bool> CompilerGlobals::refactorMainOpt("refactor-main",
  llvm::cl::desc("Refactor main, rerouting to SST/macro main wrapper"),
  llvm::cl::cat(CompilerGlobals::sstmacCategoryOpt));
llvm::cl::opt<bool> CompilerGlobals::noRefactorMainOpt("no-refactor-main",
  llvm::cl::desc("Do not refactor main, leaving symbol as is"),
  llvm::cl::cat(CompilerGlobals::sstmacCategoryOpt));

modes::Mode CompilerGlobals::mode;
int CompilerGlobals::modeMask;
bool CompilerGlobals::refactorMain = true;
std::vector<std::string> CompilerGlobals::realSystemIncludePaths;
decltype(CompilerGlobals::visitor) CompilerGlobals::visitor;

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;
using namespace modes;

/**
 * @brief isFxnPointerForm
 * @param qt
 * @return Whether a type is a function pointer type and is implicit such as (void)(*funcvar)(int);
 *         rather than typedef void(*functype)(int); functype funcvar;
 */
static bool isFxnPointerForm(QualType qt)
{
  //typedefs need no special treatment
  bool isTypeDef = isa<TypedefType>(qt.getTypePtr());
  if (isTypeDef) return false;

  return qt->isFunctionPointerType() || qt->isFunctionProtoType();
}

static std::string appendText(clang::Expr* expr, const std::string& toAppend)
{
  PrettyPrinter pp;
  pp.print(expr);
  pp.os << toAppend;
  return pp.str();
}

void
CompilerGlobals::setup(clang::CompilerInstance* CI)
{
  ci = CI;
  int skeletonize = 0;
  const char* skelStr = getenv("SSTMAC_SKELETONIZE");
  if (skelStr){
    int runSkeletonize = atoi(skelStr);
    if (runSkeletonize){
      skeletonize = 1;
    }
  }

  if (skeletonizeOpt.getNumOccurrences()){
    skeletonize = 1;
  }

  int puppetize = 0;
  const char* puppetStr = getenv("SSTMAC_PUPPETIZE");
  if (puppetStr){
    int runPuppetize = atoi(puppetStr);
    if (runPuppetize){
      puppetize = 1;
    }
  }

  if (puppetizeOpt.getNumOccurrences()){
    puppetize = 1;
  }

  int shadowize = 0;
  const char* shadowStr = getenv("SSTMAC_SHADOWIZE");
  if (shadowStr){
    int runShadowize = atoi(shadowStr);
    if (runShadowize){
      shadowize = 1;
    }
  }

  if (shadowizeOpt.getNumOccurrences()){
    shadowize = 1;
  }

  int memoize = 0;
  const char* memoStr = getenv("SSTMAC_MEMOIZE");
  if (memoStr){
    int runMemoize = atoi(memoStr);
    if (runMemoize){
      memoize = 1;
    }
  }

  if (memoizeOpt.getNumOccurrences()){
    memoize = 1;
  }

  int encapsulate = 0;
  const char* encapsulaeStr = getenv("SSTMAC_ENCAPSULATE");
  if (encapsulaeStr){
    int runEncapsulate = atoi(encapsulaeStr);
    if (runEncapsulate){
      encapsulate = 1;
    }
  }

  if (encapsulateOpt.getNumOccurrences()){
    encapsulate = 1;
  }

  int modeSum = skeletonize + memoize + shadowize + puppetize + encapsulate;
  if (modeSum > 1){
    std::cerr << "input error: can only specify one mode of "
                 "skeletonize, memoize, shadowize, puppetize, or encapsulate"
              << std::endl;
    exit(EXIT_FAILURE);
  }

  using namespace modes;
  if (modeSum == 0){
    mode = ENCAPSULATE_MODE;
  } else {
    if (encapsulate) mode = ENCAPSULATE_MODE;
    if (skeletonize) mode = SKELETONIZE_MODE;
    if (puppetize)   mode = PUPPETIZE_MODE;
    if (shadowize)   mode = SHADOWIZE_MODE;
    if (memoize)     mode = MEMOIZE_MODE;
  }
  modeMask = 1 << mode;


  if (includeListOpt.getNumOccurrences()){
    char fullpathBuffer[1024];
    std::istringstream sstr(includeListOpt.getValue());
    std::string path;
    while (std::getline(sstr, path, ':')){
      //make sure the full paths (following symlinks)
      //are included for checking
      const char* fullpath_cstr = realpath(path.c_str(), fullpathBuffer);
      if (fullpath_cstr){
        realSystemIncludePaths.emplace_back(fullpath_cstr);
      } else {
        std::cerr << "realpath(...) failed to resolve " << path
                 << ". Cannot continue." << std::endl;
        ::abort();
      }
    }
  }

  const char* mainStr = getenv("SSTMAC_REFACTOR_MAIN");
  if (mainStr){
    refactorMain = atoi(mainStr);
  }

  if (refactorMain){
    refactorMain = true;
    if (noRefactorMainOpt){
      std::cerr << "Cannot specify both refactor/no-refactor options" << std::endl;
      ::abort();
    }
  } else if (noRefactorMainOpt){
    refactorMain = false;
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
  mpiCalls_["sstmac_alltoall"] = &SkeletonASTVisitor::visitCollective;

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
  if (CompilerGlobals::toolInfoRegistration.extraInputFileParams.empty()){
    return;
  }

  os << "\n#include <sprockit/keyword_registration.h>"
     << "\nRegisterKeywords(";
  for (auto& str : CompilerGlobals::toolInfoRegistration.extraInputFileParams){
    os << "\n{\"" << str << "\", \"new keyword\" },";
  }
  os << "\n);";
}

void
SkeletonASTVisitor::initHeaders()
{
  const char* headerListFile = getenv("SSTMAC_HEADERS");
  if (headerListFile){
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
}

void
SkeletonASTVisitor::preVisitTopLevelDecl(Decl *d)
{
  setTopLevelScope(d);
  bool isGlobalVar = isa<VarDecl>(d);
  setVisitingGlobal(isGlobalVar);
}

void
SkeletonASTVisitor::postVisitTopLevelDecl(Decl * /*d*/)
{
  setVisitingGlobal(false); //and reset
}

bool
SkeletonASTVisitor::shouldVisitDecl(VarDecl* D)
{
  if (keepGlobals_ || isa<ParmVarDecl>(D) || D->isImplicit() || insideCxxMethod_){
    return false;
  }

  //ignore eli variables
  std::string varName = D->getNameAsString();
  if (varName.size() >= 5){
    std::string last4 = varName.substr(varName.size() - 4);
    if (last4 == "_eli"){
      return false;
    }
  }

  if (D->getType()->isUnionType()){
    if (D->getType()->getAsUnionType()->getDecl()->isAnonymousStructOrUnion()){
      return false;
    }
  } else if (D->getType()->isStructureType()){
    if (D->getType()->getAsStructureType()->getDecl()->isAnonymousStructOrUnion()){
      return false;
    }
  }

  bool isConst = D->getType().isConstQualified();
  bool isConstPtr = false;
  if (D->getType()->isPointerType()){
    isConstPtr = D->getType()->getPointeeType().isConstQualified();
  }
  if (isConst || isConstPtr){
    if (D->hasInit()){
      GlobalVariableVisitor visitor(D,this);
      visitor.TraverseDecl(D);
      if (!visitor.visitedGlobals()){
        //this is const and not inited from any other global variables
        return false;
      }
    } else {
      //this is const and touches no globals in initialization
      //this does not actually need to be a special global variable
      return false;
    }
  }

  //do visit if we are not in a system header
  //do NOT visit if we are not in a system header
  return !isInSystemHeader(getStart(D));
}

bool
SkeletonASTVisitor::isInSystemHeader(SourceLocation loc)
{
  PresumedLoc ploc = CompilerGlobals::SM().getPresumedLoc(loc);
  SourceLocation headerLoc = ploc.getIncludeLoc();
  if (!headerLoc.isValid()){
    return false;
  }

  char fullpathBuffer[1024];
  const char* fullpath_cstr = realpath(ploc.getFilename(), fullpathBuffer);
  if (fullpath_cstr){
    llvm::StringRef fullpath(fullpath_cstr);
    if (validHeaders_.empty()){
      //we have not been explicitly given a list of valid headers
      //just ignore all headers in default system paths
      for (const auto& system_path : CompilerGlobals::realSystemIncludePaths){
        if (fullpath.startswith(system_path)){
          return true;
        }
      }
      //found no matches
      return false;
    } else {
      return validHeaders_.find(fullpath) == validHeaders_.end();
    }
  } else {
    std::string err_str = std::string("realpath(...) failed to resolve ") + ploc.getFilename() + ". Cannot continue.";
    errorAbort(loc, err_str);
    return false; //for warnings
  }
}


bool
SkeletonASTVisitor::VisitCXXNewExpr(CXXNewExpr * /*expr*/)
{
  return true; //don't do this anymore
}

bool
SkeletonASTVisitor::TraverseDecltypeTypeLoc(clang::DecltypeTypeLoc  /*loc*/)
{
  return true;
}

bool
SkeletonASTVisitor::TraverseCXXDeleteExpr(CXXDeleteExpr* expr, DataRecursionQueue*  /*queue*/)
{
  activeBinOpIdx_ = IndexResetter;

  if (CompilerGlobals::modeActive(modes::SKELETONIZE | modes::SHADOWIZE)){
    goIntoContext(expr, [&]{
      TraverseStmt(expr->getArgument());
    });
  }

  return true;
}

bool
SkeletonASTVisitor::TraverseInitListExpr(InitListExpr* expr, DataRecursionQueue*  /*queue*/)
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
      errorAbort(expr, "operator used on null variable");
    }
  }
  return true;
}

void
SkeletonASTVisitor::executeCurrentReplacements()
{
  for (auto& pair : stmtReplacements_.back()){
    ::replace(pair.first, pair.second);
  }
}

void
SkeletonASTVisitor::replace(SourceRange rng, const std::string& repl)
{
  if (stmtContexts_.empty()){
    //go ahead an immediately do the replacement
    ::replace(rng, repl);
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
              + " is assigned to null_ptr, but is not a null_ptr itself";
          errorAbort(expr, error);
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

    Expr* toDel = binOp;
    if(binOp->getOpcode() == BO_Assign && side != LHS && !repl.empty()){
        toDel = getUnderlyingExpr(binOp->getRHS());
    }
    ::replace(toDel, repl);
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
  sstr << "null_ptr " << varName << " used in dereference";
  for (Stmt* s : loopContexts_){
    sstr << "\nconsider skeletonizing with pragma sst compute here: "
         << getStart(s).printToString(CompilerGlobals::SM());
  }
  errorAbort(expr, sstr.str());
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
          SSTComputePragma::replaceForStmt(forLoop, "");
        } else {
          nullDereferenceError(expr, nd->getNameAsString());
        }
        throw StmtDeleteException(loop);
      }
    }
    nullDereferenceError(expr, nd->getNameAsString());
  }

  if (!nullVarPrg->deleteAll() && haveActiveFxnParam()){
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
          fxnDeclLoc = getStart(fd);
        }
        std::stringstream sstr;
        sstr << "null variable " << nd->getNameAsString()
            << " mapped into non-null parameter " << pvd->getNameAsString()
            << " in function ";
        if (!fxnName.empty()){
          sstr << fxnName << " declared at "
               << fxnDeclLoc.printToString(CompilerGlobals::SM());
        }
        if (nullVarPrg->isTransitive())
          addTransitiveNullInformation(nd, sstr, nullVarPrg);
        errorAbort(expr, sstr.str());
      }
    }
  }

  bool hasRepl = nullVarPrg->hasReplacement();
  Stmt* outerMostAssignment = checkNullAssignments(nd, hasRepl);
  if (hasRepl){
    replaceNullVariableConnectedContext(expr, nullVarPrg->getReplacement());
  } else if (!activeIfs_.empty()){
    if (activeIfs_.size() > 1){
      errorAbort(activeIfs_.back(), "internal error: cannot handle nested if-stmts with null variables");
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
          warn(getStart(bop), warning);
        }
      }
    }
    deleteNullVariableStmt(outerMostAssignment);
  } else {
    //I am not assigning to anyone nor am I passed along as
    //a function parameter - but I have not been given permission to delete all
    //that's okay - I'm a meaningless standalone statement - delete me
    if (haveActiveFxnParam()){
      //but I can't just delete if I am being passed off to another expression
      std::stringstream sstr;
      sstr << "variable " << nd->getNameAsString() << " is a null_ptr "
           << " and passed to a function with no replacement specified ";
      if (nullVarPrg->isTransitive())
        addTransitiveNullInformation(nd, sstr, nullVarPrg);
      errorAbort(expr, sstr.str());
    }
    replaceNullVariableConnectedContext(expr, "");
  }
}

bool
GlobalVariableVisitor::VisitDeclRefExpr(DeclRefExpr *expr)
{
  if (parent_->isGlobal(expr)){
    visitedGlobals_ = true;
    return false;
  }
  return true;
}

bool
GlobalVariableVisitor::VisitCallExpr(CallExpr*  /*expr*/)
{
  //we have to assume this touches globals
  visitedGlobals_ = true;
  return false;
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
  if (CompilerGlobals::modeActive(modes::SKELETONIZE | modes::SHADOWIZE)){
      //first buffer argument to nullptr
      if (expr->getArg(0)->getType()->isPointerType()){
        //make sure this isn't a shortcut function without buffers
        replace(expr->getArg(0), "nullptr");
        replace(expr->getArg(3), "nullptr");
        //rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
        //rewriter_.ReplaceText(expr->getArg(3)->getSourceRange(), "nullptr");
        deletedArgsCurrentCallExpr_.insert(expr->getArg(0));
        deletedArgsCurrentCallExpr_.insert(expr->getArg(3));
      }
  }
}

void
SkeletonASTVisitor::visitReduce(CallExpr *expr)
{
  if (CompilerGlobals::modeActive(modes::SKELETONIZE | modes::SHADOWIZE)){
    //first buffer argument to nullptr
    if (expr->getArg(0)->getType()->isPointerType()){
      //make sure this isn't a shortcut function without buffers
      replace(expr->getArg(0), "nullptr");
      replace(expr->getArg(1), "nullptr");
      //rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
      //rewriter_.ReplaceText(expr->getArg(1)->getSourceRange(), "nullptr");
      deletedArgsCurrentCallExpr_.insert(expr->getArg(0));
      deletedArgsCurrentCallExpr_.insert(expr->getArg(1));
    }
  }
}

void
SkeletonASTVisitor::visitPt2Pt(CallExpr *expr)
{
  if (CompilerGlobals::modeActive(SKELETONIZE | SHADOWIZE)){
    //first buffer argument to nullptr
    if (expr->getArg(0)->getType()->isPointerType()){
      //make sure this isn't a shortcut function without buffers
      replace(expr->getArg(0), "nullptr");
      //rewriter_.ReplaceText(expr->getArg(0)->getSourceRange(), "nullptr");
      deletedArgsCurrentCallExpr_.insert(expr->getArg(0));
    }
  }
}
bool 
SkeletonASTVisitor::TraverseReturnStmt(clang::ReturnStmt* stmt, DataRecursionQueue*  /*queue*/)
{
  try {
    PragmaActivateGuard pag(stmt, this);
    if (pag.skipVisit()) return true;

    TraverseStmt(stmt->getRetValue());
  } catch (StmtDeleteException& e){
    if (e.deleted != stmt) throw e;
  }

  return true;
}

bool
SkeletonASTVisitor::TraverseCXXMemberCallExpr(CXXMemberCallExpr* expr, DataRecursionQueue*  /*queue*/)
{
  PragmaActivateGuard pag(expr, this);
  if (pag.skipVisit()){
    return true;
  }

  CXXRecordDecl* cls = expr->getRecordDecl();
  std::string clsName = cls->getNameAsString();
  if (clsName == "mpi_api"){
    FunctionDecl* decl = expr->getDirectCallee();
    if (!decl){
      errorAbort(expr, "invalid MPI call");
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
      internalError(getStart(expr), "base of CXXMemberCallExpr is not a MemberExpr");
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
      if (deletedArgsCurrentCallExpr_.find(arg) == deletedArgsCurrentCallExpr_.end()){
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
    errorAbort(toRepl, "cannot create empty reference for null_ptr");
  }

  std::string repl;
  if (type->isPointerType()){
    repl = "nullptr";
  } else {
    repl = GetAsString(type) + "()";
  }

  ::replace(toRepl, repl);
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
SkeletonASTVisitor::TraverseMemberExpr(MemberExpr *expr, DataRecursionQueue*  /*queue*/)
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
        replace(expr, gr.inlineUseText);
      } else {
        //I hate that clang makes me do this
        //source locations are all messed up and I can't just append
        std::string replText = gr.append ? appendText(expr, gr.inlineUseText) : gr.reusableText;
        replace(expr, replText);
      }
    }
  }

  PushGuard<MemberExpr*> pg(memberAccesses_, expr);
  TraverseStmt(expr->getBase());
  return true;
}

bool
SkeletonASTVisitor::TraverseCallExpr(CallExpr* expr, DataRecursionQueue*  /*queue*/)
{
  //this "breaks" connections for null variable propagation
  activeBinOpIdx_ = IndexResetter;
  try {
    PragmaActivateGuard pag(expr, this);
    if (pag.skipVisit()) return true;

    //first go into the call expression to see if we should even keep it
    //we might get a delete exception, in which case we should skip traversal
    //if we got here, we are safe to modify the function name
    if (CompilerGlobals::modeActive(SKELETONIZE | SHADOWIZE)) {
      Expr* fxn = getUnderlyingExpr(const_cast<Expr*>(expr->getCallee()));
      if (fxn->getStmtClass() == Stmt::DeclRefExprClass){
        DeclRefExpr* dref = cast<DeclRefExpr>(fxn);
        std::string fxnName = dref->getFoundDecl()->getNameAsString();
        if (sstmacFxnPrepends_.find(fxnName) != sstmacFxnPrepends_.end()){
          insertBefore(expr->getCallee(), "sstmac_");
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
          if (deletedArgsCurrentCallExpr_.find(arg) == deletedArgsCurrentCallExpr_.end()){
            TraverseStmt(arg);
          }
        } else {
          Expr* arg = expr->getArg(i);
          if (deletedArgsCurrentCallExpr_.find(arg) == deletedArgsCurrentCallExpr_.end()){
            TraverseStmt(arg);
          }
        }
      }
    });
  } catch (StmtDeleteException& e) {
    if (e.deleted != expr) throw e;
  }
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


/* TODO remove, unused
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
*/

void
SkeletonASTVisitor::arrayFxnPointerTypedef(VarDecl* D, SkeletonASTVisitor::ArrayInfo* info,
                                   std::stringstream& sstr)
{
  std::string typeStr = GetAsString(D->getType());
  auto replPos = typeStr.find("*[");
  std::string typedefText = typeStr.insert(replPos+1, info->typedefName);
  sstr << "typedef " << typedefText;
}

SkeletonASTVisitor::ArrayInfo*
SkeletonASTVisitor::checkArray(VarDecl* D)
{
  const Type* ty  = D->getType().getTypePtr();
  bool isC99array = ty->isArrayType();
  bool isConstC99array = ty->isConstantArrayType();

  if (isConstC99array){
    ArrayInfo* info = new ArrayInfo;
    info->typedefName = "array_type_" + D->getNameAsString();
    info->fqTypedefName = currentNs_->nsPrefix() + info->typedefName;
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
        errorAbort(D, "anonymous struct used in array declaration - cannot refactor");
      }
    }

    info->typedefDeclString = sstr.str();
    info->retType = info->fqTypedefName + "*";
    info->needsDeref = false;
    return info;
  } else if (isC99array){
    const ArrayType* aty = ty->getAsArrayTypeUnsafe();
    QualType ety = aty->getElementType();
    //if the element type of the array is itself a constant array
    ArrayInfo* info = new ArrayInfo;
    if (ety->isConstantArrayType()){
      //something of the form type x[][M]
      //we need to first: typedef type tx[][M];
      //replacement global will be tx* xRepl = &(sstmac_global_data + offset)
      info->typedefName = "type_" + D->getNameAsString();
      info->fqTypedefName = currentNs_->nsPrefix() + info->typedefName;
      std::stringstream sstr;
      cArrayConfig cfg;
      getArrayType(ety.getTypePtr(), cfg);
      if (isFxnPointerForm(cfg.fundamentalType)){
        arrayFxnPointerTypedef(D, info, sstr);
      } else {
        if (cfg.fundamentalTypeString == "anon"){
          errorAbort(D, "anonymous struct used in array declaration - cannot refactor");
        }
        sstr << "typedef " << cfg.fundamentalTypeString
             << " " << info->typedefName
             << "[]"  //don't do this anymore
             << cfg.arrayIndices.str();
      }
      info->typedefDeclString = sstr.str();
      info->retType = info->fqTypedefName + "*";
      info->needsDeref = false;
    } else {
      //something of the form type x[]
      //replacement global will type* xRepl;
      info->typedefName = "type_" + D->getNameAsString();
      info->fqTypedefName = currentNs_->nsPrefix() + info->typedefName;
      std::stringstream sstr;
      QualType ety = aty->getElementType();
      if (isFxnPointerForm(ety)){
        arrayFxnPointerTypedef(D, info, sstr);
      } else {
        sstr << "typedef " << GetAsString(ety)
             << " " << info->typedefName << "[]";
      }
      info->typedefDeclString = sstr.str();
      info->retType = info->fqTypedefName + "*";
      info->needsDeref = false;
      info->implicitSize = true;
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
    if (getStart(recDecl) == getStart(D)){
      //oh, well, they are both from the same spot
      //so, yeah, combined c-style var and struct decl
      return recDecl;
    }
  }
  return nullptr;
}

SkeletonASTVisitor::AnonRecord*
SkeletonASTVisitor::checkAnonStruct(VarDecl* D)
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
    if (recDecl->getNameAsString() == ""){// && getStart(recDecl) == getStart(D)){
      //actually anonymous - no name given to it
      AnonRecord* rec = new AnonRecord;
      rec->decl = recDecl;
      rec->structType = prefix;
      rec->typeName = D->getNameAsString() + "_anonymous_type";
      rec->retType = rec->structType + " "  + rec->typeName + "*";
      return rec;
    }
  }
  return nullptr;
}

CXXConstructExpr*
SkeletonASTVisitor::getCtor(VarDecl *vd)
{
  clang::Expr* expr = vd->getInit();
  while (1 && expr) {
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
  return nullptr;
}

bool
SkeletonASTVisitor::TraverseLambdaExpr(LambdaExpr* expr)
{
  if (activeFxnParams_.empty()){
    return doTraverseLambda(expr);
  } else {
    //lambdas are a glorious pain - they might be a function param
    //we don't want to the compound stmt lambda body to think we are still traversing
    //a function param, though
    PushGuard<ParmVarDecl*> pg(activeFxnParams_, nullptr);
    return doTraverseLambda(expr);
  }
}

bool
SkeletonASTVisitor::doTraverseLambda(LambdaExpr* expr)
{
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
              if (next->getNumArgs() > 0){
                needed = next->getArg(0);
              } else {
                cont = false;
              }
              break;
            }
            case Stmt::ImplicitCastExprClass: {
              ImplicitCastExpr* next = cast<ImplicitCastExpr>(needed);
              needed = next->getSubExpr();
              break;
            }
            case Stmt::CStyleCastExprClass: {
              CStyleCastExpr* next = cast<CStyleCastExpr>(needed);
              needed = next->getSubExpr();
              break;
            }
           case Stmt::UnaryOperatorClass: {
             UnaryOperator* next = cast<UnaryOperator>(needed);
             needed = next->getSubExpr();
             break;
           }
           case Stmt::CXXDependentScopeMemberExprClass: {
              CXXDependentScopeMemberExpr* next = cast<CXXDependentScopeMemberExpr>(needed);
              needed = next->getBase();
              break;
            }
            case Stmt::CXXNewExprClass: {
              //not a global variable - this got operator newed
              cont = false;
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
              errorAbort(vd, error);
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
    case LCD_ByRef: {
      return RecursiveASTVisitor<SkeletonASTVisitor>::TraverseLambdaExpr(expr);
    }
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
    Lexer::getRawToken(newLoc, res, CompilerGlobals::SM(), CompilerGlobals::CI().getLangOpts());
    if (res.getKind() == tok::semi){
      return newLoc.getLocWithOffset(1);
    } else {
      //JJW 1/22/2018
      //Need to change it to do this - D->getEndLoc() is incorrect for string literal inits
      SourceLocation next = Lexer::getLocForEndOfToken(newLoc, 1, CompilerGlobals::SM(), Printing::langOpts);
      if (next == newLoc){
        newLoc = newLoc.getLocWithOffset(1);
      } else {
        newLoc = next;
      }
    }
    ++numTries;
  }
  errorAbort(searchStart, "unable to locate end of variable declaration");
  return SourceLocation();
}

void
SkeletonASTVisitor::deleteStmt(Stmt *s)
{
  //go straight to replace, don't delay this
  ::replace(s, "");
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
      internalError(nd, pp.os.str());
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
  auto pos = name.find("struct ");
  if (pos == 0){
    return name.substr(7);
  }

  pos = name.find("class ");
  if (pos == 0){
    return name.substr(6);
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
  SourceLocation loc = getStart(D);
  int numTries = 0;
  while (numTries < 100000){
    Token res;
    Lexer::getRawToken(loc, res, CompilerGlobals::SM(), CompilerGlobals::CI().getLangOpts());
    std::string next;
    if (res.getKind() == tok::identifier){
      next = res.getIdentifierInfo()->getName().str();
    } else if (res.getKind() == tok::raw_identifier) {
      next = res.getRawIdentifier().str();
    }
    if (!next.empty() && next == D->getNameAsString()){
      return Lexer::getLocForEndOfToken(loc, 0, CompilerGlobals::SM(), Printing::langOpts);
    }
    //super annoying that I have to do this
    SourceLocation nextLoc = Lexer::getLocForEndOfToken(loc, 1, CompilerGlobals::SM(), Printing::langOpts);
    if (nextLoc == loc){
      loc = loc.getLocWithOffset(1);
    } else {
      loc = nextLoc;
    }
    ++numTries;
  }
  internalError(getStart(D), "unable to locate variable name");
  return SourceLocation();
}

bool
SkeletonASTVisitor::TraverseUnresolvedLookupExpr(clang::UnresolvedLookupExpr* expr,
                                                 DataRecursionQueue*  /*queue*/)
{
  for (auto iter=expr->decls_begin(); iter != expr->decls_end(); ++iter){
    NamedDecl* nd = *iter;
    if (nd->getKind() == Decl::VarTemplate){
      VarTemplateDecl* vtd = cast<VarTemplateDecl>(nd);
      VarDecl* vd = vtd->getCanonicalDecl()->getTemplatedDecl();
      if (variableTemplates_.find(vd) != variableTemplates_.end()){
        CompilerGlobals::rewriter.InsertText(getEnd(expr).getLocWithOffset(1), "()", false);
        return true;
      }
    }
  }
  return RecursiveASTVisitor<SkeletonASTVisitor>::TraverseUnresolvedLookupExpr(expr);
}
bool
SkeletonASTVisitor::TraverseVarTemplateDecl(VarTemplateDecl* D)
{
  try {
  PragmaActivateGuard pag(D, this);
  if (pag.skipVisit()) return true;

  if (D->getTemplatedDecl()->isConstexpr()){
    return true;
  } else {
    RecursiveASTVisitor<SkeletonASTVisitor>::TraverseVarTemplateDecl(D);
  }

  } catch (DeclDeleteException& e){
    if (e.deleted != D) throw e;
  }
  return true;
}

void
SkeletonASTVisitor::finalizePass()
{
  for (auto& pair : declsToInsertAfter_){
    auto& declPair = pair.second;

    clang::VarDecl* D = declPair.first;

    SourceLocation declEnd = getEndLoc(getEnd(D));
    CompilerGlobals::rewriter.InsertText(declEnd, declPair.second);

    /**
    bool hasTypeDecl = false;
    auto ty = D->getType().getTypePtr();
    if (ty->isStructureType()){
      RecordDecl* recDecl = ty->getAsStructureType()->getDecl();
      if (getStart(recDecl) == getStart(D)){
        //oh, well, they are both from the same spot
        //so, yeah, combined c-style var and struct decl
        //we are not allowed to replace type declarations
        //only variable declarations
        SourceLocation endRecord = recDecl->getLocEnd();
        SourceLocation endDecl = D->getLocEnd();
        SourceRange rng{endRecord, endDecl};
        //this nukes the closing bracket and semi-colon, annoyingly
        replace(rng, "}; " + declPair.second);
        hasTypeDecl = true;
      }
    }
    */

    //if (!hasTypeDecl){
    //  replace(declPair.first, declPair.second);
    //}

  }
}

bool
SkeletonASTVisitor::TraverseVarDecl(VarDecl* D)
{
  try {

  PragmaActivateGuard pag(D, this);
  if (pag.skipVisit()) return true;

  if (CompilerGlobals::pragmaConfig.makeNoChanges){
    CompilerGlobals::pragmaConfig.makeNoChanges = false;
    return true;
  }

  if (D->getDescribedVarTemplate()){
    if (D->isStaticDataMember()){
      internalError(D, "Do not yet support static members of described var template");
      /**
      const CXXRecordDecl* crd = cast<const CXXRecordDecl>(D->getDeclContext());
      VarTemplateDecl* vtd = D->getDescribedVarTemplate();
      TemplateParameterList* theList = vtd->getTemplateParameters();
      TemplateParameterList* clsList = nullptr;

      std::string clsName = crd->getNameAsString();
      if (crd->isDependentContext()){ //class is also a template
        ClassTemplateDecl* decl = crd->getDescribedClassTemplate();
        if (!decl){
          internalError(crd, *ci_, "template class has no template lists");
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
     */
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
      errorAbort(D, "null_ptr can only be applied to pointer");
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

  //memoization should do no refactoring of global variables
  if (CompilerGlobals::modeActive(MEMOIZE))
    return false;

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

  SourceManager &SM = CompilerGlobals::SM();
  std::string sourceFile = SM.getFileEntryForID(SM.getMainFileID())->getName().str();
  std::string suffix2 = sourceFile.substr(sourceFile.size()-2,2);
  bool isC = suffix2 == ".c";

  foundCMain_ = true;

  if (mainName_.empty()){
    //we need to select a unique name for the application
    //lets just do this as the source file and the date

    timeval t_st;
    gettimeofday(&t_st, 0);
    std::stringstream sstr;
    sstr << sourceFile << "_" << t_st.tv_usec;
    mainName_ = makeCxxName(sstr.str());
  }

  std::stringstream sstr;
  if (!isC) sstr << "extern \"C\" ";
  sstr << "int sstmac_user_main_" << mainName_ << "(";
  if (mainFxn->getNumParams() == 2){
    sstr << "int " << mainFxn->getParamDecl(0)->getNameAsString()
         << ", char** " << mainFxn->getParamDecl(1)->getNameAsString();
  }
  sstr << "){";

  SourceRange rng(getStart(mainFxn), getStart(mainFxn->getBody()));
  replace(rng, sstr.str());
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
    if (needGlobalData){
      sstr << "char* sstmac_global_data = get_sstmac_global_data();";
      //sstr << "printf(\"Globals=%p\\n\", sstmac_global_data);";
    }
    if (needTlsData){
      sstr << "char* sstmac_tls_data = get_sstmac_tls_data();";
    }
    for (auto d : currentGlobals){
      GlobalStandin& gs = globalStandins_[d];
      if (!gs.fxnStatic){
        sstr << gs.replText << "; ";
      }
    }
    insertBefore(body, sstr.str());
    insertAfter(body, " }");
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
  if (!D->isThisDeclarationADefinition()){
    return true;
  }
  if (D->isMain() && CompilerGlobals::refactorMain){
    replaceMain(D);
  } else if (D->isTemplateInstantiation()   
           || !D->isThisDeclarationADefinition()){
    return true;
  }

  if (D->hasAttrs()){
    for (Attr* attr : D->getAttrs()){
      if (attr->getKind() == attr::AlwaysInline){
        if (!D->isInlined()){
          SourceLocation start = D->getReturnTypeSourceRange().getBegin();
          if (start.isValid()){
            CompilerGlobals::rewriter.InsertText(start, " inline ", false);
          }
        }
      }
    }
  }


  if (D->isLocalExternDecl()){
    //weird extern declaration - skip it
    return true;
  }

  try {
    PushGuard<FunctionDecl*> pg(CompilerGlobals::astContextLists.enclosingFunctionDecls, D);
    PragmaActivateGuard pag(D, this, D->isThisDeclarationADefinition());
    if (!pag.skipVisit() && D->getBody()){
      traverseFunctionBody(D->getBody());
    }
  } catch (StmtDeleteException& e) {
    if (e.deleted != D->getBody()) throw e;
  } catch (DeclDeleteException& e) {
    if (e.deleted != D) throw e;
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
SkeletonASTVisitor::TraverseCXXDestructorDecl(CXXDestructorDecl* D)
{
  PushGuard<FunctionDecl*> pg(CompilerGlobals::astContextLists.enclosingFunctionDecls, D);
  Parent::TraverseCXXDestructorDecl(D);
  return true;
}

bool
SkeletonASTVisitor::TraverseCXXConstructorDecl(CXXConstructorDecl *D)
{
  if (D->isTemplateInstantiation())
    return true;

  IncrementGuard ig(insideCxxMethod_);
  PushGuard<CXXConstructorDecl*> pg(ctorContexts_, D);
  PushGuard<FunctionDecl*> pgf(CompilerGlobals::astContextLists.enclosingFunctionDecls, D);
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

  try {
    PragmaActivateGuard pag(D, this);
    if (D->isThisDeclarationADefinition() && !pag.skipVisit()) {
      IncrementGuard ig(insideCxxMethod_);
      PushGuard<FunctionDecl*> pg(CompilerGlobals::astContextLists.enclosingFunctionDecls, D);
      traverseFunctionBody(D->getBody());
    }
  } catch (DeclDeleteException& e) {
    if (e.deleted != D) throw e;
  }

  return true;
}

bool
SkeletonASTVisitor::VisitDependentScopeDeclRefExpr(DependentScopeDeclRefExpr* expr)
{
  if (!CompilerGlobals::astNodeMetadata.dependentScopeGlobal.empty()){
    //we have been told about a global variable that cannot be recongized
    //because it is a dependent scope expression
    std::string memberName = expr->getDeclName().getAsString();
    if (memberName == CompilerGlobals::astNodeMetadata.dependentScopeGlobal){
      auto iter = dependentStaticMembers_.find(memberName);
      if (iter == dependentStaticMembers_.end()){
        std::string warning = "variable " + memberName
            + " tagged as global, but there are no know globals with that name";
        warn(getStart(expr), warning);
      }
      std::string repl = appendText(expr, "_getter()");
      ::replace(expr, repl);
      CompilerGlobals::astNodeMetadata.dependentScopeGlobal.clear();
      return true; //skip checks below
      //std::string error = "pragma gloçbal name " + pragmaConfig_.dependentScopeGlobal
      //    + " does not match found member name " + memberName;
      //errorAbort(expr, *ci_, error);
    }
  }

  auto iter = dependentStaticMembers_.find(expr->getDeclName().getAsString());
  if (iter != dependentStaticMembers_.end()){
    clang::VarDecl* vd = iter->second;
    std::string warning = "member " + vd->getNameAsString()
        + " used in dependent scope matches global variable at "
        + getStartLocString(vd)
        + ". I can't tell if this is a global variable. If it is "
        "please let me know by using #pragma sst global " + vd->getNameAsString();
    warn(expr, warning);
  }


  return true;
}

bool
SkeletonASTVisitor::VisitCXXDependentScopeMemberExpr(clang::CXXDependentScopeMemberExpr* expr)
{
  if (!CompilerGlobals::astNodeMetadata.dependentScopeGlobal.empty()){
    //we have been told about a global variable that cannot be recongized
    //because it is a dependent scope expression
    std::string memberName = expr->getMember().getAsString();
    if (memberName == CompilerGlobals::astNodeMetadata.dependentScopeGlobal){
      auto iter = dependentStaticMembers_.find(memberName);
      if (iter == dependentStaticMembers_.end()){
        std::string warning = "variable " + memberName
            + " tagged as global, but there are no know globals with that name";
        warn(getStart(expr), warning);
      }
      std::string repl = appendText(expr, "_getter()");
      ::replace(expr, repl);
      CompilerGlobals::astNodeMetadata.dependentScopeGlobal.clear();
      return true; //skip checks below
      //std::string error = "pragma gloçbal name " + pragmaConfig_.dependentScopeGlobal
      //    + " does not match found member name " + memberName;
      //errorAbort(expr, *ci_, error);
    }
  }

  auto iter = dependentStaticMembers_.find(expr->getMember().getAsString());
  if (iter != dependentStaticMembers_.end()){
    clang::VarDecl* vd = iter->second;
    std::string warning = "member " + vd->getNameAsString()
        + " used in dependent scope matches global variable at "
        + getStartLocString(vd)
        + ". I can't tell if this is a global variable. If it is "
        "please let me know by using #pragma sst global " + vd->getNameAsString();
    warn(getStart(expr), warning);
  }


  return true;
}

bool
SkeletonASTVisitor::TraverseCompoundStmt(CompoundStmt* stmt, DataRecursionQueue*  /*queue*/)
{
  try {
    PushGuard<CompoundStmt*> pg(CompilerGlobals::astContextLists.compoundStmtBlocks, stmt);
    PragmaActivateGuard pag(stmt, this);
    if (!pag.skipVisit()){
      auto end = stmt->body_end();
      for (auto iter=stmt->body_begin(); iter != end; ++iter){
        Stmt* s = *iter;
        try {
          TraverseStmt(s);
        } catch (StmtDeleteException& e) {
          //this statement has been blown up by a pragma
          //remove it from future consideration
          *iter = zeroExpr(getStart(s));
        }
      }
    }
  } catch (StmtDeleteException& e) {
    if (e.deleted != stmt) throw e;
  }

  return true;
}

bool
SkeletonASTVisitor::TraverseFieldDecl(clang::FieldDecl* fd, DataRecursionQueue*  /*queue*/)
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

bool
SkeletonASTVisitor::TraverseUnaryOperator(UnaryOperator* op, DataRecursionQueue*  /*queue*/)
{
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
SkeletonASTVisitor::TraverseBinaryOperator(BinaryOperator* op, DataRecursionQueue*  /*queue*/)
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
SkeletonASTVisitor::TraverseIfStmt(IfStmt* stmt, DataRecursionQueue*  /*queue*/)
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
SkeletonASTVisitor::TraverseDeclStmt(DeclStmt* stmt, DataRecursionQueue*  /*queue*/)
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
SkeletonASTVisitor::TraverseDoStmt(DoStmt* S, DataRecursionQueue*  /*queue*/)
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
SkeletonASTVisitor::TraverseWhileStmt(WhileStmt* S, DataRecursionQueue*  /*queue*/)
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
SkeletonASTVisitor::TraverseForStmt(ForStmt *S, DataRecursionQueue*  /*queue*/)
{
  if (CompilerGlobals::astContextLists.enclosingFunctionDecls.empty()){
    errorAbort(S, "Traversing ForStmt with no function context");
  }

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
SkeletonASTVisitor::TraverseArraySubscriptExpr(ArraySubscriptExpr* expr, DataRecursionQueue*  /*queue*/)
{
  PushGuard<Expr*> pg(activeDerefs_, expr);
  TraverseStmt(expr->getBase());
  TraverseStmt(expr->getIdx());
  return true;
}

bool
SkeletonASTVisitor::VisitStmt(Stmt *S)
{
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
      errorAbort(D, "structure type did not return a record declaration");
    }
    typedefStructs_[str_ty->getDecl()] = D;
  } else if (ty->isUnionType()){
    auto un_ty = ty->getAsUnionType();
    if (!un_ty){
      errorAbort(D, "union type did not return a record declaration");
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

    return RecursiveASTVisitor<SkeletonASTVisitor>::TraverseDecl(D);
  } catch (DeclDeleteException& e) {
    if (e.deleted != D) throw e;
  }
  return true;
}

PragmaActivateGuard::~PragmaActivateGuard()
{
  for (SSTPragma* prg : myPragmas_){
    prg->deactivate();
  }
}

void
PragmaActivateGuard::deletePragmaText(SSTPragma *prg)
{
  //eliminate the pragma text
  SourceRange rng(prg->pragmaDirectiveLoc, prg->endPragmaLoc);
  ::replace(rng, "");
}

void
SkeletonASTVisitor::propagateNullness(Decl* target, Decl* src)
{
  //yep, it does
  if (target->getKind() != Decl::Var){
    errorAbort(target, "propagate nullness to declaration that isn't a variable");
  }
  VarDecl* vd = cast<VarDecl>(target);
  //VarDecl* svd = cast<VarDecl>(src);
  //propagate the null-ness to this new variable
  SSTNullVariablePragma* oldPragma = getNullVariable(src);
  SSTNullVariablePragma*& existing = CompilerGlobals::astNodeMetadata.nullVariables[vd];
  if (!existing){
    existing = oldPragma->clone();
    existing->setTransitive(oldPragma);
  }
}

void
SkeletonASTVisitor::nullifyIfStmt(IfStmt* if_stmt, Decl*  /*d*/)
{
  //oooooh, not good - I could really foobar things here
  //crash and burn and tell programmer to fix it
  warn(if_stmt, "null variables used as predicate in if-statement - "
       "this could produce undefined behavior - forcing always false");
  IfStmt* ifs = cast<IfStmt>(if_stmt);
  //force the replacement
  ::replace(ifs->getCond(), "0");
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
  ::replace(toRepl->getSourceRange(), repl);
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
#if CLANG_VERSION_MAJOR < 10
      e = mte->GetTemporaryExpr();
#else
      e = mte->getSubExpr();
#endif
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
        replace(replRng, repl.inlineUseText);
        return;
      }

      globalsTouched_.back().insert(md);
      std::string replText = repl.append ? appendText(expr, repl.inlineUseText) : repl.reusableText;
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
      CompilerGlobals::rewriter.InsertText(getEnd(expr).getLocWithOffset(1), "()", false);
    } else if (vtsd->isStaticDataMember()) {
      internalError(expr, "failed replacing static template member");
    }
    break; //proceed
  }
  default:
    break; //nooo!
  }
}

bool
SkeletonASTVisitor::ReplaceGlobalsPrinterHelper::handledStmt(Stmt *E, llvm::raw_ostream &OS)
{
  if (E->getStmtClass() == Stmt::DeclRefExprClass){
    DeclRefExpr* dref = cast<DeclRefExpr>(E);
    ValueDecl* vd = dref->getDecl();
    if (vd->getKind() == Decl::Var){
      return parent_->maybePrintGlobalReplacement(cast<VarDecl>(vd), OS);
    } else {
      return false;
    }
  } else {
    return false;
  }
}

std::string
SkeletonASTVisitor::printWithGlobalsReplaced(Stmt *stmt)
{
  std::string baseStr;
  llvm::raw_string_ostream os(baseStr);
  clang::LangOptions langOpts;
  langOpts.CPlusPlus = true;
  clang::PrintingPolicy policy(langOpts);
  ReplaceGlobalsPrinterHelper helper(this);
  stmt->printPretty(os, &helper, policy);
  return os.str();
}

bool
SkeletonASTVisitor::maybePrintGlobalReplacement(VarDecl* vd, llvm::raw_ostream& OS)
{
  const Decl* md = getOriginalDeclaration(vd);
  auto iter = globals_.find(md);
  if (iter != globals_.end()){
    GlobalReplacement& repl = iter->second;
    OS << repl.inlineUseText;
    return true;
  } else {
    return false;
  }
}


bool
FirstPassASTVisitor::VisitDecl(Decl *d)
{
  PragmaActivateGuard pag(d, this, true/*always do first pass pragmas*/);
  return true;
}

bool
FirstPassASTVisitor::VisitStmt(Stmt *s)
{
  PragmaActivateGuard pag(s, this, true/*always do first pass pragmas*/);
  return true;
}

bool
FirstPassASTVisitor::TraverseFunctionDecl(FunctionDecl *fd, DataRecursionQueue* /*queue*/)
{
  PushGuard<FunctionDecl*> pg(CompilerGlobals::astContextLists.enclosingFunctionDecls, fd);
  PragmaActivateGuard pag(fd, this, true/*always do first pass pragmas*/);
  if (fd->isThisDeclarationADefinition()){
    return Parent::TraverseFunctionDecl(fd);
  } else {
    return true;
  }
}

bool
FirstPassASTVisitor::TraverseCompoundStmt(CompoundStmt* cs, DataRecursionQueue* /*queue*/)
{
  PushGuard<CompoundStmt*> pg(CompilerGlobals::astContextLists.compoundStmtBlocks, cs);
  return Parent::TraverseCompoundStmt(cs);
}

FirstPassASTVisitor::FirstPassASTVisitor(SSTPragmaList& prgs) :
  pragmas_(prgs)
{
}
