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
#include "computeVisitor.h"
#include "computePragma.h"
#include "replacePragma.h"
#include "recurseAll.h"
#include "printAll.h"
#include "validateScope.h"
#include "astVisitor.h"

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

struct GlobalVarReplacer
{
  bool hasReplacement(const DeclRefExpr* expr){
    return visitor->isGlobal(const_cast<DeclRefExpr*>(expr));
  }
  std::string getReplacement(const DeclRefExpr* expr){
    return visitor->needGlobalReplacement(const_cast<NamedDecl*>(expr->getFoundDecl()));
  }

  template <class T>
  bool hasReplacement(T* t){
    return false;
  }

  template <class T>
  std::string getReplacement(T* t){
    return "";
  }

  SkeletonASTVisitor* visitor;
};

void
ComputeVisitor::visitAccessDeclRefExpr(DeclRefExpr* expr, MemoryLocation& mloc)
{
  NamedDecl* decl = expr->getFoundDecl();
  Variable& var = variables[decl];
  mloc.append(var.id);
  mloc.updateGeneration(var.generation);
  //std::cout << "access depends on variable " << (char) var.id
  //          << " at generation " << var.generation
  //          << " -> " << mloc.maxGen << std::endl;
}

void
ComputeVisitor::visitAccessCompoundStmt(CompoundStmt* stmt, Loop::Body& body, MemoryLocation& mloc)
{
  errorAbort(stmt->getLocStart(), CI, "visiting compound statement in data flow analysis");
}

void
ComputeVisitor::visitAccessBinaryOperator(BinaryOperator* op, Loop::Body& body, MemoryLocation& mloc)
{
  checkMemoryAccess(op->getLHS(), body, mloc);
  mloc.append(op->getOpcode());
  checkMemoryAccess(op->getRHS(), body, mloc);
}

void
ComputeVisitor::visitAccessParenExpr(ParenExpr* expr, Loop::Body& body, MemoryLocation& mloc)
{
  Expr* subExpr = expr->getSubExpr();
  bool needParens = true;
  switch(subExpr->getStmtClass()){
    case Stmt::ParenExprClass:
    case Stmt::DeclRefExprClass:
    case Stmt::ArraySubscriptExprClass:
      needParens = false;
      break;
    default:
      break;
  }

  if (needParens) mloc.append('(');
  checkMemoryAccess(subExpr, body, mloc);
  if (needParens) mloc.append(')');
}

void
ComputeVisitor::visitAccessImplicitCastExpr(ImplicitCastExpr* expr, Loop::Body& body, MemoryLocation& mloc)
{
  checkMemoryAccess(expr->getSubExpr(), body, mloc);
}

void
ComputeVisitor::visitAccessCStyleCastExpr(CStyleCastExpr* expr, Loop::Body& body, MemoryLocation& mloc)
{
  checkMemoryAccess(expr->getSubExpr(), body, mloc);
}

void
ComputeVisitor::visitAccessArraySubscriptExpr(
  ArraySubscriptExpr* expr, 
  Loop::Body& body, MemoryLocation& mloc,
  bool updateDependence)
{
  MemoryLocation subLoc;
  checkMemoryAccess(expr->getBase(),body,subLoc); //not a lhs
  subLoc.append('[');
  checkMemoryAccess(expr->getIdx(),body,subLoc); //not a lhs
  subLoc.append(']');
  AccessHistory& access = arrays[subLoc];
  if (updateDependence){
    mloc.updateGeneration(access.lastWriteGeneration);
    //std::cout << "access depends on " << subLoc.c_str() << " at generation "
    //          << access.lastWriteGeneration << "->" << mloc.maxGen << std::endl;
  }
  mloc.append(subLoc);
}

void
ComputeVisitor::checkMemoryAccess(clang::Stmt* stmt, Loop::Body& body, MemoryLocation& mloc)
{
#define access_case(type,stmt,...) \
  case(clang::Stmt::type##Class): \
    visitAccess##type(clang::cast<type>(stmt),__VA_ARGS__); break
  switch(stmt->getStmtClass()){
    access_case(DeclRefExpr,stmt,mloc);
    access_case(BinaryOperator,stmt,body,mloc);
    access_case(ParenExpr,stmt,body,mloc);
    access_case(ArraySubscriptExpr,stmt,body,mloc);
    access_case(ImplicitCastExpr,stmt,body,mloc);
    access_case(CStyleCastExpr,stmt,body,mloc);
    access_case(CompoundStmt,stmt,body,mloc);
    default:
      break;
  }
#undef access_case
}

void
ComputeVisitor::visitLoop(ForStmt* stmt, Loop& loop)
{
  ForLoopSpec spec;
  auto iter = explicitLoopCounts_.find(stmt);
  if (iter != explicitLoopCounts_.end()){
    //the number of loop iterations is given explicitly
    spec.init = nullptr;
    spec.predicateMax = nullptr;
    spec.stride = nullptr;
    loop.tripCount = iter->second;
  } else {
    getInitialVariables(stmt->getInit(), &spec);
    getPredicateVariables(stmt->getCond(), &spec);
    getStride(stmt->getInc(), &spec);
    //validate that the static analysis succeeded
    validateLoopControlExpr(spec.init);
    validateLoopControlExpr(spec.predicateMax);
    loop.tripCount = getTripCount(&spec);
  }

  //now lets examine the body
  addOperations(stmt->getBody(), loop.body);
}

void
ComputeVisitor::visitBodyForStmt(ForStmt* stmt, Loop::Body& body)
{
  checkStmtPragmas(stmt);
  body.subLoops.emplace_back(body.depth+1);
  visitLoop(stmt, body.subLoops.back());
}

void
ComputeVisitor::visitBodyWhileStmt(WhileStmt *stmt, Loop::Body &body)
{
  checkStmtPragmas(stmt);
  //treat this as a for loop with a certain trip count
  auto iter = explicitLoopCounts_.find(stmt);
  if (iter == explicitLoopCounts_.end()){
    errorAbort(stmt->getLocStart(), CI,
               "skeletonized while loop has unknown count - use pragma sst loop_count");
  }
  //the number of loop iterations is given explicitly
  body.subLoops.emplace_back(body.depth+1);
  auto& loop = body.subLoops.back();
  loop.tripCount = iter->second;
  addOperations(stmt->getBody(), loop.body);
}

void
ComputeVisitor::visitBodyCompoundStmt(CompoundStmt* stmt, Loop::Body& body)
{
  checkStmtPragmas(stmt);
  auto end = stmt->child_end();
  for (auto iter=stmt->child_begin(); iter != end; ++iter){
    addOperations(*iter, body);
  }
}

void
ComputeVisitor::visitBodyCompoundAssignOperator(CompoundAssignOperator* op, Loop::Body& body)
{
  visitBodyBinaryOperator(op, body);
}

void
ComputeVisitor::visitBodyUnaryOperator(UnaryOperator* op, Loop::Body& body)
{
  auto ty = op->getSubExpr()->getType();
  switch(op->getOpcode()){
  case UO_Deref:
    {
      if (ty->isPointerType()){
        auto pty = ty->getPointeeType();
        if (pty.isVolatileQualified() || ty.isVolatileQualified()){
          //treat this as a new memory access each time
          TypeInfo ti = CI.getASTContext().getTypeInfo(pty);
          body.readBytes += ti.Width / 8;
        }
      }
    }
    break;
  case UO_PostInc:
  case UO_PostDec:
  case UO_PreInc:
  case UO_PreDec:
    if (ty->isIntegerType()) {
      body.intops += 1;
    } else if (ty->isFloatingType()) {
      body.flops += 1;
    } else if (ty->isPointerType()){
      body.intops += 1;
    }
    break;
  default:
    break;
  }
  Expr* e = op->getSubExpr();
  addOperations(e, body);
}

void
ComputeVisitor::visitBodyBinaryOperator(BinaryOperator* op, Loop::Body& body)
{
  bool updateLHS = false;
  switch(op->getOpcode()){
    case BO_MulAssign:
    case BO_DivAssign:
    case BO_RemAssign:
    case BO_AddAssign:
    case BO_SubAssign:
    case BO_Assign:
      updateLHS = true; //need to update data flow
    case BO_Mul:
    case BO_Add:
    case BO_Div:
    case BO_Sub:
      if (op->getType()->isIntegerType()) {
        body.intops += 1;
      } else if (op->getType()->isFloatingType()) {
        body.flops += 1;
      } else if (op->getType()->isPointerType()){
        body.intops += 1;
      } else if (op->getType()->isDependentType()){
        //this better be a template type
        const Type* ty = op->getLHS()->getType().getTypePtr();
        if (!isa<const TemplateTypeParmType>(ty)){
          errorAbort(op->getLocStart(), CI,
                     "binary operator has non-template dependent type");
        }
      } else {
        errorAbort(op->getLocStart(), CI,
                   "binary operator in skeletonized loop does not operate on int or float types");
      }
    default:
      break;
  }
  addOperations(op->getLHS(), body, updateLHS);
  addOperations(op->getRHS(), body);
}

void
ComputeVisitor::visitBodyParenExpr(ParenExpr* expr, Loop::Body& body)
{
  addOperations(expr->getSubExpr(), body);
}

#define heisenbug fprintf(stderr, "%s:%d\n", __FILE__, __LINE__); fflush(stdout)

void
ComputeVisitor::visitBodyArraySubscriptExpr(ArraySubscriptExpr* expr, Loop::Body& body, bool isLHS)
{
  body.intops += 1; //pointer arithmetic
  //well, well - we might have a unique memory access
  addOperations(expr->getIdx(), body);
  addOperations(expr->getBase(), body);
  //we've added operations - see if this is a new memory access
  MemoryLocation mloc;
  visitAccessArraySubscriptExpr(expr, body, mloc, false); //do not update dependence
  //okay, now we have an array access to check
  AccessHistory& acc = arrays[mloc];
  /**
  std::cout << (isLHS ? "wrote " : "read ")
            << mloc.c_str() << " at rw=("
            << acc.lastReadGeneration << "," << acc.lastWriteGeneration
            << ") during gen " << currentGeneration
            << " with gen dep " << mloc.maxGen
            << " at " << expr->getLocStart().printToString(CI.getSourceManager())
            << std::endl; */
  bool newAccess = acc.newAccess(mloc.maxGen, currentGeneration, isLHS);
  if (newAccess){
    TypeInfo ti = CI.getASTContext().getTypeInfo(expr->getType());
    //std::cout << "sizeof(" << expr->getType()->getTypeClassName()
    //          << ")=" << ti.Width << std::endl;

    //fails data flow check OR first access
    if (isLHS) body.writeBytes += ti.Width / 8;
    else       body.readBytes += ti.Width / 8;
  }
}


void
ComputeVisitor::visitBodyDeclRefExpr(DeclRefExpr* expr, Loop::Body& body, bool isLHS)
{
  NamedDecl* decl = expr->getFoundDecl();
  Variable& var = getVariable(decl);
  if (isLHS) var.generation = currentGeneration;
}

void
ComputeVisitor::visitBodyImplicitCastExpr(ImplicitCastExpr* expr, Loop::Body& body, bool isLHS)
{
  addOperations(expr->getSubExpr(), body, isLHS);
}

void
ComputeVisitor::visitBodyCStyleCastExpr(CStyleCastExpr* expr, Loop::Body& body, bool isLHS)
{
  addOperations(expr->getSubExpr(), body, isLHS);
}

void
ComputeVisitor::visitBodyCallExpr(CallExpr* expr, Loop::Body& body)
{
  SourceLocation saveScope = scopeStartLine;
  //if we enter a call expr, we lose the entire scope
  scopeStartLine = SourceLocation();

  //the definition of this function better be available
  //procedure calls should NOT be INSIDE a compute intensive loop
  FunctionDecl* callee = expr->getDirectCallee();
  if (callee){
    if (callee->isTemplateInstantiation()){
      FunctionTemplateDecl* td = callee->getPrimaryTemplate();
      if (!callee->getBody()){
        std::string error = "template instance of function "
            + td->getNameAsString() + " not generated by compiler";
        errorAbort(expr->getLocStart(), CI, error);
      } else {
        //the new scope start is beginning of function
        scopeStartLine = callee->getLocStart();
        addOperations(callee->getBody(), body);
      }
    } else {
      FunctionDecl* def = callee->getDefinition();
      if (!def){
        std::stringstream sstr;
        sstr << "non-inlineable function '" << callee->getNameAsString()
             << "' inside compute-intensive code";
        warn(expr->getLocStart(), CI, sstr.str());
      } else {
        //the new scope start is beginning of function
        scopeStartLine = callee->getLocStart();
        addOperations(def->getBody(), body);
      }
    }
  } else {
    warn(expr->getLocStart(), CI,
               "non-inlinable function inside compute-intensive code");
  }
  //reset back to the parent scope
  scopeStartLine = saveScope;
}

void
ComputeVisitor::checkStmtPragmas(Stmt* s)
{
  auto matches = pragmas.getMatches(s);
  if (!matches.empty()){
    SSTPragma* prg = matches.front();
    switch (prg->cls){
    case SSTPragma::Replace: {
      SSTReplacePragma* rprg = static_cast<SSTReplacePragma*>(prg);
      std::list<const Expr*> replaced;
      rprg->run(s, replaced);
      for (auto e : replaced){
        repls.exprs[e] = rprg->replacement();
      }
    } break;
    case SSTPragma::LoopCount: {
      SSTLoopCountPragma* rprg = static_cast<SSTLoopCountPragma*>(prg);
      switch(s->getStmtClass()){
        case Stmt::ForStmtClass:
        case Stmt::WhileStmtClass:
          explicitLoopCounts_[s] = rprg->count();
          break;
        default:
          errorAbort(s->getLocStart(), CI, "pragma loop_count not applied to for loop");
          break;
      }
    } break;
    default:
      errorAbort(s->getLocStart(), CI, "invalid pragma applied to statement");
    }
  }
}

void
ComputeVisitor::visitBodyIfStmt(IfStmt *stmt, Loop::Body &body)
{
  auto matches = pragmas.getMatches(stmt);
  //by default, always assume an if statement is false
  if (matches.empty()){
    warn(stmt->getLocStart(), CI,
         "if-stmt inside compute block has no prediction hint - assuming always false");
    if (stmt->getElse()){
      addOperations(stmt->getElse(), body);
    }
    return;
  }

  if (matches.size() > 1){
    for (SSTPragma* prg : matches){
      prg->print();
    }
    errorAbort(stmt->getLocStart(), CI,
               "if-stmt inside compute block should have a single prediction hint,"
               "multiple pragmas found");
  }

  SSTPragma* prg = matches.front();
  if (prg->cls == SSTPragma::BranchPredict){
    SSTBranchPredictPragma* bprg = static_cast<SSTBranchPredictPragma*>(prg);
    if (bprg->prediction() == "true"){
      addOperations(stmt->getThen(), body);
    } else if (bprg->prediction() == "false"){
      if (stmt->getElse()){
        addOperations(stmt->getElse(), body);
      }
    } else {
      //we have a predict percentage in-between 0 and 1
      body.subLoops.emplace_back(body.depth+1);
      Loop& thenLoop = body.subLoops.back();
      thenLoop.tripCount = "1";
      thenLoop.body.branchPrediction = bprg->prediction();
      addOperations(stmt->getThen(), thenLoop.body);
      if (stmt->getElse()){
        body.subLoops.emplace_back(body.depth+1);
        Loop& elseLoop = body.subLoops.back();
        elseLoop.tripCount = "1";
        elseLoop.body.branchPrediction = "1-(" + bprg->prediction() + ")";
        addOperations(stmt->getElse(), elseLoop.body);
      }
    }
  } else {
    errorAbort(stmt->getLocStart(), CI,
               "only branch prediction pragmas should be applied to if-stmt");
  }
}

void
ComputeVisitor::visitBodySwitchStmt(SwitchStmt *stmt, Loop::Body &body)
{
  body.intops += 1; //for jump table
  body.readBytes += 8; //for jump table
  SwitchCase* sc = stmt->getSwitchCaseList();
  while (sc){
    addOperations(sc->getSubStmt(), body);
    sc = sc->getNextSwitchCase();
  }
}

void
ComputeVisitor::visitBodyDeclStmt(DeclStmt* stmt, Loop::Body& body)
{
  if (!stmt->isSingleDecl()){
    return;
  }
  Decl* d = stmt->getSingleDecl();
  if (d){
    auto matches = pragmas.getMatches(stmt);
    if (!matches.empty()){
     SSTPragma* prg = matches.front();
      if (prg->cls == SSTPragma::Replace){
        SSTReplacePragma* rprg = static_cast<SSTReplacePragma*>(prg);
        repls.decls[d] = rprg->replacement();
      }
    }
    if (isa<VarDecl>(d)){
      VarDecl* vd = cast<VarDecl>(d);
      if (vd->hasInit()){
        addOperations(vd->getInit(), body);
      }
    }
  }
}

void
ComputeVisitor::addOperations(Stmt* stmt, Loop::Body& body, bool isLHS)
{
#define body_case(type,stmt,...) \
  case(clang::Stmt::type##Class): \
    visitBody##type(clang::cast<type>(stmt),__VA_ARGS__); break
  currentGeneration++;
  switch(stmt->getStmtClass()){
    body_case(ForStmt,stmt,body);
    body_case(CompoundStmt,stmt,body);
    body_case(CompoundAssignOperator,stmt,body);
    body_case(BinaryOperator,stmt,body);
    body_case(UnaryOperator,stmt,body);
    body_case(ParenExpr,stmt,body);
    body_case(CallExpr,stmt,body);
    body_case(DeclRefExpr,stmt,body,isLHS);
    body_case(ImplicitCastExpr,stmt,body,isLHS);
    body_case(CStyleCastExpr,stmt,body,isLHS);
    body_case(ArraySubscriptExpr,stmt,body,isLHS);
    body_case(DeclStmt,stmt,body);
    body_case(IfStmt,stmt,body);
    body_case(SwitchStmt,stmt,body);
    body_case(WhileStmt,stmt,body);
    default:
      break;
  }
#undef body_case
}

void
ComputeVisitor::visitStrideCompoundStmt(clang::CompoundStmt* stmt, ForLoopSpec* spec)
{
  getStride(stmt->body_front(),spec);
}

void
ComputeVisitor::visitStrideCompoundAssignOperator(clang::CompoundAssignOperator* op, ForLoopSpec* spec)
{
  spec->stride = op->getRHS();
}

void
ComputeVisitor::visitStrideUnaryOperator(clang::UnaryOperator* op, ForLoopSpec* spec)
{
  switch(op->getOpcode()){
    case UO_PostInc:
    case UO_PreInc:
      spec->stride = nullptr;
      spec->increment = true;
      break;
    case UO_PostDec:
    case UO_PreDec:
      spec->stride = nullptr;
      spec->increment = false;
      break;
    default:
      errorAbort(op->getLocStart(), CI, "got unary operator that's not a ++/-- increment");
  }
}

#define for_case(feature,type,stmt,spec) \
  case(clang::Stmt::type##Class): \
    visit##feature##type(clang::cast<type>(stmt),spec); break

void
ComputeVisitor::getStride(Stmt* stmt, ForLoopSpec* spec)
{
  switch(stmt->getStmtClass()){
    for_case(Stride,CompoundStmt,stmt,spec);
    for_case(Stride,CompoundAssignOperator,stmt,spec);
    for_case(Stride,UnaryOperator,stmt,spec);
    default:
      errorAbort(stmt->getLocStart(), CI, "bad incrementer expression for skeletonization");
  }
}

void
ComputeVisitor::getPredicateVariables(Expr* expr, ForLoopSpec* spec)
{
  switch(expr->getStmtClass()){
    for_case(Predicate,BinaryOperator,expr,spec);
    default:
      errorAbort(expr->getLocStart(), CI, "skeletonized predicates mut be basic binary ops");
  }
}

void
ComputeVisitor::getInitialVariables(Stmt* stmt, ForLoopSpec* spec)
{
  switch(stmt->getStmtClass()){
    for_case(Initial,DeclStmt,stmt,spec);
    for_case(Initial,BinaryOperator,stmt,spec);
    default:
      errorAbort(stmt->getLocStart(), CI, "bad initial expression for skeletonization");
  }
}
#undef for_case

void
ComputeVisitor::validateLoopControlExpr(Expr* rhs)
{
  //we may have issues if the rhs references variables
  //that are scoped inside the loop
  VariableScope scope;
  scope.astContext = context;
  scope.start = context->getTopLevelScope()->getLocStart();
  //this is the START of the compute block
  //but also the END of the allowed scope for usable variables
  scope.stop = scopeStartLine;
  recurseAll<ValidateScope,NullVisit>(rhs, repls, CI, scope);
}

void
ComputeVisitor::visitPredicateBinaryOperator(BinaryOperator* op, ForLoopSpec* spec)
{
  Expr* rhs = op->getRHS();
  bool keep_going = true;
  while (keep_going){
    switch(rhs->getStmtClass()){
      case Stmt::ImplicitCastExprClass:
        rhs = cast<ImplicitCastExpr>(rhs)->getSubExpr();
        break;
      case Stmt::ParenExprClass:
        rhs = cast<ParenExpr>(rhs)->getSubExpr();
        break;
      default:
        keep_going = false;
        break;
    }
  }
  spec->predicateMax = rhs;
}

void
ComputeVisitor::visitInitialDeclStmt(clang::DeclStmt* stmt, ForLoopSpec* spec)
{
  if (!stmt->isSingleDecl()){
    errorAbort(stmt->getLocStart(), CI,
               "skeleton for loop initializer is not single declaration");
  }
  if (!isa<VarDecl>(stmt->getSingleDecl())){
    errorAbort(stmt->getLocStart(), CI,
               "declaration statement does not declare a variable");
  }
  VarDecl* var = cast<VarDecl>(stmt->getSingleDecl());
  spec->incrementer = var;
  spec->init = var->getInit();
}

void
ComputeVisitor::visitInitialBinaryOperator(clang::BinaryOperator* op, ForLoopSpec* spec)
{
  if (op->getLHS()->getStmtClass() != Stmt::DeclRefExprClass){
    errorAbort(op->getLocStart(), CI, "skeletonized loop initializer is not simple assignment");
  } else {
    DeclRefExpr* dre = cast<DeclRefExpr>(op->getLHS());
    spec->incrementer = dre->getFoundDecl();
  }
  spec->init = op->getRHS();
}


bool
ComputeVisitor::checkPredicateMax(Expr* max, std::ostream& os)
{
  auto iter = repls.exprs.find(max);
  if (iter != repls.exprs.end()){
    os << iter->second;
    return true;
  }

  //we might have overriden a declaration
  if (max->getStmtClass() == Stmt::DeclRefExprClass){
    DeclRefExpr* dref = cast<DeclRefExpr>(max);
    auto iter = repls.decls.find(dref->getDecl());
    if (iter != repls.decls.end()){
      os << iter->second;
      return true;
    }
  }

  return false;
}

std::string
ComputeVisitor::getTripCount(ForLoopSpec* spec)
{
  Expr* max = spec->increment ? spec->predicateMax : spec->init;
  Expr* min = spec->increment ? spec->init : spec->predicateMax;

  GlobalVarReplacer repl;
  repl.visitor = this->context;

  std::stringstream os;
  os << "((";
  if (max){
    bool specialReplace = checkPredicateMax(max,os);
    if (!specialReplace){
      printAll(max,os,repl);
    }
  } else {
    os << "0";
  }
  os << ")";
  if (min){
    os << "-";
    os << "(";
    printAll(min,os,repl);
    os << ")";
  }
  os << ")";
  if (spec->stride){
    os << "/ (";
    printAll(spec->stride,os,repl);
    os << ")";
  }
  return os.str();
}

void
ComputeVisitor::addLoopContribution(std::ostream& os, Loop& loop)
{
  os << "{ ";
  os << " uint64_t tripCount" << loop.body.depth << "=";
  if (loop.body.depth > 0){
    os << "tripCount" << (loop.body.depth-1) << "*";
    if (loop.body.hasBranchPrediction()){
      os << loop.body.branchPrediction << "*";
    }
  }
  os << "(" << loop.tripCount << "); ";
  if (loop.body.flops > 0){
      os << " flops += tripCount" << loop.body.depth << "*" << loop.body.flops << ";";
  }
  if (loop.body.readBytes > 0){
    os << " readBytes += tripCount" << loop.body.depth << "*" << loop.body.readBytes << ";";
  }
  if (loop.body.writeBytes > 0){
    os << " writeBytes += tripCount" << loop.body.depth << "*" << loop.body.writeBytes << ";";
  }
  if (loop.body.intops > 0){
      os << " intops += tripCount" << loop.body.depth << "*" << loop.body.intops << ";";
  }

#if 0
    printf("trip%d=%s,flops=%d,read=%d,write=%d,ops=%d\n",
         loop.body.depth, loop.tripCount.c_str(),
         loop.body.flops, loop.body.readBytes, loop.body.writeBytes, loop.body.intops);
#endif
  auto end = loop.body.subLoops.end();
  for (auto iter=loop.body.subLoops.begin(); iter != end; ++iter){
    addLoopContribution(os, *iter);
  }
  os << "}";
}

void
ComputeVisitor::setContext(Stmt* stmt){
  scopeStartLine = stmt->getLocStart();
}

void
ComputeVisitor::replaceStmt(Stmt* stmt, Rewriter& r, Loop& loop, PragmaConfig& cfg,
                            const std::string& nthread)
{
  std::stringstream sstr;
  sstr << "{ uint64_t flops=0; uint64_t readBytes=0; uint64_t writeBytes=0; uint64_t intops=0; ";
  addLoopContribution(sstr, loop);
  if (cfg.computeMemorySpec.size() != 0){
    //overwrite the static analysis
    sstr << "readBytes=" << cfg.computeMemorySpec << ";";
  }
  if (nthread.empty()){
    sstr << "sstmac_compute_detailed(flops,intops,readBytes); }";
  } else {
    sstr << "sstmac_compute_detailed_nthr(flops,intops,readBytes,"
         << nthread << "); }";
  }
  replace(stmt,r,sstr.str(),CI);
}

