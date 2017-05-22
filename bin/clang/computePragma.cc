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

#include "pragmas.h"
#include <sstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

#define dcase(type,d,rw,cfg) \
  case(clang::Decl::type): \
    visit##type##Decl(clang::cast<type##Decl>(d),rw,cfg); break

#define scase(type,s,rw) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s),rw); break

#define for_case(feature,type,stmt,spec) \
  case(clang::Stmt::type##Class): \
    visit##feature##type(clang::cast<type>(stmt),spec); break

#define body_case(type,stmt,...) \
  case(clang::Stmt::type##Class): \
    visitBody##type(clang::cast<type>(stmt),__VA_ARGS__); break

#define access_case(type,stmt,...) \
  case(clang::Stmt::type##Class): \
    visitAccess##type(clang::cast<type>(stmt),__VA_ARGS__); break



struct DataFlowMap {
  struct Access {
    uint32_t generation;
  };

  std::map<clang::NamedDecl*,uint32_t> generation;

  std::set<uint64_t> accessed;
  static void
  append(uint64_t& id, uint16_t modifier, uint16_t idx){
    id |= (modifier << idx);
  }
  uint16_t id_count;
  DataFlowMap() : id_count(0) {}
};


struct Loop {
  struct Body {
    int depth;
    int flops;
    int intops;
    int writeBytes;
    int readBytes;
    std::list<Loop> subLoops;
    Body() : flops(0), intops(0), readBytes(0), writeBytes(0) {}
  };

  std::string tripCount;
  Body body;
  Loop(int depth){
    body.depth = depth;
  }
};

struct ForLoopSpec
{
  clang::Expr* stride;
  clang::Expr* init;
  clang::Expr* predicateMax;
  clang::NamedDecl* incrementer;
};

struct MemoryLocation {
  char label[128];

  template <class T>
  void
  append(const T& t){
    //::memcpy(&label[size], &t, sizeof(T));
    T* arr = (T*) &label[size];
    *arr = t;
    size += sizeof(T);
  }

  const char* c_str() const {
    char* ret = new char[128];
    int writeIdx = 0;
    for (int i=0; i < size; ++i){
      if (label[i] != '\0'){
        ret[writeIdx++] = label[i];
      }
    }
    ret[writeIdx] = '\0';
    return ret;
  }

  void
  updateGeneration(uint32_t gen){
    maxGen = std::max(maxGen, gen);
  }

  void
  append(const MemoryLocation& mloc){
    ::memcpy(&label[size], mloc.label, mloc.size);
    size += mloc.size;
    maxGen = std::max(maxGen, mloc.maxGen);
  }

  uint32_t maxGen;
  uint8_t size;
  MemoryLocation() : maxGen(0), size(0) {}
};

struct MemoryLocationCompare {
  bool operator()(const MemoryLocation& lhs, const MemoryLocation& rhs) const {
    if (lhs.size != rhs.size) return lhs.size < rhs.size;
    int cmp = memcmp(lhs.label, rhs.label, lhs.size);
    return cmp < 0;
  }
};

struct Variable {
  uint16_t id;
  uint32_t generation;
};


struct AccessHistory {
  uint32_t lastReadGeneration;
  uint32_t lastWriteGeneration;

  AccessHistory() : lastReadGeneration(0), lastWriteGeneration(0)
  {
  }

  bool newAccess(uint32_t maxGenDependence, uint32_t currentGeneration, bool isLHS){
    if (isLHS){
      bool updated = lastWriteGeneration < maxGenDependence || lastWriteGeneration == 0;
      lastWriteGeneration = currentGeneration;
      return updated;
    } else {
      bool updated = lastReadGeneration < maxGenDependence || lastReadGeneration == 0;
      lastReadGeneration = currentGeneration;
      return updated;
    }
  }

};

struct ComputeVisitor  {

uint32_t idCount; //0 is sentinel for not inited
uint32_t currentGeneration; //0 is sentinel for not inited
std::map<MemoryLocation,AccessHistory,MemoryLocationCompare> arrays;
std::map<NamedDecl*,Variable> variables;
CompilerInstance& CI;
SSTPragmaList& pragmas;
SourceLocation scopeStartLine;
std::list<std::string> scopedVariables;
ComputeVisitor* parent;

//97 = 'a', for debug printing
ComputeVisitor(clang::CompilerInstance& c, SSTPragmaList& plist, ComputeVisitor* par) :
  CI(c), idCount(97), currentGeneration(1), pragmas(plist), parent(par)
{}

std::list<std::string>&
getScopedVariables(){
  if (parent){
    return parent->getScopedVariables();
  } else {
    return scopedVariables;
  }
}

Variable&
getVariable(NamedDecl* decl){
  Variable& var = variables[decl];
  if (var.id == 0){
    var.id = idCount++;
    //std::cout << "Assigned variable " << decl->getNameAsString()
    //          << " to internal " << (char) var.id << std::endl;
  }
  return var;
}

void
visitAccessDeclRefExpr(DeclRefExpr* expr, MemoryLocation& mloc)
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
visitAccessCompoundStmt(CompoundStmt* stmt, Loop::Body& body, MemoryLocation& mloc)
{
  errorAbort(stmt->getLocStart(), CI, "visiting compound statement in data flow analysis");
}

void
visitAccessBinaryOperator(BinaryOperator* op, Loop::Body& body, MemoryLocation& mloc)
{
  checkMemoryAccess(op->getLHS(), body, mloc);
  mloc.append(op->getOpcode());
  checkMemoryAccess(op->getRHS(), body, mloc);
}

void
visitAccessParenExpr(ParenExpr* expr, Loop::Body& body, MemoryLocation& mloc)
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
visitAccessImplicitCastExpr(ImplicitCastExpr* expr, Loop::Body& body, MemoryLocation& mloc)
{
  checkMemoryAccess(expr->getSubExpr(), body, mloc);
}

void
visitAccessCStyleCastExpr(CStyleCastExpr* expr, Loop::Body& body, MemoryLocation& mloc)
{
  checkMemoryAccess(expr->getSubExpr(), body, mloc);
}

void
visitAccessArraySubscriptExpr(ArraySubscriptExpr* expr, Loop::Body& body, MemoryLocation& mloc,
                              bool updateDependence = true)
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
checkMemoryAccess(clang::Stmt* stmt, Loop::Body& body, MemoryLocation& mloc)
{
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
}

void
visitLoop(ForStmt* stmt, Loop& loop)
{
  ForLoopSpec spec;
  getInitialVariables(stmt->getInit(), &spec);
  getPredicateVariables(stmt->getCond(), &spec);
  getStride(stmt->getInc(), &spec);
  loop.tripCount = getTripCount(&spec);

  //now lets examine the body
  addOperations(stmt->getBody(), loop.body);
}

void
visitBodyForStmt(ForStmt* stmt, Loop::Body& body)
{
  body.subLoops.emplace_back(body.depth+1);
  visitLoop(stmt, body.subLoops.back());
}

void
visitBodyCompoundStmt(CompoundStmt* stmt, Loop::Body& body)
{
  auto end = stmt->child_end();
  for (auto iter=stmt->child_begin(); iter != end; ++iter){
    addOperations(*iter, body);
  }
}

void
visitBodyCompoundAssignOperator(CompoundAssignOperator* op, Loop::Body& body)
{
  visitBodyBinaryOperator(op, body);
}

void
visitBodyBinaryOperator(BinaryOperator* op, Loop::Body& body)
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
      }
      else if (op->getType()->isFloatingType()) {
        body.flops += 1;
      }
      else if (op->getType()->isPointerType()){
        body.intops += 1;
      }
      else if (op->getType()->isDependentType()){
        //this better be a template type
        const TemplateTypeParmType* ty = cast<const TemplateTypeParmType>(op->getLHS()->getType().getTypePtr());
      }
      else {
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
visitBodyParenExpr(ParenExpr* expr, Loop::Body& body)
{
  addOperations(expr->getSubExpr(), body);
}

void
visitBodyArraySubscriptExpr(ArraySubscriptExpr* expr, Loop::Body& body, bool isLHS)
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

  if (acc.newAccess(mloc.maxGen, currentGeneration, isLHS)){
    TypeInfo ti = CI.getASTContext().getTypeInfo(expr->getType());
    //std::cout << "sizeof(" << expr->getType()->getTypeClassName()
    //          << ")=" << ti.Width << std::endl;

    //fails data flow check OR first access
    if (isLHS) body.writeBytes += ti.Width / 8;
    else       body.readBytes += ti.Width / 8;
  }
}

void
visitBodyDeclRefExpr(DeclRefExpr* expr, Loop::Body& body, bool isLHS)
{
  NamedDecl* decl = expr->getFoundDecl();
  Variable& var = getVariable(decl);
  if (isLHS) var.generation = currentGeneration;
}

void
visitBodyImplicitCastExpr(ImplicitCastExpr* expr, Loop::Body& body, bool isLHS)
{
  addOperations(expr->getSubExpr(), body, isLHS);
}

void
visitBodyCStyleCastExpr(CStyleCastExpr* expr, Loop::Body& body, bool isLHS)
{
  addOperations(expr->getSubExpr(), body, isLHS);
}

#define printbool(fxn,x) std::cout << #x << std::boolalpha << " " << fxn->x() << std::endl
#define printbools(callee) \
  printbool(callee,isLateTemplateParsed); \
  printbool(callee,hasBody); \
  printbool(callee,isDefined); \
  printbool(callee,hasTrivialBody); \
  printbool(callee,isDeleted); \
  printbool(callee,isConstexpr); \
  printbool(callee,hasSkippedBody); \
  printbool(callee,isInlined); \
  printbool(callee,isFunctionTemplateSpecialization); \
  printbool(callee,isImplicitlyInstantiable); \
  printbool(callee,isTemplateInstantiation)

void
visitBodyCallExpr(CallExpr* expr, Loop::Body& body)
{
  //the definition of this function better be available
  //procedure calls should NOT be INSIDE a compute intensive loop
  FunctionDecl* callee = expr->getDirectCallee();
  if (callee){
    if (callee->isTemplateInstantiation()){
      FunctionTemplateDecl* td = callee->getPrimaryTemplate();
      addOperations(td->getTemplatedDecl()->getBody(), body);
    } else {
      FunctionDecl* def = callee->getDefinition();
      if (!def){
        std::stringstream sstr;
        sstr << "non-inlineable function " << callee->getNameAsString()
             << " inside compute-intensive code";
        warn(expr->getLocStart(), CI, sstr.str());
      } else {
        addOperations(def->getBody(), body);
      }
    }
  } else {
    warn(expr->getLocStart(), CI,
               "non-inlinable function inside compute-intensive code");
  }
}

void
visitBodyDeclStmt(DeclStmt* stmt, Loop::Body& body)
{
  SSTPragma* prg = pragmas.takeMatch(stmt);
  if (!stmt->isSingleDecl()){
    return;
  }
  Decl* d = stmt->getSingleDecl();
  if (d){
    if (prg){
      prg->activate(d, getScopedVariables());
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
addOperations(Stmt* stmt, Loop::Body& body, bool isLHS = false)
{
  currentGeneration++;
  switch(stmt->getStmtClass()){
    body_case(ForStmt,stmt,body);
    body_case(CompoundStmt,stmt,body);
    body_case(CompoundAssignOperator,stmt,body);
    body_case(BinaryOperator,stmt,body);
    body_case(ParenExpr,stmt,body);
    body_case(CallExpr,stmt,body);
    body_case(DeclRefExpr,stmt,body,isLHS);
    body_case(ImplicitCastExpr,stmt,body,isLHS);
    body_case(CStyleCastExpr,stmt,body,isLHS);
    body_case(ArraySubscriptExpr,stmt,body,isLHS);
    body_case(DeclStmt,stmt,body);
    default:
      break;
  }
}

void
setLoopControlExpr(Expr*& lhs, Expr* rhs)
{
  //we may have issues if the rhs references variables
  //that are scoped inside the loop
}

void
visitStrideCompoundStmt(clang::CompoundStmt* stmt, ForLoopSpec* spec)
{
  getStride(stmt->body_front(),spec);
}

void
visitStrideCompoundAssignOperator(clang::CompoundAssignOperator* op, ForLoopSpec* spec)
{
  spec->stride = op->getRHS();
}

void
visitStrideUnaryOperator(clang::UnaryOperator* op, ForLoopSpec* spec)
{
  if (op->isPrefix() || op->isPostfix()){
    spec->stride = nullptr; //null indicates ++
  } else {
    errorAbort(op->getLocStart(), CI, "got unary operator that not a ++ increment");
  }
}

void
getStride(Stmt* stmt, ForLoopSpec* spec)
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
visitPredicateBinaryOperator(BinaryOperator* op, ForLoopSpec* spec)
{
  setLoopControlExpr(spec->predicateMax, op->getRHS());
  spec->predicateMax = op->getRHS();
}

void
getPredicateVariables(Expr* expr, ForLoopSpec* spec)
{
  switch(expr->getStmtClass()){
    for_case(Predicate,BinaryOperator,expr,spec);
    default:
      errorAbort(expr->getLocStart(), CI, "skeletonized predicates mut be basic binary ops");
  }
}

void
visitInitialDeclStmt(clang::DeclStmt* stmt, ForLoopSpec* spec)
{
  if (!stmt->isSingleDecl()){
    errorAbort(stmt->getLocStart(), CI,
               "skeleton for loop initializer is not single declaration");
  }
  VarDecl* var = cast<VarDecl>(stmt->getSingleDecl());
  spec->incrementer = var;
  spec->init = var->getInit();
}

void
visitInitialBinaryOperator(clang::BinaryOperator* op, ForLoopSpec* spec)
{
  if (op->getLHS()->getStmtClass() != Stmt::DeclRefExprClass){
    errorAbort(op->getLocStart(), CI, "skeletonized loop initializer is not simple assignment");
  } else {
    DeclRefExpr* dre = cast<DeclRefExpr>(op->getLHS());
    spec->incrementer = dre->getFoundDecl();
  }
  spec->init = op->getRHS();
}

void
getInitialVariables(Stmt* stmt, ForLoopSpec* spec)
{
  switch(stmt->getStmtClass()){
    for_case(Initial,DeclStmt,stmt,spec);
    for_case(Initial,BinaryOperator,stmt,spec);
    default:
      errorAbort(stmt->getLocStart(), CI, "bad initial expression for skeletonization");
  }
}

std::string
getTripCount(ForLoopSpec* spec)
{
  PrettyPrinter pp;
  if (spec->stride){
    pp.os << "(";
  }
  pp.os << "(";
  pp.print(spec->predicateMax);
  pp.os << ")";
  if (spec->init){
    pp.os << "-";
    pp.os << "(";
    pp.print(spec->init);
    pp.os << ")";
  }
  if (spec->stride){
    pp.os << ") / (";
    pp.print(spec->stride);
    pp.os << ")";
  }
  return pp.str();
}

void
addLoopContribution(std::ostream& os, Loop& loop)
{
  os << "{ ";
  for (auto& str : scopedVariables){
    //might have some special vars to "elevate" in scope
    os << str << "; ";
  }
  os << " int tripCount" << loop.body.depth << "=";
  if (loop.body.depth > 0){
    os << "tripCount" << (loop.body.depth-1) << "*";
  }
  os << "(" << loop.tripCount << "); ";
  if (loop.body.flops > 0){
      os << " flops += tripCount" << loop.body.depth << "*" << loop.body.flops << ";";
  }
  if (loop.body.readBytes > 0){
    os << " readBytes += tripCount" << loop.body.depth << "*" << loop.body.readBytes << "; ";
  }
  if (loop.body.writeBytes > 0){
    os << " writeBytes += tripCount" << loop.body.depth << "*" << loop.body.writeBytes << "; ";
  }
  if (loop.body.intops > 0){
      os << " intops += tripCount" << loop.body.depth << "*" << loop.body.intops << ";";
  }

  auto end = loop.body.subLoops.end();
  for (auto iter=loop.body.subLoops.begin(); iter != end; ++iter){
    addLoopContribution(os, *iter);
  }
  os << "}";
}

void
setLoopContext(ForStmt* stmt){
  Stmt* body = stmt->getBody();
  scopeStartLine = body->getLocStart();
}

void
replaceStmt(Stmt* stmt, Rewriter& r, Loop& loop)
{
  std::stringstream sstr;
  sstr << "{ uint64_t flops(0); uint64_t readBytes(0); uint64_t writeBytes(0); uint64_t intops(0); ";
  addLoopContribution(sstr, loop);
  sstr << "sstmac_compute_detailed(flops,intops,readBytes); /*assume write-through for now*/";
  sstr << " }";

  PresumedLoc start = CI.getSourceManager().getPresumedLoc(stmt->getLocStart());
  PresumedLoc stop = CI.getSourceManager().getPresumedLoc(stmt->getLocEnd());
  int numLinesDeleted = stop.getLine() - start.getLine();
  //to preserve the correspondence of line numbers
  for (int i=0; i < numLinesDeleted; ++i){
    sstr << "\n";
  }

  r.ReplaceText(stmt->getSourceRange(), sstr.str());
}

};


void
SSTComputePragma::visitAndReplaceStmt(Stmt* stmt, Rewriter& r)
{
  Loop loop(0); //just treat this is a loop of size 1
  loop.tripCount = "1";
  ComputeVisitor vis(*CI, *pragmaList, nullptr);
  vis.addOperations(stmt,loop.body);
  vis.replaceStmt(stmt,r,loop);
}

void
SSTComputePragma::activate(Stmt *stmt, Rewriter &r, PragmaConfig& cfg)
{
  switch(stmt->getStmtClass()){
    scase(ForStmt,stmt,r);
    scase(IfStmt,stmt,r);
    default:
      defaultAct(stmt,r);
      break;
  }
}

void
SSTComputePragma::activate(Decl* d, Rewriter& r, PragmaConfig& cfg)
{
  switch(d->getKind()){
    dcase(Function,d,r,cfg);
    dcase(CXXMethod,d,r,cfg);
    default:
      break;
  }
}

void
SSTComputePragma::defaultAct(Stmt *stmt, Rewriter &r)
{
  r.ReplaceText(stmt->getSourceRange(),"");
}


void
SSTComputePragma::visitCXXMethodDecl(CXXMethodDecl* decl, Rewriter& r, PragmaConfig& cfg)
{
  if (decl->hasBody()){
    visitAndReplaceStmt(decl->getBody(), r);
    cfg.skipNextStmt = true;
  }
}

void
SSTComputePragma::visitFunctionDecl(FunctionDecl* decl, Rewriter& r, PragmaConfig& cfg)
{
  if (decl->hasBody()){
    visitAndReplaceStmt(decl->getBody(), r);
    cfg.skipNextStmt = true;
  }
}


void
SSTComputePragma::visitIfStmt(IfStmt* stmt, Rewriter& r)
{
  visitAndReplaceStmt(stmt->getThen(), r);
  if (stmt->getElse()){
    visitAndReplaceStmt(stmt->getElse(), r);
  }
}

void
SSTComputePragma::visitForStmt(ForStmt *stmt, Rewriter &r)
{
  ComputeVisitor vis(*CI, *pragmaList, nullptr); //null, no parent
  Loop loop(0); //depth zeros
  vis.setLoopContext(stmt);
  vis.visitLoop(stmt,loop);
  vis.replaceStmt(stmt,r,loop);
}