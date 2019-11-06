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

#include "pragmas.h"
#include "astVisitor.h"
#include <sstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;
using namespace pragmas;

#define lexify(x) \
  PP.Lex(x); std::cout << x.getName(); \
  if (x.getKind() == tok::identifier) std::cout << " " << x.getIdentifierInfo()->getNameStart(); \
  std::cout << std::endl;

SourceLocation SSTPragmaHandler::pragmaDirectiveLoc;
std::map<std::string, SSTPragmaNamespace*>* PragmaRegisterMap::namespaces_ = nullptr;

static std::string getStringToken(const Token& next, clang::CompilerInstance& ci)
{
  //the next token should be a string naming the argument
  std::string argName;
  switch (next.getKind()){
    case tok::string_literal:
      argName = next.getLiteralData();
      break;
    case tok::identifier:
      argName = next.getIdentifierInfo()->getName().str();
      break;
    default: {
      std::string error = std::string("invalid pragma token of type ") + next.getName()
         + " - expected string literal argument name";
      errorAbort(next.getLocation(), ci, error);
    }
  }
  return argName;
}

static void assertToken(const Token& tok, tok::TokenKind kind, clang::CompilerInstance& ci)
{
  if (tok.getKind() != kind){
    std::string error = std::string("invalid pragma token of type ") + tok.getName()
       + " - expected " + tok::getTokenName(kind);
    errorAbort(tok.getLocation(), ci, error);
  }
}

void getLiteralDataAsString(const Token &tok, std::ostream &os)
{
  const char* data = tok.getLiteralData(); //not null-terminated, direct from buffer
  for (int i=0 ; i < tok.getLength(); ++i){
    //must explicitly add chars, this will not hit a \0
     os << data[i];
  }
}

std::string getLiteralDataAsString(const Token &tok)
{
  std::stringstream sstr;
  getLiteralDataAsString(tok, sstr);
  return sstr.str();
}

static void tokenToString(const Token& tok, std::ostream& os,
                          clang::CompilerInstance& CI)
{
  switch(tok.getKind()){
  case tok::identifier:
    os << tok.getIdentifierInfo()->getNameStart();
    break;
  case tok::semi:
    os << ";";
    break;
  case tok::l_paren:
    os << '(';
    break;
  case tok::r_paren:
    os << ')';
    break;
  case tok::comma:
    os << ',';
    break;
  case tok::less:
    os << "<";
    break;
  case tok::slash:
    os << "/";
    break;
  case tok::star:
    os << "*";
    break;
  case tok::period:
    os << ".";
    break;
  case tok::kw_return:
    os << "return ";
    break;
  case tok::coloncolon:
    os << "::";
    break;
  case tok::kw_long:
    os << "long";
    break;
  case tok::kw_int:
    os << "int";
    break;
  case tok::kw_double:
    os << "double";
    break;
  case tok::equalequal:
    os << "==";
    break;
  case tok::kw_sizeof:
    os << "sizeof";
    break;
  case tok::minus:
    os << "-";
    break;
  case tok::percent:
    os << "%";
    break;
  case tok::kw_true:
    os << "true";
    break;
  case tok::kw_false:
    os << "false";
    break;
  case tok::arrow:
    os << "->";
    break;
  case tok::kw_nullptr:
    os << "nullptr";
    break;
  case tok::kw_for:
    os << "for";
    break;
  case tok::string_literal:
  case tok::numeric_constant:
  {
    getLiteralDataAsString(tok, os);
    break;
  }
  default:
    std::cerr << "bad token: " << tok.getName() << std::endl;
    errorAbort(tok.getLocation(), CI, "invalid token in pragma");
    break;
  }
}

int SSTPragma::getActiveMode() const {
  return ASTVisitorCmdLine::getActiveMode();
}

void
SSTPragmaNamespace::addFactory(int modeMask, const std::string &name,
                               bool deleteOnUse, PragmaHandlerFactoryBase *factory)
{
  for (int i=0; i < NUM_MODES; ++i){
    int testMask = 1 << i;
    if (modeMask & testMask){
			auto &modeFactories = factories_[i];

			if(modeFactories.count(name) > 0){
				internalError("Trying to add factory with name key " 
											+ name + " for mode "	
											+ std::to_string(i) 
											+ " but it was already added.\n");
			}

      modeFactories.insert({name,factory});
    }
  }
  pragmaNames_.insert(name);
}

PragmaHandlerFactoryBase*
SSTPragmaNamespace::getFactory(Mode m, const std::string &name)
{
  auto& map = factories_[m];
  auto iter = map.find(name);
  if (iter == map.end()){
    auto* handler = new PragmaHandlerFactory<SSTNoArgsPragmaShim<SSTDoNothingPragma>, true>(name);
    map[name] = handler;
    return handler;
  } else {
    return iter->second;
  }
}

SSTPragmaNamespace*
PragmaRegisterMap::getNamespace(const std::string &ns)
{
  if (!namespaces_){
    namespaces_ = new std::map<std::string,SSTPragmaNamespace*>;
  }
  auto iter = namespaces_->find(ns);
  if (iter == namespaces_->end()){
    auto* nsObj = new SSTPragmaNamespace(ns);
    (*namespaces_)[ns] = nsObj;
    return nsObj;
  } else {
    return iter->second;
  }
}

void
SSTPragmaHandler::configure(bool delOnUse, Token& PragmaTok, Preprocessor& PP, SSTPragma* fsp)
{
  static int pragmaDepth = 0;
  static int maxPragmaDepth = 0;
  pragmas_.push_back(fsp);
  pragmaDepth++;
  maxPragmaDepth++;
  fsp->deleteOnUse = delOnUse;
  fsp->pragmaList = &pragmas_;
  fsp->name = getName();
  //fsp->startPragmaLoc = PragmaTok.getLocation();
  fsp->CI = &ci_;
  fsp->visitor = &visitor_;
  PP.EnableBacktrackAtThisPos(); //this controls the backtrack
  Token pragmaTarget;
  PP.Lex(pragmaTarget); //this might hit another pragma
  //fsp->endPragmaLoc = pragmaTarget.getLocation();
  fsp->targetLoc = pragmaTarget.getEndLoc();
  PP.Backtrack();
  if (pragmaDepth == 1){ //this is the top pragma
    //if we hit multiple pragmas, we might have redundant tokens
    //becaue of recursion weirdness in the lexer
    for (int i=1; i < maxPragmaDepth; ++i){
      Token throwAway;
      PP.Lex(throwAway);
    }
    maxPragmaDepth = 0;
  }
  --pragmaDepth;
  fsp->depth = pragmaDepth;
}

void
SSTPragmaHandler::HandlePragma(Preprocessor &PP, PragmaIntroducerKind Introducer, Token& PragmaTok)
{
  std::list<Token> tokens;
  Token next; PP.Lex(next);
  while (!next.is(tok::eod)){
    tokens.push_back(next);
    PP.Lex(next);
  }
  pragmaLoc_ = PragmaTok.getLocation();

  SSTPragma* fsp = allocatePragma(tokens);
  fsp->pragmaDirectiveLoc = pragmaDirectiveLoc;
  fsp->startPragmaLoc = PragmaTok.getLocation();
  fsp->endPragmaLoc = next.getLocation();

  configure(deleteOnUse_, PragmaTok, PP, fsp);
}

std::map<std::string, std::list<std::string>>
SSTPragma::getMap(SourceLocation loc, CompilerInstance& CI, const std::list<clang::Token>& tokens)
{
  std::map<std::string, std::list<std::string>> allArgs;
  auto iter = tokens.begin();
  auto incrementIter = [&](){
    ++iter;
    if (iter == tokens.end()){
      errorAbort(loc, CI,
       "mis-formatted pragma, pragma modifiers should be of the form 'arg(a)' or 'arg(a,b)'");
    }
  };


  int parenDepth = 0;
  std::list<std::string> argList;
  std::stringstream sstr;
  std::string argName;
  for (const Token& t : tokens){
    switch(t.getKind()){
    case tok::l_paren: {
      if (parenDepth == 0){
        argName = sstr.str();
        sstr.str("");
      } else {
        tokenToString(t, sstr, CI);
      }
      ++parenDepth;
      break;
    }
    case tok::r_paren:
      if (parenDepth == 1){
        argList.push_back(sstr.str());
        sstr.str("");
        allArgs[argName] = std::move(argList);
      } else {
        tokenToString(t, sstr, CI);
      }
      --parenDepth;
      break;
    case tok::comma:
      if (parenDepth == 1){
        argList.push_back(sstr.str());
        sstr.str("");
      } else {
        tokenToString(t, sstr, CI);
      }
      break;
    default:
      tokenToString(t, sstr, CI);
      break;
    }
  }
  return allArgs;
}

std::string
SSTPragma::getSingleString(const std::list<Token>& tokens, clang::CompilerInstance& CI)
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(tokens.begin(), tokens.end(), sstr, CI);
  return sstr.str();
}

void
SSTDeletePragma::activate(clang::Stmt* s, clang::Rewriter& r, PragmaConfig& cfg){
  replace(s,r,"",*CI);
  switch(s->getStmtClass()){
  case Stmt::ForStmtClass:
  case Stmt::CompoundStmtClass:
    break;
  default:
    break;
  }
  throw StmtDeleteException(s);
}

void
SSTDeletePragma::activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg){
  replace(d,r,"",*CI);
  throw DeclDeleteException(d);
}

void
SSTEmptyPragma::activate(clang::Decl* d, clang::Rewriter& r, PragmaConfig& cfg){
  std::stringstream sstr;
  sstr << "{" << body_;
  if (body_.size()) sstr << ";";
  sstr << "}";
#define prg_case(x,d) case Decl::x: replace(cast<x##Decl>(d)->getBody(), r, sstr.str(),*CI); break
  switch (d->getKind()){
    prg_case(Function,d);
    prg_case(CXXMethod,d);
    default:
      break;
  }
#undef prg_case
  throw DeclDeleteException(d);
}

void
SSTEmptyPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  errorAbort(s, *CI, "pragma empty should only apply to declarations, not statmements");
}

void
SSTKeepPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  cfg.makeNoChanges = true;
}

void
SSTKeepPragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  cfg.makeNoChanges = true;
}

void
SSTNewPragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
#define dcase(type,d,rw) \
  case(clang::Decl::type): \
    visit##type##Decl(clang::cast<type##Decl>(d),rw); break
  switch(d->getKind()){
    default:
      break;
  }
#undef dcase
}

void
SSTNewPragma::visitDeclStmt(DeclStmt *stmt, Rewriter &r)
{
  if (!stmt->isSingleDecl()){
    errorAbort(stmt, *CI, "cannot skeletonize multi-declarations");
  }
  Decl* decl = stmt->getSingleDecl();
  if (isa<VarDecl>(decl)){
    VarDecl* vd = cast<VarDecl>(decl);
    if (vd->hasInit()){
      Expr* init = vd->getInit();
      if (isa<CXXNewExpr>(init)){
        //we can directly skeletonize
        std::string type = GetAsString(vd->getType());
        std::string name = vd->getNameAsString();
        std::stringstream sstr;
        sstr << type << " " << name << " = nullptr;"; //don't know why - but okay, semicolon needed
        replace(stmt, r, sstr.str(), *CI);
        throw StmtDeleteException(stmt);
      } //boy, I really hope this doesn't allocate any memory
    }
  } else {
    errorAbort(stmt, *CI, "sst malloc pragma applied to non-variable declaration");
  }
}

void
SSTNewPragma::visitBinaryOperator(BinaryOperator *op, Rewriter& r)
{
  if (isa<CXXNewExpr>(op->getRHS())){
    PrettyPrinter pp;
    //this better be an equals
    pp.print(op->getLHS());
    pp.os << " = nullptr"; //don't know why - but okay, semicolon not needed
    replace(op, r, pp.os.str(),*CI);
    throw StmtDeleteException(op);
  } //boy, I really hope this doesn't allocate any memory
}

void
SSTNewPragma::activate(Stmt* stmt, Rewriter &r, PragmaConfig& cfg)
{
#define scase(type,s,rw) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s),rw); break
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt,r);
    scase(BinaryOperator,stmt,r);
    default: //well - that was stupid of you
      warn(stmt, *CI, "pragma new not applied to operator new or binary operator");
      break;
  }
#undef scase
}

void
SSTKeepIfPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  PrettyPrinter pp;
  pp.os << "if (" << ifCond_ << "){ ";
  pp.print(s); //put the original statement in the if
  pp.os << "; } else if (0)";
  r.InsertText(getStart(s), pp.os.str(), false, false);
}

void
SSTBranchPredictPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  if (s->getStmtClass() != Stmt::IfStmtClass){
    errorAbort(s, *CI, "predicate pragma not applied to if statement");
  }
  IfStmt* ifs = cast<IfStmt>(s);
  replace(ifs->getCond(), r, prediction_, *CI);
}

void
SSTOverheadPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  std::stringstream sstr;
  sstr << "sstmac_advance_time(\""
       << paramName_
       << "\");";
  r.InsertText(getStart(s), sstr.str(), false);
  cfg.newParams.insert(paramName_);
}

void
SSTAdvanceTimePragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  PrettyPrinter pp;
  std::string replacement;

  if (units_ == "sec" || units_ == "s"){
    replacement = "sstmac_compute(" + amount_ + ");";
  } else if (units_ == "msec" || units_ == "msec"){
    replacement = "sstmac_msleep(" + amount_ + ");";
  } else if (units_ == "usec" || units_ == "us"){
    replacement = "sstmac_usleep(" + amount_ + ");";
  } else if (units_ == "nsec" || units_ == "ns"){
    replacement = "sstmac_nanosleep(" + amount_ + ");";
  } else {
    std::string error = "invalid time units: " + units_ +
        ": must be sec, msec, usec, or nsec";
    errorAbort(s, *CI, error);
  }
  
  pp.os << replacement; 
  r.InsertText(getStart(s), pp.os.str(), false, false);
}

void
SSTCallFunctionPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  r.InsertText(getStart(s), repl_, false);
}

void
SSTMallocPragma::visitBinaryOperator(BinaryOperator *op, Rewriter &r)
{
  PrettyPrinter pp;
  //this better be an equals
  pp.print(op->getLHS());
  pp.os << " = 0"; //don't know why - but okay, semicolon not needed
  replace(op, r, pp.os.str(), *CI);
}

void
SSTMallocPragma::visitDeclStmt(DeclStmt *stmt, Rewriter &r)
{
  if (!stmt->isSingleDecl()){
    errorAbort(stmt, *CI, "cannot skeletonize multi-declarations");
  }
  Decl* decl = stmt->getSingleDecl();
  if (isa<VarDecl>(decl)){
    VarDecl* vd = cast<VarDecl>(decl);
    std::string type = GetAsString(vd->getType());
    std::string name = vd->getNameAsString();
    std::stringstream sstr;
    sstr << type << " " << name << " = 0;"; //don't know why - but okay, semicolon needed
    replace(stmt, r, sstr.str(), *CI);
  } else {
    errorAbort(stmt, *CI, "sst malloc pragma applied to non-variable declaration");
  }
}

void
SSTMallocPragma::activate(Stmt *stmt, Rewriter &r, PragmaConfig& cfg)
{
#define scase(type,s,rw) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s),rw); break
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt,r);
    scase(BinaryOperator,stmt,r);
    default: //just delete what follows
      //SSTDeletePragma::act(stmt,r);
      break;
  }
#undef scase
}

void
SSTReturnPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  if (s->getStmtClass() != Stmt::ReturnStmtClass){
    errorAbort(s, *CI,
              "pragma return not applied to return statement");
  }
  replace(s, r, "return " + repl_, *CI);
  throw StmtDeleteException(s);
}

void
SSTReturnPragma::activate(Decl* d, Rewriter& r, PragmaConfig& cfg)
{
  if (d->getKind() != Decl::Function){
    errorAbort(d, *CI, "pragma return not applied to function definition");
  }
  FunctionDecl* fd = cast<FunctionDecl>(d);
  if (!fd->hasBody()){
    errorAbort(d, *CI, "pragma return applied to function declaration - must be definition");
  }
  std::string repl = "{ return " + repl_ + "; }";
  replace(fd->getBody(), r, repl, *CI);
  throw DeclDeleteException(d);
}

void
SSTGlobalVariablePragma::activate(Stmt* s, Rewriter& r, PragmaConfig& cfg)
{
  cfg.dependentScopeGlobal = name_;
}

void
SSTGlobalVariablePragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  errorAbort(d, *CI, "global pragma should only be applied to statements");
}

SSTNullVariablePragma::SSTNullVariablePragma(SourceLocation loc, CompilerInstance& CI,
                                             const std::list<Token> &tokens)
 : declAppliedTo_(nullptr),
   transitiveFrom_(nullptr),
   nullSafe_(false), 
   deleteAll_(false),
   skelComputes_(false)
{
  if (tokens.empty()){
    return;
  }

  std::set<std::string> replacer;
  std::set<std::string>* inserter = nullptr;
  auto end = tokens.end();
  for (auto iter=tokens.begin(); iter != end; ++iter){
    const Token& token = *iter;
    std::string next;
    switch(token.getKind()){
    case tok::string_literal:
    case tok::numeric_constant:
      next = getLiteralDataAsString(token);
      break;
    case tok::kw_new:
      next = "new";
      break;
    case tok::kw_nullptr:
      next = "nullptr";
      break;
    case tok::identifier:
      next = token.getIdentifierInfo()->getName().str();
      break;
    default:
      std::string error = std::string("token to pragma is not a valid string name: ") + token.getName();
      errorAbort(token.getLocation(), CI, error);
      break;
    }

    if (next == "except"){
      inserter = &nullExcept_;
    } else if (next == "only"){
      inserter = &nullOnly_;
    } else if (next == "new"){
      inserter = &nullNew_;
    } else if (next == "replace"){
      inserter = &replacer;
    } else if (next == "target"){
      inserter = &targetNames_;
    } else if (next == "safe"){
      nullSafe_ = true;
    } else if (next == "delete_all"){
      deleteAll_ = true;
    } else if (next == "skel_compute"){
      skelComputes_ = true;
    } else if (inserter == nullptr){
      extras_.push_back(next);
    } else {
      inserter->insert(next);
    }
  }

  if (!replacer.empty()){
    replacement_ = *replacer.begin();
  }
}

static NamedDecl* getNamedDecl(Decl* d)
{
  switch (d->getKind()){
    case Decl::Function:
      return cast<FunctionDecl>(d);
    case Decl::Field:
      return cast<FieldDecl>(d);
    case Decl::Var:
      return cast<VarDecl>(d);
    default:
      return nullptr;
  }
}

void
SSTNullVariablePragma::doActivate(Decl* d, Rewriter& r, PragmaConfig& cfg)
{
  if (!extras_.empty()){
    errorAbort(d, *CI, "illegal null_variable spec: "
      "must be with 'only', 'except', 'new', 'replace', 'target', 'safe', 'delete_all', 'skel_compute'");
  }

  declAppliedTo_ = getNamedDecl(d);
  if (d->getKind() == Decl::Function){
    FunctionDecl* fd = cast<FunctionDecl>(d);
    if (nullSafe_){
      //this function is completely null safe
      cfg.nullSafeFunctions[fd] = this;
      return; //my work here is done
    }

    if (targetNames_.empty()){
      errorAbort(d, *CI, "null_variable pragma applied to function must give list of target null parameters");
    }

    int numHits = 0;
    for (int i=0; i < fd->getNumParams(); ++i){
      ParmVarDecl* pvd = fd->getParamDecl(i);
      if (targetNames_.find(pvd->getNameAsString()) != targetNames_.end()){
        cfg.nullVariables[pvd] = this;
        ++numHits;;
      }
    }

    if (numHits != targetNames_.size()){
      std::stringstream sstr;
      sstr << "null_variable pragma lists " << targetNames_.size()
           << " parameters, but they match " << numHits
           << " parameter names";
      errorAbort(d, *CI, sstr.str());
    }
    //no parameters matched target name
  } else {
    cfg.nullVariables[d] = this;
  }
}

void
SSTNullVariablePragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  switch(d->getKind()){
    case Decl::Function:
      break;
    case Decl::Field:{
      FieldDecl* fd = cast<FieldDecl>(d);
      if (!fd->getType()->isPointerType()){
        errorAbort(d, *CI, "only valid to apply null_variable pragma to pointers");
      }
      break;
    }
    case Decl::Var: {
      VarDecl* vd = cast<VarDecl>(d);
      if (!vd->getType()->isPointerType()){
        errorAbort(d, *CI, "only valid to apply null_variable pragma to pointers");
      }
      break;
    }
    default:
      errorAbort(d, *CI, "only valid to apply null_variable pragma to functions, variables, and members");
      break;
  }

  doActivate(d,r,cfg);
}

void
SSTNullVariablePragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  if (s->getStmtClass() == Stmt::DeclStmtClass){
    DeclStmt* ds = cast<DeclStmt>(s);
    for (auto iter=ds->decl_begin(); iter != ds->decl_end(); ++iter){
      activate(*iter,r,cfg);
    }
  } else {
    errorAbort(s, *CI,
        "pragma null_variable should only apply to declaration statements");
  }
}

static void addFields(RecordDecl* rd, PragmaConfig& cfg, bool defaultNull,
                      std::set<std::string>& fields, SSTNullVariablePragma* prg)
{
  for (auto iter=rd->decls_begin(); iter != rd->decls_end(); ++iter){
    Decl* d = *iter;
    if (d->getKind() == Decl::Field){
      FieldDecl* fd = cast<FieldDecl>(d);
      auto iter = fields.find(fd->getName());
      if (iter == fields.end()){
        if (defaultNull) cfg.nullVariables[fd] = prg;
      } else {
        if (!defaultNull) cfg.nullVariables[fd] = prg;
        fields.erase(iter);
      }
    }
  }
}


static void doActivateFieldsPragma(Decl* d, PragmaConfig& cfg, bool defaultNull,
                                   std::set<std::string>& fields, SSTNullVariablePragma* prg,
                                   clang::CompilerInstance& CI)
{
  switch (d->getKind()){
  //case Decl::Function: {
  //  return;
  //}
  case Decl::CXXRecord:
  case Decl::Record: {
    RecordDecl* rd = cast<RecordDecl>(d);
    addFields(rd, cfg, defaultNull, fields, prg);
    break;
  }
  case Decl::Typedef: {
    TypedefDecl* td = cast<TypedefDecl>(d);
    if (td->getTypeForDecl() == nullptr){
      internalError(d, CI, "typedef declaration has no underlying type");
      break;
    } else if (td->getTypeForDecl()->isStructureType()){
      RecordDecl* rd = td->getTypeForDecl()->getAsStructureType()->getDecl();
      addFields(rd, cfg, defaultNull, fields, prg);
      break;
    }
  }
  default:
    errorAbort(d, CI, "nonnull_fields pragma should only be applied to struct or function declarations");
  }

  if (!fields.empty()){
    std::stringstream sstr;
    sstr << "Provided variable name not found in attached scope for pragma nonnull_fields:\n";
    for (auto& str : fields){
      sstr << " " << str;
    }
    errorAbort(d, CI, sstr.str());
  }
}

SSTNullFieldsPragma::SSTNullFieldsPragma(SourceLocation loc, CompilerInstance &CI, const std::list<Token> &tokens) :
  SSTNullVariablePragma(loc, CI, tokens)
{
  for (auto& str : extras_){
    nullFields_.insert(str);
  }
  extras_.clear();
}

void
SSTNullFieldsPragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  doActivateFieldsPragma(d, cfg, false, nullFields_, this, *CI);
}

void
SSTNullFieldsPragma::activate(Stmt *stmt, Rewriter &r, PragmaConfig &cfg)
{
  errorAbort(stmt, *CI,
      "null_fields pragma should only be applied to struct declarations, not statements");
}

SSTNonnullFieldsPragma::SSTNonnullFieldsPragma(SourceLocation loc, CompilerInstance &CI,
                                               const std::list<Token> &tokens) :
  SSTNullVariablePragma(loc, CI, tokens)
{
  for (auto& str : extras_){
    nonnullFields_.insert(str);
  }
  extras_.clear();
}

void
SSTNonnullFieldsPragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  doActivateFieldsPragma(d, cfg, true, nonnullFields_, this, *CI);
}

void
SSTNonnullFieldsPragma::activate(Stmt *stmt, Rewriter &r, PragmaConfig &cfg)
{
  errorAbort(stmt, *CI,
     "nonull_fields pragma should only be applied to struct declarations, not statements");
}

SSTNullTypePragma::SSTNullTypePragma(SourceLocation loc, CompilerInstance& CI,
                                     const std::list<Token> &tokens)
{
  //this is valid - it just means wipe out the type
  //if (tokens.empty()){
  //  errorAbort(loc, CI, "null_type pragma requires a type argument");
  //}

  std::list<std::string> toInsert;
  bool connectPrev = false;
  int templateCount = 0;
  for (const Token& token : tokens){
    std::string next;
    bool connectNext = false;
    switch(token.getKind()){
    case tok::string_literal:
      next = token.getLiteralData();
      break;
    case tok::coloncolon:
      next = "::";
      connectNext = true;
      break;
    case tok::comma:
      next = ",";
      connectNext = connectPrev;
      break;
    case tok::less:
      next = "<";
      connectNext = true;
      templateCount++;
      break;
    case tok::greater:
      next = ">";
      templateCount--;
      connectNext = templateCount > 0;
      break;
    case tok::kw_long:
      next = "long";
      connectNext = connectPrev;
      break;
    case tok::kw_int:
      next = "int";
      connectNext = connectPrev;
      break;
    case tok::kw_double:
      next = "double";
      connectNext = connectPrev;
      break;
    case tok::identifier:
      next = token.getIdentifierInfo()->getName().str();
      break;
    default: {
      std::string err = std::string("token ") + token.getName() + " is not valid in type names";
      errorAbort(token.getLocation(), CI, err);
      break;
    }
    }
    if (connectPrev || connectNext){
      toInsert.back() += next;
    } else {
      toInsert.push_back(next);
    }
    connectPrev = connectNext;
  }

  if (toInsert.empty()){
    //just wipeout the thing entirely - just make it a char
    newType_ = "char";
  } else {
    newType_ = toInsert.front();
    toInsert.pop_front();
    for (auto& str : toInsert){
      nullExcept_.insert(str);
    }
  }
}


void
SSTNullTypePragma::activate(Decl *d, Rewriter &r, PragmaConfig &cfg)
{
  std::string name;
  switch(d->getKind()){
  case Decl::Var: {
    VarDecl* vd = cast<VarDecl>(d);
    name = vd->getNameAsString();
    break;
  }
  case Decl::Field: {
    FieldDecl* fd = cast<FieldDecl>(d);
    name = fd->getNameAsString();
    break;
  }
  default:
    errorAbort(d, *CI, "null_type pragma can only be applied to field or variable declaration");
  }
  std::string repl = newType_ + " " + name;
  replace(d,r,repl,*CI);
  doActivate(d,r,cfg);
}

void
SSTPragma::tokenStreamToString(std::list<Token>::const_iterator beg,
                               std::list<Token>::const_iterator end,
                               std::ostream& os,
                               CompilerInstance& CI)
{
  for (auto iter=beg; iter != end; ++iter){
    tokenToString(*iter, os, CI);
  }
}

SSTAdvanceTimePragma::SSTAdvanceTimePragma(SourceLocation loc, CompilerInstance& CI,
                                      const std::list<Token> &tokens)
{
  if (tokens.size() < 2){
    errorAbort(loc, CI,
      "advance_time pragma needs at least two arguments: <units> <number>");
  }
  
  //Orginal code was ausing assertion to fail inside of token::getLiteralData();
  auto iter = tokens.begin();

  std::stringstream units;  
  iter++;
  SSTPragma::tokenStreamToString(tokens.begin(), iter, units, CI);

  std::stringstream sval;
  SSTPragma::tokenStreamToString(iter, tokens.end(), sval, CI);
  units_ = units.str();
  amount_ = sval.str();
}

SSTCallFunctionPragma::SSTCallFunctionPragma(SourceLocation loc, CompilerInstance& CI,
                                             const std::list<Token> &tokens)
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(tokens.begin(), tokens.end(), sstr, CI);
  sstr << ";"; //semi-colon not required in pragma
  repl_ = sstr.str();
}

SSTStackAllocPragma::SSTStackAllocPragma(SourceLocation loc, CompilerInstance& CI,
                                         std::map<std::string, std::list<std::string>>&& in_args)
{
  mdataSize_ = "0";

  auto args = in_args;

  auto iter = args.find("free");
  if (iter != args.end()){
    toFree_ = iter->second.front();
    args.erase(iter);
  }

  iter = args.find("alloc");
  if (iter != args.end()){
    auto& list = iter->second;
    stackSize_ = list.front();
    if (list.size()  == 2){
      mdataSize_ = list.back();
    }
    args.erase(iter);
  }

  if (!stackSize_.empty() && !toFree_.empty()){
    errorAbort(loc, CI, "cannot have both free and alloc for stack pragma");
  }

  if (!args.empty()){
    errorAbort(loc, CI, "invalid token passed to pragma: allowed alloc,free");
  }
}

void
SSTStackAllocPragma::activate(Stmt *s, Rewriter &r, PragmaConfig &cfg)
{
  if (toFree_.size()){
    std::string repl = "sstmac_free_stack(" + toFree_ + ")";
    replace(s, r, repl, *CI);
    throw StmtDeleteException(s);
  } else {
    Expr* toRepl = nullptr;
    switch(s->getStmtClass()){
    case Stmt::BinaryOperatorClass: {
      BinaryOperator* bop = cast<BinaryOperator>(s);
      toRepl = bop->getRHS();
      break;
    }
    case Stmt::DeclStmtClass: {
      DeclStmt* decl = cast<DeclStmt>(s);
      VarDecl* vd = cast<VarDecl>(decl->getSingleDecl());
      toRepl = vd->getInit();
      break;
    }
    default: {
      std::string error = std::string("stack alloc pragma can only be applied to assignments - got ")
          + s->getStmtClassName();
      errorAbort(s, *CI, error);
    }
    }

    toRepl = SkeletonASTVisitor::getUnderlyingExpr(toRepl);

    std::string repl = "sstmac_alloc_stack(" + stackSize_ + "," + mdataSize_ + ")";
    replace(toRepl, r, repl, *CI);
    throw StmtDeleteException(s);
  }
}

static constexpr int ALL_MODES = SKELETONIZE | PUPPETIZE | SHADOWIZE | MEMOIZE | ENCAPSULATE;

static PragmaRegister<SSTNoArgsPragmaShim, SSTDeletePragma, true> delPragma(
    "sst", "delete", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTNoArgsPragmaShim, SSTMallocPragma, true> mallocPragma(
    "sst", "malloc", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTNoArgsPragmaShim, SSTNewPragma, true> newPragma(
    "sst", "new", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTNoArgsPragmaShim, SSTKeepPragma, true> keepPragma(
    "sst", "keep", ALL_MODES);
static PragmaRegister<SSTStringPragmaShim, SSTKeepIfPragma, true> keepIfPragma(
    "sst", "keep_if", ALL_MODES);
static PragmaRegister<SSTTokenListPragmaShim, SSTNullTypePragma, true> nullTypePragma(
    "sst", "null_type", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTTokenListPragmaShim, SSTNullVariablePragma, true> nullVariablePragma(
    "sst", "null_variable", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTTokenListPragmaShim, SSTNullVariableGeneratorPragma, true> nullVariableGenPragma(
    "sst", "start_null_variable", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTNoArgsPragmaShim, SSTNullVariableStopPragma, true> nullVarStopPragma(
    "sst", "stop_null_variable", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTStringPragmaShim, SSTEmptyPragma, true> emptyPragma(
    "sst", "empty", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTStringPragmaShim, SSTGlobalVariablePragma, true> globalPragma(
    "sst", "global", ALL_MODES);
static PragmaRegister<SSTStringPragmaShim, SSTReturnPragma, true> returnPragma(
    "sst", "return", SKELETONIZE | SHADOWIZE | PUPPETIZE);
static PragmaRegister<SSTStringPragmaShim, SSTBranchPredictPragma, true> branchPredictPragma(
    "sst", "branch_predict", SKELETONIZE | SHADOWIZE | PUPPETIZE);
static PragmaRegister<SSTTokenListPragmaShim, SSTAdvanceTimePragma, true> advanceTimePragma(
    "sst", "advance_time", SKELETONIZE | SHADOWIZE | ENCAPSULATE);
static PragmaRegister<SSTTokenListPragmaShim, SSTCallFunctionPragma, true> callFunctionPragma(
    "sst", "call", SKELETONIZE | SHADOWIZE | PUPPETIZE);
static PragmaRegister<SSTStringPragmaShim, SSTOverheadPragma, true> overheadPragma(
    "sst", "overhead", SKELETONIZE | SHADOWIZE | ENCAPSULATE);
static PragmaRegister<SSTTokenListPragmaShim, SSTNonnullFieldsPragma, true> nonnullFieldPragma(
    "sst", "nonnull_fields", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTTokenListPragmaShim, SSTNullFieldsPragma, true> nullFieldPragma(
    "sst", "null_fields", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTArgMapPragmaShim, SSTStackAllocPragma, true> stackAllocPragma(
    "sst", "stack_alloc", ALL_MODES);


