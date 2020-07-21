/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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
#include "util.h"
#include <sstream>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

#define lexify(x) \
  PP.Lex(x); std::cout << x.getName(); \
  if (x.getKind() == tok::identifier) std::cout << " " << x.getIdentifierInfo()->getNameStart(); \
  std::cout << std::endl;

SourceLocation SSTPragmaHandler::pragmaDirectiveLoc;
std::map<std::string, SSTPragmaNamespace*>* PragmaRegisterMap::namespaces_ = nullptr;

/* TODO Remove, as it is unused
static std::string getStringToken(const Token& next)
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
      errorAbort(next.getLocation(), error);
    }
  }
  return argName;
}
*/

/* TODO Remove, as it is unused
static void assertToken(const Token& tok, tok::TokenKind kind)
{
  if (tok.getKind() != kind){
    std::string error = std::string("invalid pragma token of type ") + tok.getName()
       + " - expected " + tok::getTokenName(kind);
    errorAbort(tok.getLocation(), error);
  }
}
*/

static void tokenToString(const Token& tok, std::ostream& os)
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
  case tok::ampamp:
    os << "&&";
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
    errorAbort(tok.getLocation(), "invalid token in pragma");
    break;
  }
}

int SSTPragma::getActiveMode() const {
  return CompilerGlobals::mode;
}

void
SSTPragmaNamespace::addFactory(int modeMask, const std::string &name,
                               bool  /*deleteOnUse*/, PragmaHandlerFactoryBase *factory)
{
  for (int i=0; i < modes::NUM_MODES; ++i){
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
SSTPragmaNamespace::getFactory(modes::Mode m, const std::string &name)
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
SSTPragmaHandler::configure(bool delOnUse, Token&  /*PragmaTok*/, Preprocessor& PP, SSTPragma* fsp)
{
  static int pragmaDepth = 0;
  static int maxPragmaDepth = 0;
  pragmas_.push_back(fsp);
  pragmaDepth++;
  maxPragmaDepth++;
  fsp->deleteOnUse = delOnUse;
  fsp->name = getName();
  //we need the source location of the statement beyond this pragma
  //this means lexing the next token and getting its end location
  //store our current position so we can rollback
  PP.EnableBacktrackAtThisPos();
  //the call to lex might trigger another pragma handler if we have stacked pragmas
  //this function might therefore recurse into another call to SSTPragmaHandler::configure
  Token pragmaTarget;
  PP.Lex(pragmaTarget);
  fsp->targetLoc = pragmaTarget.getEndLoc();
  //rollback to our previous position to not upset the lexer
  PP.Backtrack();
  if (pragmaDepth == 1){ //this is the top pragma
    //if we hit multiple pragmas, we might have redundant tokens
    //becaue of recursion weirdness in the lexer
    //I do not understand this behavior
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
SSTPragmaHandler::handlePragmaImpl(Preprocessor& PP, Token& PragmaTok)
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

#if CLANG_VERSION_MAJOR >= 9
void
SSTPragmaHandler::HandlePragma(Preprocessor &PP, PragmaIntroducer  /*Introducer*/, Token& PragmaTok)
{
  handlePragmaImpl(PP, PragmaTok);
}
#else
void
SSTPragmaHandler::HandlePragma(Preprocessor &PP, PragmaIntroducerKind  /*Introducer*/, Token& PragmaTok)
{
  handlePragmaImpl(PP, PragmaTok);
}
#endif

std::map<std::string, std::list<std::string>>
SSTPragma::getMap(SourceLocation loc, const std::list<clang::Token>& tokens)
{
  /**
   We want to construct a map of argument lists.
   If the pragma is:
   #pragma sst myPragma arg1(x,y,z) arg2(a,b) arg3(1)
   The resulting map would be:
   map = {
     "arg1" : { "x", "y", "z" },
     "arg2" : { "a", "b" },
     "arg3" : { "1" }
   }
   We also need to accept "standalone" entries with no arguments in parentheses
   #pragma sst myPragma arg1(x,y,z) arg2
   The resulting map would be:
   map = {
     "arg1" : { "x", "y", "z" },
     "arg2" : { }
   }
   The parsing code belows collects arguments and needs to handle
   entries with/without parenthetical argument lists
  */

  std::map<std::string, std::list<std::string>> allArgs;
  int parenDepth = 0;
  std::stringstream sstr;
  /** What the current arguments (a,b) are in a new pragma argument X(a,b) */
  std::list<std::string> argList;
  /** What the current entry (X) is in a new pragma argument X(a,b) */
  std::string argName;
  /** Whether we hit a parentheses immediately after a new token*/
  bool hitParen = false;
  for (const Token& t : tokens){
    switch(t.getKind()){
    case tok::l_paren: {
      if (parenDepth == 0){
        /* We are hitting a parentheses
           immediately after a new token */
        hitParen = true;
        /* Grab argument name and start new list */
        argName = sstr.str();
        sstr.str("");
      } else {
        /* Add the argument to the ongoing list */
        tokenToString(t, sstr);
      }
      /*This parenthesis is part of an argument entry X(a(i,j),b)*/
      ++parenDepth;
      break;
    }
    case tok::r_paren:
      if (parenDepth == 1){
        /* We are closing an argument X(a,b) */
        argList.push_back(sstr.str());
        sstr.str("");
        allArgs[argName] = std::move(argList);
        argList = {};
        hitParen = false;
      } else {
        /* We are closing a matching parenthesis
           with nested parentheses.e.g X(a(i,j),b) */
        tokenToString(t, sstr);
      }
      --parenDepth;
      break;
    case tok::comma:
      if (parenDepth == 1){
        argList.push_back(sstr.str());
        sstr.str("");
      } else {
        sstr << ",";
      }
      break;
    default:
      if (!hitParen && sstr.tellp() != 0){
        /* If we have a previous token in sstr
           but the next token is not a '(',
           then this a standalone string with (...)
        */
        allArgs[sstr.str()] = std::list<std::string>();
        sstr.str("");
      }
      tokenToString(t, sstr);
      break;
    }
  }
  /* If we have a previous token in sstr
     then this a standalone string with (...)
     at the end of the pragma */
  if (sstr.tellp() != 0){
    allArgs[sstr.str()] = std::list<std::string>();
    sstr.str("");
  }

  return allArgs;
}

std::string
SSTPragma::getSingleString(const std::list<Token>& tokens)
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(tokens.begin(), tokens.end(), sstr);
  return sstr.str();
}

void
SSTDeletePragma::activate(clang::Stmt* s){
  replace(s,"");
  throw StmtDeleteException(s);
}

void
SSTDeletePragma::activate(clang::Decl* d){
  replace(d,"");
  throw DeclDeleteException(d);
}

void
SSTEmptyPragma::activate(clang::Decl* d){
  std::stringstream sstr;
  sstr << "{" << body_;
  if (body_.size()) sstr << ";";
  sstr << "}";

  if (auto* fd = llvm::dyn_cast<FunctionDecl>(d)){
    replace(fd->getBody(), sstr.str());
  }
  throw DeclDeleteException(d);
}

void
SSTEmptyPragma::activate(Stmt *s)
{
  errorAbort(s, "pragma empty should only apply to declarations, not statmements");
}

void
SSTKeepPragma::activate(Stmt *s)
{
  CompilerGlobals::pragmaConfig.makeNoChanges = true;
}

void
SSTKeepPragma::activate(Decl *d)
{
  CompilerGlobals::pragmaConfig.makeNoChanges = true;
}

void
SSTNewPragma::activate(Decl *d)
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
SSTNewPragma::visitDeclStmt(DeclStmt *stmt)
{
  if (!stmt->isSingleDecl()){
    errorAbort(stmt, "cannot skeletonize multi-declarations");
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
        replace(stmt, sstr.str());
        throw StmtDeleteException(stmt);
      } //boy, I really hope this doesn't allocate any memory
    }
  } else {
    errorAbort(stmt, "sst malloc pragma applied to non-variable declaration");
  }
}

void
SSTNewPragma::visitBinaryOperator(BinaryOperator *op)
{
  if (isa<CXXNewExpr>(op->getRHS())){
    PrettyPrinter pp;
    //this better be an equals
    pp.print(op->getLHS());
    pp.os << " = nullptr"; //don't know why - but okay, semicolon not needed
    replace(op, pp.os.str());
    throw StmtDeleteException(op);
  } //boy, I really hope this doesn't allocate any memory
}

void
SSTNewPragma::activate(Stmt* stmt)
{
#define scase(type,s) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s)); break
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt);
    scase(BinaryOperator,stmt);
    default: //well - that was stupid of you
      warn(stmt, "pragma new not applied to operator new or binary operator");
      break;
  }
#undef scase
}

void
SSTKeepIfPragma::activate(Stmt *s)
{
  PrettyPrinter pp;
  pp.os << "if (" << ifCond_ << "){ ";
  pp.print(s); //put the original statement in the if
  pp.os << "; } else if (0)";
  CompilerGlobals::rewriter.InsertText(getStart(s), pp.os.str(), false, false);
}

void
SSTAssumeTruePragma::activate(Stmt *s)
{
  IfStmt* ifs = dyn_cast<IfStmt>(s);
  if (!ifs){
    errorAbort(s, "assume_true pragma not applied to IfStmt");
  }
  replace(ifs->getCond(), "true");
  FunctionDecl* fd = CompilerGlobals::astContextLists.enclosingFunctionDecls.back();
  Token tok; tok.setKind(tok::kw_true);
  Expr* trueConstant = tokenToExpr(fd, tok, getStart(ifs->getCond()));
  ifs->setCond(trueConstant);
}

void
SSTAssumeFalsePragma::activate(Stmt *s)
{
  IfStmt* ifs = dyn_cast<IfStmt>(s);
  if (!ifs){
    errorAbort(s, "assume_false pragma not applied to IfStmt");
  }
  replace(ifs->getCond(), "false");
  FunctionDecl* fd = CompilerGlobals::astContextLists.enclosingFunctionDecls.back();
  Token tok; tok.setKind(tok::kw_false);
  Expr* falseConstant = tokenToExpr(fd, tok, getStart(ifs->getCond()));
  ifs->setCond(falseConstant);
}


void
SSTBranchPredictPragma::activate(Stmt *s)
{
  if (s->getStmtClass() != Stmt::IfStmtClass){
    errorAbort(s, "predicate pragma not applied to if statement");
  }
  CompilerGlobals::astNodeMetadata.predicatedBlocks[cast<IfStmt>(s)] = prediction_;
}

void
SSTOverheadPragma::activate(Stmt *s)
{
  std::stringstream sstr;
  sstr << "sstmac_advance_time(\""
       << paramName_
       << "\");";
  CompilerGlobals::rewriter.InsertText(getStart(s), sstr.str(), false);
  CompilerGlobals::toolInfoRegistration.extraInputFileParams.insert(paramName_);
}

void
SSTAdvanceTimePragma::activate(Stmt *s)
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
    errorAbort(s, error);
  }
  
  pp.os << replacement; 
  CompilerGlobals::rewriter.InsertText(getStart(s), pp.os.str(), false, false);
}

void
SSTCallFunctionPragma::activate(Stmt *s)
{
  CompilerGlobals::rewriter.InsertText(getStart(s), repl_, false);
}

void
SSTMallocPragma::visitBinaryOperator(BinaryOperator *op)
{
  PrettyPrinter pp;
  //this better be an equals
  pp.print(op->getLHS());
  pp.os << " = 0"; //don't know why - but okay, semicolon not needed
  replace(op, pp.os.str());
  op->setRHS(zeroExpr(getStart(op)));
}

void
SSTMallocPragma::visitDeclStmt(DeclStmt *stmt)
{
  if (!stmt->isSingleDecl()){
    errorAbort(stmt, "cannot skeletonize multi-declarations");
  }
  Decl* decl = stmt->getSingleDecl();
  if (isa<VarDecl>(decl)){
    VarDecl* vd = cast<VarDecl>(decl);
    std::string type = GetAsString(vd->getType());
    std::string name = vd->getNameAsString();
    std::stringstream sstr;
    sstr << type << " " << name << " = 0;"; //don't know why - but okay, semicolon needed
    replace(stmt, sstr.str());
    vd->setInit(zeroExpr(getStart(stmt)));
  } else {
    errorAbort(stmt, "sst malloc pragma applied to non-variable declaration");
  }
}

void
SSTMallocPragma::activate(Stmt *stmt)
{
#define scase(type,s) \
  case(clang::Stmt::type##Class): \
    visit##type(clang::cast<type>(s)); break
  switch(stmt->getStmtClass()){
    scase(DeclStmt,stmt);
    scase(BinaryOperator,stmt);
    default: //just delete what follows
      //SSTDeletePragma::act(stmt,r);
      break;
  }
#undef scase
}

void
SSTReturnPragma::activate(Stmt *s)
{
  if (s->getStmtClass() != Stmt::ReturnStmtClass){
    errorAbort(s, "pragma return not applied to return statement");
  }
  replace(s, "return " + repl_);
  throw StmtDeleteException(s);
}

void
SSTReturnPragma::activate(Decl* d)
{
  if (d->getKind() != Decl::Function){
    errorAbort(d, "pragma return not applied to function definition");
  }
  FunctionDecl* fd = cast<FunctionDecl>(d);
  if (!fd->hasBody()){
    errorAbort(d, "pragma return applied to function declaration - must be definition");
  }
  std::string repl = "{ return " + repl_ + "; }";
  replace(fd->getBody(), repl);
  fd->setBody(nullptr);
  throw DeclDeleteException(d);
}

void
SSTGlobalVariablePragma::activate(Stmt* s)
{
  CompilerGlobals::astNodeMetadata.dependentScopeGlobal = name_;
}

void
SSTGlobalVariablePragma::activate(Decl *d)
{
  errorAbort(d, "global pragma should only be applied to statements");
}

SSTBlockingPragma::SSTBlockingPragma(SourceLocation loc, std::map<std::string, std::list<std::string>>&& args)
 : condition_("true"),
   timeout_("-1")
{
  auto iter = args.find("condition");
  if (iter != args.end()){
    condition_ = iter->second.front();
    args.erase(iter);
  }

  iter = args.find("timeout");
  if (iter != args.end()){
    timeout_ = iter->second.front();
    args.erase(iter);
  }

  iter = args.find("api");
  if (iter == args.end()){
    errorAbort(loc, "blocking pragma must give api name");
  }
  api_ = iter->second.front();
  args.erase(iter);

  if (!args.empty()){
    std::string err = "got invalid keyword in block pragma: " + args.begin()->first;
    errorAbort(loc, err);
  }
}

void
SSTBlockingPragma::activate(Stmt* s)
{
  std::string text = "sstmac_blocking_call(" + condition_ + "," + timeout_ + ",\"" + api_ + "\");";
  CompilerGlobals::rewriter.InsertText(getStart(s), text, false, false);
}

SSTNullVariablePragma::SSTNullVariablePragma(SourceLocation /**loc*/, std::map<std::string, std::list<std::string>>&& args)
 : declAppliedTo_(nullptr),
   transitiveFrom_(nullptr),
   nullSafe_(false), 
   deleteAll_(false),
   skelComputes_(false)
{
  for (auto& pair : args){
    std::string next = pair.first;
    if (next == "except"){
      nullExcept_.insert(pair.second.begin(), pair.second.end());
    } else if (next == "only"){
      nullOnly_.insert(pair.second.begin(), pair.second.end());
    } else if (next == "new"){
      nullNew_.insert(pair.second.begin(), pair.second.end());
    } else if (next == "replace"){
      replacement_ = pair.second.front();
    } else if (next == "target"){
      targetNames_.insert(pair.second.begin(), pair.second.end());
    } else if (next == "safe"){
      nullSafe_ = true;
    } else if (next == "delete_all"){
      deleteAll_ = true;
    } else if (next == "skel_compute"){
      skelComputes_ = true;
    } else {
      extras_.push_back(next);
    }
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
SSTNullVariablePragma::doActivate(Decl* d)
{
  if (!extras_.empty()){
    std::stringstream sstr;
    for (auto&& str : extras_){
      sstr << " " << str;
    }
    std::string msg = "illegal null_ptr specs:" + sstr.str() + "\n"
         + "must be with 'only', 'except', 'new', 'replace', 'target', 'safe', 'delete_all', 'skel_compute'";
    errorAbort(d, msg);
  }

  declAppliedTo_ = getNamedDecl(d);
  if (d->getKind() == Decl::Function){
    FunctionDecl* fd = cast<FunctionDecl>(d);
    if (nullSafe_){
      //this function is completely null safe
      CompilerGlobals::astNodeMetadata.nullSafeFunctions[fd] = this;
      return; //my work here is done
    }

    if (targetNames_.empty()){
      errorAbort(d, "null_ptr pragma applied to function must give list of target null parameters");
    }

    int numHits = 0;
    for (int i=0; i < fd->getNumParams(); ++i){
      ParmVarDecl* pvd = fd->getParamDecl(i);
      if (targetNames_.find(pvd->getNameAsString()) != targetNames_.end()){
        CompilerGlobals::astNodeMetadata.nullVariables[pvd] = this;
        ++numHits;;
      }
    }

    if (numHits != targetNames_.size()){
      std::stringstream sstr;
      sstr << "null_ptr pragma lists " << targetNames_.size()
           << " parameters, but they match " << numHits
           << " parameter names";
      errorAbort(d, sstr.str());
    }
    //no parameters matched target name
  } else {
    CompilerGlobals::astNodeMetadata.nullVariables[d] = this;
  }
}

void
SSTNullVariablePragma::activate(Decl *d)
{
  switch(d->getKind()){
    case Decl::Function:
      break;
    case Decl::Field:{
      FieldDecl* fd = cast<FieldDecl>(d);
      if (!fd->getType()->isPointerType()){
        errorAbort(d, "only valid to apply null_ptr pragma to pointers");
      }
      break;
    }
    case Decl::Var: {
      VarDecl* vd = cast<VarDecl>(d);
      if (!vd->getType()->isPointerType()){
        errorAbort(d, "only valid to apply null_ptr pragma to pointers");
      }
      break;
    }
    default:
      errorAbort(d, "only valid to apply null_ptr pragma to functions, variables, and members");
      break;
  }

  doActivate(d);
}

void
SSTNullVariablePragma::activate(Stmt *s)
{
  if (s->getStmtClass() == Stmt::DeclStmtClass){
    DeclStmt* ds = cast<DeclStmt>(s);
    for (auto iter=ds->decl_begin(); iter != ds->decl_end(); ++iter){
      activate(*iter);
    }
  } else {
    errorAbort(s, "pragma null_ptr should only apply to declaration statements");
  }
}

static void addFields(RecordDecl* rd, bool defaultNull,
                      std::set<std::string>& fields, SSTNullVariablePragma* prg)
{
  for (auto iter=rd->decls_begin(); iter != rd->decls_end(); ++iter){
    Decl* d = *iter;
    if (d->getKind() == Decl::Field){
      FieldDecl* fd = cast<FieldDecl>(d);
      auto iter = fields.find(fd->getName());
      if (iter == fields.end()){
        if (defaultNull) CompilerGlobals::astNodeMetadata.nullVariables[fd] = prg;
      } else {
        if (!defaultNull) CompilerGlobals::astNodeMetadata.nullVariables[fd] = prg;
        fields.erase(iter);
      }
    }
  }
}


static void doActivateFieldsPragma(Decl* d, bool defaultNull,
                                   std::set<std::string>& fields, SSTNullVariablePragma* prg)
{
  switch (d->getKind()){
  //case Decl::Function: {
  //  return;
  //}
  case Decl::CXXRecord:
  case Decl::Record: {
    RecordDecl* rd = cast<RecordDecl>(d);
    addFields(rd, defaultNull, fields, prg);
    break;
  }
  case Decl::Typedef: {
    TypedefDecl* td = cast<TypedefDecl>(d);
    if (td->getTypeForDecl() == nullptr){
      internalError(d, "typedef declaration has no underlying type");
      break;
    } else if (td->getTypeForDecl()->isStructureType()){
      RecordDecl* rd = td->getTypeForDecl()->getAsStructureType()->getDecl();
      addFields(rd, defaultNull, fields, prg);
      break;
    }
  }
  default:
    errorAbort(d, "nonnull_fields pragma should only be applied to struct or function declarations");
  }

  if (!fields.empty()){
    std::stringstream sstr;
    sstr << "Provided variable name not found in attached scope for pragma nonnull_fields:\n";
    for (auto& str : fields){
      sstr << " " << str;
    }
    errorAbort(d, sstr.str());
  }
}

SSTNullFieldsPragma::SSTNullFieldsPragma(SourceLocation loc, std::map<std::string, std::list<std::string>>&& args) :
  SSTNullVariablePragma(loc, std::move(args))
{
  for (auto& str : extras_){
    nullFields_.insert(str);
  }
  extras_.clear();
}

void
SSTNullFieldsPragma::activate(Decl *d)
{
  doActivateFieldsPragma(d, false, nullFields_, this);
}

void
SSTNullFieldsPragma::activate(Stmt *stmt)
{
  errorAbort(stmt, "null_fields pragma should only be applied to struct declarations, not statements");
}

SSTNonnullFieldsPragma::SSTNonnullFieldsPragma(SourceLocation loc, std::map<std::string, std::list<std::string>>&& args) :
  SSTNullVariablePragma(loc, std::move(args))
{
  for (auto& str : extras_){
    nonnullFields_.insert(str);
  }
  extras_.clear();
}

void
SSTNonnullFieldsPragma::activate(Decl *d)
{
  doActivateFieldsPragma(d, true, nonnullFields_, this);
}

void
SSTNonnullFieldsPragma::activate(Stmt *stmt)
{
  errorAbort(stmt, "nonull_fields pragma should only be applied to struct declarations, not statements");
}

SSTNullTypePragma::SSTNullTypePragma(SourceLocation loc, const std::list<Token> &tokens)
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
      errorAbort(token.getLocation(), err);
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
SSTNullTypePragma::activate(Decl *d)
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
    errorAbort(d, "null_type pragma can only be applied to field or variable declaration");
  }
  std::string repl = newType_ + " " + name;
  replace(d,repl);
  doActivate(d);
}

void
SSTPragma::tokenStreamToString(std::list<Token>::const_iterator beg,
                               std::list<Token>::const_iterator end,
                               std::ostream& os)
{
  for (auto iter=beg; iter != end; ++iter){
    tokenToString(*iter, os);
  }
}

SSTAdvanceTimePragma::SSTAdvanceTimePragma(SourceLocation loc, const std::list<Token> &tokens)
{
  if (tokens.size() < 2){
    errorAbort(loc, "advance_time pragma needs at least two arguments: <units> <number>");
  }
  
  //Orginal code was ausing assertion to fail inside of token::getLiteralData();
  auto iter = tokens.begin();

  std::stringstream units;  
  iter++;
  SSTPragma::tokenStreamToString(tokens.begin(), iter, units);

  std::stringstream sval;
  SSTPragma::tokenStreamToString(iter, tokens.end(), sval);
  units_ = units.str();
  amount_ = sval.str();
}

SSTCallFunctionPragma::SSTCallFunctionPragma(SourceLocation loc, const std::list<Token> &tokens)
{
  std::stringstream sstr;
  SSTPragma::tokenStreamToString(tokens.begin(), tokens.end(), sstr);
  sstr << ";"; //semi-colon not required in pragma
  repl_ = sstr.str();
}

SSTStackAllocPragma::SSTStackAllocPragma(SourceLocation loc, std::map<std::string, std::list<std::string>>&& in_args)
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
    errorAbort(loc, "cannot have both free and alloc for stack pragma");
  }

  if (!args.empty()){
    errorAbort(loc, "invalid token passed to pragma: allowed alloc,free");
  }
}

void
SSTStackAllocPragma::activate(Stmt *s)
{
  if (toFree_.size()){
    std::string repl = "sstmac_free_stack(" + toFree_ + ")";
    replace(s, repl);
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
      errorAbort(s, error);
    }
    }

    toRepl = SkeletonASTVisitor::getUnderlyingExpr(toRepl);

    std::string repl = "sstmac_alloc_stack(" + stackSize_ + "," + mdataSize_ + ")";
    replace(toRepl, repl);
    throw StmtDeleteException(s);
  }
}

using namespace modes;
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
static PragmaRegister<SSTArgMapPragmaShim, SSTNullVariablePragma, true> nullVariablePragma(
    "sst", "null_ptr", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTStringPragmaShim, SSTEmptyPragma, true> emptyPragma(
    "sst", "empty", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTStringPragmaShim, SSTGlobalVariablePragma, true> globalPragma(
    "sst", "global", ALL_MODES);
static PragmaRegister<SSTStringPragmaShim, SSTReturnPragma, true> returnPragma(
    "sst", "return", SKELETONIZE | SHADOWIZE | PUPPETIZE);
static PragmaRegister<SSTNoArgsPragmaShim, SSTAssumeTruePragma, true> assumeTruePragma(
    "sst", "assume_true", SKELETONIZE | SHADOWIZE | PUPPETIZE);
static PragmaRegister<SSTNoArgsPragmaShim, SSTAssumeFalsePragma, true> assumeFalsePragma(
    "sst", "assume_false", SKELETONIZE | SHADOWIZE | PUPPETIZE);
static PragmaRegister<SSTStringPragmaShim, SSTBranchPredictPragma, true> branchPredictPragma(
    "sst", "branch_predict", SKELETONIZE | SHADOWIZE | PUPPETIZE);
static PragmaRegister<SSTTokenListPragmaShim, SSTAdvanceTimePragma, true> advanceTimePragma(
    "sst", "advance_time", SKELETONIZE | SHADOWIZE | ENCAPSULATE);
static PragmaRegister<SSTTokenListPragmaShim, SSTCallFunctionPragma, true> callFunctionPragma(
    "sst", "call", SKELETONIZE | SHADOWIZE | PUPPETIZE);
static PragmaRegister<SSTStringPragmaShim, SSTOverheadPragma, true> overheadPragma(
    "sst", "overhead", SKELETONIZE | SHADOWIZE | ENCAPSULATE);
static PragmaRegister<SSTArgMapPragmaShim, SSTNonnullFieldsPragma, true> nonnullFieldPragma(
    "sst", "nonnull_fields", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTArgMapPragmaShim, SSTNullFieldsPragma, true> nullFieldPragma(
    "sst", "null_fields", SKELETONIZE | SHADOWIZE);
static PragmaRegister<SSTArgMapPragmaShim, SSTStackAllocPragma, true> stackAllocPragma(
    "sst", "stack_alloc", ALL_MODES);
static PragmaRegister<SSTArgMapPragmaShim, SSTBlockingPragma, true> blockingPragma(
    "sst", "blocking", SKELETONIZE | ENCAPSULATE);


