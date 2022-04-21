/**
Copyright 2009-2022 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2022, NTESS

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

#include "util.h"
#include <sstream>
#include <cstring>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

PragmaConfig CompilerGlobals::pragmaConfig;
ASTContextLists CompilerGlobals::astContextLists;
ASTNodeMetadata CompilerGlobals::astNodeMetadata;
ToolInfoRegistration CompilerGlobals::toolInfoRegistration;

CompilerInstance* CompilerGlobals::ci;
Rewriter CompilerGlobals::rewriter;


bool
isValidSrc(const std::string& filename){
  //this is really dirty and not very resilient - but I don't know how to fix this yet
  //for now just check to see if this is actually a valid source file
  size_t size = filename.size();
  if (size == 0) return false;
  std::string suffix4; if (size >= 4) suffix4 = filename.substr(size-4,3);
  std::string suffix3; if (size >= 3) suffix3 = filename.substr(size-3,3);
  std::string suffix2 = filename.substr(size-2,2);
  bool valid = suffix4 == ".cpp" || suffix3 == ".cc" || suffix2 == ".c" || suffix4 == ".cxx";
  return valid;
}

bool
isCxx(const std::string& filename){
  //this is really dirty and not very resilient - but I don't know how to fix this yet
  //for now just check to see if this is actually a valid source file
  size_t size = filename.size();
  if (size == 0) return false;
  std::string suffix4; if (size >= 4) suffix4 = filename.substr(size-4,3);
  std::string suffix3; if (size >= 3) suffix3 = filename.substr(size-3,3);
  bool valid = suffix4 == ".cpp" || suffix3 == ".cc" || suffix4 == ".cxx";
  return valid;
}

void
errorAbort(SourceLocation loc, const std::string &error)
{
  std::string errorStr;
  llvm::raw_string_ostream os(errorStr);
  loc.print(os, CompilerGlobals::SM());
  os << ": error: " << error;
  std::cerr << os.str() << std::endl;
  exit(EXIT_FAILURE);
}

void
errorAbort(const Decl *decl, const std::string &error){
  std::string errorStr;
  llvm::raw_string_ostream os(errorStr);
  decl->print(os);
  std::cerr << os.str() << std::endl;
  errorAbort(getStart(decl), error);
}

void
errorAbort(const Stmt *s, const std::string &error){
  s->dumpPretty(CompilerGlobals::CI().getASTContext());
  errorAbort(getStart(s), error);
}

void
warn(const Decl *decl, const std::string &error){
  warn(getStart(decl), error);
}

void
warn(const Stmt *s, const std::string &error){
  warn(getStart(s), error);
}


void
internalError(const Decl *decl, const std::string &error){
  internalError(getStart(decl), error);
}

void
internalError(const std::string &error){
	llvm::errs() << "Internal Error: " << error << "\n";
  exit(EXIT_FAILURE);
}


void
internalError(SourceLocation loc, const std::string &error)
{
  std::string errorStr;
  llvm::raw_string_ostream os(errorStr);
  loc.print(os, CompilerGlobals::SM());
  os << ": internal error: " + error;
  std::cerr << os.str() << std::endl;
  exit(EXIT_FAILURE);
}

void internalError(const clang::Stmt* s, const std::string& error)
{
  internalError(getStart(s), error);
}

void
warn(SourceLocation loc, const std::string &warning)
{
  std::string errorStr;
  llvm::raw_string_ostream os(errorStr);
  loc.print(os, CompilerGlobals::SM());
  os << ": warning: " << warning;
  std::cerr << os.str() << std::endl;
}

void
replace(SourceRange rng, const std::string& repl)
{
  auto& CI = CompilerGlobals::CI();
  PresumedLoc start = CI.getSourceManager().getPresumedLoc(rng.getBegin());
  PresumedLoc stop = CI.getSourceManager().getPresumedLoc(rng.getEnd());
  int numLinesDeleted = stop.getLine() - start.getLine();
  std::stringstream sstr;
  sstr << repl;
  for (int i=0; i < numLinesDeleted; ++i){
    sstr << "\n";
  }
  //sstr << "\n# " << stop.getLine()
  //     << " \"" << stop.getFilename()
  //     << "\" " << stop.getColumn()
  //     << "\n";
  CompilerGlobals::rewriter.ReplaceText(rng, sstr.str());
}

void
replace(const Decl *d, const std::string &repl)
{
  replace(d->getSourceRange(), repl);
}

void
replace(const Stmt *s, const std::string &repl)
{
  replace(s->getSourceRange(), repl);
}

void
insertBefore(const Stmt *s, const std::string &text)
{
  CompilerGlobals::rewriter.InsertText(getStart(s), text, false);
}

void
insertAfter(const Stmt *s, const std::string &text)
{
  CompilerGlobals::rewriter.InsertText(getEnd(s), text, true);
}

std::string
makeCxxName(const std::string& name)
{
  char uniqueFilePrefix[1024];
  ::strcpy(uniqueFilePrefix, name.c_str());
  int len = ::strlen(uniqueFilePrefix);
  for (int i=0; i < len; ++i){
    switch (uniqueFilePrefix[i]){
      case '-':
      case '/':
      case '.':
      case ':':
        uniqueFilePrefix[i] = '_';
        break;
    }
  }
  return uniqueFilePrefix;
}

static Expr* nameToExpr(DeclContext* ctx, const std::string& name, clang::SourceLocation loc)
{
  std::vector<NamedDecl*> matches;
  for (auto* d : ctx->decls()){
    if (auto* nd = dyn_cast<NamedDecl>(d)){
      if (nd->getNameAsString() == name){
        matches.push_back(nd);
      }
    }
  }

  if (!matches.empty()){
    NamedDecl* nd = matches.back();
    VarDecl* vd = cast<VarDecl>(nd);
    if (!vd){
      std::string error = std::string("name ") + name + " + does not map to VarDecl in construction expr";
      internalError(getStart(nd), error);
    }

    DeclRefExpr* dref = DeclRefExpr::Create(
        ctx->getParentASTContext(),
        vd->getQualifierLoc(),
        SourceLocation(),
        vd,
        /*enclosing*/ false,
        loc,
        vd->getType(),
        VK_LValue,
        vd->getFirstDecl());

    return dref;
  } else {
    return nullptr;
  }
}

Expr* tokenToExpr(DeclContext* ctx, const Token& tok, clang::SourceLocation loc)
{
  switch(tok.getKind()){
    case tok::identifier: {
      std::string varName = tok.getIdentifierInfo()->getNameStart();
      return nameToExpr(ctx, varName, loc);
    }
    case tok::string_literal: {
      std::string varName = getLiteralDataAsString(tok);
      return nameToExpr(ctx, varName, loc);
    }
    case tok::kw_true: {
      llvm::APInt api(32, 1);
      IntegerLiteral* ilit = IntegerLiteral::Create(ctx->getParentASTContext(), api,
                                                    ctx->getParentASTContext().IntTy, loc);
      return ilit;
    }
    case tok::kw_false: {
      llvm::APInt api(32, 0);
      IntegerLiteral* ilit = IntegerLiteral::Create(ctx->getParentASTContext(), api,
                                                    ctx->getParentASTContext().IntTy, loc);
      return ilit;
    }
    case tok::numeric_constant: {
      std::string valueText = getLiteralDataAsString(tok);
      if (valueText.find('.') == std::string::npos){
        //make an integer literal
        int i = std::stoi(valueText.c_str());
        //just assume 32 bit integer for now
        llvm::APInt api(32, i, true);
        IntegerLiteral* ilit = IntegerLiteral::Create(ctx->getParentASTContext(), api,
                                                      ctx->getParentASTContext().IntTy, loc);
        return ilit;
      } else {
        double d = std::stof(valueText.c_str());
        llvm::APFloat apf(d);
        FloatingLiteral* flit = FloatingLiteral::Create(ctx->getParentASTContext(), apf, true,
                                                        ctx->getParentASTContext().DoubleTy, loc);
        return flit;
      }
      break;
    }
    default:
      return nullptr;
  }
}

Expr* zeroExpr(clang::SourceLocation loc)
{
  if (CompilerGlobals::astContextLists.enclosingFunctionDecls.empty()){
    internalError(loc, "no active DeclContext - cannot make zeroExpr");
  }

  FunctionDecl* fd = CompilerGlobals::astContextLists.enclosingFunctionDecls.back();
  if (!fd){
    internalError(loc, "have null DeclContext - cannot make zeroExpr");
  }

  llvm::APInt api(32, 0);
  IntegerLiteral* ilit = IntegerLiteral::Create(fd->getParentASTContext(), api,
                                                fd->getParentASTContext().IntTy, loc);
  return ilit;
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


namespace sst {
namespace strings {

std::string replace_all(std::string const &str, std::string const& match, 
                      std::string const& replacement) {
  const auto match_len = match.size();
  const auto replacement_len = replacement.size();

  // Copy the input
  std::string result_string = str;

  // Need to keep track of where to pick the search back up so that we don't
  // end up in an infinite loop if the replacement string contains the match as
  // a substring.
  auto search_start_pos = 0;

  for(;;){
    auto match_start_position = result_string.find(match, search_start_pos);
    if (match_start_position != std::string::npos) {
      result_string.replace(match_start_position, match_len, replacement);
      search_start_pos = match_start_position + replacement_len;
    } else {
      break;
    }
  }

  return result_string;
}

std::string replace_all(std::string const &str,
    std::map<std::string, std::string> const& replacements) {

  std::string result = str;
  for(auto const& pair : replacements){
    result = replace_all(result, pair.first, pair.second);
  }

  return result;
}

} // namespace strings
} // namespace sst
