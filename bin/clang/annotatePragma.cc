/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

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
/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

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

#include "annotatePragma.h"
#include "astVisitor.h"

#include "clang/ASTMatchers/ASTMatchFinder.h"

#include <list>
#include <string>
#include <utility>
#include <vector>

namespace {
clang::FunctionDecl const *getParentFunctionDecl(clang::Stmt const *S,
                                                 clang::ASTContext &Ctx) {
  for (auto const &P : Ctx.getParents(*S)) {
    if (auto const FD = P.get<clang::FunctionDecl>()) {
      return FD;
    } else if (auto const ST = P.get<clang::Stmt>()) {
      return getParentFunctionDecl(ST, Ctx);
    }
  }

  return nullptr;
}

// Writes the tool as follows
// annotate(toolname:arg1,arg2,arg3,{line1,line2,line3}) unless one of the args
// is noinline, then only write annotate(toolname:arg1,arg2,arg3)
std::string annotationStr(std::string const &ToolName,
                          std::vector<std::string> const &Args,
                          std::pair<int, int> Lines) {
  std::string Annotation = "__attribute__((annotate(\"" + ToolName + ":";
  bool Noinline = false;
  int NumWritten = 0;
  for (auto const &A : Args) {
    if (A == "noinline") {
      Noinline = true;
      continue; // Don't pass noinline to the backend
    }

    if (NumWritten > 0) {
      Annotation += ",";
    }

    Annotation += A;
    ++NumWritten;
  }

  if (!Noinline) {
    Annotation += (Args.empty()) ? "{" : ",{";
    for (auto I = Lines.first; I <= Lines.second; ++I) {
      if (I != Lines.first) {
        Annotation += ",";
      }
      Annotation += std::to_string(I);
    }
    Annotation += "}";
  }

  Annotation += "\"))) ";
  return Annotation;
}

std::string getName(std::string const &ToolStr) {
  auto Pos = ToolStr.find('(');
  if (Pos != std::string::npos) {
    return ToolStr.substr(0, Pos);
  }
  return ToolStr;
}

// Parses out the args from a tool string, the arg themselves must be comma
// separated and cannot themselves have args
std::vector<std::string> getArgs(std::string const &ToolStr) {
  // Check for Arguments
  auto ArgStart = ToolStr.find('(');
  if (ArgStart == std::string::npos) {
    return {};
  }

  auto ArgEnd = ToolStr.find_last_of(')');
  if (ArgEnd == std::string::npos) {
    llvm::errs() << "Missing final ) in: " << ToolStr << "\n";
    exit(EXIT_FAILURE);
  }

  std::vector<std::string> Args;
  auto ArgNext = ToolStr.find_first_not_of('(', ArgStart);

  // While not the final ) get comma seperated arguments
  while (ArgNext < ArgEnd) {
    // Find the next comma or the final )
    auto ArgStop = std::min(ToolStr.find(',', ArgNext), ArgEnd);

    // Get arg and remove white space
    auto Arg = ToolStr.substr(ArgNext, ArgStop - ArgNext);
    auto StrEnd = std::remove_if(Arg.begin(), Arg.end(), isspace);
    Args.emplace_back(Arg.begin(), StrEnd);

    // Advance to the next arg
    ArgNext = ToolStr.find_first_not_of(',', ArgStop);
  }

  Args.shrink_to_fit();
  return Args;
}

// Returns a pair<string,vector<string>> where first is the name of the tool and
// second is its arguments
annotate::ToolInfo parseToolStr(std::string const &Tool) {
  return annotate::ToolInfo(getName(Tool), getArgs(Tool));
}

template <typename T>
std::pair<int, int> getLines(clang::SourceManager &Sm, T const *t) {
  return {Sm.getPresumedLineNumber(getStart(t)),
          Sm.getPresumedLineNumber(getEnd(t))};
}

} // namespace

namespace annotate {
namespace detail {

annotate::ToolInfo
getToolInfo(clang::SourceLocation Loc, std::map<std::string, std::list<std::string>> &&PragmaStrings) {

  auto ToolStrIter = PragmaStrings.find("tool");
  if (ToolStrIter == PragmaStrings.end()) {
    errorAbort(Loc, "AnnotatePragma requires a tool argument");
  }

  if (PragmaStrings.size() > 1) {
    Loc.print(llvm::errs(), CompilerGlobals::SM());
    llvm::errs()
        << "\nWarning AnnotatePragma is ignoring arguments that do not "
           "match tool:\n";

    for (auto const &Arg : PragmaStrings) {
      if (Arg.first != "tool") {
        llvm::errs() << "\t" << Arg.first << "\n";
      }
    }
  }

  return parseToolStr(ToolStrIter->second.front());
}
} // namespace detail

void SSTAnnotatePragmaImpl::activate(clang::Stmt *S) {
  switch (getActiveMode()) {
  case modes::PUPPETIZE_MODE:
    return activatePuppetize(S);
  case modes::SHADOWIZE_MODE:
    return activateShadowize(S);
  default:
    errorAbort(startPragmaLoc, "Annotation Pragmas only support modes: Puppetize, Shadowize ");
  }
}

void SSTAnnotatePragmaImpl::activate(clang::Decl *D) {
  switch (getActiveMode()) {
  case modes::PUPPETIZE_MODE:
    return activatePuppetize(D);
  case modes::SHADOWIZE_MODE:
    return activateShadowize(D);
  default:
    errorAbort(startPragmaLoc, "Annotation Pragmas only support modes: Puppetize, Shadowize");
  }
}

void SSTAnnotatePragmaImpl::activatePuppetize(clang::Stmt *S) {
  if (auto ParentFunc = getParentFunctionDecl(S, Ctx)) {
    CompilerGlobals::rewriter.InsertTextBefore(getStart(ParentFunc),
                       annotationStr(Ti_.Name(), Ti_.Args(), getLines(CompilerGlobals::SM(), S)));
    S->dumpColor();
  } else {
    errorAbort(getStart(S), "Couldn't find a parent function for the statement");
  }
}

void SSTAnnotatePragmaImpl::activatePuppetize(clang::Decl *D) {
  auto LocalD = D; // If D is not a function decl get the function
  if (auto TD = llvm::dyn_cast<clang::FunctionTemplateDecl>(LocalD)) {
    LocalD = TD->getAsFunction();
  }

  CompilerGlobals::rewriter.InsertTextBefore(getStart(LocalD), annotationStr(Ti_.Name(), Ti_.Args(),
                                                     getLines(CompilerGlobals::SM(), LocalD)));
}

void SSTAnnotatePragmaImpl::activateShadowize(clang::Stmt *S) {}

void SSTAnnotatePragmaImpl::activateShadowize(clang::Decl *D) {}

} // namespace annotate

static PragmaRegister<SSTArgMapPragmaShim, SSTAnnotatePragma, true>
    annotatePragma("sst", "placeholder",
                   modes::MEMOIZE | modes::SKELETONIZE);
