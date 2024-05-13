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

#include "memoizePragma.h"
#include "clangGlobals.h"
#include "memoizeVariableCaptureAnalyzer.h"

namespace {

template <typename T> T getNonNull(T t) {
  if (t == nullptr) {
    llvm::errs() << "Tried to access null pointer in memoizePragma.cc";
    exit(EXIT_FAILURE);
  }

  return t;
}

std::string cleanPath(std::string const &p) {
  // Don't care if it doesn't work on Windows
  auto root_end = p.find_last_of('/') + 1;
  auto out = p.substr(root_end);
  for (auto &c : out) {
    if (c == '.') {
      c = '_';
    }
  }

  return out;
}

std::string generateUniqueFunctionName(clang::SourceLocation const &Loc,
                                       clang::NamedDecl const *Decl,
                                       std::string Prefix) {
  static int counter = 0;
  Prefix += (Prefix.empty() ? "f" : "") + std::to_string(counter++);
  Prefix += "_" + Decl->getNameAsString() + "_";

  auto &SM = CompilerGlobals::SM();
  std::string path = SM.getFilename(Loc).str();
  Prefix += cleanPath(path) + std::to_string(SM.getPresumedLineNumber(Loc));

  return Prefix;
}

auto parseKeyword(PragmaArgMap const &Strings, std::string const &Key) {
  using ContainerType = std::vector<std::string>;

  // If Key was in the arguments and it is not an empty list return it.
  if (auto Vars = Strings.find(Key);
      Vars != Strings.end() && not Vars->second.empty()) {
    return std::make_optional<ContainerType>(Vars->second.begin(),
                                             Vars->second.end());
  }

  return std::optional<ContainerType>();
}

// This list will need to extend overtime
template <typename StmtDecl, typename Container>
auto getAllRequestedVarDecls(
    StmtDecl const *SD, std::optional<Container> const &VariableNames,
    std::optional<Container> const &MetaVariableNames) {

  // Always match some variables
  memoizationAutoMatcher(SD);

  auto FoundVariables =
      VariableNames ? matchNamedUsedVariables(SD, CompilerGlobals::ASTContext(),
                                              *VariableNames)
                    : matchNonLocalUsedVariables(SD, CompilerGlobals::ASTContext());

  if (MetaVariableNames) { // Maybe empty
    matchNamedMetaVariables(SD, CompilerGlobals::ASTContext(),
                            *MetaVariableNames);
  }

  return FoundVariables;
}

template <typename Fn>
std::string commaSepVars(std::vector<memoize::Variable> const &Vars, Fn &&f) {
  std::string out;

  bool isFirst = true;
  for (auto const &Var : Vars) {
    std::string comma = (isFirst) ? "" : ", ";
    isFirst = false;

    out += comma + f(Var);
  }

  return out;
}

std::string printCaptureBody(std::vector<memoize::Variable> const &Variables) {
  return "memoize::print_types(" +
         commaSepVars(Variables, [](auto const &V) { return V.getName(); }) +
         ");";
}

template <typename StmtDecl>
std::function<std::string(std::vector<memoize::Variable> const &)>
getFuncBodyGenerator(StmtDecl const *SD,
                     std::optional<std::string> const &FunctionType) {

  auto type = FunctionType.value_or("print"); // print is the default;
  if (type == "print") {
    return printCaptureBody;
  }

  errorAbort(SD, "Memoize Type(" + type +
                 ") was not reconized in SSTMemoizePragma.\n");

  return {};
}

clang::NamedDecl const *getNonNullParentDecl(clang::Stmt const *S) {
  return getNonNull(getParentDecl(S, CompilerGlobals::ASTContext()));
}

} // namespace

namespace memoize {
MemoizationStrings::MemoizationStrings(
    std::string const &name, std::vector<Variable> const &Variables,
    std::function<std::string(std::vector<Variable> const &)> fn) {

  std::string externC = CompilerGlobals::LangOpts().CPlusPlus ? "extern \"C\" " : "";

  decleration_ =
      externC + "void " + name + "(" +
      commaSepVars(Variables, [](auto const &V) { return V.getDeclType(); }) +
      ");";

  callsite_ = name + "(" +
              commaSepVars(Variables,
                           [](auto const &V) { return V.getQualifiedName(); }) +
              ");";

  // C++ global file always gets extern "C"
  definition_ = "extern \"C\" void " + name + "(" +
                commaSepVars(Variables,
                             [](auto const &V) {
                               return V.getDeclType() + " " + V.getName();
                             }) +
                "){" + fn(Variables) + "}";
}

SSTMemoizePragma::SSTMemoizePragma(clang::SourceLocation /*Loc*/,
                                   PragmaArgMap &&PragmaStrings)
    : VariableNames_(parseKeyword(PragmaStrings, "variables")),
      MetaVariableNames_(parseKeyword(PragmaStrings, "meta_variables")) {}

void SSTMemoizePragma::activate(clang::Stmt *S) {
  // Since we are a statement we will need to find the decl in which we were
  // declared, if it doesn't have a name then this throws.  Usually that would
  // mean it was the translation unit decl.
  auto ParentDecl = getNonNullParentDecl(S);

  // Captures all of the variables that were matched using the VariableNames_
  // and MetaVariableNames_ members
  std::vector<Variable> Variables;
  for (auto Var :
       getAllRequestedVarDecls(S, VariableNames_, MetaVariableNames_)) {
    Variables.emplace_back(Var, CompilerGlobals::ASTContext());
  }

  // Name for the new memoization function we are going to write.
  auto FuncName = generateUniqueFunctionName(getStart(S), ParentDecl, "");
  auto FuncBodyGen = getFuncBodyGenerator(S, CaptureType_);
  Strings_ = MemoizationStrings(FuncName, Variables, FuncBodyGen);

  CompilerGlobals::rewriter.InsertTextBefore(getStart(ParentDecl), Strings_.getDecleration() + "\n");
  CompilerGlobals::rewriter.InsertTextBefore(getStart(S), Strings_.getCallsite() + "\n");
} // namespace memoize

void SSTMemoizePragma::activate(clang::Decl *D) { }

void SSTMemoizePragma::deactivate() {
  auto &vec = CompilerGlobals::toolInfoRegistration.globalCppFunctionsToWrite;
  auto pragma = static_cast<SSTPragma *>(this);

  if (std::none_of(vec.begin(), vec.end(), [](auto const &pragma_string) {
        return pragma_string.second == "#include \"capture.h\"\n";
      })) {
    vec.push_back(std::make_pair(pragma, "#include \"capture.h\"\n"));
  }

  vec.push_back(std::make_pair(pragma, Strings_.getDefinition()));
}
} // namespace memoize

static PragmaRegister<SSTArgMapPragmaShim, memoize::SSTMemoizePragma, true>
    annotatePragma("sst", "memoize", modes::MEMOIZE);
