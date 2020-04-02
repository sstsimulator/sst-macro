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
/**
Copyright 2009-2019 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2019, NTESS

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

#ifndef bin_clang_memoizeVariableCaputreAnalyzer_H
#define bin_clang_memoizeVariableCaputreAnalyzer_H

#include "clang/ASTMatchers/ASTMatchFinder.h"

namespace detail {
// Mainly used to get a unique list of ast Nodes since
template <typename T, unsigned N>
auto toPtrSet(
    llvm::SmallVector<clang::ast_matchers::BoundNodes, N> const &Nodes,
    std::string const &ID) {
  llvm::SmallPtrSet<T const *, 4> VarDecls;

  for (auto const &Match : Nodes) {
    if (auto Var = Match.template getNodeAs<T>(ID)) {
      VarDecls.insert(Var);
    }
  }

  return VarDecls;
}

template <typename Container> std::string makeNameRegex(Container const &C) {
  std::string NameRegex;
  auto first = true;
  for (auto const &Name : C) {
    NameRegex += ((first) ? "" : "|") + Name;
    first = false;
  }

  return NameRegex;
}
} // namespace detail

template <typename StmtDecl>
auto matchNonLocalUsedVariables(StmtDecl const *SD, clang::ASTContext &Ctx,
                                std::string nameRegex = " ") {
  using namespace clang::ast_matchers;
  // clang-format off
  return detail::toPtrSet<clang::VarDecl>(
      match(
        findAll( // All
          declRefExpr( // DeclRefExprs
            to(
              varDecl( // that refer to Variables
                unless( // unless they descend from the current node
                  hasAncestor( 
                    equalsNode(SD)
                  )
                ),
                unless( // or are blacklisted
                  matchesName(nameRegex)
                )
              ).bind("varDecls") // and bind them to BindId
            )
          )
        )
      , *SD, Ctx) , "varDecls");
  // clang-format on
}

template <typename StmtDecl, typename Container>
auto matchNamedUsedVariables(StmtDecl const *SD, clang::ASTContext &Ctx,
                             Container const &VariableNames) {
  assert(not VariableNames.empty());

  using namespace clang::ast_matchers;
  auto NameRegex = detail::makeNameRegex(VariableNames);
  // clang-format off
  return detail::toPtrSet<clang::VarDecl>(
      match(
        findAll(
          declRefExpr( // DeclRefExprs
            to(
              varDecl( // that are variables 
                matchesName(NameRegex)  // and has one of the names given
              ).bind("varDecls")
            )
          )
        )
      , *SD, Ctx), "varDecls");
  // clang-format on
}

template <typename StmtDecl, typename Container>
auto matchNamedMetaVariables(StmtDecl const *SD, clang::ASTContext &Ctx,
                             Container const &MetaVariableNames) {
  assert(not MetaVariableNames.empty());
  llvm::errs() << "Warning: meta_variables are currently disabled due to "
                  "matching difficulty.\n";
  return llvm::SmallPtrSet<clang::VarDecl const *, 4>{};
}

inline clang::NamedDecl const *getParentDecl(clang::Stmt const *S,
                                             clang::ASTContext &Ctx) {
  using namespace clang::ast_matchers;
  // clang-format off
  return selectFirst<clang::NamedDecl>("ID", match(
        stmt(
          equalsNode(S),
          hasAncestor(
            namedDecl().bind("ID")
          )
        ), *S, Ctx));
  // clang-format on
}

void memoizationAutoMatcher(clang::Stmt const *S);

#endif //  bin_clang_memoizeVariableCaputreAnalyzer_H
