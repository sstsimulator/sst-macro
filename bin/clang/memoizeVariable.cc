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

#include "clang/ASTMatchers/ASTMatchFinder.h"

#include "memoizeVariable.h"
#include <optional>

namespace {
template <typename T, unsigned N>
T const *
getFirst(llvm::SmallVector<clang::ast_matchers::BoundNodes, N> const &Nodes,
         std::string const &ID) {

  for (auto const &Node : Nodes) {
    if (auto match = Node.template getNodeAs<T>(ID)) {
      return match;
    }
  }

  return nullptr;
}

clang::CXXMethodDecl const *classHasMemberFunc(clang::VarDecl const *VD,
                                               clang::ASTContext &Ctx,
                                               std::string const &MemberName) {
  using namespace clang::ast_matchers;

  // clang-format off
  return getFirst<clang::CXXMethodDecl>(
      match(
         varDecl(
           hasType(
             hasUnqualifiedDesugaredType(
               recordType(
                hasDeclaration(
                  cxxRecordDecl(
                    hasMethod(
                      cxxMethodDecl(
                        hasName(MemberName)
                      ).bind("ID")
                    )
                  )
                )
              )
            )
          )
        )
      ,*VD, Ctx), "ID");
  // clang-format on
}
} // namespace

namespace memoize {
Variable::Variable(clang::VarDecl const *VD, clang::ASTContext &Ctx)
    : Name_(VD->getNameAsString()),
      QualifiedName_(VD->getQualifiedNameAsString()),
      Type_(VD->getType().getAsString()), defName_(Name_), Var_(VD) {
  if (auto Method = classHasMemberFunc(VD, Ctx, "c_str")) {
    declType_ = Method->getReturnType().getAsString();
    llvm::errs() << "c_str return type: "
                 << Method->getReturnType().getAsString() << "\n";
  } else if (auto Method = classHasMemberFunc(VD, Ctx, "data")) {
    declType_ = Method->getReturnType().getAsString();
    llvm::errs() << "data return type: "
                 << Method->getReturnType().getAsString() << "\n";
  } else if (VD->getType()->isArrayType() ||
             VD->getType()->isConstantArrayType()) {
    declType_ = VD->getType()
                    ->getArrayElementTypeNoTypeQual()
                    ->getCanonicalTypeInternal().getAsString();
  } else {
    declType_ = VD->getType().getAsString();
  }
}
} // namespace memoize
