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

#include "memoizeVariableCaptureAnalyzer.h"
//#include "clang/Analysis/Analyses/ExprMutationAnalyzer.h"

#include "clangGlobals.h"
#include "util.h"

#include <optional>

namespace {
using namespace clang;
using namespace ast_matchers;

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

// Needs to be an optional because we want nullopt if there is no init condition
std::optional<std::pair<VarDecl const *, Expr const *>>
getLoopInitVar(ForStmt const *FS) {
  if (FS->getInit() == nullptr) {
    return std::nullopt;
  }

  // clang-format off
     auto BN = match(
        forStmt(
          hasLoopInit(
            declStmt(
              hasSingleDecl(
                varDecl(
                  hasInitializer(
                    expr(
                      unless(
                        integerLiteral()
                      )
                    ).bind("Init Expression")
                  )
                ).bind("InitVar")
              )
            )
          )
        )
      , *FS, CompilerGlobals::ASTContext());
  // clang-format on
  //
  Expr const *Exp = nullptr;
  VarDecl const *VD = nullptr;
  for (auto const &Node : BN) {
    Exp = Node.getNodeAs<Expr>("Init Expression");
    VD = Node.getNodeAs<VarDecl>("InitVar");
  }

  return std::make_optional(std::make_pair(VD, Exp));
}

std::optional<llvm::SmallPtrSet<VarDecl const *, 4>>
getLoopCondition(ForStmt const *FS, VarDecl const *LoopInit) {
  auto Condition = FS->getCond();
  if (Condition == nullptr) { // Don't handle conditionless situations
    return std::nullopt;
  }

  // clang-format off
  return std::make_optional(::detail::toPtrSet<VarDecl>(
      match(
        findAll(
          forStmt(
            hasCondition(
              binaryOperator(
                hasDescendant(
                  declRefExpr(
                    to(
                      varDecl(
                        unless(
                          equalsNode(LoopInit)
                        )
                      ).bind("ID")
                    )
                  )
                )
              )
            )
          )
        ), *FS, CompilerGlobals::ASTContext()), "ID"));
  // clang-format on
}

void forStmtVariableAutoMatcher(clang::ForStmt const *FS) {
  /*
   *  Get all the Variables declared in our ForStmt
   */
//  auto getLoopVars =
//      findAll(declStmt(forEachDescendant(varDecl().bind("LoopDeclared"))));

auto getLoopVars = forEachDescendant(declStmt(forEach(varDecl().bind("LoopDeclared"))));

  /*
   *  Get all the conditions in our ForStmt
   */
  // auto getConditionExpr = hasCondition(expr().bind("Condition"));

  // clang-format off
  //  auto getAllConditions = 
  //      findAll(
  //        stmt( // Required since these all return different stmt types, I think
  //          anyOf(
  //            forStmt(getConditionExpr), ifStmt(getConditionExpr),
  //            doStmt(getConditionExpr), whileStmt(getConditionExpr),
  //            switchStmt(getConditionExpr), conditionalOperator(getConditionExpr),
  //            binaryConditionalOperator(getConditionExpr)
  //          )
  //        )
  //      );
  
  // clang-format on

  // auto dependsOnLoopDeclaredVar = findAll(
  //     declRefExpr(
  //       to(
  //         varDecl(
  //           equalsBoundNode("LoopDeclared")
  //         )
  //       )
  //     )
  //   );
  //auto cameFromCondition = hasAncestor(expr(equalsBoundNode("Condition")));
  
  auto dependsOnLoopDeclaredVar = findAll(
      declRefExpr(
        to(
          varDecl(
            equalsBoundNode("Declared")
          )
        )
      )
    );

  auto getConditionExpr = hasCondition(expr().bind("ConditionExpr"));
  auto getAllConditions = 
      findAll(
        stmt( // I think this is required since these all return different stmt types
          anyOf(
            forStmt(getConditionExpr), ifStmt(getConditionExpr)
          )
        )
      );

  auto cameFromCondition = hasAncestor(expr(equalsBoundNode("ConditionExpr")));
  auto BN =
      match(
          stmt(
            // Get all variables declared in the loop
            anyOf(
              forEachDescendant(declStmt(forEach(varDecl().bind("Declared")))),
              anything()
            ),
            getAllConditions,
            forEachDescendant(
              expr(
                hasAncestor(expr(equalsBoundNode("ConditionExpr"))),
                unless(
                  findAll(declRefExpr(to(varDecl(equalsBoundNode("Declared")))))
                )
              ).bind("InnerExpr")
            )
        )
        , *FS, CompilerGlobals::ASTContext());  // FS is a ForStmt

  // auto BN =
  //     match(stmt(
  //             eachOf(
  //               getLoopVars
  //               ,getAllConditions
  //             )
  //             ,forEachDescendant(
  //               expr(
  //                 cameFromCondition
  //                 ,unless(
  //                    dependsOnLoopDeclaredVar
  //                 )
  //             //  //    // ,hasType(asString("int"))
  //             //     // ,unless(
  //             //     //   integerLiteral()
  //             //     // )
  //                ).bind("ConditionExpr")
  //             )
  //         ), *FS, *sst::activeASTContext);
  // clang-format on

  llvm::errs() << "\n";
  // FS->dumpPretty(*sst::activeASTContext);
  FS->dumpColor();
  llvm::errs() << "\nDeclared\n";
  for (auto const &VD : ::detail::toPtrSet<VarDecl>(BN, "Declared")) {
    llvm::errs() << VD->getNameAsString() << ", ";
  }

  llvm::errs() << "\n\nConditionsExpr\n";
  for (auto const &exp : ::detail::toPtrSet<Expr>(BN, "ConditionExpr")) {
    exp->dumpPretty(CompilerGlobals::ASTContext());
    llvm::errs() << "\n";
  }

  llvm::errs() << "\nInner Exprs\n";
  for (auto const &exp : ::detail::toPtrSet<Expr>(BN, "InnerExpr")) {
    exp->dumpPretty(CompilerGlobals::ASTContext());
    llvm::errs() << "\n";
  }

  llvm::errs() << "\n\n\n";
}

} // namespace

void memoizationAutoMatcher(clang::Stmt const *S) {
  if (auto FS = llvm::dyn_cast<clang::ForStmt>(S)) {
    forStmtVariableAutoMatcher(llvm::dyn_cast<clang::ForStmt>(S));
  } else {
    S->dumpColor();
  }
}
