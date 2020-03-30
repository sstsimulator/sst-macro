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

#ifndef bin_clang_annotatePragma_h
#define bin_clang_annotatePragma_h

#include "pragmas.h"

namespace annotate {

class ToolInfo {
public:
  ToolInfo(std::string T, std::vector<std::string> A)
      : ToolName_(std::move(T)), ToolArgs_(std::move(A)) {}

  ToolInfo() = default;

  std::string const &Name() const { return ToolName_; }
  std::vector<std::string> const &Args() const { return ToolArgs_; }

private:
  std::string ToolName_;
  std::vector<std::string> ToolArgs_;
};

namespace detail {
ToolInfo
getToolInfo(clang::SourceLocation Loc,
            std::map<std::string, std::list<std::string>> &&PragmaStrings);
}

class SSTAnnotatePragmaImpl : public SSTPragma {
public:
  SSTAnnotatePragmaImpl(annotate::ToolInfo &&Ti)
      : Ctx(CompilerGlobals::CI().getASTContext()), Ti_(std::move(Ti)) {
  }

  void activate(clang::Stmt *S) override;
  void activate(clang::Decl *D) override;

  clang::ASTContext &getAstContext() { return Ctx; }

  clang::ASTContext const &getAstContext() const { return Ctx; }

private:
  void activatePuppetize(clang::Stmt *S);
  void activateShadowize(clang::Stmt *S);

  void activatePuppetize(clang::Decl *D);
  void activateShadowize(clang::Decl *D);

  clang::ASTContext &Ctx;
  ToolInfo Ti_;
};

template <typename Derived>
class SSTAnnotatePragmaBase : public SSTAnnotatePragmaImpl {
public:
  SSTAnnotatePragmaBase(
      clang::SourceLocation Loc, std::map<std::string, std::list<std::string>> &&PragmaStrings)
      : SSTAnnotatePragmaImpl(Derived::getToolInfo(std::move(Loc),
                                                   std::move(PragmaStrings))) {}
};

} // namespace annotate

class SSTAnnotatePragma
    : public annotate::SSTAnnotatePragmaBase<SSTAnnotatePragma> {
public:
  SSTAnnotatePragma(
      clang::SourceLocation Loc,
      std::map<std::string, std::list<std::string>> &&PragmaStrings)
      : SSTAnnotatePragmaBase<SSTAnnotatePragma>(std::move(Loc),
                                                 std::move(PragmaStrings)) {}

  static annotate::ToolInfo
  getToolInfo(clang::SourceLocation Loc,
              std::map<std::string, std::list<std::string>> &&PragmaStrings) {
    return annotate::detail::getToolInfo(std::move(Loc),
                                         std::move(PragmaStrings));
  }
};

#endif // bin_clang_annotatePragma_h
