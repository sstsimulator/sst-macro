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

#include "clangGlobals.h"

#include "frontendActions.h"
#include "globalVarNamespace.h"
#include "pragmas.h"
#include "replacePragma.h"
#include "computePragma.h"
#include <sstream>
#include <fstream>
#include <iostream>
#include <sstmac/common/sstmac_config.h>

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

ReplaceAction::ReplaceAction() :
  first_pass_visitor_(pragmaList_),
  skeleton_visitor_(pragmaList_, globalNs_)
{
}

bool
#if CLANG_VERSION_MAJOR <=4
ReplaceAction::BeginSourceFileAction(CompilerInstance &CI, llvm::StringRef Filename)
#else
ReplaceAction::BeginSourceFileAction(CompilerInstance &CI)
#endif
{
  ci_ = &CI;
  return true;
}

class DeleteOpenMPPragma : public PragmaHandler
{
 public:
  DeleteOpenMPPragma() : PragmaHandler("omp") {}
#if CLANG_VERSION_MAJOR >= 9
  void HandlePragma(Preprocessor &PP, PragmaIntroducer Introducer, Token &FirstToken) override {}
#else
  void HandlePragma(Preprocessor &PP, PragmaIntroducerKind Introducer, Token &FirstToken) override {}
#endif
};

std::unique_ptr<clang::ASTConsumer>
ReplaceAction::CreateASTConsumer(clang::CompilerInstance& CI, clang::StringRef /* file */) {
  CompilerGlobals::rewriter.setSourceMgr(CI.getSourceManager(), CI.getLangOpts());
  CompilerGlobals::ci = &CI;
  initPragmas();
  return llvm::make_unique<SkeletonASTConsumer>(first_pass_visitor_, skeleton_visitor_);
}

void
ReplaceAction::ExecuteAction()
{
  if (!ci_->hasSema()) ci_->createSema(getTranslationUnitKind(), nullptr);

  ASTConsumer& Consumer = ci_->getASTConsumer();
  Sema& S = ci_->getSema();
  CompilerGlobals::setup(ci_);

  //bool PrintStats = false;
  // Also turn on collection of stats inside of the Sema object.
  //bool OldCollectStats = PrintStats;
  //std::swap(OldCollectStats, S.CollectStats);

  bool SkipFunctionBodies = false;
  Parser P(S.getPreprocessor(), S, SkipFunctionBodies);

  //okay, super annoying - I have to DELETE the openmp handlers
  DeleteOpenMPPragma deleter; ci_->getPreprocessor().RemovePragmaHandler(&deleter);

  for (auto&& pair : PragmaRegisterMap::namespaces()){
    SSTPragmaNamespace* ns = pair.second;
    for (auto&& name : ns->names()){
      PragmaHandlerFactoryBase* factory = ns->getFactory(CompilerGlobals::mode, name);
      auto* handler = factory->getHandler(skeleton_visitor_.getPragmas());
      ci_->getPreprocessor().AddPragmaHandler(ns->name(), handler);
    }
  }

  S.getPreprocessor().EnterMainSourceFile();
  P.Initialize();

  SkeletonASTConsumer& myConsumer = static_cast<SkeletonASTConsumer&>(Consumer);


  Parser::DeclGroupPtrTy ADecl;
  ExternalASTSource *External = S.getASTContext().getExternalSource();
  if (External)
    External->StartTranslationUnit(&Consumer);
  for (bool AtEOF = P.ParseFirstTopLevelDecl(ADecl); !AtEOF;
       AtEOF = P.ParseTopLevelDecl(ADecl)) {
    // If we got a null return and something *was* parsed, ignore it.  This
    // is due to a top-level semicolon, an action override, or a parse error
    // skipping something.
    if (ADecl && !myConsumer.HandleTopLevelDecl(ADecl.get()))
      return;
  }
  //do this as a second pass
  //this forces all template instances to be generated
  myConsumer.runFirstPass();
  myConsumer.runSkeletonPass();

  // Process any TopLevelDecls generated by #pragma weak.
  for (Decl *D : S.WeakTopLevelDecls())
    Consumer.HandleTopLevelDecl(DeclGroupRef(D));

  Consumer.HandleTranslationUnit(S.getASTContext());
}

struct PragmaPPCallback : public PPCallbacks {

  void PragmaDirective(SourceLocation Loc, PragmaIntroducerKind  /*Introducer*/) override {
    SSTPragmaHandler::pragmaDirectiveLoc = Loc;
  }

};

void
ReplaceAction::initPragmas()
{
  /** Need this to figure out begin location of #pragma */
  CompilerGlobals::CI().getPreprocessor().addPPCallbacks(llvm::make_unique<PragmaPPCallback>());
}

void
ReplaceAction::EndSourceFileAction()
{
  SourceManager &SM = CompilerGlobals::SM();
  std::string sourceFile = SM.getFileEntryForID(SM.getMainFileID())->getName().str();
  std::string sstSourceFile, sstGlobalFile;
  std::size_t lastSlashPos = sourceFile.find_last_of('/');
  if (lastSlashPos == std::string::npos){
    sstSourceFile = "sst." + sourceFile;
    sstGlobalFile = "sstGlobals." + sourceFile + ".cpp";
  } else {
    lastSlashPos++;
    sstSourceFile = sourceFile.substr(0, lastSlashPos) + "sst." + sourceFile.substr(lastSlashPos);
    sstGlobalFile = sourceFile.substr(0, lastSlashPos) + "sstGlobals." + sourceFile.substr(lastSlashPos) + ".cpp";
  }

  std::error_code rc;
#if CLANG_VERSION_MAJOR >= 7
  llvm::raw_fd_ostream fs(sstSourceFile, rc, llvm::sys::fs::FA_Write);
#else
  llvm::raw_fd_ostream fs(sstSourceFile, rc, llvm::sys::fs::F_RW);
#endif
  CompilerGlobals::rewriter.getEditBuffer(CompilerGlobals::rewriter.getSourceMgr().getMainFileID()).write(fs);
  fs.close();



  std::ofstream ofs(sstGlobalFile.c_str());
  if (ofs.good()){
    //add the header files needed
    ofs << "#include <sstmac/software/process/cppglobal.h>\n"
        << "#include <sstmac/software/process/memoize.h>\n\n";
    globalNs_.genSSTCode(ofs,"");
    skeleton_visitor_.registerNewKeywords(ofs);
    if (skeleton_visitor_.hasCStyleMain()){
      std::string appname = skeleton_visitor_.getAppName();
      ofs << "int userSkeletonMainInitFxn(const char* name, int (*foo)(int,char**));\n"
         << "extern \"C\" int sstmac_user_main_" << appname << "(int argc, char** argv);\n"
         << "int " << appname << "_sstmac_initer = userSkeletonMainInitFxn("
           << "\"" << appname << "\",sstmac_user_main_" << appname << ");\n\n"
           << "extern \"C\" const char exe_main_name[] = \"" << appname << "\";\n";
    }

    for(auto const& pair : CompilerGlobals::toolInfoRegistration.globalCppFunctionsToWrite){
      ofs << pair.second << "\n";
    }
  } else {
    llvm::errs() << "Failed opening " << sstGlobalFile << "\n";
    exit(EXIT_FAILURE);
  }
  ofs.close();
}
