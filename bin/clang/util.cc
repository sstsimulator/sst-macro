/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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

using namespace clang;
using namespace clang::driver;
using namespace clang::tooling;

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
errorAbort(SourceLocation loc, CompilerInstance &CI, const std::string &error)
{
  std::string errorStr;
  llvm::raw_string_ostream os(errorStr);
  loc.print(os, CI.getSourceManager());
  os << " error: " << error;
  std::cerr << os.str() << std::endl;
  exit(EXIT_FAILURE);
}

void
warn(SourceLocation loc, CompilerInstance &CI, const std::string &warning)
{
  std::string errorStr;
  llvm::raw_string_ostream os(errorStr);
  loc.print(os, CI.getSourceManager());
  os << " WARNING: " << warning;
  std::cerr << os.str() << std::endl;
}