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

#ifndef bin_clang_globalvarnamespace_H
#define bin_clang_globalvarnamespace_H

#include <string>
#include <set>
#include <map>
#include <ostream>
#include <sstream>

struct GlobalVarNamespace
{
  GlobalVarNamespace() : isPrefixSet(false), testPrefix(nullptr) {
    testPrefix = getenv("SSTMAC_CLANG_TEST_PREFIX");
  }

  std::string ns;
  std::set<std::string> replVars;
  std::map<std::string, GlobalVarNamespace> subspaces;
  char uniqueFilePrefix[256];
  const char* testPrefix;
  bool isPrefixSet;

  bool empty() const {
    return replVars.empty() && subspaces.empty();
  }

  void setFilePrefix(const char* name){
    ::strcpy(uniqueFilePrefix, name);
    int len = ::strlen(uniqueFilePrefix);
    for (int i=0; i < len; ++i){
      switch (uniqueFilePrefix[i]){
        case '-':
        case '/':
        case '.':
          uniqueFilePrefix[i] = '_';
          break;
      }
    }
    isPrefixSet = true;
  }

  void appendNamespace(const std::string& nestedNS, const std::string& newNS){
    if (ns.size() == 0){
      ns = nestedNS + "::" + newNS + "::";
    }
  }

  const std::string& nsPrefix() const {
    return ns;
  }

  const char* filePrefix() const {
    return testPrefix ? testPrefix : uniqueFilePrefix;
  }

  bool genSSTCode(std::ostream& os, const std::string& indent){
    bool nonEmpty = !replVars.empty();
    for (const std::string& var : replVars){
      os << indent << "int __offset_" << var << " = 0;\n";
      os << indent << "extern int __sizeof_" << var << ";\n";
      os << indent << "extern void* __ptr_" << var << ";\n";
      os << indent << "sstmac::GlobalVariable __gv_" << var
              << "(__offset_" << var
              << ",__sizeof_" << var
              << ",__ptr_" << var
              << ");\n";
    }
    for (auto& pair : subspaces){
      std::stringstream sstr;
      bool subNotEmpty = false;
      if (!pair.second.empty()){
        sstr << indent << "namespace " << pair.first << " {\n";
        subNotEmpty |= pair.second.genSSTCode(sstr, indent + " ");
        sstr << indent << "}\n";
      }
      if (subNotEmpty) os << sstr.str();
      nonEmpty |= subNotEmpty;
    }
    return nonEmpty;
  }

};

#endif
