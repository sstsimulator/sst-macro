/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

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

#ifndef bin_clang_globalvarnamespace_H
#define bin_clang_globalvarnamespace_H

#include <string>
#include <set>
#include <map>
#include <ostream>
#include <sstream>
#include "clangHeaders.h"

struct GlobalVarNamespace
{
  GlobalVarNamespace() : testPrefix(nullptr) {
    testPrefix = getenv("SSTMAC_CLANG_TEST_PREFIX");
  }

  struct Variable {
    bool isFxnStatic;
    bool isThreadLocal;
  };

  std::string ns;
  std::map<std::string, Variable> replVars;
  std::list<std::string> relocations;
  std::set<std::string> relocationOffsets;
  std::map<std::string, GlobalVarNamespace> subspaces;
  std::string filenamePrefix;

  /** a prefix to use in test suites to avoid fileame diffs */
  const char* testPrefix;

  bool empty() const {
    return replVars.empty() && subspaces.empty();
  }

  bool variableDefined(const std::string& scopeUniqueName) const {
    return replVars.find(scopeUniqueName) != replVars.end();
  }

  const std::string& uniqueFilePrefix(clang::CompilerInstance* ci, clang::SourceLocation loc){
    if (filenamePrefix.empty()){
      char uniqueFilePrefix[512];
      ::strcpy(uniqueFilePrefix, ci->getSourceManager().getFilename(loc).str().c_str());
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
      filenamePrefix = uniqueFilePrefix;
    }
    return filenamePrefix;
  }

  void appendNamespace(const std::string& nestedNS, const std::string& newNS){
    if (ns.size() == 0){
      if (nestedNS.size() == 0){
        ns = "::" + newNS + "::";
      } else {
        ns = nestedNS + newNS + "::";
      }
    }
  }

  const std::string& nsPrefix() const {
    return ns;
  }

  const char* filePrefix(clang::CompilerInstance* ci, clang::SourceLocation loc) {
    return testPrefix ? testPrefix : uniqueFilePrefix(ci, loc).c_str();
  }

  bool relocationOffsetDeclared(const std::string& name) const {
    return relocationOffsets.find(name) != relocationOffsets.end();
  }

  void setRelocationOffsetDeclared(const std::string& name) {
    relocationOffsets.insert(name);
  }

  bool genSSTCode(std::ostream& os, const std::string& indent){
    bool nonEmpty = !replVars.empty();
    for (auto& pair : replVars){
      auto& name = pair.first;
      Variable& var = pair.second;
      os << indent << "extern int __sizeof_" << name << ";\n";
      if (!var.isFxnStatic)
        os << indent << "extern void* __ptr_" << name << ";\n";
      os << indent << "int __offset_" << name << " = "
         << "sstmac::GlobalVariable::init(" 
              << "__sizeof_" << name
              << ",\"" << name << "\"";
      if (var.isFxnStatic){
        os << ",nullptr";
      } else {
        os  << ",__ptr_" << name;
      }
      os << "," << std::boolalpha << var.isThreadLocal
         << ");\n";
    }

    for (auto& str : relocations){
      os << "\n" << str << "\n";
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
