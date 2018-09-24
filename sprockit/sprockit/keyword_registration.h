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

#ifndef sprockit_keyword_registration_H
#define sprockit_keyword_registration_H

#include <sprockit/spkt_config.h>
#include <unordered_map>
#include <unordered_set>

#include <string>
#include <cstdio>
#include <list>

namespace sprockit {

struct SpktKeyword {
  const char* name;
  const char* descr;
  bool isNumeric = false;

  SpktKeyword(const char* nm, const char* dscr, bool isNum = false) :
    name(nm), descr(dscr), isNumeric(isNum)
  {
  }
};


class KeywordRegistration
{

 private:  
  static std::unordered_map<std::string, bool>* valid_keywords_;

  static std::unordered_set<std::string>* valid_namespaces_;

  static std::unordered_set<std::string>* removed_;

  static bool inited_;

  static void init();

  static bool is_digit(char c){
    return c >= 48 && c <= 57;
  }

 public:
  static void register_namespace(const std::string& ns);

  static void register_keyword(const std::string& name, bool is_numeric);

  static bool is_valid_keyword(const std::string& name);

  static bool is_valid_namespace(const std::string& ns);

  static void validate_namespace(const std::string& ns);

  static void validate_keyword(const std::string& name, const std::string& val);

  static void delete_statics();

  static bool do_validation_;

};

class StaticNamespaceRegister
{
 public:
  StaticNamespaceRegister(const char* ns);

  StaticNamespaceRegister(int num_ns, const char* namespaces[]);
};

class StaticKeywordRegister
{
 public:
  StaticKeywordRegister(int num_keywords, SpktKeyword keywords[]);
};

}

#define RegisterKeywords(...) \
  static sprockit::SpktKeyword _keywords_[] = { __VA_ARGS__ }; \
  static ::sprockit::StaticKeywordRegister _keyword_register_(sizeof(_keywords_) / sizeof(sprockit::SpktKeyword), _keywords_)

#define RegisterNamespaces(...) \
  static const char* _namespaces_[] = { __VA_ARGS__ }; \
  static ::sprockit::StaticNamespaceRegister _namespace_register_(sizeof(_namespaces_) / sizeof(const char*), _namespaces_)

#endif
