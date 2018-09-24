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

#include <sprockit/spkt_config.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/regexp.h>
#include <sprockit/errors.h>
#include <sprockit/output.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <unordered_map>

#include <cstdio>

namespace sprockit {

std::unordered_map<std::string,bool>* KeywordRegistration::valid_keywords_ = nullptr;
std::unordered_set<std::string>* KeywordRegistration::valid_namespaces_ = nullptr;
std::unordered_set<std::string>* KeywordRegistration::removed_ = nullptr;
bool KeywordRegistration::inited_ = false;
bool KeywordRegistration::do_validation_ = true;

static need_delete_statics<KeywordRegistration> del_statics;

static const char* removed_keywords[] = {
  "launch_name"
};

void
KeywordRegistration::delete_statics()
{
  if (valid_keywords_) delete valid_keywords_;
  if (removed_) delete removed_;
  if (valid_namespaces_) delete valid_namespaces_;
}

bool
KeywordRegistration::is_valid_namespace(const std::string& ns)
{
  return true;
}

bool
KeywordRegistration::is_valid_keyword(const std::string& name)
{
  init();

  auto it = valid_keywords_->find(name);
  if (it != valid_keywords_->end()) {
    bool isNum = it->second;
    if (isNum){
      spkt_abort_printf("got parameter %s without numeric suffix", name.c_str());
    }
    return true;
  }

  std::string substr;
  for (int i=0; i < name.size(); ++i){
    char c = name.data()[i];
    if (is_digit(c)){
      substr = name.substr(0, i);
      break;
    }
  }



  it = valid_keywords_->find(substr);
  if (it != valid_keywords_->end()){
    bool isNum = it->second;
    if (isNum){
      return true;
    }
  }

  return false;
}

void
KeywordRegistration::register_namespace(const std::string &name)
{
  init();
  valid_namespaces_->insert(name);
}

void
KeywordRegistration::register_keyword(const std::string &name, bool is_numeric)
{
  init();
  valid_keywords_->insert(std::make_pair(name,is_numeric));
}

void
KeywordRegistration::init()
{
  if (inited_) {
    return;
  }

  valid_keywords_ = new std::unordered_map<std::string,bool>;
  valid_namespaces_ = new std::unordered_set<std::string>;

  inited_ = true;

  removed_ = new std::unordered_set<std::string>;
  int num_removed = sizeof(removed_keywords) / sizeof(const char*);
  for (int i=0; i < num_removed; ++i) {
    removed_->insert(removed_keywords[i]);
  }

}

void
KeywordRegistration::validate_namespace(const std::string &ns)
{
  if (do_validation_){
    bool valid = is_valid_namespace(ns);
    if (!valid) {
      spkt_abort_printf("namespace %s is not valid - if this is not a type-o ensure that "
                        "namespace was properly registered with RegisterNamespaces(...) macro",
                        ns.c_str());
    }
  }
}

void
KeywordRegistration::validate_keyword(const std::string &name,
                                      const std::string &val)
{
  if (do_validation_) {
    bool valid = is_valid_keyword(name);
    if (!valid) {
      bool removed = removed_->find(name) != removed_->end();
      if (removed) {
        cerr0 << "WARNING: deprecated keyword " << name <<
                  " is no longer used and is being ignored.\n"
                  << "You should remove it from parameter files as it may become an error in future versions.\n";
      } else {
        spkt_abort_printf("unknown keyword name %s with value %s - if this is not a type-o ensure that "
                          "keyword was properly registered with RegisterKeywords(...) macro",
                         name.c_str(), val.c_str());
      }
    }
  }
}

StaticNamespaceRegister::StaticNamespaceRegister(const char *ns)
{
  KeywordRegistration::register_namespace(ns);
}

StaticNamespaceRegister::StaticNamespaceRegister(int num_ns, const char *namespaces[])
{
  for (int i=0; i < num_ns; ++i){
    const char* ns = namespaces[i];
    KeywordRegistration::register_namespace(ns);
  }
}

StaticKeywordRegister::StaticKeywordRegister(int num_keywords, SpktKeyword keywords[])
{
  for (int i=0; i < num_keywords; ++i) {
    SpktKeyword& kw = keywords[i];
    KeywordRegistration::register_keyword(kw.name, kw.isNumeric);
  }
}

}
