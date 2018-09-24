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

#include <sstmac/software/process/key.h>
#include <sstmac/software/process/thread_info.h>
#include <sstmac/common/thread_lock.h>
#include <sprockit/output.h>
#include <sprockit/statics.h>
#include <sprockit/errors.h>
#include <iostream>
#include <stdlib.h>

#include <vector>

namespace sstmac {
namespace sw {

std::unordered_map<std::string,int>* ftq_tag::category_name_to_id_ = nullptr;
std::unordered_map<int,std::string>* ftq_tag::category_id_to_name_ = nullptr;
ftq_tag ftq_tag::null("Null");
ftq_tag ftq_tag::compute("Compute");

ftq_tag::ftq_tag(const char *name)
{
  id_ = allocate_category_id(name);
}

int
ftq_tag::allocate_category_id(const std::string &name)
{
  if (!category_name_to_id_) {
    category_name_to_id_ = new std::unordered_map<std::string,int>;
    category_id_to_name_ = new std::unordered_map<int,std::string>;
  }
  int id = category_name_to_id_->size();
  (*category_name_to_id_)[name] = id;
  (*category_id_to_name_)[id] = name;
  return id;
}

int
ftq_tag::event_typeid(const std::string& name)
{
  auto it = category_name_to_id_->find(name);
  if (it == category_name_to_id_->end()){
    spkt_throw_printf(sprockit::value_error,
      "key::event_typeid: unknown event name %s",
      name.c_str());
  }
  return it->second;
}

}
} // end of namespace sstmac.
