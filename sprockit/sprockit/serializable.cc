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

#include <sprockit/serializable.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sprockit/errors.h>
#include <cstring>
#include <iostream>

namespace sprockit {

static need_delete_statics<serializable_factory> del_statics;
serializable_factory::builder_map* serializable_factory::builders_ = nullptr;

uint32_t
serializable_factory::add_builder(serializable_builder* builder, const char* name)
{
  if (builders_ == nullptr) {
    builders_ = new builder_map;
  }

  const char* key = name;
  int len = ::strlen(key);
  uint32_t hash = 0;
  for(int i = 0; i < len; ++i)
  {
    hash += key[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);


  builder_map& bmap = *builders_;
  serializable_builder*& current = bmap[hash];
  if (current != 0){
    std::cerr << sprockit::printf(
      "amazingly %s and %s both hash to same serializable id %u",
      current->name(), builder->name(), hash) << std::endl;
    abort();
  }
  current = builder;
  return hash;
}

void
serializable_factory::delete_statics()
{
  delete_vals(*builders_);
  delete builders_;
}

serializable*
serializable_factory::get_serializable(uint32_t cls_id)
{
  auto it = builders_->find(cls_id);
  if (it == builders_->end()) {
    spkt_abort_printf("class id %ld is not a valid serializable id",
                     cls_id);
  }
  serializable_builder* builder = it->second;
  return builder->build();
}

}