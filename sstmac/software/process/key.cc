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

namespace keypool {
static std::vector<key*>* chunks = new std::vector<key*>;
} // end of namespace keypool.

key_traits::category key::general("General");
spkt_unordered_map<std::string,int>* key::category_name_to_id_ = nullptr;
spkt_unordered_map<int,std::string>* key::category_id_to_name_ = nullptr;
uint64_t key::key_storage_size_ = 0;
static sprockit::need_delete_statics<key> del_statics;
static thread_lock lock_;

namespace key_traits {

category::category(const char* name) :
  id_(-1)
{
  id_ = key::allocate_category_id(name);

}

category::category() :
  id_(-1)
{
}

}

int
key::allocate_category_id(const std::string &name)
{
  if (!category_name_to_id_) {
    category_name_to_id_ = new spkt_unordered_map<std::string,int>;
    category_id_to_name_ = new spkt_unordered_map<int,std::string>;
  }
  int id = category_name_to_id_->size();
  (*category_name_to_id_)[name] = id;
  (*category_id_to_name_)[id] = name;
  return id;
}

int
key::event_typeid(const std::string& name)
{
  auto it = category_name_to_id_->find(name);
  if (it == category_name_to_id_->end()){
    spkt_throw_printf(sprockit::value_error,
      "key::event_typeid: unknown event name %s",
      name.c_str());
  }
  return it->second;
}

key::key() :
  keyname_id_(general.id()),
  timed_out_(false)
{
  blocked_thread_.first = nullptr;
  blocked_thread_.second = nullptr;
}

key::key(const key_traits::category& cat) :
  keyname_id_(cat.id()),
  timed_out_(false)
{
  blocked_thread_.first = nullptr;
  blocked_thread_.second = nullptr;
}


void*
key::operator new(size_t size)
{
  lock_.lock();

  if(keypool::chunks->empty()) {
    const int cacheline = 64;
    const int pagesize = 4*4096;
    int window = pagesize / cacheline;
    key_storage_size_ += window;
    keypool::chunks->reserve(key_storage_size_);
    int keysize = std::max(sizeof(key), size_t(cacheline));
    char* keyptr = (char*) ::malloc(keysize*window);
    //ensure cache alignment
    int mem_offset = ((uint64_t)keyptr) % cacheline;
    if (mem_offset != 0){
      int stride = cacheline - mem_offset;
      keyptr += stride;
      window -= 1;
    }
    for (int i=0; i < window; ++i, keyptr += keysize){
      keypool::chunks->push_back(reinterpret_cast<key*>(keyptr));
    }
  }
  void* memptr = keypool::chunks->back();
  keypool::chunks->pop_back();
  lock_.unlock();
  return memptr;
}

void
key::operator delete(void* ptr)
{
  lock_.lock();
  keypool::chunks->push_back(static_cast<key*>(ptr));
  lock_.unlock();
}

void
key::delete_statics()
{
  if (keypool::chunks) {
    for (int i=0; i < keypool::chunks->size(); ++i) {
      //just leak it for now - annoying cache alignment issues
      //::free(keypool::chunks->at(i));
    }
    keypool::chunks->clear();
    delete keypool::chunks;
    keypool::chunks = nullptr;
  }
  delete category_name_to_id_;
  delete category_id_to_name_;
}

}
} // end of namespace sstmac.