/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/software/process/key.h>
#include <sstmac/common/thread_info.h>
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

key::category key::general("General");
spkt_unordered_map<std::string,int>* key::category_name_to_id_ = 0;
spkt_unordered_map<int,std::string>* key::category_id_to_name_ = 0;
uint64_t key::key_storage_size_ = 0;
static sprockit::need_delete_statics<key> del_statics;
static thread_lock lock_;

key::category::category(const std::string &name) :
  id_(-1)
{
  id_ = key::allocate_category_id(name);

}

key::category::category() :
  id_(-1)
{
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
  blocked_thread_.first = 0;
  blocked_thread_.second = 0;
}

key::key(const category& cat) :
  keyname_id_(cat.id()),
  timed_out_(false)
{
  blocked_thread_.first = 0;
  blocked_thread_.second = 0;
}

key*
key::construct()
{
  return new key;
}

key*
key::construct(const category &name)
{
  return new key(name);
}


key::~key()
{
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
    keypool::chunks = 0;
  }
  delete category_name_to_id_;
  delete category_id_to_name_;
}

}
} // end of namespace sstmac.

