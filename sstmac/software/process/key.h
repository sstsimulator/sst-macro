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

#ifndef SSTMAC_SOFTWARE_PROCESS_KEY_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_KEY_H_INCLUDED

#include <sstmac/software/threading/threading_interface.h>
#include <sstmac/software/process/thread_data.h>
#include <sprockit/unordered.h>
#include <stdint.h>
#include <sstmac/software/process/key_fwd.h>

#include <cstring>
#include <set>

namespace sstmac {
namespace sw {

/**
 * A base type and default (empty) implementation of a handle
 * to block and unblock processes.
 */
class key  {

  /**
      Global identifier for all keys of a given type.
      This should be a STATIC object that gets passed
      into every key when selecting type.
  */
 public:
  typedef std::set<thread_data_t> blocking_t;

 public:
  static key_traits::category general;

 public:
  static key* construct(){
    return new key;
  }

  static key* construct(const key_traits::category& name){
    return new key(name);
  }

  static std::string name(int keyname_id) {
    return (*category_id_to_name_)[keyname_id];
  }

  static int allocate_category_id(const std::string& name);

  static int event_typeid(const std::string& event_name);

  static int num_categories() {
    return category_name_to_id_->size();
  }

  std::string name() const {
    return name(keyname_id_);
  }

  int  event_typeid() const {
    return keyname_id_;
  }

  virtual ~key(){}

  void* operator new(size_t size);

  void operator delete(void* ptr);

  void block_thread(thread_data_t t){
    blocked_thread_ = t;
  }

  bool still_blocked() {
    return blocked_thread_.second;
  }

  bool timed_out() const {
    return timed_out_;
  }

  void set_timed_out() {
    timed_out_ = true;
  }

  void
  clear() {
    blocked_thread_.first = 0;
    blocked_thread_.second = 0;
  }

  static void delete_statics();

 private:
  friend class operating_system;

  key();

  key(const key_traits::category& name);

 private:
  static spkt_unordered_map<std::string, int>* category_name_to_id_;
  static spkt_unordered_map<int, std::string>* category_id_to_name_;
  static uint64_t key_storage_size_;

  thread_data_t blocked_thread_;
  bool timed_out_;
  int keyname_id_;

};

}
} // end of namespace sstmac

#endif