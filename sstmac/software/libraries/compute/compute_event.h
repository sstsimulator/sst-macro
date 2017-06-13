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

#ifndef SSTMAC_HARDWARE_PROCESSOR_EVENTDATA_H_INCLUDED
#define SSTMAC_HARDWARE_PROCESSOR_EVENTDATA_H_INCLUDED

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/hardware/memory/memory_id.h>
#include <type_traits>
#include <sprockit/debug.h>
#include <sprockit/typedefs.h>
#include <stdint.h>

DeclareDebugSlot(compute_intensity);

namespace sstmac {
namespace sw {

/**
 * Input for processor models that use
 * performance counter data. Is basically just a map
 * that maps std::string keys to integer values.
 * Keys are defined in the libraries that use them.
 */
class compute_event :
 public event
{
 public:
  virtual bool
  is_timed_compute() const = 0;

  void
  set_core(int core){
    core_ = core;
  }

  std::string
  to_string() const {
    return "compute event";
  }

  int
  core() const {
    return core_;
  }

  hw::memory_access_id
  access_id() const {
    return unique_id_;
  }

  void
  set_access_id(hw::memory_access_id id) {
    unique_id_ = id;
  }

  uint64_t
  unique_id() const {
    return uint64_t(unique_id_);
  }

 private:
  int core_;

  hw::memory_access_id unique_id_;

};

template <class T>
class compute_event_impl :
 public compute_event
{
  NotSerializable(compute_event_impl)

 public:
  bool
  is_timed_compute() const override {
    return std::is_same<T,timestamp>::value;
  }

  T& data() {
    return t_;
  }


 private:
  T t_;

};

struct basic_instructions_st
{
  uint64_t mem_random = 0ULL;
  uint64_t mem_sequential = 0ULL;
  uint64_t flops = 0ULL;
  uint64_t intops = 0ULL;
};

typedef compute_event_impl<timestamp> timed_compute_event;
typedef compute_event_impl<basic_instructions_st> basic_compute_event;

}
}  // end of namespace sstmac

#endif