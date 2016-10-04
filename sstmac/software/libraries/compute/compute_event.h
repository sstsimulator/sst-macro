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

