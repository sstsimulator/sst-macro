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

#ifndef SSTMAC_BACKENDS_NATIVE_EVENTMAP_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_EVENTMAP_H_INCLUDED

#include <sstmac/common/sstmac_config.h>

#include <sstmac/common/sst_event.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_scheduler.h>
#include <cassert>
#include <map>

namespace sstmac {
namespace native {

/**
 * An event manager that relies on the eventcontainer template base class
 * to manage events with a multimap template parameter.
 */
class event_map :
  public event_manager
{
  FactoryRegister("map", event_manager, event_map,
      "Implements the event queue as an ordered multimap")
 public:
  event_map(sprockit::sim_parameters* params, parallel_runtime* rt);

  /// Run the eventmanager.
  /// The eventmanager shall return control when no more messages remain.
  virtual void
  run();


  ~event_map() throw ();

  void
  clear(timestamp zero_time = timestamp(0));

  void
  cancel_all_messages(device_id mod);

  bool
  empty() const {
    return queue_.empty();
  }

  virtual bool
  vote_to_terminate(){
    return true;
  }

 protected:
  friend class multithreaded_event_container;

  event_queue_entry*
  pop_next_event();

  void
  add_event(event_queue_entry* ev);

  /// Set off the given eventhandler at the given time.
  void
  schedule(timestamp start_time, uint32_t seqnum, event_queue_entry* ev);

  /// Called at end of run(). Calls finish() on finishers_ and calls
  /// finish_stats().
  void
  finish();

  virtual void
  do_next_event();

 protected:
  struct event_compare {
    bool operator()(event_queue_entry* lhs, event_queue_entry* rhs) {
      bool neq = lhs->time() != rhs->time();
      if (neq) return lhs->time() < rhs->time();

      if (lhs->src_location() == rhs->src_location()){
        return lhs->seqnum() < rhs->seqnum();
      } else {
        return lhs->src_location() < rhs->src_location();
      }
    }

  };
  typedef std::set<event_queue_entry*, event_compare> queue_t;
  queue_t queue_;
  /// Sentinel to track whether the event handler is running or not.
  bool running_;

};


}
} // end of namespace sstmac

#endif