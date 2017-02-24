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

#ifndef SSTMAC_BACKENDS_NATIVE_EVENTMAP_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_EVENTMAP_H_INCLUDED

#include <sstmac/common/sstmac_config.h>

#include <sstmac/backends/native/event_container.h>
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

