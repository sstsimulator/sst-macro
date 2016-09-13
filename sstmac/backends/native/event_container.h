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

#ifndef SSTMAC_BACKENDS_NATIVE_EVENTCONTAINER_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_EVENTCONTAINER_H_INCLUDED

#include <sstmac/common/event_manager.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/common/timestamp.h>

namespace sstmac {
namespace native {

/**
 * An event manager base class using standard containers.
 * The template parameter queue_t is only required to implement the
 * empty() and size() operations.  Methods which need to access or
 * modify elements, such as do_one_event() and add_event(), are virtual
 * and must be implemented by a child class, providing flexibility to the
 * interface that queue_t must provide.  In this way, eventmanagers using
 * both standard containers and container adaptors can share much of their
 * implementation despite differing access and modification interfaces.
 */
class event_container : public event_manager
{
 public:
  /// Goodbye.
  virtual
  ~event_container() throw () { }

  /// Run the eventmanager.
  /// The eventmanager shall return control when no more messages remain.
  virtual void
  run();

 protected:
  event_container(sprockit::sim_parameters* params, parallel_runtime* rt);

  virtual void
  do_next_event();

  virtual event_queue_entry*
  pop_next_event() = 0;

  virtual bool
  empty() const = 0;

  /// Called by schedule. Child class must implement.
  virtual void
  add_event(event_queue_entry* ev)=0;

  virtual bool
  vote_to_terminate(){
    return true;
  }

  /// Sentinel to track whether the event handler is running or not.
  bool running_;

   /// Time of last event executed.
  timestamp last_update_sim_;

#if SSTMAC_DEBUG_THREAD_EVENTS
  virtual void open_debug_file(){}
  virtual void close_debug_file(){}
#endif

 protected:
  /// Set off the given eventhandler at the given time.
  void
  schedule(timestamp start_time, uint32_t seqnum, event_queue_entry* ev);

  /// Called at end of run(). Calls finish() on finishers_ and calls
  /// finish_stats().
  void
  finish();

};

}
} // end of namespace sstmac



#endif

