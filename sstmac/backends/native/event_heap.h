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

#ifndef SSTMAC_BACKENDS_NATIVE_event_heap_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_event_heap_H_INCLUDED

#if !SSTMAC_INTEGRATED_SST_CORE

#include <sstmac/backends/native/event_container.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_scheduler.h>

#include <queue>
#include <vector>

namespace sstmac {
namespace native {

/**
 * An event manager that relies on the eventcontainer template base class
 * to manage events with a priority queue template parameter.
 */
class event_heap :
  public event_container
{

 public:
  event_heap(sprockit::sim_parameters* params, parallel_runtime* rt) :
    event_container(params, rt)
  {
  }

  ~event_heap() throw ();

  /// Clear all events and set time back to a zero of your choice.
  /// \throw sprockit::illformed_error if the event manager is running.
  void
  clear(timestamp zero_time = timestamp(0));

  bool
  empty() const {
    return queue_.empty();
  }

  void
  cancel_all_messages(device_id canceled_loc);

 protected:
  typedef std::priority_queue<event*,
          std::vector<event_queue_entry*>, std::greater<event_queue_entry*> > queue_t;

  event_queue_entry*
  pop_next_event();

  void
  add_event(event_queue_entry* ev);

 protected:
   queue_t queue_;

};

}
} // end of namespace sstmac

#endif // !SSTMAC_INTEGRATED_SST_CORE

#endif

