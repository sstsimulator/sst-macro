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


#include <sstmac/common/config.h>

#include <sstmac/backends/native/event_heap.h>
#include <sprockit/errors.h>
#include <sstmac/common/event_scheduler.h>

namespace sstmac {
namespace native {

SpktRegister("heap", event_manager, event_heap,
    "Implements the event queue as a min-heap (priority queue)");

//
// Goodbye.
//
event_heap::~event_heap() throw ()
{
}

event_queue_entry*
event_heap::pop_next_event()
{
  event_queue_entry* ev = queue_.top();
  queue_.pop();
  return ev;
}

void
event_heap::add_event(event_queue_entry* ev)
{
  queue_.push( ev);
}

//
// Clear all events and set time back to a zero of your choice.
//
void
event_heap::clear(timestamp zero_time)
{
  if (running_) {
    spkt_throw(sprockit::illformed_error,
        "event_heap::clear: event manager is running");
  }
  while(!queue_.empty()) {
    queue_.pop();
  }
  set_now(zero_time);
}

void
event_heap::cancel_all_messages(device_id canceled_loc)
{
  queue_t temp;
  while (!queue_.empty()) {
    event_queue_entry* t = queue_.top();
    if (!(t->event_location() == canceled_loc)) {
      temp.push(t);
    }
    queue_.pop();
  }
  swap(temp,queue_);
}

}
} // end of namespace sstmac

