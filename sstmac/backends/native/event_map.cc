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

#include <sstmac/common/sstmac_config.h>

#include <sstmac/backends/native/event_map.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/util.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace native {

SpktRegister("map", event_manager, event_map,
    "Implements the event queue as an ordered multimap");


event_map::~event_map() throw ()
{
}

event_queue_entry*
event_map::pop_next_event()
{
  queue_t::iterator it = queue_.begin();
  event_queue_entry* ev = *it;
  queue_.erase(it);
  return ev;
}

void
event_map::add_event(event_queue_entry* ev)
{
#if SSTMAC_SANITY_CHECK
  if (ev->event_location().type() == device_id::null){
    spkt_abort_printf("got uninitialized event location for %s",
      sprockit::to_string(ev).c_str());
  } else if (ev->src_location().type() == device_id::null){
    spkt_abort_printf("got uninitialized src location for %s",
      sprockit::to_string(ev).c_str());
  }
  size_t old_size = queue_.size();
#endif
  queue_.insert(ev);
#if SSTMAC_SANITY_CHECK
  if (queue_.size() == old_size){
    spkt_abort_printf("inserted event, but comparison erroneously matched previous event");
  }
#endif
}

//
// Clear all events and set time back to a zero of your choice.
//
void
event_map::clear(timestamp zero_time)
{
  if (running_) {
    spkt_throw(sprockit::illformed_error,
      "event_map::clear: event manager is running");
  }
  queue_.clear();
  set_now(zero_time);
}

void
event_map::cancel_all_messages(device_id canceled_loc)
{
  queue_t::iterator it = queue_.begin(), end = queue_.end();
  while (it != end){
    queue_t::iterator tmp = it++;
    event_queue_entry* ev = *tmp;

    if (ev->event_location() == canceled_loc){
      delete ev;
      queue_.erase(tmp);
    }
  }
}

}
} // end of namespace sstmac

