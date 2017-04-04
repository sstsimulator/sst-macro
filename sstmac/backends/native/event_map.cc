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

//
// Hello.
//
event_map::event_map(sprockit::sim_parameters* params, parallel_runtime* rt) :
  running_(false),
  event_manager(params, rt)
{
  set_now(timestamp(0));
}


void
event_map::do_next_event()
{
  event_queue_entry* ev = pop_next_event();
  set_now(ev->time());
  //debug_printf(sprockit::dbg::all_events,
  //  "running event %s", sprockit::to_string(ev).c_str());

  ev->execute();
  delete ev;
}

#if DEBUG_DETERMINISM
extern std::map<device_id,std::ofstream*> outs;
#endif

//
// Run the eventmanager.
//
void
event_map::run()
{
  if (running_) {
    spkt_throw(sprockit::illformed_error,
              "event_map::run: event manager already running.");
  }
  running_ = true;
  stopped_ = false;

#if SSTMAC_SANITY_CHECK
  int n_events = 0;
  clock_t t1 = clock();
  clock_t t2;
#endif

#if SSTMAC_DEBUG_THREAD_EVENTS
  open_debug_file();
#endif

  while (1){
    while (!empty() && !stopped_) {
      do_next_event();
    }
    bool terminate = vote_to_terminate();
    if (terminate)
      break;
  }

#if SSTMAC_DEBUG_THREAD_EVENTS
  close_debug_file();
#endif

  running_ = false;

  if (empty() || finish_on_stop_) {
    complete_ = true;
    finish();
  }

#if DEBUG_DETERMINISM
  std::map<device_id,std::ofstream*>::iterator it, end = outs.end();
  for (it=outs.begin(); it != end; ++it){
    it->second->close();
  }
#endif
}


void
event_map::schedule(timestamp start_time, uint32_t seqnum, event_queue_entry* ev)
{
  if (start_time < now()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "event_map::schedule: scheduling event in the past: now=%ld units, ev=%ld units",
                     now().ticks(), start_time.ticks());
  }

  ev->set_time(start_time);
  ev->set_seqnum(seqnum);

  double delta = fabs(start_time.sec() - 1.29380e-04);
  if (delta < 1e-5){
    fflush(stdout);
    //abort();
  }

  //debug_printf(sprockit::dbg::all_events,
  //  "adding event to run at %10.5e: %s",
  //  start_time.sec(), sprockit::to_string(ev).c_str());
  add_event(ev);
}

void
event_map::finish()
{
}

}
} // end of namespace sstmac

