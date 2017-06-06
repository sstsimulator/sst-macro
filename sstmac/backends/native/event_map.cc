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

#include <sstmac/common/sstmac_config.h>

#include <sstmac/backends/native/event_map.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/util.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace native {

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