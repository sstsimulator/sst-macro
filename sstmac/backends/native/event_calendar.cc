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

#include <sstmac/backends/native/event_calendar.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"event_calendar_max_time",
"event_calendar_epoch_length",
"event_calendar_search_window",
);

namespace sstmac {
namespace native {


event_calendar::event_calendar(sprockit::sim_parameters* params, parallel_runtime* rt)
  : event_manager(params, rt)
{
  //go max 1000
  timestamp max_time = params->get_optional_time_param("event_calendar_max_time", 1000);
  timestamp epoch_time = params->get_optional_time_param("event_calendar_epoch_length", 0.01);
  timestamp search_window = params->get_optional_time_param("event_calendar_search_window", 1e-5);
  //int ps_per_tick = params->reread_optional_int_param("timestamp_resolution", 1);


  num_ticks_epoch_ = epoch_time.ticks_int64();
  current_epoch_.resize(num_ticks_epoch_);

  long max_ticks = max_time.ticks_int64();
  num_epochs_ = max_ticks / num_ticks_epoch_;
  future_epochs_.resize(num_epochs_);

  num_ticks_search_window_ = search_window.ticks_int64();
  long num_search_windows = max_ticks / num_ticks_search_window_;
  search_windows_.resize(num_search_windows, 0);
  num_windows_epoch_ = num_ticks_epoch_ / num_ticks_search_window_;

  if (num_ticks_epoch_ % num_ticks_search_window_){
    spkt_throw_printf(sprockit::value_error,
        "event_calendar::init: number of ticks in search window (%ld) must divide ticks in epoch (%ld)",
        num_ticks_search_window_, num_ticks_epoch_);
  }

  current_window_idx_ = 0;
  current_epoch_start_idx_ = 0;
  epoch_number_ = 0;
  num_events_left_ = 0;
  current_search_window_ = 0;
}

event_calendar::~event_calendar() throw ()
{
  if (running_){
    std::cerr << "Warning: event calendar exiting while running\n";
  }
}

void
event_calendar::go_to_next_epoch()
{
  ++epoch_number_;
  current_epoch_start_idx_ += num_ticks_epoch_;
  std::list<event*>& ev_list = future_epochs_[epoch_number_];
  std::list<event*>::iterator it, end = ev_list.end();
  for (it=ev_list.begin(); it != end; ++it){
    event* ev = *it;
    add_event(ev);
  }
  current_window_idx_ = 0;
  //all these events got added twice
  num_events_left_ -= ev_list.size();
  ev_list.clear();
}

event*
event_calendar::pop_next_event_in_search_window()
{
  long start_idx = current_window_idx_;
  //search window guaranteed to have an event
  while (!current_epoch_[current_window_idx_]){
    ++current_window_idx_;
  }
  event* ev = current_epoch_[current_window_idx_];

  /** pull the event out */
  current_epoch_[current_window_idx_] = ev->next;
  ev->next = 0;

  --num_events_left_;
  return ev;
}

event*
event_calendar::pop_next_event()
{
  if (search_windows_[current_search_window_]){
    --search_windows_[current_search_window_];
    return pop_next_event_in_search_window();
  }

  while (search_windows_[current_search_window_] == 0){
    ++current_search_window_;
    if (current_search_window_ % num_windows_epoch_ == 0){
      go_to_next_epoch();
    }
  }

  //found the next event
  current_window_idx_ = (current_search_window_ * num_ticks_search_window_) % num_ticks_epoch_;
  --search_windows_[current_search_window_];
  return pop_next_event_in_search_window();
}

void
event_calendar::add_event(event* ev)
{
  long nticks = ev->time().ticks_int64();
  long delta_ticks = nticks - current_window_idx_ - current_epoch_start_idx_;
  if (delta_ticks >= num_ticks_epoch_){
    //future epoch
    long epoch_num = nticks / num_ticks_epoch_;
    future_epochs_[epoch_num].push_back(ev);
  }
  else {
    //i can schedule this now
    long window_idx = nticks % num_ticks_epoch_;

    /**
        Put events on the tail matches event_map ordering
        for simultaenous events - for debugging purposes
    if (current_epoch_[window_idx]){
      event* next = current_epoch_[window_idx];
      while (next->next){
        next = next->next;
      }
      next->next = ev;
    }
    else {
      current_epoch_[window_idx] = ev;
    }
    */
    ev->next = 0;
    ev->next = current_epoch_[window_idx];
    current_epoch_[window_idx] = ev;
    long search_window = nticks / num_ticks_search_window_;
    ++search_windows_[search_window];
  }
  ++num_events_left_;
}

//
// Clear all events and set time back to a zero of your choice.
//
void
event_calendar::clear(const timestamp &zero_time)
{
  if (running_) {
    spkt_throw(sprockit::illformed_error,
      "event_calendar::clear: event manager is running");
  }

  for (long idx=0; idx < num_ticks_epoch_; ++idx){
    event* ev = current_epoch_[idx];
    while (ev){
      event* prev = ev;
      ev = ev->next;
      prev->next = 0;
    }
    current_epoch_[idx] = 0;
  }

  for (long idx=0; idx < num_epochs_; ++idx){
    future_epochs_[idx].clear();
  }

  set_now(zero_time);
}

void
event_calendar::cancel_all_messages(device_id canceled_loc)
{
  spkt_throw(sprockit::unimplemented_error,
    "event_calendar::cancel_all_messages: not able to simulate failures right now");
}

}
} // end of namespace sstmac