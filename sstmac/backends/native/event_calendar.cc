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

SpktRegister("calendar", event_manager, event_calendar,
  "Implements the event queue as an O(1) event calendar. Provides faster scheduling if many, many events");


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


