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

#ifndef EVENT_CALENDAR_H
#define EVENT_CALENDAR_H

#include <sstmac/backends/native/event_container.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_scheduler.h>

#include <map>

namespace sstmac {
namespace native {

/**
 * An event manager that relies on the eventcontainer template base class
 * to manage events with a multimap template parameter.
 */
class event_calendar :
  public event_container
{

 public:
  event_calendar(sprockit::sim_parameters* params, parallel_runtime* rt);

  ~event_calendar() throw ();

  void
  clear(timestamp zero_time = timestamp(0));

  void
  cancel_all_messages(device_id mod);

  bool
  empty() const {
    return num_events_left_ == 0;
  }

 protected:
  event*
  pop_next_event();

  event*
  pop_next_event_in_search_window();

  void
  go_do_one_event();

  void
  add_event(event* ev);

  void
  go_to_next_epoch();

 protected:
  int num_events_left_;

  long num_ticks_epoch_;

  long num_epochs_;

  long current_window_idx_;

  long current_epoch_start_idx_;

  long num_ticks_search_window_;

  long current_search_window_;

  long num_windows_epoch_;

  int epoch_number_;

  std::vector<uint32_t> search_windows_;

  std::vector<std::list<event*> > future_epochs_;

  std::vector<event*> current_epoch_;

};

}
} // end of namespace sstmac

#endif // EVENT_CALENDAR_H
