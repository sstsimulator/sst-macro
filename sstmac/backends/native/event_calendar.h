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
  FactoryRegister("calendar", event_manager, event_calendar,
    "Implements the event queue as an O(1) event calendar. Provides faster scheduling if many, many events")

 public:
  event_calendar(sprockit::sim_parameters* params, parallel_runtime* rt);

  ~event_calendar() throw ();

  void clear(timestamp zero_time = timestamp(0));

  void cancel_all_messages(device_id mod);

  bool empty() const {
    return num_events_left_ == 0;
  }

 protected:
  event* pop_next_event();

  event* pop_next_event_in_search_window();

  void go_do_one_event();

  void add_event(event* ev);

  void go_to_next_epoch();

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