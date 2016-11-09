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

#include <sstream>

#include <sstmac/common/sst_event.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/thread_info.h>

namespace sstmac {

void
handler_event_queue_entry::execute()
{
  handler_->handle(ev_to_deliver_);
}

handler_event_queue_entry::handler_event_queue_entry(event* ev,
                           event_handler* hand,
                           device_id src_loc) :
  ev_to_deliver_(ev),
  handler_(hand),
  event_queue_entry(hand->event_location(), src_loc)
{
}


} // end of namespace sstmac

