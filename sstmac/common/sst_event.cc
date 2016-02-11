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

std::string
handler_event::to_string() const
{
  std::stringstream ss;
  ss << "sst_event(handler=" << (handler_ ? handler_->to_string() : "null")
     << ",  msg = " << (msg_to_deliver_ ? msg_to_deliver_->to_string()
                        : "null") << ")";
  return ss.str();
}

void
handler_event::execute()
{
  handler_->handle(msg_to_deliver_);
}

handler_event::handler_event(const sst_message::ptr& msg,
                           event_handler* hand,
                           event_loc_id src_loc) :
  msg_to_deliver_(msg),
  handler_(hand),
  event(hand->event_location(), src_loc)
{
}

void
null_msg_event::execute()
{
  handler_->handle(sst_message::ptr());
}




} // end of namespace sstmac

