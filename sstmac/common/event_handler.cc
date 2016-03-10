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

#include <sstmac/common/event_handler.h>
#include <limits>

namespace sstmac {

event_loc_id event_loc_id::null = event_loc_id(switch_id(std::numeric_limits<int32_t>::max()));
event_loc_id event_loc_id::uninitialized = event_loc_id(switch_id(std::numeric_limits<int32_t>::max()-1));

void
multi_event_handler::handle(sst_message* msg)
{
  std::map<sst_message::message_type_t, handle_functor*>::iterator
    it = fxns_.find(msg->type());
  if (it == fxns_.end()){
    spkt_throw_printf(sprockit::value_error,
        "multi_event_handler::handle: %s cannot handle type %s for message %s",
        to_string().c_str(),
        msg->type().tostr(),
        msg->to_string().c_str());
  }
  handle_functor* fxn = it->second;
  fxn->handle(this, msg);
}

void
multi_event_handler::handle_type(sst_message::message_type_t ty, handle_functor *handler)
{
  fxns_[ty] = handler;
}

} // end of namespace sstmac

