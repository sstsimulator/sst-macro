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

#include <sstmac/software/libraries/library.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/sstmac_env.h>

ImplementFactory(sstmac::sw::library);

namespace sstmac {
namespace sw {

library::library(const std::string& libname, software_id sid, operating_system* os) :
  sid_(sid), libname_(libname), os_(os),
  addr_(os->addr())
{
  os_->register_lib(this);
}

device_id
library::event_location() const
{
  return os_->event_location();
}

library::~library()
{
  os_->unregister_lib(this);
}

void
library::incoming_event(event* ev)
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "%s::incoming_event: this library should only block, never receive incoming",
     to_string().c_str());
}

void
blocking_library::wait_event(event *ev, key::category cat)
{
  key* k = key::construct(cat);
  blocked_events_[ev] = k;
  os_->block(k);
  delete k;
}

void
blocking_library::incoming_event(event *ev)
{
  key* k = blocked_events_[ev];
  if (!k){
    spkt_throw(sprockit::value_error,
               "blocking_library::incoming_event: got invalid event");
  }
  blocked_events_.erase(ev);
  os_->unblock(k);
}


}
} //end namespace


