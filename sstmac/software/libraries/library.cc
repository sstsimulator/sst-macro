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

namespace sstmac {
namespace sw {

library::~library()
{
}

void
library::unregister_all_libs()
{
  os_->unregister_all_libs(this);
}

void
library::register_lib(library* lib)
{
#if SSTMAC_SANITY_CHECK
  if (!os_){
    spkt_throw(sprockit::value_error,
        "library::register_lib: os not initialized yet")
  }
#endif
  os_->register_lib(this, lib);
}

void
library::init_os(operating_system* os)
{
  os_ = os;

#if SSTMAC_SANITY_CHECK
  if (!os->params()){
    spkt_throw(sprockit::null_error, "operating_system::params is null");
  }
#endif
  consume_params(os->params());
}

void
library::consume_params(sprockit::sim_parameters* params)
{
  //do nothing by default
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


