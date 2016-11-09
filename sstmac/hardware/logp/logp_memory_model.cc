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

#include <sstmac/hardware/logp/logp_memory_model.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

SpktRegister("logP | simple | LogP | logp", memory_model,logp_memory_model,
            "Implements a simple memory model that is just a single link");

logp_memory_model::~logp_memory_model()
{
  if (link_) delete link_;
}

logp_memory_model::logp_memory_model(sprockit::sim_parameters* params, node* nd)
  : memory_model(params, nd) //no self events
{

  lat_ = params->get_time_param("latency");
  bw_ = params->get_bandwidth_param("bandwidth");
  link_ = new link(bw_, lat_);
}

void
logp_memory_model::access(long bytes,
                            double max_bw,
                            callback* cb)
{
  mem_debug("simple model: doing access of %ld bytes", bytes);

  timestamp delta_t = link_->new_access(now(), bytes, max_bw);
  parent_node_->schedule_delay(delta_t, cb);
}

timestamp
logp_memory_model::link::new_access(timestamp now, long size, double max_bw)
{
  max_bw = std::min(max_bw, bw_);
  timestamp n(std::max(now.sec(), last_access_.sec()));
  timestamp access = lat_ + timestamp((double) size / max_bw);
  last_access_ = n + access;
  timestamp delta = last_access_ - now;
  return delta;
}



}
} /* namespace sstmac */

