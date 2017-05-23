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

#include <sstmac/hardware/logp/logp_memory_model.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

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