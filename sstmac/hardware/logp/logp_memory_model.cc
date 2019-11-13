/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

LogPMemoryModel::~LogPMemoryModel()
{
  if (link_) delete link_;
}

LogPMemoryModel::LogPMemoryModel(uint32_t id, SST::Params& params, Node* node)
  : MemoryModel(id, params, node)
{

  lat_ = TimeDelta(params.find<SST::UnitAlgebra>("latency").getValue().toDouble());

  auto bw = params.find<SST::UnitAlgebra>("bandwidth").getValue();
  if (bw.toDouble() == 0){
    spkt_abort_printf("Zero or missing memory.bandwidth parameter");
  }
  min_byte_delay_ = TimeDelta(bw.inverse().toDouble());
  link_ = new Link(min_byte_delay_, lat_);
}

void
LogPMemoryModel::accessFlow(uint64_t bytes, TimeDelta byte_request_delay, Callback* cb)
{
  mem_debug("simple model: doing access of %ld bytes", bytes);

  TimeDelta delta_t = link_->newAccess(now(), bytes, byte_request_delay);
  parent_node_->sendDelayedExecutionEvent(delta_t, cb);
}

void
LogPMemoryModel::accessRequest(int  /*linkId*/, Request * /*req*/)
{
  spkt_abort_printf("LogP does not support single memory requests - only flow-level calls");
}

TimeDelta
LogPMemoryModel::Link::newAccess(Timestamp now, uint64_t size, TimeDelta byte_request_delay)
{
  TimeDelta actual_byte_delay = byte_request_delay + byte_delay_;
  Timestamp base = std::max(now, last_access_);
  TimeDelta access = lat_ + actual_byte_delay * size;
  last_access_ = base + access;
  TimeDelta delta = last_access_ - now;
  return delta;
}



}
} /* namespace sstmac */
