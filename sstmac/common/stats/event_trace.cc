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

#include <sstmac/common/stats/event_trace.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {

void
event_trace::simulation_finished(timestamp end)
{
}

void
event_trace::clear()
{
  spkt_throw(sprockit::unimplemented_error, "event_trace::clear");
}

void
event_trace::dump_local_data()
{
  spkt_throw(sprockit::unimplemented_error, "event_trace::dump_local_data");
}

void
event_trace::dump_global_data()
{
  spkt_throw(sprockit::unimplemented_error, "event_trace::dump_global_data");
}

void
event_trace::reduce(stat_collector *coll)
{
  spkt_throw(sprockit::unimplemented_error, "event_trace::reduce");
}

void
event_trace::global_reduce(parallel_runtime *rt)
{
  spkt_throw(sprockit::unimplemented_error, "event_trace::global_reduce");
}

event_trace::event_trace(sprockit::sim_parameters *params) :
  stat_collector(params)
{
  start_ = params->get_optional_time_param("start", 0);
  stop_ = params->get_optional_time_param("stop", 1e15);
}

void
event_trace::collect(int event_typeid,
                     const std::string& name,
                     node_id node,
                     long threadid,
                     int aid, int tid,
                     long ticks_begin,
                     long num_ticks)
{
  spkt_throw(sprockit::unimplemented_error, "event_trace::collect");
}

}