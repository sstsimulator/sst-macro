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

#ifndef SSTMAC_COMMON_STATS_EVENT_TRACE_H_
#define SSTMAC_COMMON_STATS_EVENT_TRACE_H_

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/node_address.h>

namespace sstmac {

class event_trace :
  public stat_collector
{
  FactoryRegister("event_trace", stat_collector, event_trace)
 public:
  event_trace(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "event trace";
  }

  virtual void
  simulation_finished(timestamp end) override;

  void
  collect(int key_typeid,
          const std::string& name,
          node_id addr,
          long threadid,
          int aid, int tid,
          long ticks_begin,
          long num_ticks);

  void
  reduce(stat_collector *coll) override;

  void
  dump_local_data() override;

  void
  dump_global_data() override;

  void
  global_reduce(parallel_runtime *rt) override;

  void
  clear() override;

  virtual
  ~event_trace() {
  }

  stat_collector*
  do_clone(sprockit::sim_parameters* params) const override {
    return new event_trace(params);
  }

 protected:
  long start_;
  long stop_;

};

}

#endif /* EVENT_TRACE_H_ */