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

#ifndef SSTMAC_COMMON_STATS_EVENT_TRACE_H_
#define SSTMAC_COMMON_STATS_EVENT_TRACE_H_

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/node_address.h>

namespace sstmac {

class event_trace :
  public stat_collector
{

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

