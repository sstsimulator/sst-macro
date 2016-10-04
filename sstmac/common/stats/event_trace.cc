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

#include <sstmac/common/stats/event_trace.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {

SpktRegister("event_trace", stat_collector, event_trace);

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

