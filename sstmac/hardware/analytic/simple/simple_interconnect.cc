#ifndef SIMPLE_INTERCONNECT_CC
#define SIMPLE_INTERCONNECT_CC

#include <sstmac/hardware/analytic/simple/simple_interconnect.h>
#include <sstmac/hardware/analytic/simple/simple_switch.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

#if !SSTMAC_INTEGRATED_SST_CORE
SpktRegister("simple", interconnect, simple_interconnect,
     "Models the network interconnect via a simple analytic function of latency/bandwidth");
#endif

simple_interconnect::simple_interconnect(sprockit::sim_parameters* params, event_manager* mgr,
                    partition* part, parallel_runtime* rt) :
  interconnect(override_params(params), mgr, part, rt)
{

  double bw = params->get_bandwidth_param("bandwidth");
  inverse_bw_ = 1.0 / bw;
  hop_latency_ = params->get_time_param("hop_latency");
  inj_latency_ = params->get_time_param("injection_latency");
}

sprockit::sim_parameters*
simple_interconnect::override_params(sprockit::sim_parameters* params)
{
  params->add_param_override("switch.model", "simple");
  return params;
}

}
}


#endif // SIMPLE_INTERCONNECT_CC

