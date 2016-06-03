#include <sstmac/hardware/analytic/simple/null_nic.h>
#include <sstmac/hardware/common/packet.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/network/network_message.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

#if !SSTMAC_INTEGRATED_SST_CORE

namespace sstmac {
namespace hw {

SpktRegister("nullnic | null", nic, null_nic,
            "Implements a nic which immediately injects messages with no memory reads or latency");


void
null_nic::init_factory_params(sprockit::sim_parameters* params)
{
  params->add_param_override("injection_latency", "0us");
  params->add_param_override("injection_bandwidth", "100000GB/s");
  simple_nic::init_factory_params(params);
}

}
}

#endif
