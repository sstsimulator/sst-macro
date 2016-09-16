#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/nic/nic.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/backends/common/sim_partition.h>

namespace sstmac {
namespace hw {


structured_topology::structured_topology(sprockit::sim_parameters* params,
                                         InitMaxPortsIntra i1,
                                         InitGeomEjectID i2) :
  topology(params,i1,i2)
{
}

void
structured_topology::endpoint_eject_paths_on_switch(
   node_id dest_addr,
   switch_id sw_addr,
   structured_routable::path_set &paths) const
{
  int node_offset = dest_addr % endpoints_per_switch_;
  int switch_port = node_offset + max_ports_intra_network_;
  paths.resize(1);
  paths[0].outport = switch_port;
  paths[0].vc = 0;
  paths[0].geometric_id = eject_geometric_id_ + node_offset;
}

void
structured_topology::configure_injection_geometry(std::vector<int>& redundancies)
{
  for (int i=0; i < endpoints_per_switch_; ++i){
    redundancies[i+eject_geometric_id_] = injection_redundancy_;
  }
}

}
}


