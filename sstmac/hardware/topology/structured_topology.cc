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
  topology(params),
  max_ports_intra_network_(-1),
  eject_geometric_id_(-1),
  max_ports_injection_(-1),
  netlinks_per_switch_(-1)
{
  concentration_ = params->get_optional_int_param("concentration",1);
  netlinks_per_switch_ = concentration_;

  injection_redundancy_ = params->get_optional_int_param("injection_redundant", 1);
  max_ports_injection_ = netlinks_per_switch_;

  sprockit::sim_parameters* netlink_params = params->get_optional_namespace("netlink");
  if (netlink_params->has_scoped_param("model") &&
      netlink_params->get_scoped_param("model") != "null"){
    num_nodes_per_netlink_ = netlink_params->get_int_param("concentration");
    netlinks_per_switch_ /= num_nodes_per_netlink_;
    if (netlinks_per_switch_ == 0){
      spkt_abort_printf("Error - netlink concentration cannot be higher than node concentration");
    }
  } else {
    num_nodes_per_netlink_ = 1;
  }
}

void
structured_topology::endpoint_eject_paths_on_switch(
   node_id dest_addr,
   switch_id sw_addr,
   routable::path_set &paths) const
{
  int node_offset = dest_addr % netlinks_per_switch_;
  int switch_port = node_offset + max_ports_intra_network_;
  paths.resize(1);
  paths[0].outport = switch_port;
  paths[0].vc = 0;
  paths[0].geometric_id = eject_geometric_id_ + node_offset;
}

void
structured_topology::configure_injection_geometry(std::vector<int>& redundancies)
{
  for (int i=0; i < netlinks_per_switch_; ++i){
    redundancies[i+eject_geometric_id_] = injection_redundancy_;
  }
}

void
structured_topology::nodes_connected_to_ejection_switch(switch_id swaddr,
                                   std::vector<injection_port>& nodes) const
{
  nodes_connected_to_injection_switch(swaddr, nodes);
}

void
structured_topology::nodes_connected_to_injection_switch(switch_id swaddr,
                                   std::vector<injection_port>& nodes) const
{
  nodes.resize(concentration_);
  for (int i = 0; i < concentration_; i++) {
    injection_port& port = nodes[i];
    port.nid = swaddr*concentration_ + i;
    port.port = i + max_ports_intra_network_;
  }
}

}
}


