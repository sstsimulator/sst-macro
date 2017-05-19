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