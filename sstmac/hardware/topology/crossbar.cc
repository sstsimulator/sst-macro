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
// crossbar.cc: Implementation of crossbar networks.
//
// Author: Curtis Janssen <cljanss@sandia.gov>

#include <sstream>
#include <sstmac/hardware/topology/crossbar.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("crossbar | xbar", topology, crossbar);

crossbar::crossbar(sprockit::sim_parameters* params) :
  structured_topology(params,
                      InitMaxPortsIntra::I_Remembered,
                      InitGeomEjectID::I_Remembered)
{
  std::vector<int> args;
  params->get_vector_param("geometry", args);
  size_ = args[0];
  max_ports_intra_network_ = num_switches();
  eject_geometric_id_ = max_ports_intra_network_;
}

void
crossbar::configure_vc_routing(std::map<routing::algorithm_t, int> &m) const
{
  m[routing::minimal] = 1;
  m[routing::minimal_adaptive] = 1;
  m[routing::valiant] = 2;
  m[routing::ugal] = 3;
}

void
crossbar::minimal_route_to_switch(switch_id current_sw_addr,
                                  switch_id dest_sw_addr,
                                  routable::path &path) const
{
  path.vc = 0;
  path.outport = dest_sw_addr;
}

void
crossbar::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  int n_switches = num_switches();
  conns.resize(n_switches - 1);
  int cidx = 0;
  for (int i=0; i < n_switches; ++i){
    if (i == src) continue;

    conns[cidx].src = src;
    conns[cidx].dst = i;
    conns[cidx].src_outport = i;
    conns[cidx].dst_inport = src;
    ++cidx;
  }
}

void
crossbar::configure_individual_port_params(switch_id src, sprockit::sim_parameters *switch_params) const
{
  topology::configure_individual_port_params(0, num_switches(), switch_params);
}

}
} //end of namespace sstmac

