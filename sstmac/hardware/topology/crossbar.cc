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
crossbar::connect_objects(sprockit::sim_parameters* params, internal_connectable_map& objects)
{
  sprockit::sim_parameters* link_params = params->get_namespace("link");
  for (int i = 0; i < objects.size(); i++) {
    switch_id me(i);

    for (int j = 0; j < objects.size(); j++) {
      switch_id them(j);
      if (i != j) {
        int outport = j;
        int inport = i;

        objects[me]->connect_output(
          link_params,
          outport,
          inport,
          objects[them]);

        objects[them]->connect_input(
          link_params,
          outport, inport,
          objects[me]);
      }
    }
  }
}

}
} //end of namespace sstmac

