
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


#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_HYPERCUBE_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_HYPERCUBE_H_INCLUDED

#include <sstmac/hardware/topology/hdtorus.h>

namespace sstmac {
namespace hw {

class hypercube :
  public hdtorus
{
 public:
  hypercube(sprockit::sim_parameters* params);

  virtual std::string
  to_string() const override {
    return "hdtorus topology";
  }

  virtual ~hypercube() {}

  void
  minimal_route_to_switch(
    switch_id src,
    switch_id dst,
    routable::path& path) const override;

  bool
  uniform_network_ports() const override {
    return false;
  }

  bool
  uniform_switches_non_uniform_network_ports() const override {
    return true;
  }

  void
  connected_outports(switch_id src, std::vector<connection>& conns) const override;

  void
  configure_individual_port_params(switch_id src,
           sprockit::sim_parameters *switch_params) const override;

  inline int
  convert_to_port(int dim, int dir) const {
    return dim_to_outport_[dim] + dir;
  }

  int
  minimal_distance(switch_id src, switch_id dst) const override;

 protected:
  int radix_;
  int ndim_;
  std::vector<int> dim_to_outport_;

};

}
} //end of namespace sstmac

#endif // HYPERCUBE_H

