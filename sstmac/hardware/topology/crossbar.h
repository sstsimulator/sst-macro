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
#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_CROSSBAR_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_CROSSBAR_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

/**
 *  @class crossbar
 *  The crossbar network generates a network which connects
    all nodes with only two hops: those to and from the crossbar.
 */
class crossbar : public structured_topology
{
 public:
  std::string
  to_string() const override {
    return "crossbar topology";
  }

  virtual ~crossbar() {}

  crossbar(sprockit::sim_parameters* params);

  int
  diameter() const override {
    return 1;
  }

  int
  num_leaf_switches() const override {
    return size_;
  }

  int minimal_distance(switch_id src, switch_id dst) const override {
    return 1;
  }

  bool
  uniform_network_ports() const override {
    return true;
  }

  bool
  uniform_switches_non_uniform_network_ports() const override {
    return true;
  }

  bool
  uniform_switches() const override {
    return true;
  }

  void
  configure_individual_port_params(switch_id src,
        sprockit::sim_parameters *switch_params) const override;

  void
  connected_outports(switch_id src,
       std::vector<connection>& conns) const override;

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const override;

  void
  minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    routable::path& path) const override;

  virtual int
  num_switches() const override {
    return size_;
  }

 private:
  long size_;

};

}
} //end of namespace sstmac

#endif

