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


//
// Author: Jeremiah Wilke <jjwilke@sandia.gov>

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_butterfly_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_butterfly_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

/**
 * @brief The abstract_butterfly class
 * Encapsulates operations common to both butterfly and flattened_butterfly
 */
class abstract_butterfly :
  public structured_topology
{
 public:
  typedef enum {
    up_dimension = 0,
    down_dimension = 1
  } dimension_t;

 public:
  virtual ~abstract_butterfly() {}

  /**
   * @brief kary
   * @return The branching degree of the butterfly
   */
  int
  kary() const {
    return kary_;
  }

  /**
   * @brief nfly
   * @return The number of stages in the butterfly
   */
  int
  nfly() const {
    return nfly_;
  }

  /**
   * @brief num_switches_per_col
   * The butterfly is physically laid out as a 2D-graid of cols and rows
   * @return The number of switches in a column of the 2D physical layout
   */
  int
  num_switches_per_col() const {
    return nswitches_per_col_;
  }

  /**
   * @brief num_switches_per_col
   * The butterfly is physically laid out as a 2D-graid of cols and rows.
   * A butterfly is an indirect network. Only the first column of switches
   * are actually connected to compute nodes.
   * @return The number of switches in a column of the 2D physical layout
   */
  virtual int
  num_leaf_switches() const {
    return nswitches_per_col_;
  }

  int
  diameter() const {
    return nfly_ + 1;
  }

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const;

 protected:
  abstract_butterfly(sprockit::sim_parameters* params,
                     InitMaxPortsIntra i1,
                     InitGeomEjectID i2);

 protected:
  int kary_;
  int nfly_;
  long nswitches_per_col_;

 private:
  sprockit::sim_parameters*
  override_params(sprockit::sim_parameters* params);

};

/**
 * @brief The butterfly class
 * Encapsulates a butterfly topology as described in "High Performance Datacenter Networks"
 * by Abts and Kim
 */
class butterfly :
  public abstract_butterfly
{

 public:
  butterfly(sprockit::sim_parameters* params);

  virtual std::string
  to_string() const override {
    return "butterfly";
  }

  virtual ~butterfly() {}

  virtual int
  num_switches() const override {
    return nswitches_per_col_ * nfly_;
  }

  int
  minimal_distance(switch_id src, switch_id dst) const override;

  void
  minimal_route_to_switch(switch_id current_sw_addr,
                          switch_id dest_sw_addr,
                          routable::path &path) const override;

  bool
  uniform_switches() const override {
    return true;
  }

  bool
  uniform_network_ports() const override {
    return true;
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

  switch_id
  netlink_to_ejection_switch(
    node_id nodeaddr,
    int &switch_port) const override;

  void
  nodes_connected_to_injection_switch(switch_id swaddr,
                                      std::vector<injection_port>& nodes) const override;

  void
  nodes_connected_to_ejection_switch(switch_id swaddr,
                                     std::vector<injection_port>& nodes) const override;

 private:
  int
  last_col_index_start() const {
    return last_col_index_start_;
  }

 private:
  long last_col_index_start_;

};

}
} //end of namespace sstmac

#endif

