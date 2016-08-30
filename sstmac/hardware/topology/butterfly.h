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

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

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

  virtual void
  productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    structured_routable::path& path) const;

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const;

 protected:
  void
  compute_switch_coords(switch_id uid, coordinates& coords) const;

  switch_id
  switch_number(const coordinates &coords) const;

 protected:
  int kary_;
  int nfly_;
  long nswitches_per_col_;

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
  virtual std::string
  to_string() const {
    return "butterfly";
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual ~butterfly() {}

  virtual int
  num_switches() const {
    return nswitches_per_col_ * nfly_;
  }

  void
  minimal_route_to_coords(
    const coordinates &src_coords,
    const coordinates &dest_coords,
    structured_routable::path& path) const;

  int
  minimal_distance(
    const coordinates& src_coords,
    const coordinates& dest_coords) const;

  switch_id
  switch_number(const coordinates &coords) const;

  virtual void
  connect_objects(internal_connectable_map& switches);

  switch_id
  endpoint_to_ejection_switch(
    node_id nodeaddr,
    int &switch_port) const;

  virtual int
  convert_to_port(int dim, int dir) const;

  /**
    Each level in a kary-nfly counts
    as a dimension.
  */
  int ndimensions() const {
    return nfly_;
  }

  std::vector<node_id>
  nodes_connected_to_injection_switch(switch_id swaddr) const;

  std::vector<node_id>
  nodes_connected_to_ejection_switch(switch_id swaddr) const;

  void
  productive_paths(
    structured_routable::path_set &paths,
    const coordinates &current,
    const coordinates &dst);

 private:
  virtual void
  compute_switch_coords(switch_id uid, coordinates& coords) const;

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

