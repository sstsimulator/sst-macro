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

// butterfly.h: Interface for butterfly networks.
//
// Based on butterfly.c by Jim Schutt
// Adapted by Curtis Janssen <cljanss@ca.sandia.gov>
#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_butterfly_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_butterfly_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

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

  virtual long
  num_stages() const {
    return nfly_;
  }

  int
  kary() const {
    return kary_;
  }

  int
  nfly() const {
    return nfly_;
  }

  int
  num_switches_per_col() const {
    return nswitches_per_col_;
  }

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
    geometry_routable::path& path) const;

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

  int
  last_col_index_start() const {
    return last_col_index_start_;
  }

  virtual int
  num_switches() const {
    return nswitches_per_col_ * nfly_;
  }

  void
  minimal_route_to_coords(
    const coordinates &src_coords,
    const coordinates &dest_coords,
    geometry_routable::path& path) const;

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

  std::string
  default_router() const {
    return "minimal";
  }

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
    geometry_routable::path_set &paths,
    const coordinates &current,
    const coordinates &dst);

 protected:
  virtual void
  compute_switch_coords(switch_id uid, coordinates& coords) const;

 protected:
  long last_col_index_start_;

};

}
} //end of namespace sstmac

#endif

