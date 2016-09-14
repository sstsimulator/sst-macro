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

// fattree.h: Interface for fat tree networks.
//
// Author: Jeremiah Wilke <jjwilke@sandia.gov>

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FATTREE_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FATTREE_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>


namespace sstmac {
namespace hw {

class abstract_fat_tree :
  public structured_topology
{
 public:
  typedef enum {
   up_dimension = 1,
   down_dimension = 0
  } dimension_t;

  int
  l() const {
    return l_;
  }

  int
  k() const {
    return k_;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual int
  ndimensions() const {
    //fat-tree is indexed by row and column
    return 2;
  }

  static int
  pow(int a, int exp){
    int res = 1;
    for (int i=0; i < exp; ++i){
      res *= a;
    }
    return res;
  }

  int
  diameter() const {
    return (l_ + 1) * 2;
  }

  virtual int
  num_leaf_switches() const {
    return numleafswitches_;
  }

  std::vector<node_id>
  nodes_connected_to_injection_switch(switch_id swaddr) const;

  std::vector<node_id>
  nodes_connected_to_ejection_switch(switch_id swaddr) const;

 protected:
  std::vector<node_id>
  nodes_connected_to_switch(switch_id swaddr) const;

 protected:
  int l_, k_, numleafswitches_;
  int toplevel_;
};

/**
 * @class fat_tree
 * The fat tree network generates a k-ary fat tree with l tiers
 */
class fat_tree :
  public abstract_fat_tree
{

 public:
  virtual std::string
  to_string() const {
    return "fat tree topology";
  }

  virtual ~fat_tree() {}

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  connect_objects(sprockit::sim_parameters* params, internal_connectable_map& switches);

  virtual int
  convert_to_port(int dim, int dir) const;

  virtual int
  num_switches() const {
    return numleafswitches_ * l_;
  }

  std::string
  default_router() const {
    return "fattree";
  }

  coordinates
  neighbor_at_port(switch_id sid, int port);

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const;

  switch_id
  switch_number(const coordinates &coords) const;

  void
  productive_path(
    int dim,
    const coordinates &src,
    const coordinates &dst,
    structured_routable::path& path) const;

  void
  minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    structured_routable::path& path) const;

  void
  minimal_route_to_coords(
    const coordinates &src_coords,
    const coordinates &dest_coords,
    structured_routable::path& path) const;

  int
  minimal_distance(
    const coordinates& src_coords,
    const coordinates& dest_coords) const;

  int
  switch_at_row_col(int row, int col) const {
    return row * numleafswitches_ + col;
  }

  static int
  upColumnConnection(int k, int myColumn, int upPort, int columnSize);

  static int
  downColumnConnection(int k, int myColumn, int downPort, int columnSize);

 protected:
  virtual void
  compute_switch_coords(switch_id uid, coordinates& coords) const;

};

class simple_fat_tree : public abstract_fat_tree
{
 public:
  virtual std::string
  to_string() const {
    return "simple fat tree topology";
  }

  virtual ~simple_fat_tree() {}

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  connect_objects(sprockit::sim_parameters* params,
                  internal_connectable_map& switches);

  int
  num_switches() const {
    return num_switches_;
  }

  int
  convert_to_port(int dim, int dir) const;

  virtual void
  partition(
    int* switches_per_lp,
    int *switch_to_lp,
    int *switch_to_thread,
    int& local_num_switches,
    int me,
    int nproc,
    int nthread,
    int noccupied);

  void
  minimal_route_to_coords(
    const coordinates &src_coords,
    const coordinates &dest_coords,
    structured_routable::path &path) const;

  switch_id
  switch_number(const coordinates &coords) const;

  int
  minimal_distance(const coordinates &src_coords, const coordinates &dest_coords) const;

  void
  productive_path(int dim,
    const coordinates &src,
    const coordinates &dst,
    structured_routable::path &path) const;

  void
  compute_switch_coords(switch_id swid, coordinates &coords) const;

  coordinates
  neighbor_at_port(switch_id sid, int port);

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const;

  void
  minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    structured_routable::path& path) const;

  int
  level(switch_id sid) const;

 private:
  int num_hops(int srcLevel, int srcOffset, int dstLevel, int dstOffset) const;

  int num_hops_to_node(node_id src, node_id dst) const;

 protected:
  std::vector<int> level_offsets_;

  int num_switches_;

  std::vector<double> tapering_;


};

}
} //end of namespace sstmac

#endif

