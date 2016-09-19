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
  diameter() const override {
    return (l_ + 1) * 2;
  }

  virtual int
  num_leaf_switches() const override {
    return numleafswitches_;
  }

  void
  nodes_connected_to_injection_switch(switch_id swaddr,
                            std::vector<injection_port>& nodes) const override;

  void
  nodes_connected_to_ejection_switch(switch_id swaddr,
                            std::vector<injection_port>& nodes) const override;

 protected:
  abstract_fat_tree(sprockit::sim_parameters* params,
                    InitMaxPortsIntra i1,
                    InitGeomEjectID i2);

 protected:
  int l_, k_, numleafswitches_;
  int toplevel_;

 private:
  sprockit::sim_parameters*
  override_params(sprockit::sim_parameters* params);
};

/**
 * @class fat_tree
 * The fat tree network generates a k-ary fat tree with l tiers
 */
class fat_tree :
  public abstract_fat_tree
{

 public:
  fat_tree(sprockit::sim_parameters* params);

  virtual std::string
  to_string() const override {
    return "fat tree topology";
  }

  inline int up_port(int dir) const {
    return  k_ + dir;
  }

  inline int down_port(int dir) const {
    return dir;
  }

  virtual ~fat_tree() {}

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
  connected_outports(switch_id src, std::vector<connection>& conns) const override;

  void
  configure_individual_port_params(switch_id src,
      sprockit::sim_parameters *switch_params) const override;


  virtual int
  num_switches() const override {
    return numleafswitches_ * l_;
  }

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const override;

  void
  minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    routable::path& path) const override;

  int
  minimal_distance(
    switch_id src,
    switch_id dest) const override;

  int
  switch_at_row_col(int row, int col) const {
    return row * numleafswitches_ + col;
  }

  void
  compute_row_col(switch_id sid, int& row, int& col) const {
    row = sid / numleafswitches_;
    col = sid % numleafswitches_;
  }

  static int
  upColumnConnection(int k, int myColumn, int upPort, int columnSize);

  static int
  downColumnConnection(int k, int myColumn, int downPort, int columnSize);

};

class simple_fat_tree : public abstract_fat_tree
{
 public:
  simple_fat_tree(sprockit::sim_parameters* params);

  virtual std::string
  to_string() const override {
    return "simple fat tree topology";
  }

  virtual ~simple_fat_tree() {}

  bool
  uniform_network_ports() const override {
    return false;
  }

  bool
  uniform_switches_non_uniform_network_ports() const override {
    return false;
  }

  bool
  uniform_switches() const override {
    return false;
  }

  void
  connected_outports(switch_id src, std::vector<connection>& conns) const override;

  void
  configure_nonuniform_switch_params(switch_id src,
        sprockit::sim_parameters* switch_params) const override;

  void
  configure_individual_port_params(switch_id src,
      sprockit::sim_parameters *switch_params) const override;

  int
  num_switches() const override {
    return num_switches_;
  }

  int
  convert_to_port(int dim, int dir) const;

  virtual void
  create_partition(
    int* switches_per_lp,
    int *switch_to_lp,
    int *switch_to_thread,
    int& local_num_switches,
    int me,
    int nproc,
    int nthread,
    int noccupied) const override;

  int
  minimal_distance(switch_id src,
                   switch_id dest) const override;

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const override;

  void
  minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    routable::path& path) const override;

  int
  level(switch_id sid) const;

 private:
  int num_hops(int srcLevel, int srcOffset, int dstLevel, int dstOffset) const;

 protected:
  std::vector<int> level_offsets_;

  int num_switches_;

  std::vector<double> tapering_;


};

}
} //end of namespace sstmac

#endif

