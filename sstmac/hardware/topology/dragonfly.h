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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_DRAGONFLY_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_DRAGONFLY_H_INCLUDED

#include <sstmac/hardware/topology/cartesian_topology.h>

namespace sstmac {
namespace hw {

class dragonfly : public cartesian_topology
{

 public:
  dragonfly(sprockit::sim_parameters* params);

  typedef enum {
    x_dimension = 0,
    y_dimension = 1,
    g_dimension = 2
  } dimension_t;

  typedef enum {
    upX_vc = 0,
    downX_vc = 1
  } x_vc_t;

  typedef enum {
    upY_vc = 0,
    downY_vc = 1
  } y_vc_t;

 public:
  std::string
  to_string() const override {
    return "dragonfly";
  }

  dimension_t
  dim_for_port(int port){
    if (port >= (x_ + y_)){
      return g_dimension;
    } else if (port >= x_){
      return y_dimension;
    } else {
      return x_dimension;
    }
  }

  bool
  uniform_network_ports() const override {
    return false;
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

  virtual ~dragonfly() {}

  int
  ndimensions() const {
    return 3;
  }

  int
  numX() const {
    return x_;
  }

  int
  numY() const {
    return y_;
  }

  int
  numG() const {
    return g_;
  }

  int
  group_con() const {
    return group_con_;
  }

  inline void
  get_coords(switch_id sid, int& x, int& y, int& g) const {
    x = computeX(sid);
    y = computeY(sid);
    g = computeG(sid);
  }

  int get_uid(int x, int y, int g) const {
    return g * (x_ * y_) + y * (x_) + x;
  }

  inline int x_port(int x) const {
    return x;
  }

  inline int y_port(int y) const {
    return x_ + y;
  }

  inline int g_port(int g) const {
    return x_ + y_ + g;
  }

  inline int computeX(switch_id sid) const {
    return sid % x_;
  }

  inline int computeY(switch_id sid) const {
    return (sid / x_) % y_;
  }

  inline int computeG(switch_id sid) const {
    return (sid / (x_*y_));
  }

  virtual int
  num_switches() const override {
    return x_ * y_ * g_;
  }

  int
  num_leaf_switches() const override {
    return x_ * y_ * g_;
  }

  void minimal_route_to_switch(
      switch_id current_sw_addr,
      switch_id dest_sw_addr,
      routable::path &path) const override;

  int
  minimal_distance(switch_id src, switch_id dst) const override;

  virtual int
  diameter() const override {
    return 5;
  }

  virtual switch_id
  random_intermediate_switch(switch_id current_sw,
                             switch_id dest_sw = switch_id(-1)) override;

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const override;

  virtual void
  new_routing_stage(routable* rtbl) override;

  virtual void
  configure_geometric_paths(std::vector<int> &redundancies);

  coordinates
  switch_coords(switch_id) const override;

  switch_id
  switch_addr(const coordinates &coords) const override;

 protected:
  virtual void
  find_path_to_group(int myX, int myY, int myG, int dstG,
                     int& dstX, int& dstY,
                     routable::path& path) const;

  bool
  find_y_path_to_group(int myX, int myG, int dstG, int& dstY,
                       routable::path& path) const;

  bool
  find_x_path_to_group(int myY, int myG, int dstG, int& dstX,
                       routable::path& path) const;

  virtual bool
  xy_connected_to_group(int myX, int myY, int myG, int dstG) const;

 protected:
  int x_;
  int y_;
  int g_;
  int group_con_;
  bool true_random_intermediate_;

  void
  setup_port_params(sprockit::sim_parameters* params,
                    int dim, int dimsize) const;

  static std::string
  set_string(int x, int y, int g)
  {
    return sprockit::printf("{ %d %d %d }", x, y, g);
  }

 private:
  int convert_to_port(int dim, int dir) const {
    if      (dim == x_dimension) return x_port(dir);
    else if (dim == y_dimension) return y_port(dir);
    else if (dim == g_dimension) return g_port(dir);
    else return -1;
  }

  int
  xyg_dir_to_group(int myX, int myY, int myG, int dir) const;

};

}
} //end of namespace sstmac

#endif

