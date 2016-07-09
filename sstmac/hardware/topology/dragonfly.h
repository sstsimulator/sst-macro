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
  virtual std::string
  to_string() const {
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

  virtual ~dragonfly() {}

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  void
  init_common_params(sprockit::sim_parameters* params);

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

  void
  get_coords(long uid, int &x, int &y, int &g) const;

  long
  get_uid(int x, int y, int g) const;

  switch_id
  switch_number(const coordinates &coords) const;

  virtual int
  num_switches() const {
    return x_ * y_ * g_;
  }

  int
  num_leaf_switches() const {
    return x_ * y_ * g_;
  }

  virtual void
  connect_objects(internal_connectable_map& switches);

  virtual int
  diameter() const {
    return 5;
  }

  coordinates
  neighbor_at_port(switch_id sid, int port);

  virtual switch_id
  random_intermediate_switch(switch_id current_sw,
                             switch_id dest_sw = switch_id(-1));

  virtual int
  convert_to_port(int dim, int dir) const;

  void
  configure_vc_routing(std::map<routing::algorithm_t, int> &m) const;

  virtual void
  productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    routable::path& path) const;

  void
  minimal_route_to_coords(
    const coordinates &src_coords,
    const coordinates &dest_coords,
    routable::path& path) const;

  int
  minimal_distance(
    const coordinates& src_coords,
    const coordinates& dest_coords) const;

  void
  nearest_neighbor_partners(
    const coordinates& src_sw_coords, int port,
    std::vector<node_id>& partners) const;

  void
  bit_complement_partners(
    const coordinates &src_sw_coords, int port,
    std::vector<node_id>& partners) const;

  void
  tornado_send_partners(
    const coordinates &src_sw_coords, int port,
    std::vector<node_id>& partners) const;

  void
  tornado_recv_partners(
    const coordinates &src_sw_coords, int port,
    std::vector<node_id>& partners) const;

  virtual void
  new_routing_stage(routable* rtbl);

  virtual void
  configure_geometric_paths(std::vector<int> &redundancies);

 protected:
  void
  minimal_route_to_group(int myX, int myY, int myG, int& dim, int& dir, int dstg) const;

  virtual void
  find_path_to_group(int myX, int myY, int myG, int& dsty, int& dstx, int dstg) const;

  int
  minimal_route_to_X(int hisx) const;

  int
  minimal_route_to_Y(int hisy) const;

  virtual void
  compute_switch_coords(switch_id uid, coordinates& coords) const;

  int
  find_y_path_to_group(int myX, int myG, int dstg) const;

  int
  find_x_path_to_group(int myY, int myG, int dstg) const;

  virtual bool
  xy_connected_to_group(int myX, int myY, int myG, int dstg) const;

  int
  xyg_dir_to_group(int myX, int myY, int myG, int dir) const;

 protected:
  int x_;
  int y_;
  int g_;
  int group_con_;
  bool true_random_intermediate_;

  static std::string
  set_string(int x, int y, int g)
  {
    return sprockit::printf("{ %d %d %d }", x, y, g);
  }

};

}
} //end of namespace sstmac

#endif

