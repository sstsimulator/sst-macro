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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_TILED_DRAGONFLY_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_TILED_DRAGONFLY_H_INCLUDED

#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/topology/multipath_topology.h>
#include <set>

namespace sstmac {
namespace hw {

class tiled_dragonfly : public dragonfly, public multipath_topology
{
  private:

    typedef std::pair<int,int> xy_t;
    typedef std::list<xy_t> xy_list_t;
    typedef xy_list_t::iterator xy_list_iter;
    typedef std::vector< xy_list_t* > coor_xy_map_t;
    typedef std::vector< coor_xy_map_t* > conn_map_t;
    typedef std::map<int, xy_list_t* > coormap_xy_map_t;
    typedef std::vector< coormap_xy_map_t* > gconn_map_t;

    typedef struct connection
  {
    int src_group;
    xy_t src_switch_xy;
    xy_t src_port_xy;
    int dst_group;
    xy_t dst_switch_xy;
    xy_t dst_port_xy;
  } connection;

  std::list<connection> intragrp_conns_;
  std::list<connection> intergrp_conns_;
  mutable conn_map_t intragrp_conn_map_; //FIXME mutable is a hack
  mutable gconn_map_t intergrp_conn_map_; //FIXME mutable is a hack

  // outports_[srcid][geomid] is a list of outports
  std::vector< std::vector< std::list<int> > > outports_;

  std::string intragroup_file_;
  std::string intergroup_file_;
  int tiles_x_, tiles_y_, tiles_inj_;
  std::vector<int> injection_ports_;
  mutable std::map<int,int> port_to_geomid_; //FIXME mutable is a hack
  std::vector< std::set<int> > switch_to_connected_groups_;
  int n_tiles_;
  int n_geom_paths_;

 public:
  virtual std::string
  to_string() const override {
    return "tiled_dragonfly";
  }

  tiled_dragonfly(sprockit::sim_parameters* params);

  virtual ~tiled_dragonfly() {}

  bool
  uniform_network_ports() const override {
    return true;
  }

  bool
  uniform_switches_non_uniform_network_ports() const override {
    return false;
  }

  bool
  uniform_switches() const override {
    return true;
  }

  virtual switch_id
  node_to_injection_switch(
        node_id nodeaddr, uint16_t ports[], int& num_ports) const override
  {
    spkt_throw(sprockit::unimplemented_error, "tiled switch should never connect directly to node");
  }

  virtual switch_id
  node_to_ejection_switch(
        node_id nodeaddr, uint16_t ports[], int& num_ports) const override
  {
        spkt_throw(sprockit::unimplemented_error, "tiled switch should never connect directly to node");
  }

  virtual switch_id
  netlink_to_injection_switch(
      node_id nodeaddr, uint16_t ports[], int &num_ports) const override;

  virtual switch_id
  netlink_to_ejection_switch(
      node_id nodeaddr, uint16_t ports[], int &num_ports) const override;

  virtual void
  eject_paths_on_switch(
      node_id dest_addr,
      switch_id sw_addr,
      routable::path_set &paths) const;

  virtual bool
  xy_connected_to_group(int myX, int myY, int myG,
                        int dstg) const;

  void
  connected_outports(
      switch_id src,
      std::vector<sstmac::hw::topology::connection>& conns)
  const override;

  //---------------------------------------------------------------------
  // Multipath topology
  //---------------------------------------------------------------------

  virtual void
  configure_geometric_paths(std::vector<int> &redundancies) override;

  virtual void
  get_redundant_paths(routable::path& inPath,
                      routable::path_set& outPaths,
                      switch_id addr) const  override;

 private:

  //---------------------------------------------------------------------
  // Connection setup
  //---------------------------------------------------------------------

  void
  read_intragroup_connections();

  void
  read_intergroup_connections();

  void
  configure_outports();

  //---------------------------------------------------------------------
  // Utility functions
  //---------------------------------------------------------------------

  void
  check_switch_x(int n) {
    if (n < 0 || n >= x_)
      spkt_throw_printf(
            sprockit::value_error,"switch x value %d out of range",n);
  }

  void
  check_switch_y(int n) {
    if (n < 0 || n >= y_)
      spkt_throw_printf(
            sprockit::value_error,"switch y value %d out of range",n);
  }

  void
  check_switch_g(int n) {
    if (n < 0 || n >= g_)
      spkt_throw_printf(
            sprockit::value_error,"switch group value %d out of range",n);
  }

  void
  check_port_x(int n) {
    if (n < 0 || n >= tiles_x_)
      spkt_throw_printf(
            sprockit::value_error,"switch x value %d out of range",n);
  }

  void
  check_port_y(int n) {
    if (n < 0 || n >= tiles_y_)
      spkt_throw_printf(
            sprockit::value_error,"switch y value %d out of range",n);
  }

  void
  check_switch_xyg(int x, int y, int g) {
    check_switch_x(x);
    check_switch_y(y);
    check_switch_g(g);
  }

  void
  check_port_xy(int x, int y) {
    check_port_x(x);
    check_port_y(y);
  }

};

}
} //end of namespace sstmac

#endif

