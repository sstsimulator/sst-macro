/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_TILED_DRAGONFLY_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_TILED_DRAGONFLY_H_INCLUDED

#include <sstmac/hardware/topology/dragonfly.h>
#include <set>

namespace sstmac {
namespace hw {

#if 0

class tiled_dragonfly : public dragonfly
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

  std::string intragroup_file_;
  std::string intergroup_file_;
  int tiles_x_, tiles_y_, tiles_inj_;
  std::vector<int> injection_ports_;
  mutable std::map<int,int> port_to_geomid_; //FIXME mutable is a hack
  std::vector< std::set<int> > switch_to_connected_groups_;
  int n_tiles_;

 public:
  virtual std::string
  to_string() const override {
    return "tiled_dragonfly";
  }

  virtual ~tiled_dragonfly() {}

  tiled_dragonfly(sprockit::sim_parameters* params);

  void
  connect_objects(sprockit::sim_parameters* params, internal_connectable_map& switches);

  void
  configure_geometric_paths(std::vector<int> &redundancies);

  virtual void
  minimal_routes_to_switch(
      switch_id current_sw_addr,
      switch_id dest_sw_addr,
      structured_routable::path &current_path,
      structured_routable::path_set &paths) const;

  virtual void
  minimal_routes_to_coords(
      const coordinates &src_coords,
      const coordinates &dest_coords,
      structured_routable::path &current_path,
      structured_routable::path_set &paths) const;

  virtual bool
  xy_connected_to_group(int myX, int myY, int myG,
                        int dstg) const;
  switch_id
  netlink_to_injection_switch(
      node_id nodeaddr, int ports[], int &num_ports) const;

  switch_id
  netlink_to_ejection_switch(
      node_id nodeaddr, int ports[], int &num_ports) const;

  virtual void
  eject_paths_on_switch(
      node_id dest_addr,
      switch_id sw_addr,
      structured_routable::path_set &paths) const;

  // throw unimplemented exception on the following
  virtual void
  minimal_route_to_coords(
    const coordinates &current_coords,
    const coordinates &dest_coords,
    structured_routable::path& path) const;

  virtual int
  port(int replica, int dim, int dir);

  virtual int
  convert_to_port(int dim, int dir) const;


 private:
  int xy_to_int(xy_t xy) const
  {
    return xy.second * tiles_x_ + xy.first;
  }

  void
  read_intragroup_connections();

  void
  read_intergroup_connections();

  void
  make_intragroup_connections(sprockit::sim_parameters* params,
                              internal_connectable_map& objects);

  void
  make_intergroup_connections(sprockit::sim_parameters* params,
                              internal_connectable_map &objects);

  void
  make_geomid();

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

#endif

}
} //end of namespace sstmac

#endif