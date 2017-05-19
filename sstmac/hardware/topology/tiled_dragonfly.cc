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

#include <sstmac/hardware/topology/tiled_dragonfly.h>
#include <math.h>
#include <sstream>
#include <sprockit/stl_string.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/fileio.h>
#include <sprockit/regexp.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"intragroup_connection_file",
"intergroup_connection_file",
);

using namespace sprockit;

#if 0

namespace sstmac {
namespace hw {

SpktRegister("tiled_dragonfly | tiled_dfly", topology, tiled_dragonfly);

tiled_dragonfly::tiled_dragonfly(sprockit::sim_parameters* params) :
  dragonfly(params)
{
  // get connection filenames
  intragroup_file_ = params->get_optional_param("intragroup_connection_file",
                                                "intragroup.txt");
  intergroup_file_ = params->get_optional_param("intergroup_connection_file",
                                                "intergroup.txt");

  // get tile config
  tiles_x_ = params->get_int_param("tiles_per_row");
  tiles_y_ = params->get_int_param("tiles_per_col");
  n_tiles_ = tiles_x_ * tiles_y_;

  // get injection port config
  if (params->has_param("injection_ports"))
    params->get_vector_param("injection_ports",injection_ports_);
  else
    spkt_throw(sprockit::input_error, "tiled_dragonfly needs injection_ports specified");
  top_debug("ntiles in x,y dimensions: %d,%d", tiles_x_, tiles_y_);
  int max_port = *std::max_element(injection_ports_.begin(),injection_ports_.end());
  if (max_port > n_tiles_ - 1)
    spkt_throw_printf(sprockit::value_error,"max port index %d exceeds max tile index %d",
                      max_port,n_tiles_-1);
  max_ports_injection_ = injection_ports_.size();

  max_ports_intra_network_ = n_tiles_ - max_ports_injection_;
  eject_geometric_id_ = x_ + y_ + group_con_ - 2; // first eject port

  // initialize data structures
  int group_size = x_ * y_;
  for(int i=0; i < group_size; ++i) {
    intragrp_conn_map_.push_back(new coor_xy_map_t);
    for(int j=0; j < group_size; ++j) {
      intragrp_conn_map_[i]->push_back(new xy_list_t);
    }
  }
  int nswitches = num_switches();
  for(int i=0; i < nswitches; ++i)
    intergrp_conn_map_.push_back(new coormap_xy_map_t);
  switch_to_connected_groups_.resize(nswitches);

  // check that parents didn't overwrite max_ports_injection_ incorrectly
  if (max_ports_injection_ != injection_ports_.size())
    spkt_throw(sprockit::input_error, "tiled_dragonfly inconsistent number of injection ports");
}

void
tiled_dragonfly::connect_objects(sprockit::sim_parameters* params,
                                 internal_connectable_map& objects)
{
  sprockit::sim_parameters* link_params = params->get_namespace("link");
  read_intragroup_connections();
  read_intergroup_connections();
  make_intragroup_connections(link_params, objects);
  make_intergroup_connections(link_params, objects);
  make_geomid();
}

void
tiled_dragonfly::configure_geometric_paths(std::vector<int> &redundancies)
{
  int npaths = x_ + y_ + group_con_ - 2 + netlinks_per_switch_;
  redundancies.resize(npaths);
  //do x paths, then y paths, then g paths
  int path = 0;
  for (int x=0; x < (x_ - 1); ++x, ++path){
    redundancies[path] = red_[x_dimension];
  }
  for (int y=0; y < (y_ -1); ++y, ++path){
    redundancies[path] = red_[y_dimension];
  }
  for (int g=0; g < group_con_; ++g, ++path){
    redundancies[path] = red_[g_dimension];
  }
  configure_injection_geometry(redundancies);
}

void
tiled_dragonfly::minimal_routes_to_switch(switch_id current_sw_addr,
                                          switch_id dest_sw_addr,
                                          structured_routable::path &current_path,
                                          structured_routable::path_set &paths) const
{
  coordinates src = switch_coords(current_sw_addr);
  coordinates dst = switch_coords(dest_sw_addr);
  minimal_routes_to_coords(src, dst, current_path, paths);
}

void
tiled_dragonfly::minimal_routes_to_coords(
  const coordinates &src_coords,
  const coordinates &dest_coords,
  structured_routable::path &current_path,
  structured_routable::path_set &paths) const
{
  top_debug("tiled dfly: minimal routes from %s to %s",
            set_string(src_coords[0], src_coords[1], src_coords[2]).c_str(),
            set_string(dest_coords[0], dest_coords[1], dest_coords[2]).c_str());

  int src_id = switch_number(src_coords);
  int dst_id = switch_number(dest_coords);

  //if we crossed a global link in the past, set to 1
  current_path.vc = current_path.metadata_bit(
        structured_routable::crossed_timeline) ? 1 : 0;

  xy_list_t* ports;
  coordinates dst(dest_coords);
  coordinates src(src_coords);
  if (src[g_dimension] == dst[g_dimension]) { // intragroup routing
    src[g_dimension] = dst[g_dimension] = 0;
    if (src[0] != dst[0] && src[1] != dst[1]) {
      dst[1] = src[1];
    }
    if (!intragrp_conn_map_[switch_number(src)]->size()) {
      spkt_throw(sprockit::invalid_key_error,"src %s not found in intragroup map",
                 set_string(src[0], src[1], src[2]).c_str());
    }
    coor_xy_map_t& dst_map = *intragrp_conn_map_[switch_number(src)];
    if (!dst_map[switch_number(dst)]->size()) {
      spkt_throw(sprockit::invalid_key_error,"dst %s not found in intragroup map",
                 set_string(dst[0], dst[1], dst[2]).c_str());
    }
    ports = dst_map[switch_number(dst)];
  }
  else { // intergroup routing

    // find x,y coords for destination that is connected to target group
    coordinates new_dst(3);
    dragonfly::find_path_to_group(src[0], src[1], src[2],
                                  new_dst[0], new_dst[1], dst[2]);
    top_debug("find_path_to_group: target x,y is %d,%d",new_dst[0],new_dst[1]);

    // find_path_to_group returns src x,y != dst x,y if we still need
    // to make local hop(s) to get to correct switch for global hop
    if (new_dst[0] != src[0] || new_dst[1] != src[1]) {
      new_dst[2] = src[2]; // staying in local group
      minimal_routes_to_coords(src_coords,new_dst,current_path,paths); //recursive
      return;
    }

    // otherwise we're ready for intergroup hop
    else {
      new_dst[2] = dst[2]; // go global

      // we need to take one of our global links
      coormap_xy_map_t* dst_map = intergrp_conn_map_[switch_number(src)];
      if (!dst_map->size())
        spkt_throw(sprockit::value_error,
                   "src switch %s has no intergroup links",
                   set_string(src[0], src[1], src[2]).c_str());

      // but at this point we don't actually know what coordinates we're
      // jumping to, so we need to iterate to find which of our possible gobal
      // destinations matches the target, then use the ports that go there
      for (coormap_xy_map_t::iterator dst_it = dst_map->begin();
           dst_it != dst_map->end(); ++dst_it) {
        coordinates temp(3);
        compute_switch_coords(switch_id(dst_it->first),temp);
        if (temp[2] == dst[2] && dst_it->second->size()) {
          ports = dst_it->second;
          continue;
        }
      }
      if (!ports)
        spkt_throw(sprockit::value_error,"couldn't find global connection ports");
    }
  }

  int i=0;
  for (xy_list_iter iter = ports->begin();
       iter != ports->end(); ++iter, ++i) {

    // copy over outport
    int outport = iter->first;
    paths[i].outport = outport;

    // determine and write the geometric id
    paths[i].geometric_id = port_to_geomid_[outport];

    // copy over virtual channel and related metadata
    paths[i].vc = current_path.vc;
    if (current_path.metadata_bit(structured_routable::crossed_timeline))
      paths[i].set_metadata_bit(structured_routable::crossed_timeline);

    top_debug("minimal_routes_to_coords: outport: %d, geometric id: %d, vc: %d",
              paths[i].outport, paths[i].geometric_id, paths[i].vc);
  }
}

bool
tiled_dragonfly::xy_connected_to_group(int myX, int myY, int myG, int dstg) const
{
  coordinates me(3);
  me[0] = myX;
  me[1] = myY;
  me[2] = myG;
  const std::set<int>& my_groups = switch_to_connected_groups_[switch_number(me)];
  const std::set<int>::iterator iter = my_groups.find(dstg);
  if (iter != my_groups.end())
    return true;
  return false;
}

switch_id
tiled_dragonfly::netlink_to_injection_switch(
    node_id nodeaddr, int ports[], int &num_ports) const
{
  num_ports = injection_redundancy_;
  long net_id = nodeaddr / netlinks_per_switch_;
  int local_endpoint_id = nodeaddr % netlinks_per_switch_;
  int first_index = local_endpoint_id * injection_redundancy_;
  int i=first_index;
  for (int pi=0; pi < injection_redundancy_; ++i,++pi)
    ports[pi] = injection_ports_[i];
  return switch_id(net_id);
}

switch_id
tiled_dragonfly::netlink_to_ejection_switch(
    node_id nodeaddr, int ports[], int &num_ports) const
{
  return netlink_to_injection_switch(nodeaddr,ports,num_ports);
}

void
tiled_dragonfly::eject_paths_on_switch(
   node_id dest_addr, switch_id sw_addr, structured_routable::path_set &paths) const
{
  paths.resize(injection_redundancy_);
  int node_offset = dest_addr % netlinks_per_switch_;
  int index = node_offset * injection_redundancy_;
  for (int i=0; i < injection_redundancy_; ++i, ++index){
    paths[i].outport = injection_ports_[index];
    paths[i].vc = 0;
    paths[i].geometric_id = eject_geometric_id_ + node_offset;
  }
}

void
tiled_dragonfly::minimal_route_to_coords(
  switch_id src,
  switch_id dst,
  structured_routable::path& path) const
{
  spkt_throw(sprockit::unimplemented_error, "tiled_dragonfly::minimal_route_to_coords");
}

int
tiled_dragonfly::port(int replica, int dim, int dir)
{
  spkt_throw(sprockit::unimplemented_error, "tiled_dragonfly::port");
}

int
tiled_dragonfly::convert_to_port(int dim, int dir) const
{
  spkt_throw(sprockit::unimplemented_error, "tiled_dragonfly::convert_to_port");
}

void
tiled_dragonfly::read_intragroup_connections()
{
  std::ifstream in;
  sprockit::SpktFileIO::open_file(in, intragroup_file_);
  if (!in.is_open()) {
    spkt_throw_printf(sprockit::input_error,
                      "tiled_connect_objects: could not find intragroup connection file %s",
                      intragroup_file_.c_str());
  }

  while (!in.eof())
  {
    int src_x, src_y, src_g = 0, src_port_x, src_port_y,
        dst_x, dst_y, dst_g = 0, dst_port_x, dst_port_y;
    std::string delimiter;
    in >> src_x >> src_y;
    in >> delimiter;
    in >> src_port_x >> src_port_y;
    in >> delimiter;
    in >> dst_x >> dst_y;
    in >> delimiter;
    in >> dst_port_x >> dst_port_y;

    // validate
    check_switch_x(src_x);
    check_switch_y(src_y);
    check_port_xy(src_port_x,src_port_y);
    check_switch_x(dst_x);
    check_switch_y(dst_y);
    check_port_xy(dst_port_x,dst_port_y);

    connection c;
    c.src_group = 0;
    c.src_switch_xy = xy_t( src_x, src_y );
    c.src_port_xy = xy_t( src_port_x, src_port_y );
    c.dst_group = 0;
    c.dst_switch_xy = xy_t( dst_x, dst_y );
    c.dst_port_xy = xy_t( dst_port_x, dst_port_y );
    intragrp_conns_.push_back(c);
  }
}

void
tiled_dragonfly::read_intergroup_connections()
{
  std::ifstream in;
  sprockit::SpktFileIO::open_file(in, intergroup_file_);
  if (!in.is_open()) {
    spkt_throw_printf(sprockit::input_error,
                      "tiled_connect_objects: could not find intergroup connection file %s",
                      intergroup_file_.c_str());
  }

  while (!in.eof())
  {
    int src_x, src_y, src_g, src_port_x, src_port_y,
        dst_x, dst_y, dst_g, dst_port_x, dst_port_y;
    std::string delimiter;
    in >> src_x >> src_y >> src_g;
    in >> delimiter;
    in >> src_port_x >> src_port_y;
    in >> delimiter;
    in >> dst_x >> dst_y >> dst_g;
    in >> delimiter;
    in >> dst_port_x >> dst_port_y;

    // validate
    check_switch_xyg(src_x,src_y,src_g);
    check_port_xy(src_port_x,src_port_y);
    check_switch_xyg(dst_x,dst_y,dst_g);
    check_port_xy(dst_port_x,dst_port_y);

    connection c;
    c.src_switch_xy = xy_t( src_x, src_y );
    c.src_group = src_g;
    c.src_port_xy = xy_t( src_port_x, src_port_y );
    c.dst_switch_xy = xy_t( dst_x, dst_y );
    c.dst_group = dst_g;
    c.dst_port_xy = xy_t( dst_port_x, dst_port_y );
    intergrp_conns_.push_back(c);
  }
}


void
tiled_dragonfly::make_intragroup_connections(sprockit::sim_parameters* params,
                                             internal_connectable_map& objects)
{
  for (int g=0; g<numG(); ++g) {
    for( std::list<connection>::iterator it=intragrp_conns_.begin();
         it!=intragrp_conns_.end(); ++it ) {

      int src_g = 0;
      int dst_g = 0;
      int src_x = it->src_switch_xy.first;
      int src_y = it->src_switch_xy.second;
      int dst_x = it->dst_switch_xy.first;
      int dst_y = it->dst_switch_xy.second;

      int src_port_x = it->src_port_xy.first;
      int src_port_y = it->src_port_xy.second;
      int dst_port_x = it->dst_port_xy.first;
      int dst_port_y = it->dst_port_xy.second;

      coordinates src_coords = coordinates(3);
      src_coords[0] = src_x;
      src_coords[1] = src_y;
      src_coords[2] = 0;

      coordinates dst_coords = coordinates(3);
      dst_coords[0] = dst_x;
      dst_coords[1] = dst_y;
      dst_coords[2] = 0;

      switch_id src_id( get_uid(src_x,src_y,g) );
      switch_id dst_id( get_uid(dst_x,dst_y,g) );
      int outport = src_port_y * tiles_x_ + src_port_x;
      int inport = dst_port_y * tiles_x_ + dst_port_x;

      // hack to map Edison's two geomtric cofigurations back to a single config
      int port_remap[48] = {
        7, -1, -1, -1, -1, -1, -1, 0, 14, 15,
        -1, -1, -1, -1, 8, 9, 20, 23, 28, 36,
        16, 17, 18, 19, 29, 31, 38, 21, 24, 25,
        26, 27, 30, 37, 39, 22, 32, 33, 34, 35,
        -1, -1, -1, -1, -1, -1, -1, -1 };
      if (src_x > 7) {
        if (port_remap[outport] < 0)
          spkt_throw(sprockit::value_error, "bad remap value\n");
        outport = port_remap[outport];
      }
      if (dst_x > 7) {
        if (port_remap[inport] < 0)
          spkt_throw(sprockit::value_error, "bad remap value\n");
        inport = port_remap[inport];
      }

      top_debug("node %d,%s connecting to node %d,%s: outport %d, inport %d",
                int(src_id), set_string(src_x, src_y, g).c_str(),
                int(dst_id), set_string(dst_x, dst_y, g).c_str(),
                outport,inport);

      if(g==0) {
        int src = switch_number(src_coords);
        int dst = switch_number(dst_coords);
        intragrp_conn_map_[src]->at(dst)->push_back(
            std::pair<int,int>(outport,inport));
      }

      objects[src_id]->connect_output(params,outport,inport,
                               objects[dst_id]);
      objects[dst_id]->connect_input(params,outport,inport,
                               objects[src_id]);
    }
  }
}

void
tiled_dragonfly::make_intergroup_connections(sprockit::sim_parameters* params,
                                             internal_connectable_map& objects)
{
  for( std::list<connection>::iterator it=intergrp_conns_.begin();
       it!=intergrp_conns_.end(); ++it ) {

    int src_x = it->src_switch_xy.first;
    int src_y = it->src_switch_xy.second;
    int src_g = it->src_group;
    int dst_x = it->dst_switch_xy.first;
    int dst_y = it->dst_switch_xy.second;
    int dst_g = it->dst_group;

    int src_port_x = it->src_port_xy.first;
    int src_port_y = it->src_port_xy.second;
    int dst_port_x = it->dst_port_xy.first;
    int dst_port_y = it->dst_port_xy.second;

    coordinates src_coords = coordinates(3);
    src_coords[0] = src_x;
    src_coords[1] = src_y;
    src_coords[2] = src_g;

    coordinates dst_coords = coordinates(3);
    dst_coords[0] = dst_x;
    dst_coords[1] = dst_y;
    dst_coords[2] = dst_g;

    switch_id src_id( get_uid(src_x,src_y,src_g) );
    switch_id dst_id( get_uid(dst_x,dst_y,dst_g) );
    int outport = src_port_y * tiles_x_ + src_port_x;
    int inport = dst_port_y * tiles_x_ + dst_port_x;

    top_debug("node %d,%s connecting to node %d,%s",
              int(src_id), set_string(src_x, src_y, src_g).c_str(),
              int(dst_id), set_string(dst_x, dst_y, dst_g).c_str());

    // used by xy_connected_to_groups
    switch_to_connected_groups_[src_id].insert(dst_g);


    if (!intergrp_conn_map_[src_id]->count(dst_id)){
#if SPKT_HAVE_CPP11
      intergrp_conn_map_[src_id]->emplace(dst_id,new xy_list_t);
#else
      //intergrp_conn_map_[src_id][dst_id] = new xy_list_t;
#endif
    }
    intergrp_conn_map_[src_id]->at(dst_id)->push_back(
          std::pair<int,int>(outport,inport));

    objects[src_id]->connect_output(params,outport,inport,
                             objects[dst_id]);
    objects[dst_id]->connect_input(params,outport,inport,
                             objects[src_id]);
  }
}

void
tiled_dragonfly::make_geomid()
{
  // initialize
  for (int i=0; i < n_tiles_; ++i) {
    port_to_geomid_[i] = -1;
  }

  // all intragrp connections have same geometric port assignments
  // (only one group in connection file)
  coordinates src = coordinates(3);
  src[0] = 0;
  src[1] = 0;
  src[2] = 0;
  int next_x = 0, next_y = x_ - 2, next_g = x_ + y_ - 2;
  coor_xy_map_t* links = intragrp_conn_map_[0];
  for (int i=1; i < links->size(); ++i) {
    std::list< std::pair<int,int> >* port_list = links->at(i);
    if (port_list->size()) {
      coordinates dst(3);
      compute_switch_coords(switch_id(i),dst);
      int id;
      if (dst[0] == src[0]) {
        id = next_y;
        ++next_y;
      }
      else if (dst[1] == src[1]) {
        id = next_x;
        ++next_x;
      }
      std::list< std::pair<int,int> >::iterator port_iter = port_list->begin();
      for (; port_iter != port_list->end(); ++port_iter) {
        int outport = port_iter->first;
        port_to_geomid_[outport] = id;
      }
    }
  }

  // assume intergrp connections have same geometric port assigments
  coormap_xy_map_t* glinks = intergrp_conn_map_[0];
  for (coormap_xy_map_t::iterator lnk_it = glinks->begin();
       lnk_it != glinks->end(); ++lnk_it) {
    std::list< std::pair<int,int> >* port_list = lnk_it->second;
    if (port_list->size()) {
      std::list< std::pair<int,int> >::iterator port_iter = port_list->begin();
      for (; port_iter != port_list->end(); ++port_iter) {
        int outport = port_iter->first;
        port_to_geomid_[outport] = next_g;
      }
      ++next_g;
    }
  }

  // injection links are last
  for (int i=0; i < injection_ports_.size(); ++i) {
    port_to_geomid_[injection_ports_[i]] = next_g;
    ++next_g;
  }

}

} } //end of namespace sstmac


#endif