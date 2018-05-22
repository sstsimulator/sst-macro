/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#include <sstmac/hardware/topology/tiled_cascade.h>
#include <math.h>
#include <sstream>
#include <algorithm>
#include <sprockit/stl_string.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/fileio.h>
#include <sprockit/regexp.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
{ "intragroup_connection_file", "a file specifying port connections within a group" },
{ "intergroup_connection_file", "a file specifying port connections between groups" },
);

using namespace sprockit;

namespace sstmac {
namespace hw {

tiled_cascade::tiled_cascade(sprockit::sim_parameters* params) :
  cascade(params)
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
    sprockit::abort("tiled_cascade needs injection_ports specified");
  top_debug("ntiles in x,y dimensions: %d,%d", tiles_x_, tiles_y_);
  int max_port = *std::max_element(injection_ports_.begin(),injection_ports_.end());
  if (max_port > n_tiles_ - 1)
    spkt_throw_printf(sprockit::value_error,"max port index %d exceeds max tile index %d",
                      max_port,n_tiles_-1);
  max_ports_injection_ = injection_ports_.size();

  // initialize data structures
  int group_size = x_ * y_;
  int nswitches = num_switches();
  for(int i=0; i < nswitches; ++i) {
    intragrp_conn_map_.push_back(new coor_xy_map_t);
    for(int j=0; j < nswitches; ++j) {
      intragrp_conn_map_[i]->push_back(new xy_list_t);
    }
  }
  for(int i=0; i < nswitches; ++i)
    intergrp_conn_map_.push_back(new coormap_xy_map_t);
  switch_to_connected_groups_.resize(nswitches);

  n_geom_paths_ = x_ + y_ + g_ + netlinks_per_switch_;
  outports_.resize(nswitches);
  for (int i=0; i < nswitches; ++i)
    outports_[i].resize(n_geom_paths_);

  read_intragroup_connections();
  read_intergroup_connections();
  configure_outports();
}

switch_id
tiled_cascade::netlink_to_injection_switch(
    node_id nodeaddr, uint16_t ports[], int &num_ports) const
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
tiled_cascade::netlink_to_ejection_switch(
    node_id nodeaddr, uint16_t ports[], int &num_ports) const
{
  return netlink_to_injection_switch(nodeaddr,ports,num_ports);
}

bool
tiled_cascade::xy_connected_to_group(int myX, int myY, int myG, int dstg) const
{
  coordinates me(3);
  me[0] = myX;
  me[1] = myY;
  me[2] = myG;
  const std::set<int>& my_groups = switch_to_connected_groups_[switch_addr(me)];
  const std::set<int>::iterator iter = my_groups.find(dstg);
  if (iter != my_groups.end())
    return true;
  return false;
}

void
tiled_cascade::connected_outports(switch_id src, std::vector<sstmac::hw::topology::connection>& conns) const
{
  int max_num_conns = red_[0] * (x_ - 1) + red_[1] * (y_ - 1) + group_con_;
  conns.resize(max_num_conns);
  int index=0;

  coor_xy_map_t* dst_vec = intragrp_conn_map_[src];
  for (coor_xy_map_t::iterator dst_it = dst_vec->begin();
       dst_it != dst_vec->end(); ++dst_it) {
    xy_list_t* conn_list = *dst_it;
    for (xy_list_iter conn_it = conn_list->begin();
         conn_it != conn_list->end(); ++conn_it) {
      sstmac::hw::topology::connection& conn = conns[index];
      conn.src = src;
      conn.dst = dst_it - dst_vec->begin();
      conn.src_outport = (*conn_it).first;
      conn.dst_inport = (*conn_it).second;
//      std::cerr << sprockit::printf("adding intra conn: src:%d dst:%d op:%d ip:%d\n",
//             conn.src, conn.dst, conn.src_outport, conn.dst_inport);
      ++index;
    }
  }

  coormap_xy_map_t* dst_map = intergrp_conn_map_[src];
  for (coormap_xy_map_t::iterator dst_it = dst_map->begin();
       dst_it != dst_map->end(); ++dst_it) {
    xy_list_t* conn_list = (*dst_it).second;
    for (xy_list_iter conn_it = conn_list->begin();
         conn_it != conn_list->end(); ++conn_it) {
      sstmac::hw::topology::connection& conn = conns[index];
      conn.src = src;
      conn.dst = (*dst_it).first;
      conn.src_outport = (*conn_it).first;
      conn.dst_inport = (*conn_it).second;
//      std::cerr << sprockit::printf("adding inter conn: src:%d dst:%d op:%d ip:%d\n",
//             conn.src, conn.dst, conn.src_outport, conn.dst_inport);
      ++index;
    }
  }

}



//---------------------------------------------------------------------
// Multipath topology
//---------------------------------------------------------------------

void
tiled_cascade::get_redundant_paths(packet::path& current,
                                   packet::path_set& paths,
                                   switch_id addr) const
{
  int geomid = current.geometric_id;
  int dim;
  if (geomid < eject_geometric_id_){
    //intranetwork routing
    if (geomid < x_)
      dim = 0;
    else if (geomid < (x_ + y_))
      dim = 1;
    else
      dim = 2;

    int red = red_[dim];
    paths.resize(red);
    const std::list<int> &ports = outports_[addr][geomid];

    int pi=0;
    for (std::list<int>::const_iterator it = ports.begin();
         it != ports.end(); ++it) {
      paths[pi] = current;
      paths[pi].geometric_id = geomid;
      paths[pi].set_outport(*it);
      ++pi;
    }
  }
  else {
    //ejection routing
    paths.resize(injection_redundancy_);
    int offset = (geomid - eject_geometric_id_) * injection_redundancy_;
    for (int i=0; i < injection_redundancy_; ++i) {
      paths[i] = current;
      paths[i].set_outport(injection_ports_[offset+i]);
    }
  }
}

void
tiled_cascade::configure_geometric_paths(std::vector<int> &redundancies)
{
  redundancies.resize(n_geom_paths_);
  for (int i=0; i < n_geom_paths_; ++i)
    redundancies[i] = 0;

  for (int srcid=0; srcid < num_switches(); ++srcid) {
    for (int geomid=0; geomid < n_geom_paths_; ++geomid) {
      int gsize = outports_[srcid][geomid].size();
      if (gsize > redundancies[geomid])
        redundancies[geomid] = gsize;
    }
  }

  for (int i=0; i < netlinks_per_switch_; ++i){
    redundancies[eject_geometric_id_ + i] = injection_redundancy_;
  }
}

//-----------------------------------------------------------------------
// Connection Setup
//-----------------------------------------------------------------------

void
tiled_cascade::read_intragroup_connections()
{
  // read the file and make list of connections
  std::ifstream in;
  sprockit::SpktFileIO::open_file(in, intragroup_file_);
  if (!in.is_open()) {
    spkt_throw_printf(sprockit::input_error,
                      "tiled_connect_objects: could not find intragroup connection file %s",
                      intragroup_file_.c_str());
  }

  int src_x, src_y, src_port_x, src_port_y,
      dst_x, dst_y, dst_port_x, dst_port_y;
  while (in >> src_x)
  {
    std::string delimiter;
    in >> src_y;
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

  // insert connections into more convenient data structure
  for (int g=0; g<numG(); ++g) {
    for( std::list<connection>::iterator it=intragrp_conns_.begin();
         it!=intragrp_conns_.end(); ++it ) {

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

      // hack to map Edison's two geometric configurations back to a single config
//      int port_remap[48] = {
//        7, -1, -1, -1, -1, -1, -1, 0, 14, 15,
//        -1, -1, -1, -1, 8, 9, 20, 23, 28, 36,
//        16, 17, 18, 19, 29, 31, 38, 21, 24, 25,
//        26, 27, 30, 37, 39, 22, 32, 33, 34, 35,
//        -1, -1, -1, -1, -1, -1, -1, -1 };
//      if (src_x > 7) {
//        if (port_remap[outport] < 0)
//          sprockit::abort("bad remap value\n");
//        outport = port_remap[outport];
//      }
//      if (dst_x > 7) {
//        if (port_remap[inport] < 0)
//          sprockit::abort("bad remap value\n");
//        inport = port_remap[inport];
//      }

      top_debug("node %d,%s connecting to node %d,%s: outport %d, inport %d",
                int(src_id), set_string(src_x, src_y, g).c_str(),
                int(dst_id), set_string(dst_x, dst_y, g).c_str(),
                outport,inport);

      intragrp_conn_map_[src_id]->at(dst_id)->push_back(
          std::pair<int,int>(outport,inport));
    }
  }
}

void
tiled_cascade::read_intergroup_connections()
{
  // read the file and make list of connections
  std::ifstream in;
  sprockit::SpktFileIO::open_file(in, intergroup_file_);
  if (!in.is_open()) {
    spkt_throw_printf(
          sprockit::input_error,
          "tiled_connect_objects: could not find intergroup connection file %s",
          intergroup_file_.c_str());
  }

  int src_x, src_y, src_g, src_port_x, src_port_y,
      dst_x, dst_y, dst_g, dst_port_x, dst_port_y;
  while (in >> src_x)
  {
    std::string delimiter;
    in >> src_y >> src_g;
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

  // insert connections into more convenient data structure
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
      intergrp_conn_map_[src_id]->emplace(dst_id,new xy_list_t);
    }
    intergrp_conn_map_[src_id]->at(dst_id)->push_back(
          std::pair<int,int>(outport,inport));
  }
}

void
tiled_cascade::configure_outports()
{
  int geomid, srcid, dstid;
  for (int srcx=0; srcx < x_; ++srcx) {
    for (int srcy=0; srcy < y_; ++srcy) {
      for (int srcg=0; srcg < g_; ++srcg) {
        srcid = get_uid(srcx, srcy, srcg);

        // intragroup
        for (int dstx=0; dstx < x_; ++dstx) {
          for (int dsty=0; dsty < y_; ++dsty) {
            int dstg = srcg;
            dstid = get_uid(dstx, dsty, dstg);
            if (srcy == dsty)
              geomid = dstx;
            else if (srcx == dstx)
              geomid = x_ + dsty;

            // iterate over group connections, add outports to list
            xy_list_t* conn_list =
                intragrp_conn_map_[srcid]->at(dstid);
            for (xy_list_iter it=conn_list->begin();
                 it != conn_list->end(); ++it) {
              outports_[srcid][geomid].push_back(it->first);
            }
          }
        }

        // intergroup
        coormap_xy_map_t* glinks = intergrp_conn_map_[srcid];
        for (coormap_xy_map_t::iterator lnk_it = glinks->begin();
             lnk_it != glinks->end(); ++lnk_it) {
          std::list< std::pair<int,int> >* port_list = lnk_it->second;
          if (port_list->size()) {
            std::list< std::pair<int,int> >::iterator port_iter = port_list->begin();
            for (; port_iter != port_list->end(); ++port_iter) {
              int gx, gy, gg;
              get_coords(lnk_it->first, gx, gy, gg);
              outports_[srcid][x_ + y_ + gg].push_back(port_iter->first);
            }
          }
        }

      }
    }
  }

  // could check here that all nonzero redundancies are equal
  int max=0;
  for (int i=0; i<x_; ++i)
    max = std::max(max, (int) outports_[0][i].size());
  red_[0] = max;
  max = 0;
  for (int i=x_; i<x_+y_; ++i)
    max = std::max(max, (int) outports_[0][i].size());
  red_[1] = max;
  max = 0;
  for (int i=x_+y_; i<x_+y_+g_; ++i)
    max = std::max(max, (int) outports_[0][i].size());
  red_[2] = max;
}

} } //end of namespace sstmac
