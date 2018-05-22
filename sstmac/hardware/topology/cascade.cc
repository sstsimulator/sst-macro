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

#include <sstmac/hardware/topology/cascade.h>
#include <sstmac/hardware/router/router.h>
#include <math.h>
#include <sstream>
#include <sprockit/stl_string.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/sim_parameters.h>

RegisterKeywords(
{ "topology_group_connections", "DEPRECATED: for group-based topologies, number of group connections per router" },
{ "group_connections", "for group-based topologies (dragonfly), number of group connections per router" },
{ "true_random_intermediate", "" },
);

namespace sstmac {
namespace hw {

static const double PI = 3.141592653589793238462;

cascade::cascade(sprockit::sim_parameters* params) :
  cartesian_topology(params,
                     InitMaxPortsIntra::I_Remembered,
                     InitGeomEjectID::I_Remembered)
{
  x_ = dimensions_[0];
  y_ = dimensions_[1];
  g_ = dimensions_[2];

  group_con_ = params->get_int_param("group_connections");
  true_random_intermediate_ =
      params->get_optional_bool_param("true_random_intermediate",
                                      false);

  //can never have more group connections than groups
  if (group_con_ >= g_){
    cerr0 << sprockit::printf("WARNING: requested %d group connections, "
                        "but max allowable is %d for %d groups - resetting value\n",
                        group_con_, g_-1, g_);
    group_con_ = g_ - 1;
  }

  max_ports_intra_network_ = x_ + y_ + g_;
  eject_geometric_id_ = max_ports_intra_network_;
}

void
cascade::configure_geometric_paths(std::vector<int> &redundancies)
{
  int npaths = x_ + y_ + group_con_ + netlinks_per_switch_;
  redundancies.resize(npaths);
  //do x paths, then y paths, then g paths
  int path = 0;
  for (int x=0; x < x_; ++x, ++path){
    redundancies[path] = red_[x_dimension];
  }
  for (int y=0; y < y_; ++y, ++path){
    redundancies[path] = red_[y_dimension];
  }
  for (int g=0; g < group_con_; ++g, ++path){
    redundancies[path] = red_[g_dimension];
  }
  configure_injection_geometry(redundancies);
}

switch_id
cascade::random_intermediate(router* rtr, switch_id current_sw, switch_id dest_sw, uint32_t seed)
{
  long nid = current_sw;
  uint32_t attempt = 0;
  while (current_sw == nid) {

    int srcX, srcY, srcG, dstX, dstY, dstG, hisX, hisY, hisG;
    get_coords(current_sw, srcX, srcY, srcG);
    get_coords(dest_sw, dstX, dstY, dstG);

    hisX = rtr->random_number(x_, attempt, seed);
    hisY = rtr->random_number(y_, attempt, seed);
    if(!true_random_intermediate_ && dstG == srcG) {
      // already on the correct group
      hisG = srcG;
    } else {
      //randomly select a group
      hisG = rtr->random_number(g_, attempt, seed);
      //now figure out which x,y,g fills the path
      //find_path_to_group(srcX, srcY, srcG, hisX, hisY, hisG);
    }

    nid = get_uid(hisX, hisY, hisG);
    ++attempt;
  }

  return switch_id(nid);
}

bool
cascade::xy_connected_to_group(int myX, int myY, int myG, int dstg) const
{
  int gstride = std::max(1, g_ / group_con_);
  int gconns = 0;
  int my_group_id = myX + myY * x_;
  int goffset = my_group_id % g_;
  int theg = myG + goffset;
  for (int g=0; g < group_con_; ++g, theg += gstride){
    theg = theg % g_;
    if (theg == dstg)
        return true;
  }
  return false;
}

bool
cascade::find_y_path_to_group(router* rtr, int myX, int myG, int dstG, int& dstY,
                              packet::path& path) const
{
  int ystart = rtr->random_number(y_,0,42);
  for (int yy = 0; yy < y_; ++yy) {
    dstY = (ystart + yy) % y_;
    if (xy_connected_to_group(myX, dstY, myG, dstG)) {
      path.set_outport(y_port(dstY));
      return true;
    }
  }
  return false;
}

bool
cascade::find_x_path_to_group(router* rtr, int myY, int myG, int dstG, int& dstX,
                              packet::path& path) const
{
  int xstart = rtr->random_number(x_,0,42);
  for (int xx = 0; xx < x_; ++xx) {
    dstX = (xstart + xx) % x_;
    if (xy_connected_to_group(dstX, myY, myG, dstG)) {
      path.set_outport( x_port(dstX) );
      return true;
    }
  }
  return false;
}

void
cascade::find_path_to_group(router* rtr, int myX, int myY, int myG,
                            int dstG, int& dstX, int& dstY,
                            packet::path& path) const
{
  //see if we can go directly to the group
  if (xy_connected_to_group(myX, myY, myG, dstG)){
    path.set_outport(g_port(dstG));
    dstX = myX;
    dstY = myY;
    return;
  }

  if (find_x_path_to_group(rtr, myY, myG, dstG, dstX, path)){
    dstY = myY;
    return;
  }

  if (find_y_path_to_group(rtr, myX, myG, dstG, dstY, path)){
    dstX = myX;
    return;
  }

  //both x and y need to change
  int xstart = 0;
  for (int xx = 0; xx < x_; ++xx) {
    dstX = (xstart + xx) % x_;
    if (find_y_path_to_group(rtr, dstX, myG, dstG, dstY, path)){
      return;
    }
  }

  spkt_throw_printf(sprockit::value_error,
    "cascade::route: unable to find path from group %d to group %d",
    myG, dstG);
}


void
cascade::minimal_route_to_switch(
  router* rtr,
  switch_id src,
  switch_id dst,
  packet::path &path) const
{
  int srcX, srcY, srcG; get_coords(src, srcX, srcY, srcG);
  int dstX, dstY, dstG; get_coords(dst, dstX, dstY, dstG);
  int interX, interY;
  if (srcG != dstG){
    find_path_to_group(rtr, srcX, srcY, srcG, dstG, interX, interY, path);
    top_debug("cascade routing from (%d,%d,%d) to (%d,%d,%d) through "
              "gateway (%d,%d,%d) on port %d",
              srcX, srcY, srcG, dstX, dstY, dstG,
              interX, interY, srcG, path.outport());
  } else if (srcX != dstX){
    path.set_outport( x_port(dstX) );
    top_debug("cascade routing X from (%d,%d,%d) to (%d,%d,%d) on port %d",
              srcX, srcY, srcG, dstX, dstY, dstG, path.outport());
  } else if (srcY != dstY){
    path.set_outport( y_port(dstY) );
    top_debug("cascade routing Y from (%d,%d,%d) to (%d,%d,%d) on port %d",
              srcX, srcY, srcG, dstX, dstY, dstG, path.outport());
  } else {
    spkt_abort_printf("cascade routing error from %d to %d", src, dst);
  }
}

int
cascade::minimal_distance(switch_id src, switch_id dst) const
{
  int dist = 0;
  int srcX, srcY, srcG; get_coords(src, srcX, srcY, srcG);
  int dstX, dstY, dstG; get_coords(dst, dstX, dstY, dstG);
  if (srcG == dstG){
    if (srcX != dstX) ++dist;
    if (srcY != dstY) ++dist;
  } else {
    return 5; //just - don't bother for now
    /**
    packet::path path;
    int interX;
    int interY;
    find_path_to_group(srcX, srcY, srcG, dstG, interX, interY, path);
    dist = 1; //group hop
    if (srcX != interX) ++dist;
    if (srcY != interY) ++dist;
    if (dstX != interX) ++dist;
    if (dstY != interY) ++dist;
    */
  }
  return dist;
}




void
cascade::setup_port_params(sprockit::sim_parameters* params, int dim, int dimsize) const
{
  sprockit::sim_parameters* link_params = params->get_namespace("link");
  double bw = link_params->get_bandwidth_param("bandwidth");
  int bufsize = params->get_optional_byte_length_param("buffer_size", 0);

  double port_bw = bw * red_[dim];
  int credits = bufsize * red_[dim];

  for (int i=0; i < dimsize; ++i){
    int port = convert_to_port(dim, i);
  //std::cout << "setting port " << port << " to " << port_bw << " " << credits << std::endl;
    sprockit::sim_parameters* port_params = topology
        ::setup_port_params(port, credits, port_bw, link_params, params);
  }
}

void
cascade::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  int max_num_conns = (x_ - 1) + (y_ - 1) + group_con_;
  conns.resize(max_num_conns);

  int myx;
  int myy;
  int myg;
  get_coords(src, myx, myy, myg);

  int cidx = 0;

  for (int x = 0; x < x_; x++) {
    if (x != myx) {
      switch_id dst(get_uid(x, myy, myg));
      connection& conn = conns[cidx];
      conn.src = src;
      conn.dst = dst;
      conn.src_outport = x_port(x);
      conn.dst_inport = x_port(myx);
      ++cidx;
    }
  }

  for (int y = 0; y < y_; y++) {
    if (y != myy) {
      switch_id dst(get_uid(myx, y, myg));
      connection& conn = conns[cidx];
      conn.src = src;
      conn.dst = dst;
      conn.src_outport = y_port(y);
      conn.dst_inport = y_port(myy);
      ++cidx;
    }
  }

  for (int g=0; g < group_con_; ++g){
    int dstg = xyg_dir_to_group(myx,myy,myg,g);
    if (dstg == myg) continue;

    switch_id dst(get_uid(myx, myy, dstg));
    connection& conn = conns[cidx];
    conn.src = src;
    conn.dst = dst;
    conn.src_outport = g_port(dstg);
    conn.dst_inport = g_port(myg);
    ++cidx;
  }

  conns.resize(cidx);
}

void
cascade::configure_individual_port_params(switch_id src,
                                            sprockit::sim_parameters *switch_params) const
{
  setup_port_params(switch_params, x_dimension, x_);
  setup_port_params(switch_params, y_dimension, y_);
  setup_port_params(switch_params, g_dimension, g_);
}

int
cascade::xyg_dir_to_group(int myX, int myY, int myG, int dir) const
{
  int gspace = std::max(1, g_ / group_con_);
  int myid = myX + myY * x_;
  int gset = myid % g_;
  return (myG + gset + gspace * dir) % g_;
}

coordinates
cascade::switch_coords(switch_id uid) const
{
  coordinates coords(3);
  get_coords(uid, coords[0], coords[1], coords[2]);
  return coords;
}

switch_id
cascade::switch_addr(const coordinates &coords) const
{
  return get_uid(coords[0], coords[1], coords[2]);
}


}
} //end of namespace sstmac
