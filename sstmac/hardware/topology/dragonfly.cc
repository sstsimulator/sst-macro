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
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/router/router.h>
#include <math.h>
#include <sstream>
#include <sprockit/stl_string.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("dragonfly | dfly", topology, dragonfly);

static const double PI = 3.141592653589793238462;

void
dragonfly::init_factory_params(sprockit::sim_parameters* params)
{
  init_common_params(params);
  max_ports_intra_network_ = x_ + y_ + g_;
  eject_geometric_id_ = max_ports_intra_network_;
  max_ports_injection_ = endpoints_per_switch_;
  cartesian_topology::init_factory_params(params);
}

void
dragonfly::init_common_params(sprockit::sim_parameters* params)
{
  std::vector<int> args;
  params->get_vector_param("geometry", args);
  x_ = args[0];
  y_ = args[1];
  g_ = args[2];

  group_con_ = params->get_int_param("group_connections");
  true_random_intermediate_ =
      params->get_optional_bool_param("true_random_intermediate",
                                      false);

  //can never have more group connections than groups
  if (group_con_ >= g_){
    cerr0 << sprockit::printf("WARNING: requested %d group connections, but max allowable is %d for %d groups - resetting value\n",
                        group_con_, g_-1, g_);
    group_con_ = g_ - 1;
  }

  endpoints_per_switch_ = params->get_optional_int_param("concentration", 1);
}

void
dragonfly::productive_path(
  int dim,
  const coordinates &src,
  const coordinates &dst,
  routable::path& path) const
{
  //if we crossed a global link in the past, set to 1
  path.vc = path.metadata_bit(routable::crossed_timeline) ? 1 : 0;
  int nextDim, nextDir;
  if (dim == g_dimension){
    int myX = src[x_dimension];
    int myY = src[y_dimension];
    int myG = src[g_dimension];
    int dstg = dst[g_dimension];
    minimal_route_to_group(myX, myY, myG, nextDim, nextDir, dstg);
    if  (nextDim == g_dimension){
        path.set_metadata_bit(routable::crossed_timeline);
    }
  }
  else {
    nextDim = dim;;
    nextDir = dst[dim];
  }
  path.outport = convert_to_port(nextDim, nextDir);
}

void
dragonfly::configure_geometric_paths(std::vector<int> &redundancies)
{
  int npaths = x_ + y_ + group_con_ + endpoints_per_switch_;
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

void
dragonfly::get_coords(long uid, int &x, int &y, int &g) const
{
  x = uid % x_;
  y = (uid / x_) % y_;
  g = uid / (x_ * y_);
}

long
dragonfly::get_uid(int x, int y, int g) const
{
  return g * (x_ * y_) + y * (x_) + x;
}

void
dragonfly::compute_switch_coords(switch_id uid, coordinates& coords) const
{
  coords.resize(3);
  get_coords(uid, coords[0], coords[1], coords[2]);
}

coordinates
dragonfly::neighbor_at_port(switch_id sid, int port)
{
  coordinates coords;
  compute_switch_coords(sid, coords);

  int dim, dir;
  if (port >= (x_ + y_)){ //g
    if (port >= (x_ + y_ + g_)) // eject
      return coords;
    int dir = port - (x_ + y_);
    coords[g_dimension] =
        xyg_dir_to_group(coords[x_dimension], coords[y_dimension],
                        coords[g_dimension], dir);
  }
  else if (port >= x_) //y
    coords[y_dimension] = port - x_;
  else //x
    coords[x_dimension] = port;
  return coords;
}

int
dragonfly::convert_to_port(int dim, int dir) const
{
  if (dim == x_dimension) {
    return dir;
  }
  else if (dim == y_dimension) {
    return x_ + dir;
  }
  else {
    return x_ + y_ + dir;
  }
}

switch_id
dragonfly::switch_number(const coordinates &coords) const
{
  long uid = get_uid(coords[0], coords[1], coords[2]);
  return switch_id(uid);
}

switch_id
dragonfly::random_intermediate_switch(switch_id current_sw, switch_id dest_sw)
{
  long nid = current_sw;
  uint32_t attempt = 0;
  while (current_sw == nid) {

    int srcX, srcY, srcG, dstX, dstY, dstG, hisX, hisY, hisG;
    get_coords(current_sw, srcX, srcY, srcG);
    get_coords(dest_sw, dstX, dstY, dstG);

    if(!true_random_intermediate_ && dstG == srcG) {
      // already on the correct group
      hisG = srcG;
      hisX = random_number(x_, attempt);
      hisY = random_number(y_, attempt);
    }
    else {
      //randomly select a group
      hisG = random_number(g_, attempt);
      //now figure out which x,y,g fills the path
      find_path_to_group(srcX, srcY, srcG, hisX, hisY, hisG);
    }

    nid = get_uid(hisX, hisY, hisG);
    ++attempt;
  }

  return switch_id(nid);
}

bool
dragonfly::xy_connected_to_group(int myX, int myY, int myG, int dstg) const
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

int
dragonfly::find_y_path_to_group(int myX, int myG, int dstg) const
{
  int ystart = random_number(y_,0);
  for (int yy = 0; yy < y_; ++yy) {
    int dsty = (ystart + yy) % y_;
    if (xy_connected_to_group(myX, dsty, myG, dstg)) {
      return dsty;
    }
  }
  return -1;
}

int
dragonfly::find_x_path_to_group(int myY, int myG, int dstg) const
{
  int xstart = random_number(x_,0);
  for (int xx = 0; xx < x_; ++xx) {
    int dstx = (xstart + xx) % x_;
    if (xy_connected_to_group(dstx, myY, myG, dstg)) {
      return dstx;
    }
  }
  return -1;
}

void
dragonfly::find_path_to_group(int myX, int myY, int myG,
                              int& dstx, int& dsty, int dstg) const
{
  //see if we can go directly to the group
  if (xy_connected_to_group(myX, myY, myG, dstg)){
    dstx = myX;
    dsty = myY;
    return;
  }

  int x_or_y = 0; 
  //try to find x,y path first
  int x_first = 0;
  if (x_or_y == x_first) {
    dstx = find_x_path_to_group(myY, myG, dstg);
    if (dstx >= 0) {
      dsty = myY;
      return;
    }
  }
  else {
    dsty = find_y_path_to_group(myX, myG, dstg);
    if (dsty >= 0) {
      dstx = myX;
      return;
    }
  }

  //both x and y need to change
  int xstart = 0; 
  for (int xx = 0; xx < x_; ++xx) {
    dstx = (xstart + xx) % x_;
    dsty = find_y_path_to_group(dstx, myG, dstg);
    if (dsty >= 0) {
      return;  //path found
    }
  }

  spkt_throw_printf(sprockit::value_error,
    "dragonfly::route: unable to find path from group %d to group %d",
    myG, dstg);
}

void
dragonfly::minimal_route_to_group(int myX, int myY, int myG,
     int& dim, int& dir, int dstg) const
{
  int dsty, dstx;
  find_path_to_group(myX, myY, myG, dstx, dsty, dstg);

  debug_printf(sprockit::dbg::router,
    "Minimal route to group from src (%d,%d,%d) goes through (%d,%d,%d)",
    myX, myY, myG, dstx, dsty, dstg);
    

  if (dstx != myX) {
    dim = dragonfly::x_dimension;
    dir = dstx;
  }
  else if (dsty != myY) {
    dim = dragonfly::y_dimension;
    dir = dsty;
  }
  else {
    dim = dragonfly::g_dimension;
    dir = dstg;
  }
}

int
dragonfly::minimal_route_to_X(int hisx) const
{
  return hisx;
}

int
dragonfly::minimal_route_to_Y(int hisy) const
{
  return hisy;
}

void
dragonfly::minimal_route_to_coords(
  const coordinates &current_coords,
  const coordinates &dest_coords,
  routable::path& path) const
{
  debug_printf(sprockit::dbg::router,
    "Finding dragonfly minimal route from %s to %s",
    current_coords.to_string().c_str(),
    dest_coords.to_string().c_str());

  if (current_coords[g_dimension] != dest_coords[g_dimension]){
    productive_path(g_dimension, current_coords, dest_coords, path);
  }
  else if (current_coords[x_dimension] != dest_coords[x_dimension]){
    productive_path(x_dimension, current_coords, dest_coords, path);
  }
  else if (current_coords[y_dimension] != dest_coords[y_dimension]){
    productive_path(y_dimension, current_coords, dest_coords, path);
  }
}

int
dragonfly::minimal_distance(
  const coordinates &current_coords,
  const coordinates &dest_coords) const
{
  int dstx, dsty;
  if (current_coords[2] == dest_coords[2]) {
    int path_length = 0;
    if (dest_coords[0] != current_coords[0]) {
      ++path_length;
    }
    if (dest_coords[1] != current_coords[1]) {
      ++path_length;
    }
    //same group, all that matters is x,y difference
    return path_length;
  }

  find_path_to_group(current_coords[0], current_coords[1], current_coords[2],
                     dstx, dsty, dest_coords[2]);

  int path_length = 1; //group jump
  //we might need x,y jump before group jump
  if (dstx != current_coords[0]) {
    ++path_length;
  }
  if (dsty != current_coords[1]) {
    ++path_length;
  }
  //we might need x,y jump after group jump
  if (dstx != dest_coords[0]) {
    ++path_length;
  }
  if (dsty != dest_coords[1]) {
    ++path_length;
  }
  return path_length;
}

/// coordinates go x,y - group
void
dragonfly::connect_objects(internal_connectable_map& objects)
{
  connectable::config cfg;
  cfg.ty = connectable::RedundantConnection;
  for (long i = 0; i < objects.size(); i++) {
    std::vector<int> connected;
    std::vector<int> connected_red;

    switch_id me(i);
    int myx;
    int myy;
    int myg;
    get_coords(i, myx, myy, myg);

    internal_connectable_map::iterator itme = objects.find(me);

    if (itme == objects.end()) {
      spkt_throw_printf(
        sprockit::spkt_error,
        "dragon::connect_switches(): can't find switch when connecting:   uid=%d  addr=%ld",
        get_uid(myx, myy, myg), long(me));
    }

    for (int x = 0; x < x_; x++) {
      if (x != myx) {

        switch_id them(get_uid(x, myy, myg));

        internal_connectable_map::iterator it1 = objects.find(them);

        if (it1 == objects.end()) {
          spkt_throw_printf(
            sprockit::spkt_error,
            "dragon::connect_train_tracks(): can't find switch 1 when connecting:   uid=%d  addr=%ld",
            get_uid(x, myy, myg), long(them));
        }

        int outport = convert_to_port(x_dimension, x);
        int inport = convert_to_port(x_dimension, myx);

        top_debug("node %d,%s connecting to node %d,%s on ports %d:%d",
            int(me), set_string(myx, myy, myg).c_str(),
            int(them), set_string(x, myy, myg).c_str(),
            outport, inport);

        cfg.red = red_[x_dimension];
        objects[me]->connect(
          outport,
          inport,
          connectable::output,
          objects[them], &cfg);

        objects[them]->connect(
          outport,
          inport,
          connectable::input,
          objects[me], &cfg);

        if (outputgraph_) {
          connected.push_back(them);
          connected_red.push_back(red_[0]);
        }
      }
    }

    for (int y = 0; y < y_; y++) {
      if (y != myy) {

        switch_id them(get_uid(myx, y, myg));

        int outport = convert_to_port(y_dimension, y);
        int inport = convert_to_port(y_dimension, myy);

        top_debug("node %d,%s connecting to node %d,%s on ports %d:%d",
            int(me), set_string(myx, myy, myg).c_str(),
            int(them), set_string(myx, y, myg).c_str(),
            outport, inport);

        cfg.red = red_[y_dimension];
        objects[me]->connect(
          outport,
          inport,
          connectable::output,
          objects[them], &cfg);

        objects[them]->connect(
          outport,
          inport,
          connectable::input,
          objects[me], &cfg);

        if (outputgraph_) {
          connected.push_back(them);
          connected_red.push_back(red_[1]);
        }
      }
    }

    for (int g=0; g < group_con_; ++g){
      int dstg = xyg_dir_to_group(myx,myy,myg,g);

      long uid = get_uid(myx, myy, dstg);
      switch_id them(uid);

      int outport = convert_to_port(g_dimension, dstg);
      int inport = convert_to_port(g_dimension, myg);

      top_debug("node %d,%s connecting to node %d,%s on ports %d:%d",
        int(me), set_string(myx, myy, myg).c_str(),
        int(them), set_string(myx, myy, dstg).c_str(),
        outport, inport);

      cfg.red = red_[g_dimension];
      objects[me]->connect(
          outport,
          inport,
          connectable::output,
          objects[them], &cfg);

      objects[them]->connect(
          outport,
          inport,
          connectable::input,
          objects[me], &cfg);

    }

    if (outputgraph_) {
      for (int k = 0; k < endpoints_per_switch_; k++) {
        cout0 << (i * endpoints_per_switch_ + k);
        for (int l = 0; l < connected.size(); l++) {
          for (int m = 0; m < endpoints_per_switch_; m++) {
            cout0 << " " << (connected[l] * endpoints_per_switch_ + m) << ":"
                      << connected_red[l];

          }
        }

        for (int m = 0; m < endpoints_per_switch_; m++) {
          if (m != k) {
            cout0 << " " << (i * endpoints_per_switch_ + m) << ":" << 100;
          }
        }
        cout0 << "\n";
      } //end k loop
    } //end outputgraph if
  } //end objects loop
}

void
dragonfly::configure_vc_routing(std::map<routing::algorithm_t, int>& m) const
{
  m[routing::minimal] = 2;
  m[routing::minimal_adaptive] = 2;
  m[routing::valiant] = 4;
  m[routing::ugal] = 6;
}

void
dragonfly::nearest_neighbor_partners(const coordinates &src_sw_coords,
    int port, std::vector<node_id>& partners) const
{
  //+X,-X,+Y,-Z and group connections
  partners.clear();
  partners.resize(4 + group_con_);
  int idx = 0;
  long nid;
  coordinates tmp_coords(src_sw_coords);

  //get the plus,minus 1 in the x-direction
  tmp_coords[x_dimension] = (src_sw_coords[x_dimension] + 1) % x_;
  partners[idx++] = node_addr(tmp_coords, port);

  tmp_coords[x_dimension] = (src_sw_coords[x_dimension] - 1 + x_) % x_;
  partners[idx++] = node_addr(tmp_coords, port);

  //reset coords
  tmp_coords[x_dimension] = src_sw_coords[x_dimension];

  tmp_coords[y_dimension] = (src_sw_coords[y_dimension] + 1) % y_;
  partners[idx++] = node_addr(tmp_coords, port);

  tmp_coords[y_dimension] = (src_sw_coords[y_dimension] - 1 + y_) % y_;
  partners[idx++] = node_addr(tmp_coords, port);

  //reset coords
  tmp_coords[y_dimension] = src_sw_coords[y_dimension];

  //now we have to figure out our group connections
  for (int g = 0; g < group_con_; g++) {
    int gid = xyg_dir_to_group(src_sw_coords[x_dimension],
                               src_sw_coords[y_dimension],
                               src_sw_coords[g_dimension], g);
    if (gid == src_sw_coords[g_dimension]) {
      continue;
    }

    tmp_coords[g_dimension] = gid;
    partners[idx++] = node_addr(tmp_coords, port);
  }
  partners.resize(idx);
}

void
dragonfly::tornado_recv_partners(
  const coordinates &src_sw_coords,
  int port,
  std::vector<node_id>& partners) const
{
  partners.resize(1);
  coordinates tmp_coords(src_sw_coords);
  tmp_coords[x_dimension]
    = (src_sw_coords[x_dimension] - (x_ - 1) / 2 + x_) % x_;
  tmp_coords[y_dimension]
    = (src_sw_coords[y_dimension] - (y_ - 1) / 2 + y_) % y_;
  tmp_coords[g_dimension]
    = (src_sw_coords[g_dimension] - (g_ - 1) / 2 + g_) % g_;

  partners[0] = node_addr(tmp_coords, port);
}

void
dragonfly::tornado_send_partners(
  const coordinates &src_sw_coords,
  int port,
  std::vector<node_id>& partners) const
{
  partners.resize(1);
  coordinates tmp_coords(src_sw_coords);
  tmp_coords[x_dimension] = (src_sw_coords[x_dimension] + (x_ - 1) / 2)
                            % x_;
  tmp_coords[y_dimension] = (src_sw_coords[y_dimension] + (y_ - 1) / 2)
                            % y_;
  tmp_coords[g_dimension] = (src_sw_coords[g_dimension] + (g_ - 1) / 2)
                            % g_;

  partners[0] = node_addr(tmp_coords, port);
}

void
dragonfly::bit_complement_partners(
  const coordinates &src_sw_coords,
  int port,
  std::vector<node_id>& partners) const
{
  partners.resize(1);
  coordinates tmp_coords(src_sw_coords);
  tmp_coords[x_dimension] = x_ - src_sw_coords[x_dimension] - 1;
  tmp_coords[y_dimension] = y_ - src_sw_coords[y_dimension] - 1;
  tmp_coords[g_dimension] = g_ - src_sw_coords[g_dimension] - 1;

  partners[0] = node_addr(tmp_coords, port);
}

void
dragonfly::new_routing_stage(routable* rtbl)
{
  rtbl->current_path().unset_metadata_bit(routable::crossed_timeline);
}

int
dragonfly::xyg_dir_to_group(int myX, int myY, int myG, int dir) const
{
  int gspace = std::max(1, g_ / group_con_);
  int myid = myX + myY * x_;
  int gset = myid % g_;
  return (myG + gset + gspace * dir) % g_;
}

}
} //end of namespace sstmac



