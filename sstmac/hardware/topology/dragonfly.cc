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

static RegisterEnum(routing_info::metadata_slot, crossed_global_link);

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
  routing_info::path& path) const
{
  //if we crossed a global link in the past, set to 1
  path.vc = path.metadata_bit(crossed_global_link) ? 1 : 0;
  int nextDim, nextDir;
  if (dim == g_dimension){
    int myX = src[x_dimension];
    int myY = src[y_dimension];
    int myG = src[g_dimension];
    int dstg = dst[g_dimension];
    minimal_route_to_group(myX, myY, myG, nextDim, nextDir, dstg);
    if  (nextDim == g_dimension){
        path.set_metadata_bit(crossed_global_link);
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

  debug_printf(sprockit::dbg::routing,
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
  routing_info::path& path) const
{
  debug_printf(sprockit::dbg::routing,
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
  double intra_group_weight = 1.0;
  double inter_group_weight = 1.0; //use red for now
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

        objects[me]->connect_weighted(
          outport,
          inport,
          connectable::output,
          objects[them],
          intra_group_weight,
          red_[x_dimension]);

        objects[them]->connect(
          outport,
          inport,
          connectable::input,
          objects[me]);

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

        objects[me]->connect_weighted(
          outport,
          inport,
          connectable::output,
          objects[them],
          intra_group_weight,
          red_[y_dimension]);

        objects[them]->connect(
          outport,
          inport,
          connectable::input,
          objects[me]);

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

      objects[me]->connect_weighted(
          outport,
          inport,
          connectable::output,
          objects[them],
          inter_group_weight,
          red_[g_dimension]);

      objects[them]->connect(
          outport,
          inport,
          connectable::input,
          objects[me]);

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

double
dragonfly::rad_d() const
{

  int d = ((2.0 * (double) x_ + 1) / 2.0) / sin(
            (180.0 / (double) g_) / (180.0 / PI));

  return d;
}

void
dragonfly::get_switch_XYZ(int dflyx, int dflyy, int dflyg, double &x,
                          double &y, double &z)
{
  double d = rad_d();
  double groups = g_;
  x = d * sin((dflyg / groups) * 2 * PI) + (x_ - 2 * dflyx + 0.5) * cos(
        (dflyg / groups) * 2 * PI);
  y = d * cos((dflyg / groups) * 2 * PI) - (x_ - 2 * dflyx + 0.5) * sin(
        (dflyg / groups) * 2 * PI);

  z = 2 * dflyy;
}

vis::vis_obj*
dragonfly::create_global_vis_link(vis::vis_engine* eng,
                                  int dflyx, int dflyy, int fromg, int tog)
{

  double d = rad_d();
  double x1 = 0, y1 = 0, z1 = 0, x2 = 0, y2 = 0, z2 = 0;
  get_switch_XYZ(dflyx, dflyy, fromg, x1, y1, z1);
  get_switch_XYZ(dflyx, dflyy, tog, x2, y2, z2);

  x1 += 0.5 * cos((fromg / g_) * 2 * PI);
  y1 += 0.5 * sin((fromg / g_) * 2 * PI);

  x2 += 0.5 * cos((tog / g_) * 2 * PI);
  y2 += 0.5 * sin((tog / g_) * 2 * PI);

  //  double x = sqrt(pow(x2 - x1, 2.0) + pow(y2 - y1, 2.0));

  // double theta = 0;
  //   theta = asin((x2 - x1) / x);


  // double diff_ang = (tog - fromg) * (360 / g_);
  // double half_ang = diff_ang / 2.0;
  // double x = d * sin(half_ang / (180.0 / PI));

  //double fake = (tog + fromg) / 2.0;
  //double phi = (360 / g_) * std::min(fake, g_ - fake);
  //double theta = 90 - phi;

  // vis::vis_obj* lnk = eng->create_cylinder((x1 + x2) / 2.0,
  //     (y1 + y2) / 2.0, (z1 + z2) / 2.0, 0.125,  x, vis::VIS_Z, theta);

  double p1[3];
  double p2[3];

  p1[0] = x1;
  p1[1] = y1;
  p1[2] = z1;

  p2[0] = x2;
  p2[1] = y2;
  p2[2] = z2;

  vis::vis_obj* lnk = eng->create_line(p1, p2);

  return lnk;
}

void
dragonfly::display_nodes(const vis_switch_map &switches,
                         vis::vis_engine* eng,
                         std::list<vis::vis_obj*> &objs)
{

  for (long i = 0; i < (int) switches.size(); i++) {
    switch_id me(i);
    coordinates coords = switch_coords(me);

    double x = 0, y = 0, z = 0;
    get_switch_XYZ(coords[0], coords[1], coords[2], x, y, z);

    vis::vis_obj* vo = eng->create_cube(x, y, z, 1,
                                            360 - coords[2] * (360 / g_), vis::VIS_Z);
    //  vo->rotate(vis::VIS_Z, dflyg * (360 / g_));

    const vis_switch_map::mapped_type& myswitch = mapget(switches, me,
        "network switch for dragonfly");
    myswitch->set_vis_obj(vo);
    myswitch->set_vis_engine(eng);
    objs.push_back(vo);

    int gspace = std::max(1, g_ / group_con_);
    int myid = coords[0] + coords[1] * x_;
    int gset = myid % g_;
    for (int g = 0; g < group_con_; g++) {

      int gid = (gset + gspace * g) % g_;
      if (gid != coords[2]) {

        vis::vis_obj* glnk = create_global_vis_link(eng, coords[0],
                                 coords[1], coords[2], gid);
        objs.push_back(glnk);
        myswitch->set_link_vis(glnk, g_dimension, gid);
      }

    }

    /*  for (int z = 0; z < (int) dimensions_.size(); z++)
     {
     double x1 = coords[0] * 2;
     double y1 = coords[1] * 2;
     double z1 = coords[2] * 2;

     double x2 = coords[0] * 2;
     double y2 = coords[1] * 2;
     double z2 = coords[2] * 2;
     int dir;

     bool onegood = (coords[z] != dimensions_[z] - 1);
     bool twogood = (coords[z] != 0);

     if (z == 0)
     {

     x1 += 1.5;
     z1 += 0.5;
     y1 += 0.25;

     x2 -= 0.5;
     z2 += 0.5;
     y2 += 0.75;

     dir = vis::vis_engine::VIS_Z;
     }
     else if (z == 1)
     {
     y1 += 1.5;
     z1 += 0.25;
     x1 += 0.5;

     y2 -= 0.5;
     z2 += 0.75;
     x2 += 0.5;
     dir = vis::vis_engine::VIS_Y;
     }
     else
     {
     z1 += 1.5;
     y1 += 0.25;
     x1 += 0.5;

     z2 -= 0.5;
     y2 += 0.75;
     x2 += 0.5;
     dir = vis::vis_engine::VIS_X;
     }

     const vismaptype::mapped_type& myswitch = mapget(switches, me,
     "vis switch for hdtorus");
     if (onegood)
     {
     vis::vis_obj* c1 = eng->create_cylinder(x1, y1, z1, 0.125, 1,
     dir);
     objs.push_back(c1);
     SSTMAC_DEBUG << "switch " << i << " setting vis link for dim " << z
     << " dir " << topology::POS << "\n";
     myswitch->set_link_vis(c1, z, topology::POS);
     }

     if (twogood)
     {
     vis::vis_obj* c2 = eng->create_cylinder(x2, y2, z2, 0.125, 1,
     dir);
     SSTMAC_DEBUG << "switch " << i << " setting vis link for dim " << z
     << " dir " << topology::NEG << "\n";
     myswitch->set_link_vis(c2, z, topology::NEG);
     objs.push_back(c2);
     }
     }*/
  }
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
dragonfly::new_routing_stage(routing_info& rinfo)
{
  rinfo.current_path().unset_metadata_bit(crossed_global_link);
}

int
dragonfly::xyg_dir_to_group(int myX, int myY, int myG, int dir) const
{
  //int gstride = std::max(1, g_ / group_con_);
  //int my_group_id = myx + myy * x_;
  //int goffset = my_group_id % g_;
  //int dstg = myg + goffset;
  //for (int g=0; g < group_con_; ++g, dstg += gstride){
  //  dstg = dstg % g_;

  int gspace = std::max(1, g_ / group_con_);
  int myid = myX + myY * x_;
  int gset = myid % g_;
  return (myG + gset + gspace * dir) % g_;
}

}
} //end of namespace sstmac



