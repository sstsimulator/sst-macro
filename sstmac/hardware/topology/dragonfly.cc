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

dragonfly::dragonfly(sprockit::sim_parameters* params) :
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

  endpoints_per_switch_ = params->get_optional_int_param("concentration", 1);
  max_ports_intra_network_ = x_ + y_ + g_;
  eject_geometric_id_ = max_ports_intra_network_;
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

switch_id
dragonfly::random_intermediate_switch(switch_id current_sw, switch_id dest_sw)
{
  long nid = current_sw;
  uint32_t attempt = 0;
  while (current_sw == nid) {

    int srcX, srcY, srcG, dstX, dstY, dstG, hisX, hisY, hisG;
    get_coords(current_sw, srcX, srcY, srcG);
    get_coords(dest_sw, dstX, dstY, dstG);

    hisX = random_number(x_, attempt);
    hisY = random_number(y_, attempt);
    if(!true_random_intermediate_ && dstG == srcG) {
      // already on the correct group
      hisG = srcG;

    }
    else {
      //randomly select a group
      hisG = random_number(g_, attempt);
      //now figure out which x,y,g fills the path
      //find_path_to_group(srcX, srcY, srcG, hisX, hisY, hisG);
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

bool
dragonfly::find_y_path_to_group(int myX, int myG, int dstG, int& dstY,
                                routable::path& path) const
{
  int ystart = random_number(y_,0);
  for (int yy = 0; yy < y_; ++yy) {
    dstY = (ystart + yy) % y_;
    if (xy_connected_to_group(myX, dstY, myG, dstG)) {
      path.outport = y_port(dstY);
      return true;
    }
  }
  return false;
}

bool
dragonfly::find_x_path_to_group(int myY, int myG, int dstG, int& dstX,
                                routable::path& path) const
{
  int xstart = random_number(x_,0);
  for (int xx = 0; xx < x_; ++xx) {
    dstX = (xstart + xx) % x_;
    if (xy_connected_to_group(dstX, myY, myG, dstG)) {
      path.outport = x_port(dstX);
      return true;
    }
  }
  return false;
}

void
dragonfly::find_path_to_group(int myX, int myY, int myG,
                              int dstG, int& dstX, int& dstY,
                              routable::path& path) const
{
  //see if we can go directly to the group
  if (xy_connected_to_group(myX, myY, myG, dstG)){
    path.outport = g_port(dstG);
    dstX = myX;
    dstY = myY;
    path.set_metadata_bit(routable::crossed_timeline);
    return;
  }

  if (find_x_path_to_group(myY, myG, dstG, dstX, path)){
    dstY = myY;
    return;
  }

  if (find_y_path_to_group(myX, myG, dstG, dstY, path)){
    dstX = myX;
    return;
  }

  //both x and y need to change
  int xstart = 0; 
  for (int xx = 0; xx < x_; ++xx) {
    dstX = (xstart + xx) % x_;
    if (find_y_path_to_group(dstX, myG, dstG, dstY, path)){
      return;
    }
  }

  spkt_throw_printf(sprockit::value_error,
    "dragonfly::route: unable to find path from group %d to group %d",
    myG, dstG);
}


void
dragonfly::minimal_route_to_switch(
    switch_id src,
    switch_id dst,
    routable::path &path) const
{
  path.vc = path.metadata_bit(routable::crossed_timeline) ? 1 : 0;
  int srcX, srcY, srcG; get_coords(src, srcX, srcY, srcG);
  int dstX, dstY, dstG; get_coords(dst, dstX, dstY, dstG);
  int interX, interY;
  if (srcG != dstG){
    find_path_to_group(srcX, srcY, srcG, dstG, interX, interY, path);
    top_debug("dragonfly routing from (%d,%d,%d) to (%d,%d,%d) through "
              "gateway (%d,%d,%d) on port %d",
              srcX, srcY, srcG, dstX, dstY, dstG,
              interX, interY, srcG, path.outport);
  }
  else if (srcX != dstX){
    top_debug("dragonfly routing X from (%d,%d,%d) to (%d,%d,%d) on port %d",
              srcX, srcY, srcG, dstX, dstY, dstG, path.outport);
    path.outport = x_port(dstX);
  } else if (srcY != dstY){
    top_debug("dragonfly routing Y from (%d,%d,%d) to (%d,%d,%d) on port %d",
              srcX, srcY, srcG, dstX, dstY, dstG, path.outport);
    path.outport = y_port(dstY);
  }
}

int
dragonfly::minimal_distance(switch_id src, switch_id dst) const
{
  int dist = 0;
  int srcX, srcY, srcG; get_coords(src, srcX, srcY, srcG);
  int dstX, dstY, dstG; get_coords(dst, dstX, dstY, dstG);
  if (srcG == dstG){
    if (srcX != dstX) ++dist;
    if (srcY != dstY) ++dist;
  }
  else {
    routable::path path;
    int interX;
    int interY;
    find_path_to_group(srcX, srcY, srcG, dstG, interX, interY, path);
    dist = 1; //group hop
    if (srcX != interX) ++dist;
    if (srcY != interY) ++dist;
    if (dstX != interX) ++dist;
    if (dstY != interY) ++dist;
  }
  return dist;
}




void
dragonfly::setup_port_params(sprockit::sim_parameters* params, int dim, int dimsize)
{
  sprockit::sim_parameters* link_params = params->get_namespace("link");
  double bw = link_params->get_bandwidth_param("bandwidth");
  int bufsize = params->get_byte_length_param("buffer_size");

  double port_bw = bw * red_[dim];
  int credits = bufsize * red_[dim];

  for (int i=0; i < dimsize; ++i){
    int port = convert_to_port(dim, i);
    sprockit::sim_parameters* port_params = topology
        ::setup_port_params(port, credits, port_bw, link_params, params);
  }
}

/// coordinates go x,y - group
void
dragonfly::connect_objects(sprockit::sim_parameters* params, internal_connectable_map& objects)
{
  setup_port_params(params, x_dimension, x_);
  setup_port_params(params, y_dimension, y_);
  setup_port_params(params, g_dimension, g_);


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

        int outport = x_port(x);
        int inport = x_port(myx);

        top_debug("node %d,%s connecting to node %d,%s on ports %d:%d",
            int(me), set_string(myx, myy, myg).c_str(),
            int(them), set_string(x, myy, myg).c_str(),
            outport, inport);

        sprockit::sim_parameters* port_params = get_port_params(params, outport);

        objects[me]->connect_output(
          port_params,
          outport,
          inport,
          objects[them]);

        objects[them]->connect_input(
          port_params,
          outport,
          inport,
          objects[me]);

      }
    }

    for (int y = 0; y < y_; y++) {
      if (y != myy) {

        switch_id them(get_uid(myx, y, myg));

        int outport = y_port(y);
        int inport = y_port(myy);

        top_debug("node %d,%s connecting to node %d,%s on ports %d:%d",
            int(me), set_string(myx, myy, myg).c_str(),
            int(them), set_string(myx, y, myg).c_str(),
            outport, inport);

        sprockit::sim_parameters* port_params = get_port_params(params, outport);

        objects[me]->connect_output(
          port_params,
          outport,
          inport,
          objects[them]);

        objects[them]->connect_input(
          port_params,
          outport,
          inport,
          objects[me]);

      }
    }

    for (int g=0; g < group_con_; ++g){
      int dstg = xyg_dir_to_group(myx,myy,myg,g);
      if (dstg == myg) continue;

      long uid = get_uid(myx, myy, dstg);
      switch_id them(uid);

      int outport = g_port(dstg);
      int inport = g_port(myg);

      top_debug("node %d,%s connecting to node %d,%s on ports %d:%d",
        int(me), set_string(myx, myy, myg).c_str(),
        int(them), set_string(myx, myy, dstg).c_str(),
        outport, inport);

      sprockit::sim_parameters* port_params = get_port_params(params, outport);

      objects[me]->connect_output(
          port_params,
          outport,
          inport,
          objects[them]);

      objects[them]->connect_input(
          port_params,
          outport,
          inport,
          objects[me]);

    }

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

coordinates
dragonfly::switch_coords(switch_id uid) const
{
  coordinates coords(3);
  get_coords(uid, coords[0], coords[1], coords[2]);
  return coords;
}

switch_id
dragonfly::switch_addr(const coordinates &coords) const
{
  return get_uid(coords[0], coords[1], coords[2]);
}


}
} //end of namespace sstmac



