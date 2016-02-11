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
// fattree.cc: Implementation of fat tree networks.
//
// Author: Curtis Janssen <cljanss@ca.sandia.gov>
#include <sstream>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/sim_parameters.h>

#include <math.h>

namespace sstmac {
namespace hw {

SpktRegister("fattree | ftree", topology, fat_tree,
  "Fat tree topology with L levels and radix K.  This fat tree is actually implemented with commodity switches. Each level of the fat tree has the same number of switches.  This is equivalent to archetypal fat tree with fatter links being replaced by MORE links.");

SpktRegister("simple_fattree", topology, simple_fat_tree);

std::string
abstract_fat_tree::name() const
{
  std::ostringstream ostr;
  ostr << "FatTree(" << l_ << "," << k_ << ")";
  return ostr.str();
}

void
abstract_fat_tree::init_factory_params(sprockit::sim_parameters *params)
{
  std::vector<int> args;
  params->get_vector_param("geometry", args);
  if (args.size() != 2) {
    spkt_throw_printf(sprockit::input_error,
                     "fat_tree::init_factory_params: geometry needs 2 parameters, got %d",
                     args.size());
  }
  l_ = args[0];
  k_ = args[1];
  sprockit::sim_parameters* rtr_params = params->get_optional_namespace("router");
  rtr_params->add_param_override("radix", k_);
  rtr_params->add_param_override("num_levels", l_);

  /**
   sstkeyword = {
     gui=4 2;
     docstring=Vector of size 2.  The first parameter gives the number of levels (L) in the fat tree.
     The second parameter gives the radix (K) of the fat tree.  The number of leaf switches
     is K^(L-1).  If L=3 and K=4, e.g., you have the following fat tree
     1 -> 4 -> 16 switches at the bottom.
   }
   */
  toplevel_ = l_ - 1;
  numleafswitches_ = pow(k_, l_ - 1);

  if (params->has_param("tapering")){
    params->get_vector_param("tapering", tapering_);
  } else {
    tapering_.resize(toplevel_, 1.0);
  }

  if (tapering_.size() != toplevel_){
    spkt_throw_printf(sprockit::value_error,
      "fat_tree::tapering array of size %d is not of correct size %d",
      tapering_.size(), toplevel_);
  }
}

void
fat_tree::minimal_route_to_switch(
  switch_id current_sw_addr,
  switch_id dest_sw_addr,
  routing_info::path& path) const
{
  spkt_throw_printf(sprockit::unimplemented_error, "fattree::minimal_route_to_switch");
}

std::vector<node_id>
abstract_fat_tree::nodes_connected_to_injection_switch(switch_id swaddr) const
{
  return nodes_connected_to_switch(swaddr);
}

std::vector<node_id>
abstract_fat_tree::nodes_connected_to_ejection_switch(switch_id swaddr) const
{
  return nodes_connected_to_switch(swaddr);
}


std::vector<node_id>
abstract_fat_tree::nodes_connected_to_switch(switch_id swaddr) const
{
  if (swaddr >= numleafswitches_){
    return std::vector<node_id>();
  } else {
    return structured_topology::nodes_connected_to_switch(swaddr);
  }
}

void
fat_tree::init_factory_params(sprockit::sim_parameters* params)
{
  abstract_fat_tree::init_factory_params(params);

  toplevel_id_start_ = numleafswitches_ * (l_ - 1);
  max_ports_injection_ = endpoints_per_switch_ = params->get_optional_int_param("concentration", k_);
  max_ports_intra_network_ = 2*k_;
  eject_geometric_id_ = max_ports_intra_network_;
  structured_topology::init_factory_params(params);
}

void
fat_tree::productive_path(
  int dim,
  const coordinates &src,
  const coordinates &dst,
  routing_info::path& path) const
{
  spkt_throw_printf(
    sprockit::illformed_error,
    "fattree::get_productive_dir should never be called."
    "productive outports are determined differently from other topologies");
}

void
fat_tree::connect_group(
  internal_connectable_map& objects,
  int group_stride,
  int down_group_size,
  long down_group_offset,
  int up_group_size,
  long up_group_offset,
  double bw_multiplier)
{
  int red = 1;
  long down_partner = down_group_offset;
  
  //total hack for now to taper bandwidth when appropriate
  //

  top_debug("fat tree: connecting group stride=%d down_size=%d down_offset=%d up_size=%d up_offset=%d",
       group_stride,
       down_group_size, down_group_offset,
       up_group_size, up_group_offset);
  
  for (int d=0; d < down_group_size; ++d, down_partner += group_stride) {
    switch_id down_addr(down_partner);
    connectable* down_switch = objects[down_addr];

    long up_partner = up_group_offset;
    for (int u=0; u < up_group_size; ++u, up_partner += group_stride) {
      // the top level is a little bit weird
      // we only have half the number of switches on the top level
      // the 1/2 number of switches sort of simulates a full level
      // because it only connects downward and has no up connections
      long actual_up_partner = up_partner;
      int port_offset = 0;
      if (up_partner >= toplevel_id_start_) {
        //in an l=3,k=2 tree for example
        //switch 8 connects as if it were both switch 8 and 9
        //switch 9 connects as if it were both switch 9 and 10
        long level_id = up_partner - toplevel_id_start_;
        actual_up_partner = (level_id / 2) + toplevel_id_start_;
        port_offset = k_ * (level_id % 2);
      }
      switch_id up_addr(actual_up_partner);
      connectable* up_switch = objects[up_addr];

      int up_port = convert_to_port(up_dimension, u);
      int down_port = convert_to_port(down_dimension, d + port_offset);

      top_debug("fattree: connecting up=(%d,%d) to down=(%d,%d) on port_offset=%d",
              u, up_port, d, down_port, port_offset);

      down_switch->connect_weighted(
        up_port, //up is out and down is in... got it!??!
        down_port,
        connectable::output,
        up_switch,
        bw_multiplier, red); // up
      up_switch->connect(
        up_port,
        down_port,
        connectable::input,
        down_switch);

      up_switch->connect_weighted(
        down_port, //down is out and up is in... got it?!?
        up_port,
        connectable::output,
        down_switch,
        bw_multiplier, red); //down
      down_switch->connect(
        down_port,
        up_port,
        connectable::input,
        up_switch);

      top_debug("fat tree: connection %ld:%d:(up,%d) <-> %ld:%d:(down,%d)",
         down_partner, up_port, u,
         actual_up_partner, down_port, d + port_offset);

    }
  }
}

void
fat_tree::connect_section(
  internal_connectable_map& objects,
  int group_stride,
  int num_groups_per_section,
  int down_group_size,
  long down_section_offset,
  int up_group_size,
  long up_section_offset,
  double bw_multiplier
)
{
  for (int grp=0; grp < num_groups_per_section; ++grp) {
    long up_group_offset = up_section_offset + grp;
    long down_group_offset = down_section_offset + grp;
    connect_group(objects, group_stride,
                  down_group_size, down_group_offset,
                  up_group_size, up_group_offset,
                  bw_multiplier);
  }
}

void
fat_tree::connect_objects(internal_connectable_map& objects)
{
  long level_size = numleafswitches_;
  int group_size = k_;
  int group_stride = 1;
  long down_level_start = 0;
  long up_level_start = numleafswitches_;

  /**
  Consider an l=4, k=2 tree
  On L=0, we have
  0 <-> 8 <-> 1
  0 <-> 9 <-> 1
  2 <-> 10 <-> 3
  2 <-> 11 <-> 3
  Etc...
  6 <-> 15 <-> 7
  This level is divided into 4 sections/4 groups
  {0,1,8,9} form one connection group/section
  {2,3,10,11} form another connection group/section, etc

  On L=1, we have
  8 <-> 16 <-> 10
  8 <-> 18 <-> 10
  9 <-> 17 <-> 11
  9 <-> 19 <-> 11
  ---
  12 <-> 20 <-> 14
  12 <-> 22 <-> 14
  13 <-> 21 <-> 15
  13 <-> 23 <-> 15
  This level is divided into 2 sections/4 groups. Even switches
  8,9 do connect to the same switches, their links "cross"
  putting them into the same tree "cross" section
  {8,9,19,11,16,17,18,19} form a connection section
        {8,10,16,18} form a connection group
        {9,11,17,19} form another connection group
  {12,13,14,15,20,21,22,23} for a connection section
        {12,14,20,22} form a connection group
        {13,15,21,23} form another connection group

  For L=3, the top level, we have
  16 <-> 24* <-> 20
  16 <-> 28* <-> 20
  17 <-> 25* <-> 21
  17 <-> 29* <-> 21
  18 <-> 26* <-> 22
  18 <-> 30* <-> 22
  19 <-> 27* <-> 23
  19 <-> 31* <-> 23
  The level is now all one section (all switches have cross links)
  We still have 4 groups.
  Switches 24-31 have an asterisk indicating it's actually just
  a 'virtual' label.  The top level only has 4 switches, not 8.
  We only need 4 switches because top level switches can use all
  2*k ports for down connection.  Thus, each top level switch
  performs the role of two 'virtual' switches.
  Switch 24 actually performs the virtual role 24,25
  Switch 25 actually performs the virtual role 26,27
  Thus, the actual set of connections is
  16 <-> 24 <-> 20
  16 <-> 26 <-> 20
  17 <-> 24 <-> 21
  17 <-> 26 <-> 21
  18 <-> 25 <-> 22
  18 <-> 27 <-> 22
  19 <-> 25 <-> 23
  19 <-> 27 <-> 23
  */
  for (int l = 0; l < toplevel_; l++) {
    int num_tree_sections = level_size / group_stride / k_;
    int section_size = level_size / num_tree_sections;
    int num_groups = level_size / group_size;
    int num_groups_per_section = num_groups / num_tree_sections;
    for (int secnum=0; secnum < num_tree_sections; ++secnum) {
      long up_section_offset = up_level_start + section_size * secnum;
      long down_section_offset = down_level_start + section_size * secnum;
      connect_section(objects, group_stride,
                      num_groups_per_section,
                      group_size, down_section_offset,
                      group_size, up_section_offset,
                      tapering_.at(l));
    }
    down_level_start += numleafswitches_;
    up_level_start += numleafswitches_;
    group_stride *= k_;
  }
}

void
fat_tree::configure_vc_routing(std::map<routing::algorithm_t, int> &m) const
{
  m[routing::minimal] = 2; //up and down
}

switch_id
fat_tree::switch_number(const coordinates &coords) const
{
  long levels_down = coords[0];
  if (levels_down == 0) {
    long my_col = coords[1];
    //we have l - 1 rows of numleafswitches
    //before getting to my row
    long uid = (l_ - 1) * numleafswitches_ + my_col;
    return switch_id(uid);
  }

  long branch_offset = 0;
  long router_degeneracy = numleafswitches_ / k_;
  for (int i=1; i <= levels_down; ++i) {
    long offset_increment = coords[i] * router_degeneracy;
    branch_offset += offset_increment;
    router_degeneracy /= k_;
  }
  long replicate_num = coords[levels_down+1];
  long levels_up = l_ - 1 - levels_down;
  long sid = branch_offset + replicate_num + levels_up * numleafswitches_;
  return switch_id(sid);
}

void
fat_tree::compute_switch_coords(switch_id uid, coordinates& coords) const
{
  // first we indicate which level of the tree
  int levels_up = uid / numleafswitches_;
  int levels_down = l_ - 1 - levels_up;
  long my_col = uid % numleafswitches_;
  if (levels_down == 0) {
    coords[0] = 0;
    coords[1] = my_col;
    coords.resize(2);
    return;
  }

  // first coordinate indicates the level counting top-down
  // second set of coordinates indicates each branch taken on the way down
  // final coordinate indicates which "replicate" this switch is
  // a "virtual" fat-tree switch is implemented via multiple commodity switches
  int num_coords = 1 + levels_down + 1;
  coords.resize(num_coords);
  coords[0] = levels_down;
  long router_degeneracy = numleafswitches_ / k_;
  long replicate_num = 0;
  long branch_offset = 0;
  for (int lvl=1; lvl <= levels_down; ++lvl) {
    //figure out which branch to take
    long idx = my_col - branch_offset;
    long branch = idx / router_degeneracy;
    coords[lvl] = branch;
    replicate_num = idx % router_degeneracy;
    branch_offset += branch*router_degeneracy;
    router_degeneracy /= k_;
  }
  coords[levels_down+1] = replicate_num;
}

void
fat_tree::minimal_route_to_coords(
  const coordinates &src_coords,
  const coordinates &dest_coords,
  routing_info::path& path) const
{
  spkt_throw_printf(sprockit::unimplemented_error, "fattree::minimal_route_to_coords");
}

coordinates
fat_tree::neighbor_at_port(switch_id sid, int port)
{
  coordinates my_coords = switch_coords(sid);
  if (port < 0)
    return my_coords;

  int dir = port % k_;
  int dim = port / k_;
  int ncoords = my_coords.size();
  if (dim == up_dimension){
    my_coords[0] -=1;
    my_coords.resize(ncoords-1); //just truncate by 1
    my_coords[ncoords-2] = dir;
  } else {
    my_coords[0] += 1;
    my_coords.push_back(dir);
  }
  return my_coords;
}

int
fat_tree::convert_to_port(int dim, int dir) const
{
  return (dim * k_ + dir);
}

int
fat_tree::minimal_distance(const coordinates &src_coords,
                           const coordinates &dest_coords) const
{
  //can we go straight down? or do we need to go up
  //first coordinate is the number of levels down
  int smallest_depth = std::min(src_coords[0], dest_coords[0]);
  //find the largest index that differs
  int num_branches_different = 0;
  for (int i=smallest_depth; i >= 1; --i) {
    if (src_coords[i] != dest_coords[i]) {
      num_branches_different = smallest_depth - i + 1;
    }
  }

  int csize = src_coords.size();
  int dsize = dest_coords.size();
  if (num_branches_different == 0 && csize == dsize) {
    //we might be on the same branch, but
    //we might be different replicate numbers
    int lastidx = csize - 1;
    //if different, two hops are needed
    //one up, then one back down to correct replicate number
    return src_coords[lastidx] == dest_coords[lastidx] ? 0 : 2;
  }
  else {
    // we need to go up and back down max_idx steps
    // then fill out the difference
    return 2*num_branches_different + abs(csize - dsize);
  }
}

void
simple_fat_tree::partition(
  int* switches_per_lp,
  int *switch_to_lp,
  int *switch_to_thread,
  int& local_num_switches,
  int me,
  int nproc,
  int nthread,
  int noccupied)
{
  int nworkers = nproc * nthread;

  //partition all the occupied switches
  int sw_per_worker = noccupied / nworkers;
  if (noccupied % sw_per_worker) ++sw_per_worker;

  int switches_at_level = num_leaf_switches();
  int occ_at_level = noccupied;
  int swIdx = 0;
  int localIdx = 0;
  top_debug("simple fat tree k=%d l=%d partitioning %d switches onto %d procs x %d threads",
    k_, l_, num_switches(), nproc, nthread);
  for (int l=0; l < l_; ++l){
    top_debug("simple fat tree partitioning %d switches, %d occupied on level %d onto %d procs x %d threads",
      switches_at_level, occ_at_level, l, nproc, nthread);

    int switches_per_worker = occ_at_level / nworkers;
    if (occ_at_level % nworkers) ++switches_per_worker;
    for (int i=0; i < occ_at_level; ++i, ++swIdx){
      int worker = i / switches_per_worker;
      int lp = worker / nthread;
      switch_to_lp[swIdx] = lp;
      switches_per_lp[lp]++;
      if (lp == me){
        int thr = worker % nthread;
        switch_to_thread[localIdx] = thr;
        ++localIdx;
        top_debug("occupied switch %d(%d) assigned to proc %d, thread %d at local index %d", 
          swIdx, i, lp, thr, localIdx);
      }
    }

    int unocc_at_level = switches_at_level - occ_at_level;
    int switches_per_thread = unocc_at_level / nthread;
    if (unocc_at_level % nthread) ++switches_per_thread;
    for (int i=0; i < unocc_at_level; ++i, ++swIdx){
      //assign all these switches to the LAST proc
      int lp = nproc - 1;
      switch_to_lp[swIdx] = lp; //empty, assigned to zero
      switches_per_lp[lp]++;
      int thr = i / switches_per_thread;
      if (lp == me){
        switch_to_thread[localIdx] = thr;
        ++localIdx;
        top_debug("unoccupied switch %d(%d) assigned to proc %d, thread %d at local index %d", 
          swIdx, i, lp, thr, localIdx);
      }
    }

    switches_at_level /= k_;
    occ_at_level /= k_;
    occ_at_level = std::max(1, occ_at_level);
  }

  local_num_switches  = localIdx;
}

int
simple_fat_tree::num_hops(int srcLevel, int srcOffset, int dstLevel, int dstOffset) const
{
  int distance = 0;
  while (srcOffset != dstOffset){
    if (srcLevel <= dstLevel){
      srcOffset /= k_;
      ++srcLevel;
    } else {
      dstOffset /= k_;
      ++dstLevel;
    }
    ++distance;
  }

  return distance;
}

int
simple_fat_tree::num_hops_to_node(node_id src, node_id dst) const
{
  int src_sw = src / endpoints_per_switch_;
  int dst_sw = dst / endpoints_per_switch_;
  return num_hops(0, src_sw, 0, dst_sw);
}

int
simple_fat_tree::minimal_distance(
  const coordinates &src_coords,
  const coordinates &dest_coords) const
{
  abort();
  return num_hops(src_coords[0], src_coords[1], dest_coords[0], dest_coords[1]);
}

void
simple_fat_tree::init_factory_params(sprockit::sim_parameters *params)
{
  abstract_fat_tree::init_factory_params(params);
  int nswitches = numleafswitches_;
  int offset = 0;
  int level = 0;
  level_offsets_.resize(l_);
  num_switches_ = 0;
  while (nswitches >= 1){
    level_offsets_[level] = offset;
    top_debug("fat_tree: setting level offset %d to %d", level, offset);
    offset += nswitches;
    num_switches_ += nswitches;
    nswitches /= k_;
    level++;
  }
  top_debug("fat_tree: computed %d total switches on %d levels",
            num_switches_, l_);
  max_ports_injection_ = endpoints_per_switch_ = params->get_optional_int_param("concentration", k_);
  max_ports_intra_network_ = k_ + 1;
  structured_topology::init_factory_params(params);
}

void
simple_fat_tree::connect_objects(internal_connectable_map &switches)
{
  int nswitches = numleafswitches_;
  int bw_multiplier = 1;
  double red = 1.0;
  int stopLevel = l_ - 1;
  for (int l=0; l < stopLevel; ++l){
    int down_offset = level_offsets_[l];
    int up_offset = level_offsets_[l+1];
    double tapering = tapering_[l];
    for (int s=0; s < nswitches; ++s){
      int down_id = down_offset + s;
      int up_id = up_offset + s/k_;
      connectable* down_switch = switches[switch_id(down_id)];
      connectable* up_switch = switches[switch_id(up_id)];
      double link_weight = bw_multiplier * tapering;

      int down_switch_outport = k_;
      int down_switch_inport = down_switch_outport;
      int up_switch_outport = s % k_;
      int up_switch_inport = up_switch_outport;

      top_debug(
       "Connecting %d(%d):%d->%d(%d):%d between levels %d,%d with multiplier=%d, tapering=%12.8f",
       down_id, s, down_switch_outport,
       up_id, s/k_, up_switch_inport,
       l, l+1, bw_multiplier, tapering);

      down_switch->connect_weighted(
        down_switch_outport,
        up_switch_inport,
        connectable::output,
        up_switch,
        link_weight, red); // up
      up_switch->connect(
        down_switch_outport,
        up_switch_inport,
        connectable::input,
        down_switch);

      top_debug(
       "Connecting %d(%d):%d->%d(%d):%d between levels %d,%d with multiplier=%d, tapering=%12.8f",
       up_id, s/k_, up_switch_outport,
       down_id, s, down_switch_inport,
       l, l+1, bw_multiplier, tapering);

      up_switch->connect_weighted(
        up_switch_outport,
        down_switch_inport,
        connectable::output,
        down_switch,
        link_weight, red); //down
      down_switch->connect(
        up_switch_outport,
        down_switch_inport,
        connectable::input,
        up_switch);

    }
    nswitches /= k_;
    bw_multiplier *= k_;
  }
}

int
simple_fat_tree::level(switch_id sid) const
{
  int level_stop = level_offsets_.size() - 1;
  for (int i=0; i < level_stop; ++i){
    if (sid >= level_offsets_[i] && sid < level_offsets_[i+1]){
      return i;
    }
  }
  return toplevel_;
}

void
simple_fat_tree::minimal_route_to_switch(
  switch_id current_sw_addr,
  switch_id dest_sw_addr,
  routing_info::path &path) const
{
  int src_level = level(current_sw_addr);
  int dst_level = level(dest_sw_addr);
  //question is whether I go up or down
  if (dst_level >= src_level){ //definitely have to go up
    path.outport = k_;
    path.vc = 0;
    top_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d",
            int(dest_sw_addr), dst_level,
            int(current_sw_addr), src_level);
  } else {
    //walk up from the destination switch - see if it hits the source
    int dstLevelOffset = dest_sw_addr - level_offsets_[dst_level];
    int dstLevelTmp = dst_level;
    int downPort;
    while (dstLevelTmp < src_level){
      downPort = dstLevelOffset % k_;
      dstLevelOffset /= k_;
      dstLevelTmp++;
    }
    int parentAtSrcLevel = dstLevelOffset + level_offsets_[src_level];
    if (parentAtSrcLevel == current_sw_addr){
      top_debug("fat_tree: routing down to get to s=%d,l=%d from s=%d,l=%d on port %d",
              int(dest_sw_addr), dst_level,
              int(current_sw_addr), src_level,
              downPort);
      //yep, we can hit the dest switch on the way down
      path.outport = downPort;
      path.vc = 1; //down, down
    } else {
      top_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d",
              int(dest_sw_addr), dst_level,
              int(current_sw_addr), src_level);
      path.outport = k_;
      path.vc = 0;
    }
  }
}

void
simple_fat_tree::compute_switch_coords(switch_id swid, coordinates &coords) const
{
  int srcLevel = level(swid);
  int srcOffset = swid - level_offsets_[srcLevel];
  coords.resize(2);
  coords[0] = srcLevel;
  coords[1] = srcOffset;
}

coordinates
simple_fat_tree::neighbor_at_port(switch_id sid, int port)
{
  coordinates coords(2);
  int srcLevel = level(sid);
  int srcOffset = sid - level_offsets_[srcLevel];
  if (port == k_){
    //going up
    coords[0] = srcLevel + 1;
    int dstOffset = srcOffset / k_;
    coords[1] = dstOffset;
  } else {
    //going down
    coords[0] = srcLevel - 1;
    int dstOffset = srcOffset * k_;
    coords[1] = dstOffset;
  }
  return coords;
}

int
simple_fat_tree::convert_to_port(int dim, int dir) const
{
  if (dim == up_dimension){
    return k_;
  } else {
    return dir;
  }
}

void
simple_fat_tree::productive_path(int dim,
  const coordinates &src,
  const coordinates &dst,
  routing_info::path &path) const
{
  spkt_throw(sprockit::unimplemented_error,
     "simple_fat_tree should never route through productive_path function");
}

void
simple_fat_tree::configure_vc_routing(std::map<routing::algorithm_t, int> &m) const
{
  m[routing::minimal] = 2; //up and down
}

void
simple_fat_tree::minimal_route_to_coords(
  const coordinates &src_coords,
  const coordinates &dest_coords,
  routing_info::path &path) const
{
  spkt_throw(sprockit::unimplemented_error,
     "simple_fat_tree should never route with coords");
}

switch_id
simple_fat_tree::switch_number(const coordinates &coords) const
{
  int level = coords[0];
  int offset = coords[1];
  return switch_id(level_offsets_[level] + offset);
}

}
} //end of namespace sstmac

