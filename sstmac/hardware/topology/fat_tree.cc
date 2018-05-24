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

#include <sstream>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/hardware/router/router.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/stl_string.h>
#include <sprockit/keyword_registration.h>

#include <math.h>

RegisterKeywords(
{ "baseline_bandwidth", "" },
{ "num_core_switches", "number of core switches at top-level (fixed 3-level fat-tree)" },
{ "num_agg_subtrees", "number of pods or subtree (fixed 3-level fat-tree)" },
{ "leaf_switches_per_subtree", "number of leaf/injection switches per pod (fixed 3-level fat-tree)" },
{ "agg_switches_per_subtree", "number of aggregation (2nd-level switches) per subtree (pod) (fixed 3-level fat-tree)"},
{ "up_ports_per_leaf_switch", "number of up ports per leaf switch" },
{ "down_ports_per_agg_switch", "number of down ports per aggregation switch" },
{ "up_ports_per_agg_switch", "number of up ports per aggregation switch" },
{ "down_ports_per_core_switch", "number of down ports per core switch" },
{ "leaf_bandwidth_multiplier", "scale factor for leaf switch xbar bandwidth"},
{ "agg_bandwidth_multiplier", "scale factor for aggregation switch xbar bandwidth"},
{ "core_bandwidth_multiplier", "scale factor for core switch xbar bandwidth"},
);

namespace sstmac {
namespace hw {

/*------------------------------------------------------------------------------
  abstract_fat_tree
  ----------------------------------------------------------------------------*/

abstract_fat_tree::abstract_fat_tree(sprockit::sim_parameters *params,
                                     InitMaxPortsIntra i1,
                                     InitGeomEjectID i2) :
  structured_topology(params, i1, i2)
{
  num_core_switches_ =
      params->get_int_param("num_core_switches");
  num_agg_subtrees_ =
      params->get_int_param("num_agg_subtrees");
  leaf_switches_per_subtree_ =
      params->get_int_param("leaf_switches_per_subtree");
  agg_switches_per_subtree_ =
      params->get_int_param("agg_switches_per_subtree");

  num_leaf_switches_ = leaf_switches_per_subtree_ * num_agg_subtrees_;
  num_agg_switches_ = agg_switches_per_subtree_ * num_agg_subtrees_;
}

void
abstract_fat_tree::nodes_connected_to_injection_switch(
    switch_id swaddr, std::vector<injection_port>& nodes) const
{
  if (swaddr >= num_leaf_switches_){
    nodes.resize(0);
  } else {
    structured_topology::nodes_connected_to_injection_switch(swaddr, nodes);
  }
}

void
abstract_fat_tree::nodes_connected_to_ejection_switch(
    switch_id swaddr, std::vector<injection_port>& nodes) const
{
  nodes_connected_to_injection_switch(swaddr, nodes);
}

void
abstract_fat_tree::minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    packet::path& path) const
{
  int src_level = level(current_sw_addr);
  int dst_level = level(dest_sw_addr);
  //question is whether I go up or down
  if (dst_level >= src_level){ //definitely have to go up
    path.set_outport(up_port(src_level));
    path.vc = 0;
    top_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d",
              int(dest_sw_addr), dst_level,
              int(current_sw_addr), src_level);
  } else if (src_level == 2){
    //definitely have to go down
    int dst_subtree = dst_level == 0 ? inj_subtree(dest_sw_addr) : agg_subtree(dest_sw_addr);
    path.set_outport(down_port(dst_subtree));
    path.vc = 0;
    top_debug("fat_tree: routing down to get to s=%d,l=%d from s=%d,l=%d on port %d",
              int(dest_sw_addr), dst_level,
              int(current_sw_addr), src_level,
              path.outport());
  } else if (src_level == 1){
    //going to level 0, but may have to go up or down to get there
    int my_tree = agg_subtree(current_sw_addr);
    int dst_tree = inj_subtree(dest_sw_addr);
    if (dst_tree == my_tree){
      //okay, great, I should have direct link
      path.set_outport(dest_sw_addr % leaf_switches_per_subtree_);
      path.vc = 0;
      top_debug("fat_tree: routing down to get to s=%d,l=%d from s=%d,l=%d on port %d within tree %d",
                int(dest_sw_addr), dst_level,
                int(current_sw_addr), src_level,
                path.outport(), my_tree);
    } else {
      //nope, have to go to core to hope over to other tree
      path.set_outport(up_port(src_level));
      path.vc = 0;
      top_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d hopping from tree %d to tree %d",
                      int(dest_sw_addr), dst_level,
                      int(current_sw_addr), src_level,
                      my_tree, dst_tree);
    }
  }
}

int
abstract_fat_tree::minimal_distance(
  switch_id src,
  switch_id dst) const
{
  if (src == dst) return 0;

  int srcLevel = level(src);
  int dstLevel = level(dst);
  if (srcLevel == 2){
    return srcLevel - dstLevel;
  } else if (dstLevel == 2){
    return dstLevel - srcLevel;
  }


  int srcTree = subtree(src);
  int dstTree = subtree(dst);
  if (srcTree == dstTree){
    if (srcLevel == dstLevel){
      //okay - a bit weird
      //I have to hop up then hop down to get where I want
      return 2;
    } else {
      //I can hop directly up or down to desired location
      return 1;
    }
  } else {
    //have to go to core
    return (2-srcLevel) + (1-dstLevel);
  }
}

void
abstract_fat_tree::write_bw_params(
    sprockit::sim_parameters *switch_params,
    double multiplier) const
{
  sprockit::sim_parameters* xbar_params = switch_params->get_namespace("xbar");
  double bw = xbar_params->get_bandwidth_param("bandwidth");
  // we are overwriting params -
  // we have to make sure that the original baseline bandwidth is preserved
  double baseline_bw =
      xbar_params->get_optional_bandwidth_param("baseline_bandwidth", bw);
  double xbar_bw = baseline_bw * multiplier;
  (*xbar_params)["bandwidth"].setBandwidth(xbar_bw/1e9, "GB/s");
  (*xbar_params)["baseline_bandwidth"].setBandwidth(baseline_bw/1e9, "GB/s");
}

/*------------------------------------------------------------------------------
  fat_tree
  ----------------------------------------------------------------------------*/

fat_tree::fat_tree(sprockit::sim_parameters* params) :
  abstract_fat_tree(params,
                    InitMaxPortsIntra::I_Remembered,
                    InitGeomEjectID::I_Remembered)
{
  up_ports_per_leaf_switch_ =
      params->get_int_param("up_ports_per_leaf_switch");
  down_ports_per_agg_switch_ =
      params->get_int_param("down_ports_per_agg_switch");
  up_ports_per_agg_switch_ =
      params->get_int_param("up_ports_per_agg_switch");
  down_ports_per_core_switch_ =
      params->get_int_param("down_ports_per_core_switch");

  int leaf_ports = concentration() + up_ports_per_leaf_switch_;
  int agg_ports = down_ports_per_agg_switch_ +  up_ports_per_agg_switch_;
  int la_ports = std::max(leaf_ports,agg_ports);
  max_ports_intra_network_ =
      std::max(la_ports,down_ports_per_core_switch_);
  // currently assumes port_id == geometric_id (no redundancy)
  eject_geometric_id_ = max_ports_intra_network_;

  // check for errors
  check_input();
}

void
fat_tree::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  conns.clear();

  // find row
  int row;
  int num_non_core = num_leaf_switches_ + num_agg_switches_;
  if (src < num_leaf_switches_)
    row = 0;
  else if (num_leaf_switches_ <= src && src < num_non_core )
    row = 1;
  else if (num_non_core <= src)
    row = 2;

  // leaf switch
  if (row == 0){
    int my_subtree = src / leaf_switches_per_subtree_;
    int my_subtree_spot = src % leaf_switches_per_subtree_;
    for (int up_port=0; up_port < up_ports_per_leaf_switch_; ++up_port){
      int subtree_down_port =
          my_subtree_spot + up_port * leaf_switches_per_subtree_;
      int agg_partner_spot =
          subtree_down_port / down_ports_per_agg_switch_;
      int agg_partner_port =
          subtree_down_port % down_ports_per_agg_switch_;
      int agg_partner_switch =
          num_leaf_switches_
          + my_subtree * agg_switches_per_subtree_
          + agg_partner_spot;
      connection next;
      next.dst = agg_partner_switch;
      next.dst_inport = agg_partner_port;
      next.src = src;
      next.src_outport = up_port;
      conns.push_back(next);
      top_debug("fat-tree connecting switch:port leaf %i:%i to agg %i:%i",
                src, up_port, agg_partner_switch, agg_partner_port);
    }
  }

  // aggregation switch
  else if (row == 1){
    int my_subtree = (src - num_leaf_switches_)
        / agg_switches_per_subtree_;
    int my_subtree_spot = (src - num_leaf_switches_)
        % agg_switches_per_subtree_;
    int agg_spot = src - num_leaf_switches_;
    // up ports
    for (int up_port=0; up_port < up_ports_per_agg_switch_; ++up_port){
      int global_core_down_port =
          up_port * num_agg_switches_
          + my_subtree_spot * num_agg_subtrees_ + my_subtree;
      int core_partner_spot =
          global_core_down_port / down_ports_per_core_switch_;
      int core_partner_port =
          global_core_down_port % down_ports_per_core_switch_;
      int core_partner_switch =
          num_leaf_switches_ + num_agg_switches_
          + core_partner_spot;
      connection next;
      next.dst = core_partner_switch;
      next.dst_inport = core_partner_port;
      next.src = src;
      next.src_outport = down_ports_per_agg_switch_ + up_port;
      conns.push_back(next);
      top_debug("fat-tree connecting switch:port agg %i:%i to core %i:%i",
                src, next.src_outport, core_partner_switch, core_partner_port);
    }
    // down ports
    for (int dwn_port=0; dwn_port < down_ports_per_agg_switch_;
         ++dwn_port){
      int leaf_spot = dwn_port % leaf_switches_per_subtree_;
      int leaf_port = my_subtree_spot +
          dwn_port / leaf_switches_per_subtree_ *
          leaf_switches_per_subtree_;
      int leaf_partner_switch =
          my_subtree * leaf_switches_per_subtree_ + leaf_spot;
      connection next;
      next.dst = leaf_partner_switch;
      next.dst_inport = leaf_port;
      next.src = src;
      next.src_outport = dwn_port;
      conns.push_back(next);
      top_debug("fat-tree connecting switch:port agg %i:%i to leaf %i:%i",
                src, dwn_port, leaf_partner_switch, leaf_port);
    }
  }

  // core switch
  else if (row == 2){
    // down ports
    for (int dwn_port=0; dwn_port < down_ports_per_core_switch_;
         ++dwn_port){
      int core_spot = src - num_agg_switches_ - num_leaf_switches_;
      int global_core_port = core_spot * down_ports_per_core_switch_ + dwn_port;
      int agg_subtree = global_core_port % num_agg_subtrees_;
      int agg_subtree_spot = (global_core_port % num_agg_switches_)
          /  num_agg_subtrees_;
      int agg_port = global_core_port / num_agg_switches_;
      int agg_partner_switch = num_leaf_switches_
          + agg_subtree * agg_switches_per_subtree_ + agg_subtree_spot;
      connection next;
      next.dst = agg_partner_switch;
      next.dst_inport = agg_port + down_ports_per_agg_switch_;
      next.src = src;
      next.src_outport = dwn_port;
      conns.push_back(next);
      top_debug("fat-tree connecting switch:port core %i:%i to agg %i:%i",
                src, dwn_port, agg_partner_switch, next.dst_inport);
    }
  }
}

// insert all core switch ports that lead to any switch in the
// subtree designated by next_tree
void
fat_tree::connected_core_down_ports(switch_id src, int next_tree, std::vector<int>& ports) const
{
  int lvl = level(src);
  if (lvl != 2)
    return;

  ports.clear(); // just to be safe
  std::vector<connection> conns;
  connected_outports(src, conns);
  for (auto it=conns.begin(); it != conns.end(); ++it) {
    int conn_subtree = subtree(it->dst);
    if (conn_subtree == next_tree)
      ports.push_back(it->src_outport);
  }
}

// insert all aggregator switch ports that lead to the switch
// designated by dst_leaf
void
fat_tree::connected_agg_down_ports(switch_id src, int dst_leaf, std::vector<int>& ports) const
{
  int lvl = level(src);
  if (lvl != 1)
    return;

  ports.clear(); // just to be safe
  std::vector<connection> conns;
  connected_outports(src, conns);
  for (auto it=conns.begin(); it != conns.end(); ++it)
    if (it->dst == dst_leaf)
      ports.push_back(it->src_outport);
}

void
fat_tree::configure_nonuniform_switch_params(switch_id src,
                           sprockit::sim_parameters *switch_params) const
{
  int my_level = level(src);

  int n_leaf_port = up_ports_per_leaf_switch_ + concentration();
  int n_agg_port = up_ports_per_agg_switch_ + down_ports_per_agg_switch_;
  int n_core_port = down_ports_per_core_switch_;
  int min_port = std::min(n_leaf_port,n_agg_port);
  min_port = std::min(min_port,n_core_port);

  double multiplier = 1.0;
  if (my_level == 0 && n_leaf_port > min_port)
    multiplier *= double(n_leaf_port) / double(min_port);
  else if (my_level == 1 && n_agg_port > min_port)
    multiplier *= double(n_agg_port) / double(min_port);
  else if (my_level == 2 && n_core_port > min_port)
    multiplier *= double(n_core_port) / double(min_port);

  if ( my_level == 0 &&
       switch_params->has_param("leaf_bandwidth_multiplier"))
    multiplier = switch_params->get_double_param("leaf_bandwidth_multiplier");
  else if ( my_level == 1 &&
            switch_params->has_param("agg_bandwidth_multiplier"))
    multiplier = switch_params->get_double_param("agg_bandwidth_multiplier");
  else if ( my_level == 2 &&
            switch_params->has_param("core_bandwidth_multiplier"))
    multiplier = switch_params->get_double_param("core_bandwidth_multiplier");

  top_debug("fat_tree: scaling switch %i by %lf",src,multiplier);
  write_bw_params(switch_params, multiplier);


}

void
fat_tree::check_input() const
{
  int val;

  // check that there are enough down ports
  if (down_ports_per_core_switch_ < num_agg_subtrees_) {
    spkt_throw_printf(sprockit::input_error,
          "down_ports_per_core_switch (%d) must be >= num_agg_subtrees (%d)",
          down_ports_per_core_switch_,num_agg_subtrees_);
  }
  if (down_ports_per_agg_switch_ < leaf_switches_per_subtree_) {
    spkt_throw_printf(sprockit::input_error,
          "down_ports_per_agg_switch (%d) must be >= leaf_switches_per_subtree (%d)",
          down_ports_per_agg_switch_,leaf_switches_per_subtree_);
  }

  // check that down ports mod subtrees/switches is zero
  val = down_ports_per_core_switch_ % num_agg_subtrees_;
  if (val != 0) {
    spkt_throw_printf(sprockit::input_error,
          "down_ports_per_core_switch mod total num_agg_subtrees must equal zero (%d mod %d = %d)",
          down_ports_per_core_switch_,num_agg_subtrees_,val);
  }
  val = down_ports_per_agg_switch_ % leaf_switches_per_subtree_;
  if (val != 0) {
    spkt_throw_printf(sprockit::input_error,
          "down_ports_per_agg_switch mod leaf_switches_per_subtree must be zero (%d mod %d = %d)",
          down_ports_per_agg_switch_,leaf_switches_per_subtree_,val);
  }

  // check that there are enough up ports
  if (up_ports_per_agg_switch_ * agg_switches_per_subtree_ < num_core_switches_) {
    spkt_throw_printf(sprockit::input_error,
          "up_ports_per_agg_switch * agg_switches_per_subtree (%d*%d) must be >= num_core_switches (%d)",
          up_ports_per_agg_switch_,agg_switches_per_subtree_,num_core_switches_);
  }
  if (up_ports_per_leaf_switch_ < agg_switches_per_subtree_) {
    spkt_throw_printf(sprockit::input_error,
          "up_ports_per_leaf_switch (%d) must be >= agg_switches_per_subtree (%d)",
          up_ports_per_leaf_switch_,agg_switches_per_subtree_);
  }

  // check that up ports mod switches is zero
  val = up_ports_per_leaf_switch_ % agg_switches_per_subtree_;
  if (val != 0) {
    spkt_throw_printf(sprockit::input_error,
          "up_ports_per_leaf_switch mod agg_switches_per_subtree must be zero (%d mod %d = %d)",
          up_ports_per_leaf_switch_,agg_switches_per_subtree_,val);
  }

  // check that leaf-agg ports match up
  int ndown = num_core_switches_ * down_ports_per_core_switch_;
  int nup = num_agg_switches_ * up_ports_per_agg_switch_;
  if (ndown != nup) {
    spkt_throw_printf(sprockit::input_error,
          "num_core_switches * down_ports_per_core_switch must equal total number of aggregation switches * up_ports_per_agg_switch (%d != %d)",
          ndown,nup);
  }

  // check that agg-core ports match up
  ndown = agg_switches_per_subtree_ * down_ports_per_agg_switch_;
  nup = leaf_switches_per_subtree_ * up_ports_per_leaf_switch_;
  if (ndown != nup) {
    spkt_throw_printf(sprockit::input_error,
          "agg_switches_per_subtree * down_ports_per_agg_switch must equal leaf_switches_per_subtree * up_ports_per_leaf_switch (%d != %d)",
          ndown,nup);
  }
}

/*------------------------------------------------------------------------------
  tapered_fat_tree
  ----------------------------------------------------------------------------*/

tapered_fat_tree::tapered_fat_tree(sprockit::sim_parameters *params) :
  abstract_fat_tree(params,
                    InitMaxPortsIntra::I_Remembered,
                    InitGeomEjectID::I_Remembered)
{
  agg_bw_multiplier_ = agg_switches_per_subtree_;

  int max_up_port = std::max(up_port(0), up_port(1));
  int max_core_port = num_agg_subtrees_;
  max_ports_intra_network_ = std::max(max_up_port, max_core_port);

  eject_geometric_id_ = max_ports_intra_network_;
}

void
tapered_fat_tree::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  int myRow = level(src);
  if (myRow == 2){
    //core switch
    conns.resize(num_agg_subtrees_);
    int inport = up_port(1);
    for (int s=0; s < num_agg_subtrees_; ++s){
      connection& conn = conns[s];
      conn.src = src;
      conn.dst = num_leaf_switches_ + s;
      conn.src_outport = s;
      conn.dst_inport = inport;
    }
  } else if (myRow == 1){
    //agg switch
    int myTree = agg_subtree(src);
    int myOffset = myTree * leaf_switches_per_subtree_;
    conns.resize(leaf_switches_per_subtree_ + 1);
    int inport = up_port(0);
    for (int s=0; s < leaf_switches_per_subtree_; ++s){
      connection& conn = conns[s];
      conn.src = src;
      conn.dst = myOffset + s;
      conn.src_outport = s;
      conn.dst_inport = inport;
    }
    connection& upconn = conns[leaf_switches_per_subtree_];
    upconn.src = src;
    upconn.dst = core_switch_id();
    upconn.src_outport = up_port(1);
    upconn.dst_inport = myTree;
  } else {
    //inj switch
    int myTree = inj_subtree(src);
    int myOffset = src % leaf_switches_per_subtree_;
    conns.resize(1);
    int outport = up_port(0);
    connection& conn = conns[0];
    conn.src = src;
    conn.dst = num_leaf_switches_ + myTree;
    conn.src_outport = outport;
    conn.dst_inport = myOffset;
  }
}

void
tapered_fat_tree::configure_individual_port_params(switch_id src,
                                  sprockit::sim_parameters *switch_params) const
{
  sprockit::sim_parameters* link_params = switch_params->get_namespace("link");
  int buffer_size = link_params->get_optional_byte_length_param("buffer_size", 0);
  double bw = link_params->get_bandwidth_param("bandwidth");
  double taper = link_params->get_optional_double_param("core_taper",1.0);
  int taperedBufSize = buffer_size * agg_bw_multiplier_ * taper;
  double taperedBw = bw * agg_bw_multiplier_ * taper;
  int myLevel = level(src);

  if (myLevel == 0){
    //inj switch
    int outport = up_port(0);
    setup_port_params(outport,
                      buffer_size,
                      bw,
                      link_params, switch_params);
  } else if (myLevel == 1){
    //I have up and down links
    //My up link is tapered
    int outport = up_port(1);
    setup_port_params(outport,
                      taperedBufSize,
                      taperedBw,
                      link_params, switch_params);

    //My down links are not
    for (int s=0; s < leaf_switches_per_subtree_; s++){
      int outport = s;
      setup_port_params(outport,
                        buffer_size,
                        bw,
                        link_params,
                        switch_params);
    }
  } else {
    //I have only down links
    for (int s=0; s < num_agg_subtrees_; ++s){
      int outport = s;
      setup_port_params(outport,
                        taperedBufSize,
                        taperedBw,
                        link_params, switch_params);
    }
  }
}

void
tapered_fat_tree::configure_nonuniform_switch_params(switch_id src,
                           sprockit::sim_parameters *switch_params) const
{
  int myLevel = level(src);
  double multiplier = 1.0;
  if (myLevel == 0){
    //okay - nothing doing - normal switches
  } else if (myLevel == 1){
    //this switch is modeling the functionaliy of X commodity switches
    multiplier = agg_switches_per_subtree_;
  } else {
    //this switch is modeling the functionality of X commodity switches
    multiplier = num_core_switches_;
  }

  top_debug("abstract_fat_tree: scaling switch %i by %lf",src,multiplier);
  write_bw_params(switch_params,multiplier);
}

void
tapered_fat_tree::create_partition(
  int *switch_to_lp,
  int *switch_to_thread,
  int me,
  int nproc,
  int nthread,
  int noccupied) const
{
  spkt_throw_printf(sprockit::unimplemented_error, "tapered_fat_tree::create_partition");
/**
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
*/
}

}
} //end of namespace sstmac
