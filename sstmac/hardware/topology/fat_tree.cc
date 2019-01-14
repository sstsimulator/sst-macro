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

static constexpr double box_size   = 1.0;
static constexpr double box_stride = 1.5;
static constexpr double row_gap = 4.0;

/*------------------------------------------------------------------------------
  abstract_fat_tree
  ----------------------------------------------------------------------------*/

AbstractFatTree::AbstractFatTree(sprockit::sim_parameters *params) :
  StructuredTopology(params)
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

  static const double TWO_PI = 6.283185307179586;
  double circumference_needed = box_stride * 1.1 * num_leaf_switches_; //1.6 factor for spacing, extra room
  vtk_radius_ = circumference_needed / TWO_PI;
  vtk_subtree_theta_ = TWO_PI / num_agg_subtrees_;
}

void
AbstractFatTree::writeBwParams(
    sprockit::sim_parameters *switch_params,
    double multiplier) const
{
  if (switch_params->has_namespace("xbar")){
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
}

/*------------------------------------------------------------------------------
  fat_tree
  ----------------------------------------------------------------------------*/

FatTree::FatTree(sprockit::sim_parameters* params) :
  AbstractFatTree(params)
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

  // check for errors
  check_input();
}

Topology::vtk_switch_geometry
FatTree::getVtkGeometry(SwitchId sid) const
{
  int core_row_cutoff = num_leaf_switches_ + num_agg_switches_;
  int agg_row_cutoff = num_leaf_switches_;
  int num_in_row = 0;
  int row = 0;
  int slot = 0;
  int subtree = 0;
  double midpoint = 0;
  std::vector<vtk_switch_geometry::port_geometry> ports;
  /**
  if (sid >= core_row_cutoff){
    row = 2;
    slot = (sid - core_row_cutoff);
    int ports_per_core_switch = up_ports_per_agg_switch_*num_agg_switches_/num_core_switches_;
    ports.resize(ports_per_core_switch);
    for (int p=0; p < ports_per_core_switch; ++p){
      ports[p] = plusZface;
    }
    num_in_row = num_core_switches_;
    midpoint = double(num_core_switches_) * 0.5;
  } else if (sid >= agg_row_cutoff){
    row = 1;
    int offset = sid - agg_row_cutoff;
    ports.resize(down_ports_per_agg_switch_ + up_ports_per_agg_switch_);
    for (int p=0; p < down_ports_per_agg_switch_; ++p){
      ports[p] = plusYface;
    }
    for (int p=0; p < up_ports_per_agg_switch_; ++p){
      ports[p+down_ports_per_agg_switch_] = minusYface;
    }
    subtree = offset / agg_switches_per_subtree_;
    slot = offset % agg_switches_per_subtree_;
    num_in_row = num_agg_switches_;
    midpoint = double(agg_switches_per_subtree_) * 0.5;
  } else {
    row = 0;
    int offset = sid;
    ports.resize(up_ports_per_leaf_switch_);
    for (int p=0; p < up_ports_per_leaf_switch_; ++p){
      ports[p] = minusYface;
    }
    subtree = offset / leaf_switches_per_subtree_;
    slot = offset % leaf_switches_per_subtree_;
    num_in_row = num_leaf_switches_;
    midpoint = double(leaf_switches_per_subtree_) * 0.5;
  }
  */

  double xSize = box_size;
  double ySize = box_size;
  double zSize = box_size * 0.25;
  double xCorner = 0.0;
  double yCorner = 0.0;
  double zCorner = 0.0;
  double theta = 0;
  double midpoint_delta = slot - midpoint;
  if (row == 2){
    //center the core switches around the origin
    xCorner = midpoint_delta * (box_stride * 2);
    yCorner = -1;
    xSize = box_size * 2;
    ySize = box_size * 2;
  } else {
    xCorner = midpoint_delta * box_stride;
    yCorner = vtk_radius_ + box_size*row_gap*(1-row); //agg switches point into circle
    theta = subtree * vtk_subtree_theta_;
  }

  vtk_switch_geometry geom(xSize, ySize, zSize,
                           xCorner, yCorner, zCorner, theta,
                           std::move(ports));

  return geom;
}

void
FatTree::connectedOutports(SwitchId src, std::vector<connection>& conns) const
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
  } else if (row == 1){   // aggregation switch
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
  } else if (row == 2){   // core switch
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

void
FatTree::configureNonuniformSwitchParams(SwitchId src,
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
  writeBwParams(switch_params, multiplier);
}

void
FatTree::check_input() const
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

void
FatTree::endpointsConnectedToInjectionSwitch(SwitchId swaddr,
                                   std::vector<injection_port>& nodes) const
{
  if (level(swaddr) > 0){
    nodes.clear();
    return;
  }

  nodes.resize(concentration_);
  for (int i = 0; i < concentration_; i++) {
    injection_port& port = nodes[i];
    port.nid = swaddr*concentration_ + i;
    port.switch_port = up_ports_per_leaf_switch_ + i;
    port.ep_port = 0;
  }
}

/*------------------------------------------------------------------------------
  tapered_fat_tree
  ----------------------------------------------------------------------------*/

TaperedFatTree::TaperedFatTree(sprockit::sim_parameters *params) :
  AbstractFatTree(params)
{
  agg_bw_multiplier_ = agg_switches_per_subtree_;
}

void
TaperedFatTree::connectedOutports(SwitchId src, std::vector<connection>& conns) const
{
  int myRow = level(src);
  if (myRow == 2){
    //core switch
    conns.resize(num_agg_subtrees_);
    int inport = upPort(1);
    for (int s=0; s < num_agg_subtrees_; ++s){
      connection& conn = conns[s];
      conn.src = src;
      conn.dst = num_leaf_switches_ + s;
      conn.src_outport = s;
      conn.dst_inport = inport;
    }
  } else if (myRow == 1){
    //agg switch
    int myTree = aggSubtree(src);
    int myOffset = myTree * leaf_switches_per_subtree_;
    conns.resize(leaf_switches_per_subtree_ + 1);
    int inport = upPort(0);
    for (int s=0; s < leaf_switches_per_subtree_; ++s){
      connection& conn = conns[s];
      conn.src = src;
      conn.dst = myOffset + s;
      conn.src_outport = s;
      conn.dst_inport = inport;
    }
    connection& upconn = conns[leaf_switches_per_subtree_];
    upconn.src = src;
    upconn.dst = coreSwitchId();
    upconn.src_outport = upPort(1);
    upconn.dst_inport = myTree;
  } else {
    //inj switch
    int myTree = injSubtree(src);
    int myOffset = src % leaf_switches_per_subtree_;
    conns.resize(1);
    int outport = upPort(0);
    connection& conn = conns[0];
    conn.src = src;
    conn.dst = num_leaf_switches_ + myTree;
    conn.src_outport = outport;
    conn.dst_inport = myOffset;
  }
}

void
TaperedFatTree::configureIndividualPortParams(SwitchId src,
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
    int outport = upPort(0);
    setupPortParams(outport,
                      buffer_size,
                      bw,
                      link_params, switch_params);
  } else if (myLevel == 1){
    //I have up and down links
    //My up link is tapered
    int outport = upPort(1);
    setupPortParams(outport,
                      taperedBufSize,
                      taperedBw,
                      link_params, switch_params);

    //My down links are not
    for (int s=0; s < leaf_switches_per_subtree_; s++){
      int outport = s;
      setupPortParams(outport,
                        buffer_size,
                        bw,
                        link_params,
                        switch_params);
    }
  } else {
    //I have only down links
    for (int s=0; s < num_agg_subtrees_; ++s){
      int outport = s;
      setupPortParams(outport,
                        taperedBufSize,
                        taperedBw,
                        link_params, switch_params);
    }
  }
}

void
TaperedFatTree::configureNonuniformSwitchParams(SwitchId src,
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
  writeBwParams(switch_params,multiplier);
}

void
TaperedFatTree::endpointsConnectedToInjectionSwitch(SwitchId swaddr,
                                   std::vector<injection_port>& nodes) const
{
  if (level(swaddr) > 0){
    nodes.clear();
    return;
  }

  nodes.resize(concentration_);
  for (int i = 0; i < concentration_; i++) {
    injection_port& port = nodes[i];
    port.nid = swaddr*concentration_ + i;
    port.switch_port = i;
    port.ep_port = 0;
  }
}


void
TaperedFatTree::createPartition(
  int *switch_to_lp,
  int *switch_to_thread,
  int me,
  int nproc,
  int nthread,
  int noccupied) const
{
  spkt_throw_printf(sprockit::unimplemented_error, "tapered_fat_tree::createPartition");
/**
  int nworkers = nproc * nthread;

  //partition all the occupied switches
  int sw_per_worker = noccupied / nworkers;
  if (noccupied % sw_per_worker) ++sw_per_worker;

  int switches_at_level = numLeafSwitches();
  int occ_at_level = noccupied;
  int swIdx = 0;
  int localIdx = 0;
  top_debug("simple fat tree k=%d l=%d partitioning %d switches onto %d procs x %d threads",
    k_, l_, numSwitches(), nproc, nthread);
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
