/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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

FatTree::FatTree(SST::Params& params) :
  StructuredTopology(params)
{
  num_core_switches_ =
      params.find<int>("num_core_switches");
  num_agg_subtrees_ =
      params.find<int>("num_agg_subtrees");
  leaf_switches_per_subtree_ =
      params.find<int>("leaf_switches_per_subtree");
  agg_switches_per_subtree_ =
      params.find<int>("agg_switches_per_subtree");

  num_leaf_switches_ = leaf_switches_per_subtree_ * num_agg_subtrees_;
  num_agg_switches_ = agg_switches_per_subtree_ * num_agg_subtrees_;

  static const double TWO_PI = 6.283185307179586;
  double circumference_needed = box_stride * 1.1 * num_leaf_switches_; //1.6 factor for spacing, extra room
  vtk_radius_ = circumference_needed / TWO_PI;
  vtk_subtree_theta_ = TWO_PI / num_agg_subtrees_;

  up_ports_per_leaf_switch_ =
      params.find<int>("up_ports_per_leaf_switch");
  down_ports_per_agg_switch_ =
      params.find<int>("down_ports_per_agg_switch");
  up_ports_per_agg_switch_ =
      params.find<int>("up_ports_per_agg_switch");
  down_ports_per_core_switch_ =
      params.find<int>("down_ports_per_core_switch");

  // check for errors
  checkInput();

  int max_num_nodes = num_leaf_switches_ * concentration_;
  max_nodes_ = params.find<int>("max_nodes", max_num_nodes);
  //allow empty node slots - but make sure we aren't asking
  //for more nodes than possible
  if (max_nodes_ > max_num_nodes){
    spkt_abort_printf("Bad max nodes for fat-tree: %d > %d, which is max possible based on switches",
                      max_nodes_, max_num_nodes);
  }
}

Topology::VTKSwitchGeometry
FatTree::getVtkGeometry(SwitchId  /*sid*/) const
{
  int row = 0;
  int slot = 0;
  int subtree = 0;
  double midpoint = 0;
  std::vector<VTKSwitchGeometry::port_geometry> ports;
  /**
  int core_row_cutoff = num_leaf_switches_ + num_agg_switches_;
  int agg_row_cutoff = num_leaf_switches_;
  int num_in_row = 0;
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

  VTKSwitchGeometry geom(xSize, ySize, zSize,
                           xCorner, yCorner, zCorner, theta,
                           std::move(ports));

  return geom;
}

std::string
FatTree::portTypeName(SwitchId sid, int port) const
{
  if (sid < num_leaf_switches_){
    if (port < up_ports_per_leaf_switch_){
      return "leaf->agg";
    } else {
      return "injection";
    }
  } else if (sid < (num_leaf_switches_ + num_agg_switches_)){
    if (port < down_ports_per_agg_switch_){
      return "agg->leaf";
    } else {
      return "agg->core";
    }
  } else {
    return "core->agg";
  }
}

void
FatTree::connectedOutports(SwitchId src, std::vector<Connection>& conns) const
{
  conns.clear();

  // find row
  int row = -1;
  int num_non_core = num_leaf_switches_ + num_agg_switches_;
  if (src < num_leaf_switches_)
    row = 0;
  else if (num_leaf_switches_ <= src && src < num_non_core )
    row = 1;
  else if (num_non_core <= src)
    row = 2;
  else {
    spkt_abort_printf("Could not initialize row in FatTree connectedOutports\n");
  }

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
      Connection next;
      next.dst = agg_partner_switch;
      next.dst_inport = agg_partner_port;
      next.src = src;
      next.src_outport = up_port;
      conns.push_back(next);
      top_debug("fat-tree connecting switch:port leaf %i:%i to agg %i:%i",
                src, up_port, agg_partner_switch, agg_partner_port);
      if (agg_partner_port >= down_ports_per_agg_switch_){
        spkt_abort_printf("Leaf switch port %d:%d connecting to bad agg port %d:%d",
            src, up_port, agg_partner_switch, agg_partner_port);
      }
    }
  } else if (row == 1){   // aggregation switch
    int my_subtree = (src - num_leaf_switches_)
        / agg_switches_per_subtree_;
    int my_subtree_spot = (src - num_leaf_switches_)
        % agg_switches_per_subtree_;
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
      Connection next;
      next.dst = core_partner_switch;
      next.dst_inport = core_partner_port;
      next.src = src;
      next.src_outport = down_ports_per_agg_switch_ + up_port;
      conns.push_back(next);
      top_debug("fat-tree connecting switch:port agg %i:%i to core %i:%i",
                src, next.src_outport, core_partner_switch, core_partner_port);
      if (core_partner_port >= down_ports_per_core_switch_){
        spkt_abort_printf("Agg switch port %d:%d connecting to bad core port %d:%d",
            src, next.src_outport, core_partner_switch, core_partner_port);
      }
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
      Connection next;
      next.dst = leaf_partner_switch;
      next.dst_inport = leaf_port;
      next.src = src;
      next.src_outport = dwn_port;
      conns.push_back(next);
      top_debug("fat-tree connecting switch:port agg %i:%i to leaf %i:%i",
                src, dwn_port, leaf_partner_switch, leaf_port);
      //port is out of range
      if (leaf_port >= up_ports_per_leaf_switch_){
        spkt_abort_printf("Agg switch port %d:%d connecting to bad leaf port %d:%d",
            src, dwn_port, leaf_partner_switch, leaf_port);
      }
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
      Connection next;
      next.dst = agg_partner_switch;
      next.dst_inport = agg_port + down_ports_per_agg_switch_;
      next.src = src;
      next.src_outport = dwn_port;
      conns.push_back(next);
      top_debug("fat-tree connecting switch:port core %i:%i to agg %i:%i",
                src, dwn_port, agg_partner_switch, next.dst_inport);
      if (next.dst_inport < down_ports_per_agg_switch_){
        spkt_abort_printf("Core switch port %d:%d connecting to bad agg port %d:%d",
            src, dwn_port, agg_partner_switch, agg_port);
      }
    }
  }
}

double
FatTree::portScaleFactor(uint32_t addr, int  /*port*/) const
{
  int my_level = level(addr);

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

  return multiplier;
}

void
FatTree::checkInput() const
{
  int val;

  // check that there are enough down ports
  if (down_ports_per_core_switch_ < num_agg_subtrees_) {
    spkt_throw_printf(sprockit::InputError,
          "down_ports_per_core_switch (%d) must be >= num_agg_subtrees (%d)",
          down_ports_per_core_switch_,num_agg_subtrees_);
  }
  if (down_ports_per_agg_switch_ < leaf_switches_per_subtree_) {
    spkt_throw_printf(sprockit::InputError,
          "down_ports_per_agg_switch (%d) must be >= leaf_switches_per_subtree (%d)",
          down_ports_per_agg_switch_,leaf_switches_per_subtree_);
  }

  // check that down ports mod subtrees/switches is zero
  val = down_ports_per_core_switch_ % num_agg_subtrees_;
  if (val != 0) {
    spkt_throw_printf(sprockit::InputError,
          "down_ports_per_core_switch mod total num_agg_subtrees must equal zero (%d mod %d = %d)",
          down_ports_per_core_switch_,num_agg_subtrees_,val);
  }
  val = down_ports_per_agg_switch_ % leaf_switches_per_subtree_;
  if (val != 0) {
    spkt_throw_printf(sprockit::InputError,
          "down_ports_per_agg_switch mod leaf_switches_per_subtree must be zero (%d mod %d = %d)",
          down_ports_per_agg_switch_,leaf_switches_per_subtree_,val);
  }

  // check that there are enough up ports
  if (up_ports_per_agg_switch_ * agg_switches_per_subtree_ < num_core_switches_) {
    spkt_throw_printf(sprockit::InputError,
          "up_ports_per_agg_switch * agg_switches_per_subtree (%d*%d) must be >= num_core_switches (%d)",
          up_ports_per_agg_switch_,agg_switches_per_subtree_,num_core_switches_);
  }
  if (up_ports_per_leaf_switch_ < agg_switches_per_subtree_) {
    spkt_throw_printf(sprockit::InputError,
          "up_ports_per_leaf_switch (%d) must be >= agg_switches_per_subtree (%d)",
          up_ports_per_leaf_switch_,agg_switches_per_subtree_);
  }

  // check that up ports mod switches is zero
  val = up_ports_per_leaf_switch_ % agg_switches_per_subtree_;
  if (val != 0) {
    spkt_throw_printf(sprockit::InputError,
          "up_ports_per_leaf_switch mod agg_switches_per_subtree must be zero (%d mod %d = %d)",
          up_ports_per_leaf_switch_,agg_switches_per_subtree_,val);
  }

  // check that leaf-agg ports match up
  int ndown = num_core_switches_ * down_ports_per_core_switch_;
  int nup = num_agg_switches_ * up_ports_per_agg_switch_;
  if (ndown != nup) {
    spkt_throw_printf(sprockit::InputError,
          "num_core_switches * down_ports_per_core_switch must equal total number of aggregation switches * up_ports_per_agg_switch (%d != %d)",
          ndown,nup);
  }

  // check that agg-core ports match up
  ndown = agg_switches_per_subtree_ * down_ports_per_agg_switch_;
  nup = leaf_switches_per_subtree_ * up_ports_per_leaf_switch_;
  if (ndown != nup) {
    spkt_throw_printf(sprockit::InputError,
          "agg_switches_per_subtree * down_ports_per_agg_switch must equal leaf_switches_per_subtree * up_ports_per_leaf_switch (%d != %d)",
          ndown,nup);
  }
}

void
FatTree::endpointsConnectedToInjectionSwitch(SwitchId swaddr,
                                   std::vector<InjectionPort>& nodes) const
{
  if (level(swaddr) > 0){
    nodes.clear();
    return;
  }

  nodes.clear();
  for (int i = 0; i < concentration_; i++) {
    InjectionPort port;
    port.nid = swaddr*concentration_ + i;
    if (port.nid < max_nodes_){
      port.switch_port = up_ports_per_leaf_switch_ + i;
      port.ep_port = 0;
      nodes.push_back(port);
    }
  }
}

}
} //end of namespace sstmac
