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

#include <sstream>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/hardware/router/router.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/stl_string.h>
#include <sprockit/keyword_registration.h>

#include <math.h>

RegisterKeywords(
"branching",
"num_levels",
"num_inj_switches_per_subtree",
"num_core_switches",
"num_agg_subtrees",
"num_agg_switches_per_subtree"
);

namespace sstmac {
namespace hw {


abstract_fat_tree::abstract_fat_tree(sprockit::sim_parameters *params,
                                     InitMaxPortsIntra i1,
                                     InitGeomEjectID i2) :
  structured_topology(params, i1, i2)
{
}

void
abstract_fat_tree::nodes_connected_to_injection_switch(switch_id swaddr,
                                                       std::vector<injection_port>& nodes) const
{
  if (swaddr >= numleafswitches_){
    nodes.resize(0);
  } else {
    structured_topology::nodes_connected_to_injection_switch(swaddr, nodes);
  }
}

void
abstract_fat_tree::nodes_connected_to_ejection_switch(switch_id swaddr,
                                                      std::vector<injection_port>& nodes) const
{
  nodes_connected_to_injection_switch(swaddr, nodes);
}

fat_tree::fat_tree(sprockit::sim_parameters* params) :
  abstract_fat_tree(params,
                    InitMaxPortsIntra::I_Remembered,
                    InitGeomEjectID::I_Remembered)
{
  if (params->has_param("geometry")){
    std::vector<int> args;
    params->get_vector_param("geometry", args);
    if (args.size() != 2) {
      spkt_throw_printf(sprockit::input_error,
                       "fat_tree::init_factory_params: geometry needs 2 parameters, got %d",
                       args.size());
    }
    l_ = args[0];
    k_ = args[1];
  } else {
    l_ = params->get_int_param("num_levels");
    k_ = params->get_int_param("branching");
  }

  numleafswitches_ = pow(k_, l_ - 1);
  toplevel_ = l_ - 1;

  max_ports_intra_network_ = 2*k_;
  eject_geometric_id_ = max_ports_intra_network_;
}

int
fat_tree::upColumnConnection(int k, int myColumn, int upPort, int myBranchSize)
{
  upPort = upPort % k;
  int myReplicaID = myColumn % myBranchSize;
  int portStride = myBranchSize;
  int upBranchSize = myBranchSize*k;
  int myVirtualBranch = myColumn/myBranchSize;
  int upVirtualBranch = myVirtualBranch/k;
  int ret = upVirtualBranch*upBranchSize + upPort*portStride + myReplicaID;
  //printf("(c=%d,vc=%d,p=%d)->(c=%d,vc=%d)",
  //     myColumn, myVirtualColumn, upPort, ret, upVirtualColumn);
  return ret;
}

int
fat_tree::downColumnConnection(int k, int myColumn, int downPort, int myBranchSize)
{
  downPort = downPort % k;
  int myVirtualBranch = myColumn / myBranchSize;
  int myReplicaID = myColumn % myBranchSize;
  int lowerBranchSize = myBranchSize / k;
  int lowerReplicaID = myReplicaID % lowerBranchSize;
  return myVirtualBranch*k + downPort*lowerBranchSize + lowerReplicaID;
}


void
fat_tree::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  int branchSize = 1;
  conns.resize(2*k_);
  int myRow, myCol;
  compute_row_col(src, myRow, myCol);
  for (int row=0; row < myRow; ++row){
    branchSize *= k_;
  }
  int myBranch = myCol / branchSize;
  int cidx = 0;
  if (myRow < toplevel_){
    for (int k=0; k < k_; ++k){
      int upColumn = upColumnConnection(k_, myCol, k, branchSize);
      switch_id upDst = switch_at_row_col(myRow+1,upColumn);
      int upPort = up_port(k);
      int downPort = down_port(myBranch % k_);

      connection& conn = conns[cidx];
      conn.src = src;
      conn.dst = upDst;
      conn.src_outport = upPort;
      conn.dst_inport = downPort;
      ++cidx;
    }
  }

  if (myRow > 0){
    branchSize /= k_;
    for (int col=0; col < numleafswitches_; ++col){
      for (int k=0; k < k_; ++k){
        int upColumn = upColumnConnection(k_, col, k, branchSize);
        if (upColumn == myCol){
          int branch = col / branchSize;
          switch_id downDst = switch_at_row_col(myRow-1, col);
          int upPort = up_port(k);
          int downPort = down_port(branch % k_);
          connection& conn = conns[cidx];
          conn.src = src;
          conn.dst = downDst;
          conn.src_outport = downPort;
          conn.dst_inport = upPort;
          ++cidx;
        }
      }
    }
  }

  conns.resize(cidx);
}

void
fat_tree::configure_individual_port_params(switch_id src,
                                 sprockit::sim_parameters *switch_params) const
{
  topology::configure_individual_port_params(0, 2*k_, switch_params);
}

void
fat_tree::configure_vc_routing(std::map<routing::algorithm_t, int> &m) const
{
  m[routing::minimal] = 2; //up and down
}

void
fat_tree::minimal_route_to_switch(
  switch_id current_sw_addr,
  switch_id dest_sw_addr,
  routable::path& path) const
{
  spkt_throw_printf(sprockit::unimplemented_error, "fattree::minimal_route_to_switch");
}

int
fat_tree::minimal_distance(switch_id src,
                           switch_id dst) const
{
  int srcRow = src / numleafswitches_;
  int srcCol = src % numleafswitches_;
  int dstRow = dst / numleafswitches_;
  int dstCol = dst % numleafswitches_;

  int startRow = std::min(srcRow, dstRow);
  int branchSize = pow(k_, startRow);
  int srcBranch = srcCol / branchSize;
  int dstBranch = dstCol / branchSize;
  int stopRow = startRow;
  //keep going up until these land in the same branch
  while (srcBranch != dstBranch){
    branchSize *= k_;
    srcBranch = srcCol / branchSize;
    dstBranch = dstCol / branchSize;
    ++stopRow;
  }

  int distance = (stopRow - srcRow)  + (stopRow - dstRow);
  return distance;
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
  spkt_throw_printf(sprockit::unimplemented_error, "simple_fat_tree::create_partition");
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

int
tapered_fat_tree::minimal_distance(
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


  int srcTree = sub_tree(src);
  int dstTree = sub_tree(dst);
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

tapered_fat_tree::tapered_fat_tree(sprockit::sim_parameters *params) :
  abstract_fat_tree(params,
                    InitMaxPortsIntra::I_Remembered,
                    InitGeomEjectID::I_Remembered)
{
  num_inj_switches_per_subtree_ = params->get_int_param("num_inj_switches_per_subtree");
  num_agg_switches_per_subtree_ = params->get_int_param("num_agg_switches_per_subtree");
  num_agg_subtrees_ = params->get_int_param("num_agg_subtrees");
  num_core_switches_ = params->get_int_param("num_core_switches");
  num_inj_switches_ = num_inj_switches_per_subtree_ * num_agg_subtrees_;

  //double numerator = num_inj_switches_per_subtree_ + num_agg_switches_per_subtree_;
  //double down_link_fraction = double(num_inj_switches_per_subtree_) / numerator;
  //double up_link_fraction = num_agg_switches_per_subtree_ / numerator;

  //agg_bw_multiplier_ = up_link_fraction * num_agg_switches_per_subtree_;
  agg_bw_multiplier_ = num_agg_switches_per_subtree_;

  numleafswitches_ = num_inj_switches_per_subtree_ * num_agg_subtrees_;

  int max_up_port = std::max(up_port(0), up_port(1));
  int max_core_port = num_agg_subtrees_;
  max_ports_intra_network_ = std::max(max_up_port, max_core_port);
  
  eject_geometric_id_ = max_ports_intra_network_;

  num_switches_ = num_inj_switches_ + num_agg_subtrees_ + 1;
}

void
tapered_fat_tree::configure_individual_port_params(switch_id src,
                                  sprockit::sim_parameters *switch_params) const
{
  sprockit::sim_parameters* link_params = switch_params->get_namespace("link");
  int buffer_size = link_params->get_int_param("buffer_size");
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
    for (int s=0; s < num_inj_switches_per_subtree_; s++){
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
    multiplier = num_agg_switches_per_subtree_;
  } else {
    //this switch is modeling the functionality of X commodity switches
    multiplier = num_core_switches_;
  }

  sprockit::sim_parameters* xbar_params = switch_params->get_namespace("xbar");
  double bw = xbar_params->get_bandwidth_param("bandwidth");
  //we are overwriting params - we have to make sure that the original baseline bandwidth is preserved
  double baseline_bw = xbar_params->get_optional_bandwidth_param("baseline_bandwidth", bw);
  double xbar_bw = baseline_bw * multiplier;
  (*xbar_params)["bandwidth"].setBandwidth(xbar_bw/1e9, "GB/s");
  (*xbar_params)["baseline_bandwidth"].setBandwidth(baseline_bw/1e9, "GB/s");
  configure_individual_port_params(src, switch_params);
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
      conn.dst = num_inj_switches_ + s;
      conn.src_outport = s;
      conn.dst_inport = inport;
    }
  } else if (myRow == 1){
    //agg switch
    int myTree = agg_sub_tree(src);
    int myOffset = myTree * num_inj_switches_per_subtree_;
    conns.resize(num_inj_switches_per_subtree_ + 1);
    int inport = up_port(0);
    for (int s=0; s < num_inj_switches_per_subtree_; ++s){
      connection& conn = conns[s];
      conn.src = src;
      conn.dst = myOffset + s;
      conn.src_outport = s;
      conn.dst_inport = inport;
    }
    connection& upconn = conns[num_inj_switches_per_subtree_];
    upconn.src = src;
    upconn.dst = core_switch_id();
    upconn.src_outport = up_port(1);
    upconn.dst_inport = myTree;
  } else {
    //inj switch
    int myTree = inj_sub_tree(src);
    int myOffset = src % num_inj_switches_per_subtree_;
    conns.resize(1);
    int outport = up_port(0);
    connection& conn = conns[0];
    conn.src = src;
    conn.dst = num_inj_switches_ + myTree;
    conn.src_outport = outport;
    conn.dst_inport = myOffset;
  }
}

int
tapered_fat_tree::level(switch_id sid) const
{
  if (sid == core_switch_id()){
    return 2;
  } else if (sid >= num_inj_switches_){
    return 1;
  } else {
    return 0;
  }
}

void
tapered_fat_tree::minimal_route_to_switch(
  switch_id current_sw_addr,
  switch_id dest_sw_addr,
  routable::path &path) const
{
  int src_level = level(current_sw_addr);
  int dst_level = level(dest_sw_addr);
  //question is whether I go up or down
  if (dst_level >= src_level){ //definitely have to go up
    path.outport = up_port(src_level);
    path.vc = 0;
    top_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d",
            int(dest_sw_addr), dst_level,
            int(current_sw_addr), src_level);
  } else if (src_level == 2){
    //definitely have to go down
    int dst_sub_tree = dst_level == 0 ? inj_sub_tree(dest_sw_addr) : agg_sub_tree(dest_sw_addr);
    path.outport = dst_sub_tree;
    path.vc = 0;
    top_debug("fat_tree: routing down to get to s=%d,l=%d from s=%d,l=%d on port %d",
            int(dest_sw_addr), dst_level,
            int(current_sw_addr), src_level,
            path.outport);
  } else if (src_level == 1){
    //going to level 0, but may have to go up or down to get there
    int my_tree = agg_sub_tree(current_sw_addr);
    int dst_tree = inj_sub_tree(dest_sw_addr);
    if (dst_tree == my_tree){
      //okay, great, I should have direct link
      path.outport = dest_sw_addr % num_inj_switches_per_subtree_;
      path.vc = 0;
      top_debug("fat_tree: routing up to get to s=%d,l=%d from s=%d,l=%d hopping from tree %d to tree %d",
              int(dest_sw_addr), dst_level,
              int(current_sw_addr), src_level,
              my_tree, dst_tree);
    } else {
      //nope, have to go to core to hope over to other tree
      path.outport = up_port(src_level);
      path.vc = 0;
      top_debug("fat_tree: routing down to get to s=%d,l=%d from s=%d,l=%d on port %d within tree %d",
              int(dest_sw_addr), dst_level,
              int(current_sw_addr), src_level,
              path.outport, my_tree);
    }
  }
}

void
tapered_fat_tree::configure_vc_routing(std::map<routing::algorithm_t, int> &m) const
{
  m[routing::minimal] = 1; //up and down
}


}
} //end of namespace sstmac