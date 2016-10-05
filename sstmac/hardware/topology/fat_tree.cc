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
// Author: Jeremiah Wilke <jjwilke@sandia.gov>
#include <sstream>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/hardware/router/router.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sprockit/sim_parameters.h>

#include <math.h>

namespace sstmac {
namespace hw {

SpktRegister("fattree | ftree", topology, fat_tree,
  "Fat tree topology with L levels and radix K.  This fat tree is actually implemented with commodity switches. Each level of the fat tree has the same number of switches.  This is equivalent to archetypal fat tree with fatter links being replaced by MORE links.");

SpktRegister("simple_fattree", topology, simple_fat_tree);

sprockit::sim_parameters*
abstract_fat_tree::override_params(sprockit::sim_parameters* params)
{
  if (params->has_param("num_leaf_switches")){
    numleafswitches_ = params->get_int_param("num_leaf_switches");
    k_ = params->get_int_param("k");
    l_ = 1;
    int switches_on_level = numleafswitches_;
    do {
      switches_on_level /= k_;
      ++l_;
    } while (switches_on_level > 1);
  } else {
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

    numleafswitches_ = pow(k_, l_ - 1);
  }
  toplevel_ = l_ - 1;
  return params;
}

abstract_fat_tree::abstract_fat_tree(sprockit::sim_parameters *params,
                                     InitMaxPortsIntra i1,
                                     InitGeomEjectID i2) :
  structured_topology(override_params(params), i1, i2)
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
simple_fat_tree::create_partition(
  int* switches_per_lp,
  int *switch_to_lp,
  int *switch_to_thread,
  int& local_num_switches,
  int me,
  int nproc,
  int nthread,
  int noccupied) const
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
simple_fat_tree::minimal_distance(
  switch_id src,
  switch_id dst) const
{
  int srcLevel = level(src);
  int dstLevel = level(dst);
  int srcOffset = src - level_offsets_[srcLevel];
  int dstOffset = dst - level_offsets_[dstLevel];
  return num_hops(srcLevel, srcOffset, dstLevel, dstOffset);
}

simple_fat_tree::simple_fat_tree(sprockit::sim_parameters *params) :
  abstract_fat_tree(params,
                    InitMaxPortsIntra::I_Remembered,
                    InitGeomEjectID::I_Remembered)
{
  int nswitches = numleafswitches_ * k_;
  int offset = 0;
  int level = 0;
  level_offsets_.resize(l_);
  level_sizes_.resize(l_);
  num_switches_ = 0;
  do {
    nswitches /= k_;
    nswitches = std::max(1,nswitches); //no fewer than 1
    level_offsets_[level] = offset;
    level_sizes_[level] = nswitches;
    top_debug("fat_tree: setting level %d to %d<->%d", level, offset, offset + nswitches);
    offset += nswitches;
    num_switches_ += nswitches;
    level++;
  } while (nswitches > 1); //until we have coalesced into a single switch
  top_debug("fat_tree: computed %d total switches on %d levels",
            num_switches_, l_);
  max_ports_intra_network_ = k_ + 1;
  eject_geometric_id_ = max_ports_intra_network_;

  /**
    The tapering array specifies the tapering for the up links for a given row
    and the corresponding xbar bandwidths for the given row. In general, makes
    most sense for initial tapering value to be 1.0 - but could be different.
  */
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
simple_fat_tree::configure_individual_port_params(switch_id src,
                                  sprockit::sim_parameters *switch_params) const
{
  sprockit::sim_parameters* link_params = switch_params->get_namespace("link");
  int buffer_size = switch_params->get_int_param("buffer_size");
  double bw = link_params->get_bandwidth_param("bandwidth");
  int myLevel = level(src);
  int up_link_bw_multiplier = 1;
  int kRatio;
  for (int l=0; l < myLevel; ++l){
    kRatio = level_sizes_[l] / level_sizes_[l+1];
    up_link_bw_multiplier *= kRatio;
  }
  //the down links have less bandwidth than the uplinks
  int down_link_bw_multiplier = up_link_bw_multiplier / kRatio;

  if (myLevel < toplevel_){
    //I have an up connection that needs to be tapered
    double tapering = tapering_[myLevel];
    //the tapering array specifies the tapering factor
    //for the up links of a given row
    //tapering[1] = 0.25 means that row 1's uplinks (and xbar) have 0.25x
    //the bandwidth of a full fat-tree
    int taperedBufSize = buffer_size*up_link_bw_multiplier*tapering;
    double taperedBw = bw*up_link_bw_multiplier*tapering;
    int outport = k_;
    setup_port_params(outport,
                      taperedBufSize,
                      taperedBw,
                      link_params, switch_params);
  }

  if (myLevel > 0){ // I have down connections that need to be tapered
    //my down links match the tapering of the uplinks from the row below
    double tapering = tapering_[myLevel - 1];
    int taperedBufSize = buffer_size*down_link_bw_multiplier*tapering;
    double taperedBw = bw*down_link_bw_multiplier*tapering;
    for (int s=0; s < k_; ++s){
      //all of my up links are tapered
      int outport = s;
      setup_port_params(outport,
                        taperedBufSize,
                        taperedBw,
                        link_params, switch_params);
    }
  }
}

void
simple_fat_tree::configure_nonuniform_switch_params(switch_id src,
                           sprockit::sim_parameters *switch_params) const
{
  int myLevel = level(src);
  int multiplier = 1;
  for (int l=0; l < myLevel; ++l){
    int kRatio = level_sizes_[l] / level_sizes_[l+1];
    multiplier *= kRatio;
  }
  sprockit::sim_parameters* xbar_params = switch_params->get_namespace("xbar");
  double bw = xbar_params->get_bandwidth_param("bandwidth");
  //we are overwriting params - we have to make sure that the original baseline bandwidth is preserved
  double baseline_bw = xbar_params->get_optional_bandwidth_param("baseline_bandwidth", bw);
  double xbar_bw = baseline_bw * multiplier * tapering_[myLevel];
  (*xbar_params)["bandwidth"].setBandwidth(xbar_bw/1e9, "GB/s");
  (*xbar_params)["baseline_bandwidth"].setBandwidth(baseline_bw/1e9, "GB/s");
  configure_individual_port_params(src, switch_params);
}

void
simple_fat_tree::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  int myRow = level(src);
  conns.resize(k_+1);
  int cidx = 0;
  if (myRow < toplevel_){
    int kRatio = level_sizes_[myRow] / level_sizes_[myRow+1];
    //I have up connections
    int myCol = src - level_offsets_[myRow];
    int upDst = level_offsets_[myRow+1] + myCol / kRatio;
    int down_switch_outport = k_;
    int up_switch_inport = myCol % k_;

    connection& conn = conns[cidx];
    conn.src = src;
    conn.dst = upDst;
    conn.src_outport = down_switch_outport;
    conn.dst_inport = up_switch_inport;
    ++cidx;
  }

  if (myRow > 0){
    //I have down connections
    int kRatio = level_sizes_[myRow-1] / level_sizes_[myRow];
    int myCol = src - level_offsets_[myRow];
    //the offset of switches on row below that start connecting
    int offset = kRatio * myCol + level_offsets_[myRow-1];
    for (int i=0; i < kRatio; ++i){ //number of switches that connect to me
      switch_id downDst = i + offset;
      int up_switch_outport = i;
      int down_switch_inport = k_;
      connection& conn = conns[cidx];
      conn.src = src;
      conn.dst = downDst;
      conn.src_outport = up_switch_outport;
      conn.dst_inport = down_switch_inport;
      ++cidx;
    }
  }

  conns.resize(cidx);
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
  routable::path &path) const
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
simple_fat_tree::configure_vc_routing(std::map<routing::algorithm_t, int> &m) const
{
  m[routing::minimal] = 2; //up and down
}


}
} //end of namespace sstmac

