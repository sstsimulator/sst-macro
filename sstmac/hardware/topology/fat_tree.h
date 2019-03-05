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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FATTREE_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FATTREE_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>


namespace sstmac {
namespace hw {


/*------------------------------------------------------------------------------
  abstract_fat_tree
  ----------------------------------------------------------------------------*/

class AbstractFatTree :
  public StructuredTopology
{
 public:
  typedef enum {
   up_dimension = 1,
   down_dimension = 0
  } dimension_t;

  int diameter() const override {
    return 4;
  }

  SwitchId numLeafSwitches() const override {
    return num_leaf_switches_;
  }


  inline int injSubtree(const SwitchId sid) const {
    return sid / leaf_switches_per_subtree_;
  }

  int leafSwitchesPerSubtree() const {
    return leaf_switches_per_subtree_;
  }

 protected:
  AbstractFatTree(SST::Params& params);

  virtual int aggSubtree(const SwitchId sid) const {
    return (sid - num_leaf_switches_) / agg_switches_per_subtree_;
  }

  // used for minimal_fat_tree routing
  virtual int upPort(int level) const = 0;
  virtual int downPort(int dst_tree) const = 0;

  int num_leaf_switches_;
  int num_agg_subtrees_;
  int leaf_switches_per_subtree_;
  int agg_switches_per_subtree_;
  int num_agg_switches_;
  int num_core_switches_;
  double vtk_radius_;
  double vtk_subtree_theta_;

 private:
  sprockit::SimParameters*
  override_params(SST::Params& params);
};


/*------------------------------------------------------------------------------
  fat_tree
  ----------------------------------------------------------------------------*/

/**
 * @class fat_tree
 * The fat tree network generates a k-ary fat tree with l tiers
 */
class FatTree :
  public AbstractFatTree
{

 public:
  SPKT_REGISTER_DERIVED(
    Topology,
    FatTree,
    "macro",
    "fat_tree",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a fat-tree, with possible tapering")

  FatTree(SST::Params& params);

  virtual std::string toString() const override {
    return "fat tree topology";
  }

  virtual ~FatTree() {}

  SwitchId numSwitches() const override {
    return num_leaf_switches_ + num_agg_switches_ + num_core_switches_;
  }

  int level(SwitchId sid) const {
    int num_non_core = num_leaf_switches_ + num_agg_switches_;
    if (sid < num_leaf_switches_)
      return 0;
    else if (sid >= num_non_core)
      return 2;
    return 1;
  }

  int subtree(SwitchId sid) const {
    if (sid < num_leaf_switches_){
      return sid / leaf_switches_per_subtree_;
    }
    SwitchId offset = sid - num_leaf_switches_;
    if (offset < num_agg_switches_){
      return offset / aggSwitchesPerSubtree();
    } else {
      return -1;
    }
  }

  int maxNumPorts() const override {
    int first_max = std::max(concentration() + up_ports_per_leaf_switch_,
                             down_ports_per_agg_switch_ + up_ports_per_agg_switch_);
    return std::max(first_max, down_ports_per_core_switch_);
  }

  int numUpPorts(SwitchId sid) const {
    int lvl = level(sid);
    switch (lvl) {
    case 0:
      return up_ports_per_leaf_switch_;
    case 1:
      return up_ports_per_agg_switch_;
    }
    return 0; // else core (lvl==2)
  }

  int firstUpPort(SwitchId sid) const {
    int lvl = level(sid);
    switch (lvl) {
    case 0:
      return 0;
    case 1:
      return down_ports_per_agg_switch_;
    }
    // else core (lvl==2)
    spkt_throw_printf(sprockit::ValueError,
                      "requested first up port on core switch");
    return -1;
  }

  bool isCurvedVtkLink(SwitchId sid, int port) const override {
    return false;
  }

  void endpointsConnectedToInjectionSwitch(
      SwitchId swaddr,
      std::vector<InjectionPort>& nodes) const override;

  int numAggSubtrees() const {
    return num_agg_subtrees_;
  }

  int numAggSwitches() const {
    return num_agg_switches_;
  }

  int aggSwitchesPerSubtree() const {
    return agg_switches_per_subtree_;
  }

  int leafSwitchesPerSubtree() const {
    return leaf_switches_per_subtree_;
  }

  int upPortsPerAggSwitch() const {
    return up_ports_per_agg_switch_;
  }

  int downPortsPerAggSwitch() const {
    return down_ports_per_agg_switch_;
  }

  int upPortsPerLeafSwitch() const {
    return up_ports_per_leaf_switch_;
  }

  VTKSwitchGeometry getVtkGeometry(SwitchId sid) const override;

  void connectedOutports(SwitchId src, std::vector<Connection>& conns) const override;

  double portScaleFactor(uint32_t addr, int port) const override;

  int minimalDistance(SwitchId src, SwitchId dst) const {
    if (src == dst){
      return 0;
    }
    int src_tree = aggSubtree(src);
    int dst_tree = aggSubtree(dst);
    if (src_tree == dst_tree){
      return 2;
    } else {
      return 4;
    }
  }

  int numHopsToNode(NodeId src, NodeId dst) const override {
    return minimalDistance(src/concentration_, dst/concentration_);
  }

 protected:
  // used for minimal_fat_tree routing
  inline int upPort(int level) const override {
    if (level == 0){
      return 0;
    } else if (level == 1) {
      return down_ports_per_agg_switch_;
    } else {
      spkt_abort_printf("Bad up port level %d - should be 0 or 1", level);
      return -1; //make gcc happy
    }
  }
  inline int downPort(int dst_tree) const override {
      return dst_tree * agg_switches_per_subtree_;
  }

 private:
  int up_ports_per_leaf_switch_;
  int down_ports_per_agg_switch_;
  int up_ports_per_agg_switch_;
  int down_ports_per_core_switch_;

  double leaf_agg_bw_;
  double agg_core_bw_;

  void checkInput() const;
};


/*------------------------------------------------------------------------------
  tapered_fat_tree
  ----------------------------------------------------------------------------*/

class TaperedFatTree : public AbstractFatTree
{
 public:
  SPKT_REGISTER_DERIVED(
    Topology,
    TaperedFatTree,
    "macro",
    "tapered_fat_tree",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a fat-tree with simple tapering models")

  TaperedFatTree(SST::Params& params);

  virtual std::string toString() const override {
    return "tapered fat-tree topology";
  }

  virtual ~TaperedFatTree() {}

  SwitchId numSwitches() const override {
    return num_leaf_switches_ + num_agg_subtrees_ + 1;
  }

  int level(SwitchId sid) const {
    if (sid == coreSwitchId()){
      return 2;
    } else if (sid >= num_leaf_switches_){
      return 1;
    } else {
      return 0;
    }
  }

  inline int subtree(const SwitchId sid) const {
    int lvl = level(sid);
    switch (lvl) {
    case 0:
      return injSubtree(sid);
    case 1:
      return aggSubtree(sid);
    case 2:
      return num_agg_subtrees_;
    default:
     spkt_abort_printf("Bad level %d - should be <= 2", lvl);
     return -1; //make gcc happy
    }
  }

  int minimalDistance(SwitchId src, SwitchId dst) const {
    if (src == dst){
      return 0;
    }
    int src_tree = aggSubtree(src);
    int dst_tree = aggSubtree(dst);
    if (src_tree == dst_tree){
      return 2;
    } else {
      return 4;
    }
  }

  int numHopsToNode(NodeId src, NodeId dst) const override {
    return minimalDistance(src/concentration_, dst/concentration_);
  }

  void endpointsConnectedToInjectionSwitch(
      SwitchId swaddr, std::vector<InjectionPort>& nodes) const override;

  int maxNumPorts() const override {
    int first_max = std::max(concentration() + 1, leaf_switches_per_subtree_ + 1);
    return std::max(first_max, num_agg_subtrees_);
  }

  inline int upPort(int level) const override {
    if (level == 0){
      //port is after all the compute nodes
      return concentration();
    } else if (level == 1){
      //I have this many down ports - up port comes after
      return leaf_switches_per_subtree_;
    } else {
      spkt_abort_printf("invalid level %d - cannot go up on fat tree level %d", level, level);
      return -1;
    }
  }

  int downPort(int dst_tree) const override {
    return dst_tree;
  }

  inline int aggSubtree(SwitchId sid) const override {
    return (sid - num_leaf_switches_);
  }

  double portScaleFactor(uint32_t addr, int port) const override;

  void connectedOutports(SwitchId src, std::vector<Connection>& conns) const override;

 protected:
  virtual void createPartition(
    int* switch_to_lp,
    int* switch_to_thread,
    int me,
    int nproc,
    int nthread,
    int noccupied) const override;

 private:
  inline SwitchId coreSwitchId() const {
    return num_leaf_switches_ + num_agg_subtrees_;
  }
  double agg_bw_multiplier_;

};

}
} //end of namespace sstmac

#endif
