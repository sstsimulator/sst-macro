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

class abstract_fat_tree :
  public structured_topology
{
 public:
  typedef enum {
   up_dimension = 1,
   down_dimension = 0
  } dimension_t;

  int diameter() const override {
    return 4;
  }

  int num_leaf_switches() const override {
    return num_leaf_switches_;
  }

  virtual int level(switch_id sid) const = 0;

  inline int subtree(const switch_id sid) const {
    int lvl = level(sid);
    switch (lvl) {
    case 0:
      return inj_subtree(sid);
    case 1:
      return agg_subtree(sid);
    case 2:
      return num_agg_subtrees_;
    }
  }

  inline int core_subtree() const {
    return num_agg_subtrees_;
  }

  void nodes_connected_to_injection_switch(
      switch_id swaddr,
      std::vector<injection_port>& nodes) const override;

  void nodes_connected_to_ejection_switch(
      switch_id swaddr,
      std::vector<injection_port>& nodes) const override;

  void minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    packet::path& path) const;

  int minimal_distance(switch_id src, switch_id dest) const override;

 protected:
  abstract_fat_tree(sprockit::sim_parameters* params,
                    InitMaxPortsIntra i1,
                    InitGeomEjectID i2);

  inline int inj_subtree(const switch_id sid) const {
    return sid / leaf_switches_per_subtree_;
  }

  virtual int agg_subtree(const switch_id sid) const {
    return (sid - num_leaf_switches_) / agg_switches_per_subtree_;
  }

  // used for minimal_fat_tree routing
  virtual int up_port(int level) const = 0;
  virtual int down_port(int dst_tree) const = 0;

  void write_bw_params(sprockit::sim_parameters *switch_params,
                       double multiplier) const;

  int num_leaf_switches_;
  int num_agg_subtrees_;
  int leaf_switches_per_subtree_;
  int agg_switches_per_subtree_;
  int num_agg_switches_;
  int num_core_switches_;

 private:
  sprockit::sim_parameters*
  override_params(sprockit::sim_parameters* params);
};


/*------------------------------------------------------------------------------
  fat_tree
  ----------------------------------------------------------------------------*/

/**
 * @class fat_tree
 * The fat tree network generates a k-ary fat tree with l tiers
 */
class fat_tree :
  public abstract_fat_tree
{
  FactoryRegister("fat_tree", topology, fat_tree,
    "Flexible fat-tree topology with 3 levels.")

 public:
  fat_tree(sprockit::sim_parameters* params);

  virtual std::string to_string() const override {
    return "fat tree topology";
  }

  virtual ~fat_tree() {}

  int num_switches() const override {
    return num_leaf_switches_ + num_agg_switches_ + num_core_switches_;
  }

  int level(switch_id sid) const override {
    int num_non_core = num_leaf_switches_ + num_agg_switches_;
    if (sid < num_leaf_switches_)
      return 0;
    else if (sid >= num_non_core)
      return 2;
    return 1;
  }

  int num_up_ports(switch_id sid) const {
    int lvl = level(sid);
    switch (lvl) {
    case 0:
      return up_ports_per_leaf_switch_;
    case 1:
      return up_ports_per_agg_switch_;
    }
    return 0; // else core (lvl==2)
  }

  int first_up_port(switch_id sid) const {
    int lvl = level(sid);
    switch (lvl) {
    case 0:
      return 0;
    case 1:
      return down_ports_per_agg_switch_;
    }
    // else core (lvl==2)
    spkt_throw_printf(sprockit::value_error,
                      "requested first up port on core switch");
    return -1;
  }

  bool uniform_network_ports() const override {
    return true;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return false;
  }

  bool uniform_switches() const override {
    return false;
  }

  void connected_outports(switch_id src, std::vector<connection>& conns)
  const override;

  void connected_core_down_ports(sstmac::switch_id, int, std::vector<int>&)
  const;

  void connected_agg_down_ports(sstmac::switch_id, int, std::vector<int>&)
  const;

  void configure_nonuniform_switch_params(
      switch_id src,
      sprockit::sim_parameters *switch_params)
  const override;

  void configure_individual_port_params(
      switch_id src,
      sprockit::sim_parameters *switch_params)
  const override { }

protected:

  // used for minimal_fat_tree routing
  inline int up_port(int level) const override {
    if (level == 0) return 0;
    else if (level == 1) return down_ports_per_agg_switch_;
  }
  inline int down_port(int dst_tree) const override {
      return dst_tree * agg_switches_per_subtree_;
  }

 private:
  int up_ports_per_leaf_switch_;
  int down_ports_per_agg_switch_;
  int up_ports_per_agg_switch_;
  int down_ports_per_core_switch_;

  void check_input() const;
};


/*------------------------------------------------------------------------------
  tapered_fat_tree
  ----------------------------------------------------------------------------*/

class tapered_fat_tree : public abstract_fat_tree
{
  FactoryRegister("tapered_fat_tree", topology, tapered_fat_tree)
 public:
  tapered_fat_tree(sprockit::sim_parameters* params);

  virtual std::string to_string() const override {
    return "tapered fat-tree topology";
  }

  virtual ~tapered_fat_tree() {}

  int num_switches() const override {
    return num_leaf_switches_ + num_agg_subtrees_ + 1;
  }

  int level(switch_id sid) const override {
    if (sid == core_switch_id()){
      return 2;
    } else if (sid >= num_leaf_switches_){
      return 1;
    } else {
      return 0;
    }
  }

  bool uniform_network_ports() const override {
    return false;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return false;
  }

  bool uniform_switches() const override {
    return false;
  }

  void connected_outports(switch_id src, std::vector<connection>& conns)
  const override;

  void configure_individual_port_params(switch_id src,
      sprockit::sim_parameters *switch_params) const override;

  void configure_nonuniform_switch_params(switch_id src,
        sprockit::sim_parameters* switch_params) const override;

protected:

  inline int agg_subtree(switch_id sid) const {
    return (sid - num_leaf_switches_);
  }

  inline int up_port(int level) const {
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

  int down_port(int dst_tree) const override {
    return dst_tree;
  }

  virtual void create_partition(
    int* switch_to_lp,
    int* switch_to_thread,
    int me,
    int nproc,
    int nthread,
    int noccupied) const override;

 private:
  inline switch_id core_switch_id() const {
    return num_leaf_switches_ + num_agg_subtrees_;
  }
  double agg_bw_multiplier_;

};

}
} //end of namespace sstmac

#endif
