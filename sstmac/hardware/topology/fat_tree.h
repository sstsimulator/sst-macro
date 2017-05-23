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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FATTREE_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FATTREE_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>


namespace sstmac {
namespace hw {

class abstract_fat_tree :
  public structured_topology
{
 public:
  typedef enum {
   up_dimension = 1,
   down_dimension = 0
  } dimension_t;

  static int pow(int a, int exp){
    int res = 1;
    for (int i=0; i < exp; ++i){
      res *= a;
    }
    return res;
  }

  virtual int num_leaf_switches() const override {
    return numleafswitches_;
  }

  void nodes_connected_to_injection_switch(switch_id swaddr,
                            std::vector<injection_port>& nodes) const override;

  void nodes_connected_to_ejection_switch(switch_id swaddr,
                            std::vector<injection_port>& nodes) const override;

 protected:
  abstract_fat_tree(sprockit::sim_parameters* params,
                    InitMaxPortsIntra i1,
                    InitGeomEjectID i2);

 protected:
  int numleafswitches_;

 private:
  sprockit::sim_parameters*
  override_params(sprockit::sim_parameters* params);
};

/**
 * @class fat_tree
 * The fat tree network generates a k-ary fat tree with l tiers
 */
class fat_tree :
  public abstract_fat_tree
{
  FactoryRegister("fattree | fat_tree | ftree", topology, fat_tree,
    "Fat tree topology with L levels and radix K.  This fat tree is actually implemented with"
    " commodity switches. Each level of the fat tree has the same number of switches."
    "This is equivalent to archetypal fat tree with fatter links being replaced by MORE links")

 public:
  fat_tree(sprockit::sim_parameters* params);

  virtual std::string to_string() const override {
    return "fat tree topology";
  }

  inline int up_port(int dir) const {
    return  k_ + dir;
  }

  inline int down_port(int dir) const {
    return dir;
  }

  virtual ~fat_tree() {}

  bool uniform_network_ports() const override {
    return true;
  }

  int l() const {
    return l_;
  }

  int k() const {
    return k_;
  }

  int diameter() const override {
    return (l_ + 1) * 2;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return true;
  }

  bool uniform_switches() const override {
    return true;
  }

  void connected_outports(switch_id src, std::vector<connection>& conns) const override;

  void configure_individual_port_params(switch_id src,
      sprockit::sim_parameters *switch_params) const override;


  virtual int num_switches() const override {
    return numleafswitches_ * l_;
  }

  void configure_vc_routing(std::map<routing::algorithm_t, int> &m) const override;

  void minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    routable::path& path) const override;

  int minimal_distance(
    switch_id src,
    switch_id dest) const override;

  int switch_at_row_col(int row, int col) const {
    return row * numleafswitches_ + col;
  }

  void compute_row_col(switch_id sid, int& row, int& col) const {
    row = sid / numleafswitches_;
    col = sid % numleafswitches_;
  }

  static int upColumnConnection(int k, int myColumn, int upPort, int columnSize);

  static int downColumnConnection(int k, int myColumn, int downPort, int columnSize);

 private:
  int toplevel_;
  int k_;
  int l_;
};

class tapered_fat_tree : public abstract_fat_tree
{
  FactoryRegister("tapered_fat_tree | simple_fattree", topology, tapered_fat_tree)
 public:
  tapered_fat_tree(sprockit::sim_parameters* params);

  virtual std::string to_string() const override {
    return "simple fat tree topology";
  }

  virtual ~tapered_fat_tree() {}

  bool uniform_network_ports() const override {
    return false;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return false;
  }

  bool uniform_switches() const override {
    return false;
  }

  void connected_outports(switch_id src, std::vector<connection>& conns) const override;

  void configure_nonuniform_switch_params(switch_id src,
        sprockit::sim_parameters* switch_params) const override;

  void configure_individual_port_params(switch_id src,
      sprockit::sim_parameters *switch_params) const override;

  int num_switches() const override {
    return num_switches_;
  }

  int convert_to_port(int dim, int dir) const;

  virtual void create_partition(
    int* switch_to_lp,
    int* switch_to_thread,
    int me,
    int nproc,
    int nthread,
    int noccupied) const override;

  int minimal_distance(switch_id src, switch_id dest) const override;

  void configure_vc_routing(std::map<routing::algorithm_t, int> &m) const override;

  void minimal_route_to_switch(
    switch_id current_sw_addr,
    switch_id dest_sw_addr,
    routable::path& path) const override;

  int level(switch_id sid) const;

  inline int inj_sub_tree(switch_id sid) const {
    return sid / num_inj_switches_per_subtree_;
  }

  inline int agg_sub_tree(switch_id sid) const {
    return (sid - num_inj_switches_);
  }

  inline int sub_tree(switch_id sid) const {
    if (sid > num_inj_switches_){
      return agg_sub_tree(sid);
    } else {
      return inj_sub_tree(sid);
    }
  }

  int up_port(int level) const {
    if (level == 0){
      //port is after all the compute nodes
      return concentration();
    } else if (level == 1){
      //I have this many down ports - up port comes after
      return num_inj_switches_per_subtree_;
    } else {
      spkt_abort_printf("invalid level %d - cannot go up on fat tree level %d", level, level);
    }
  }

  int diameter() const override {
    return 4;
  }

 private:
  switch_id core_switch_id() const {
    return num_inj_switches_ + num_agg_subtrees_;
  }
  int num_inj_switches_;
  int num_agg_subtrees_;
  int num_inj_switches_per_subtree_;
  int num_agg_switches_per_subtree_;
  int num_core_switches_;
  int num_switches_;
  double agg_bw_multiplier_;

};

}
} //end of namespace sstmac

#endif