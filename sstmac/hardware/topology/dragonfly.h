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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_dragonfly_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_dragonfly_H_INCLUDED

#include <sstmac/hardware/topology/cartesian_topology.h>

namespace sstmac {
namespace hw {

class dragonfly;

class inter_group_wiring {
 public:
  DeclareFactory(inter_group_wiring,
    int, /* a=num switches per group */
    int, /* g=num groups */
    int /* h=num group links per switch */
  )
                

  /**
   * @brief group_port
   * @param srcA
   * @param srcG
   * @param dstG
   * @return The port on which router (srcA, srcG) connects to group dstG
   */
  virtual int input_group_port(int srcA, int srcG, int srcH, int dstA, int dstG) const = 0;

  /**
   * @brief connected_routers
   * @param a The src router index within the group
   * @param g The src router group
   * @param connected [in-out] The routers (switch id) for each inter-group interconnection
   * @return The number of routers in connected array
   */
  virtual void connected_routers(int a, int g, std::vector<int>& connected) const = 0;

  /**
   * @brief connected_to_group
   * @param srcG
   * @param dstG
   * @param connected [in-out] The list of all intra-group routers in range (0 ... a-1)
   *                  that have connections to a router in group dstG
   * @return The number of routers in group srcG with connections to dstG
   */
  virtual void connected_to_group(int srcG, int dstG, std::vector<std::pair<int,int>>& connected) const = 0;

  virtual switch_id random_intermediate(router* rtr, switch_id current_sw, switch_id dest_sw, uint32_t seed);

 protected:
  /**
   * @brief inter_group_wiring
   * @param params
   * @param a  The number of routers per group
   * @param g  The number of groups
   * @param h  The number of group links per router
   */
  inter_group_wiring(sprockit::sim_parameters* params, int a, int g, int h);

 protected:
  /** Number of routers per group */
  int a_;
  /** Number of groups */
  int g_;
  /** Number of group connections per router */
  int h_;
};

/**
 * @brief The dragonfly class
 * A canonical dragonfly with notation/structure matching the Dally paper
 * Technology-Driven, Highly-Scalable Dragonfly Topology
 */
class dragonfly : public cartesian_topology
{
  FactoryRegister("dragonfly", topology, dragonfly)
 public:
  dragonfly(sprockit::sim_parameters* params);

 public:
  std::string to_string() const override {
    return "dragonfly";
  }

  bool uniform_network_ports() const override {
    return false;
  }

  bool is_global_port(int port) const {
    return port >= a_;
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

  virtual ~dragonfly() {}

  int ndimensions() const {
    return 2;
  }

  /**
   * @brief Following Dally notation
   * @return the number of routers in a group
   */
  int a() const {
    return a_;
  }

  /**
   * @brief Following Dally notation
   * @return the number of inter-group connections per router
   */
  int h() const {
    return h_;
  }

  /**
   * @brief Following Dally notation
   * @return the total number of groups
   */
  int g() const {
    return g_;
  }

  /**
   * @brief get_coords
   * @param sid
   * @param a
   * @param g
   */
  inline void get_coords(switch_id sid, int& a, int& g) const {
    a = computeA(sid);
    g = computeG(sid);
  }

  int get_uid(int a, int g) const {
    return a + g * a_;
  }

  inline int computeA(switch_id sid) const {
    return sid % a_;
  }

  inline int computeG(switch_id sid) const {
    return sid / a_;
  }

  int num_switches() const override {
    return a_ * g_;
  }

  int num_leaf_switches() const override {
    return a_ * g_;
  }

  void minimal_route_to_switch(
      switch_id current_sw_addr,
      switch_id dest_sw_addr,
      packet::path &path) const;

  int minimal_distance(switch_id src, switch_id dst) const override;

  int diameter() const override {
    return 3;
  }

  virtual void configure_geometric_paths(std::vector<int> &redundancies);

  coordinates switch_coords(switch_id sid) const override {
    coordinates c(2);
    c[0] = computeA(sid);
    c[1] = computeG(sid);
    return c;
  }

  switch_id switch_addr(const coordinates &coords) const override {
    return get_uid(coords[0], coords[1]);
  }

  inter_group_wiring* group_wiring() const {
    return group_wiring_;
  }

  switch_id random_intermediate(router* rtr, switch_id current, switch_id dest, uint32_t seed){
    return group_wiring_->random_intermediate(rtr,current,dest,seed);
  }

 protected:
  int a_;
  int h_;
  int g_;
  inter_group_wiring* group_wiring_;

  void setup_port_params(sprockit::sim_parameters* params,
                    int red, int port_offset, int num_ports) const;

};

}
} //end of namespace sstmac

#endif
