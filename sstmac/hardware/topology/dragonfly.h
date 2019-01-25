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

class Dragonfly;

class InterGroupWiring {
 public:
  DeclareFactoryArgs(InterGroupWiring,
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
  virtual int inputGroupPort(int srcA, int srcG, int srcH, int dstA, int dstG) const = 0;

  /**
   * @brief connected_routers
   * @param a The src router index within the group
   * @param g The src router group
   * @param connected [in-out] The routers (switch id) for each inter-group interconnection
   * @return The number of routers in connected array
   */
  virtual void connectedRouters(int a, int g, std::vector<int>& connected) const = 0;

  /**
   * @brief connected_to_group
   * @param srcG
   * @param dstG
   * @param connected [in-out] The list of all intra-group routers in range (0 ... a-1)
   *                  that have connections to a router in group dstG
   * @return The number of routers in group srcG with connections to dstG
   */
  virtual void connectedToGroup(int srcG, int dstG, std::vector<std::pair<int,int>>& connected) const = 0;

  virtual SwitchId randomIntermediate(Router* rtr, SwitchId current_sw, SwitchId dest_sw, uint32_t seed);

 protected:
  /**
   * @brief inter_group_wiring
   * @param params
   * @param a  The number of routers per group
   * @param g  The number of groups
   * @param h  The number of group links per router
   */
  InterGroupWiring(SST::Params& params, int a, int g, int h);

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
class Dragonfly : public CartesianTopology
{
  FactoryRegister("dragonfly", Topology, Dragonfly)
 public:
  Dragonfly(SST::Params& params);

 public:
  std::string toString() const override {
    return "dragonfly";
  }

  bool isGlobalPort(int port) const {
    return port >= a_;
  }

  int maxNumPorts() const override {
    return a_ + h_ + concentration();
  }

  vtk_switch_geometry getVtkGeometry(SwitchId sid) const override;

  bool isCurvedVtkLink(SwitchId sid, int port) const override;

  void connectedOutports(SwitchId src, std::vector<connection>& conns) const override;

  virtual ~Dragonfly() {}

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
  inline void getCoords(SwitchId sid, int& a, int& g) const {
    a = computeA(sid);
    g = computeG(sid);
  }

  int getUid(int a, int g) const {
    return a + g * a_;
  }

  inline int computeA(SwitchId sid) const {
    return sid % a_;
  }

  inline int computeG(SwitchId sid) const {
    return sid / a_;
  }

  SwitchId numSwitches() const override {
    return a_ * g_;
  }

  SwitchId numLeafSwitches() const override {
    return a_ * g_;
  }

  int minimalDistance(SwitchId src, SwitchId dst) const;

  int numHopsToNode(NodeId src, NodeId dst) const override {
    return minimalDistance(src / concentration_, dst / concentration_);
  }

  int diameter() const override {
    return 3;
  }

  void endpointsConnectedToInjectionSwitch(SwitchId swaddr,
         std::vector<injection_port>& nodes) const override;

  coordinates switchCoords(SwitchId sid) const override {
    coordinates c(2);
    c[0] = computeA(sid);
    c[1] = computeG(sid);
    return c;
  }

  SwitchId switchAddr(const coordinates &coords) const override {
    return getUid(coords[0], coords[1]);
  }

  InterGroupWiring* groupWiring() const {
    return group_wiring_;
  }

  SwitchId randomIntermediate(Router* rtr, SwitchId current, SwitchId dest, uint32_t seed){
    return group_wiring_->randomIntermediate(rtr,current,dest,seed);
  }

 protected:
  int a_;
  int h_;
  int g_;

  double vtk_edge_size_;
  double vtk_radius_;
  double vtk_box_length_;
  double vtk_group_radians_;
  double vtk_switch_radians_;

  InterGroupWiring* group_wiring_;

};

}
} //end of namespace sstmac

#endif
