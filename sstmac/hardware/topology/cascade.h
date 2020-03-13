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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_CASCADE_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_CASCADE_H_INCLUDED

#include <sstmac/hardware/topology/cartesian_topology.h>

namespace sstmac {
namespace hw {

/**
 * @brief The cascade class
 * Variant of dragonfly found in Cray Cascade (Aries) systems
 */
class Cascade : public CartesianTopology
{
 public:
  SPKT_REGISTER_DERIVED(
    Topology,
    Cascade,
    "macro",
    "cascade",
    "implements a cascade topology - dragonfly with 2D groups")

  Cascade(SST::Params& params);

  typedef enum {
    x_dimension = 0,
    y_dimension = 1,
    g_dimension = 2
  } dimension_t;

  typedef enum {
    upX_vc = 0,
    downX_vc = 1
  } x_vc_t;

  typedef enum {
    upY_vc = 0,
    downY_vc = 1
  } y_vc_t;

 public:
  std::string toString() const override {
    return "cascade";
  }

  void endpointsConnectedToInjectionSwitch(SwitchId swaddr,
          std::vector<InjectionPort>& nodes) const override;

  dimension_t dimForPort(int port){
    if (port >= (x_ + y_)){
      return g_dimension;
    } else if (port >= x_){
      return y_dimension;
    } else {
      return x_dimension;
    }
  }

  int maxNumPorts() const override {
    return x_ + y_ + g_ + concentration();
  }

  bool isGlobalPort(uint16_t port) const {
     return port >= (x_ + y_);
  }

  void connectedOutports(SwitchId src, std::vector<Connection>& conns) const override;

  virtual ~Cascade() {}

  int ndimensions() const {
    return 3;
  }

  int numX() const {
    return x_;
  }

  int numY() const {
    return y_;
  }

  int numG() const {
    return g_;
  }

  int group_con() const {
    return group_con_;
  }

  inline void get_coords(SwitchId sid, int& x, int& y, int& g) const {
    x = computeX(sid);
    y = computeY(sid);
    g = computeG(sid);
  }

  int get_uid(int x, int y, int g) const {
    return g * (x_ * y_) + y * (x_) + x;
  }

  inline int x_port(int x) const {
    return x;
  }

  inline int y_port(int y) const {
    return x_ + y;
  }

  inline int g_port(int g) const {
    return x_ + y_ + g;
  }

  inline int computeX(SwitchId sid) const {
    return sid % x_;
  }

  inline int computeY(SwitchId sid) const {
    return (sid / x_) % y_;
  }

  inline int computeG(SwitchId sid) const {
    return (sid / (x_*y_));
  }

  SwitchId numSwitches() const override {
    return x_ * y_ * g_;
  }

  SwitchId numLeafSwitches() const override {
    return x_ * y_ * g_;
  }

  double portScaleFactor(uint32_t addr, int port) const override;

  void minimalRouteToSwitch(
      Router* rtr,
      SwitchId current_sw_addr,
      SwitchId dest_sw_addr,
      Packet::Header* hdr) const;

  int minimalDistance(SwitchId src, SwitchId dst) const;

  int numHopsToNode(NodeId src, NodeId dst) const override {
    return minimalDistance(src / concentration_, dst / concentration_);
  }

  int diameter() const override {
    return 5;
  }

  coordinates switchCoords(SwitchId) const override;

  SwitchId switchAddr(const coordinates &coords) const override;

 protected:
  void find_path_to_group(Router* rtr, int myX, int myY, int myG, int dstG,
                     int& dstX, int& dstY, Packet::Header* hdr) const;

  bool find_y_path_to_group(Router* rtr, int myX, int myG, int dstG, int& dstY,
                       Packet::Header* hdr) const;

  bool find_x_path_to_group(Router* rtr, int myY, int myG, int dstG, int& dstX,
                       Packet::Header* hdr) const;

  bool xy_connected_to_group(int myX, int myY, int myG, int dstG) const;

 protected:
  int x_;
  int y_;
  int g_;
  int group_con_;

  static std::string set_string(int x, int y, int g)
  {
    return sprockit::sprintf("{ %d %d %d }", x, y, g);
  }

 private:
  int convertToPort(int dim, int dir) const {
    if      (dim == x_dimension) return x_port(dir);
    else if (dim == y_dimension) return y_port(dir);
    else if (dim == g_dimension) return g_port(dir);
    else return -1;
  }

  int xygDirToGroup(int myX, int myY, int myG, int dir) const;

};

}
} //end of namespace sstmac

#endif
