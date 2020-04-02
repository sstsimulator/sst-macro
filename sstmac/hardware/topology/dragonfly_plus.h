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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_dragonfly_plus_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_dragonfly_plus_H_INCLUDED

#include <sstmac/hardware/topology/dragonfly.h>

namespace sstmac {
namespace hw {

/**
 * @brief The dragonfly class
 * A canonical dragonfly with notation/structure matching the Dally paper
 * Technology-Driven, Highly-Scalable Dragonfly Topology
 */
class DragonflyPlus : public Dragonfly
{
 public:
  SPKT_REGISTER_DERIVED(
    Topology,
    DragonflyPlus,
    "macro",
    "dragonfly_plus",
    "implements a Dragonfly+ with fat-tree groups")

  DragonflyPlus(SST::Params& params);

  std::string toString() const override {
    return "dragonfly+";
  }

  bool isGlobalPort(int port) const {
    return port >= 2*a_;
  }

  void connectedOutports(SwitchId src, std::vector<Connection>& conns) const override;

  ~DragonflyPlus() override {}

  std::string portTypeName(SwitchId sid, int port) const override;

  VTKSwitchGeometry getVtkGeometry(SwitchId sid) const override;

  int ndimensions() const {
    return 3;
  }

  void endpointsConnectedToInjectionSwitch(SwitchId swaddr,
         std::vector<InjectionPort>& nodes) const override;

  int maxNumPorts() const override {
    return std::max(a_ + h_, a_ + concentration());
  }

  /**
   * @brief get_coords
   * @param sid
   * @param a
   * @param g
   */
  inline void getCoords(SwitchId sid, int& row, int& a, int& g) const {
    row = sid / num_leaf_switches_;
    a = sid % a_;
    g = (sid % num_leaf_switches_) / a_;
  }

  int getUid(int row, int a, int g) const {
    return row*num_leaf_switches_ + g*a_ + a;
  }

  inline int computeRow(SwitchId sid) const {
    return sid / num_leaf_switches_;
  }

  inline int computeA(SwitchId sid) const {
    return sid % a_;
  }

  inline int computeG(SwitchId sid) const {
    return (sid % num_leaf_switches_) / a_;
  }

  SwitchId numSwitches() const override {
    return 2 * a_ * g_;
  }

  SwitchId numLeafSwitches() const override {
    return num_leaf_switches_;
  }

  bool isCurvedVtkLink(SwitchId  /*sid*/, int  /*port*/) const override {
    return false;
  }

  int minimalDistance(SwitchId src, SwitchId dst) const;

  int numHopsToNode(NodeId src, NodeId dst) const override {
    return minimalDistance(src / concentration_, dst/ concentration_);
  }

  int diameter() const override {
    return 5;
  }

  coordinates switchCoords(SwitchId sid) const override {
    coordinates c(3);
    c[0] = computeRow(sid);
    c[1] = computeA(sid);
    c[2] = computeG(sid);
    return c;
  }

  SwitchId switchAddr(const coordinates &coords) const override {
    return getUid(coords[0], coords[1], coords[2]);
  }

 private:
  int num_leaf_switches_;
  double vtk_row_spacing_;
};

}
} //end of namespace sstmac

#endif
