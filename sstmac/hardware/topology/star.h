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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_STAR_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_STAR_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

/**
 *  @class star
 *  The star topology generates a network which connects
    each node a central hub.
 */
class Star : public StructuredTopology
{
 public:
  SPKT_REGISTER_DERIVED(
    Topology,
    Star,
    "macro",
    "star",
    "stat topology with every switch connected to central router")

  std::string toString() const override {
    return "star topology";
  }

  ~Star() override {}

  Star(SST::Params& params);

  int diameter() const override {
    return 1;
  }

  int maxNumPorts() const override {
    return concentration();
  }

  SwitchId numLeafSwitches() const override {
    return 1;
  }

  int minimalDistance(SwitchId  /*src*/, SwitchId  /*dst*/) const {
    return 1;
  }

  int numHopsToNode(NodeId  /*src*/, NodeId  /*dst*/) const override {
    return 1;
  }

  void endpointsConnectedToInjectionSwitch(SwitchId swaddr,
               std::vector<InjectionPort>& nodes) const override;

  void connectedOutports(SwitchId src,
       std::vector<Connection>& conns) const override;

  SwitchId numSwitches() const override {
    return 1;
  }

  NodeId numNodes() const override {
    return concentration_;
  }

};

}
} //end of namespace sstmac

#endif
