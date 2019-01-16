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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FULLY_CONNECTED_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FULLY_CONNECTED_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

/**
 *  @class fully_connected
 *  The fully_connected network generates a network which connects
    each node to every other node.
 */
class FullyConnected : public StructuredTopology
{
  FactoryRegister("fully_connected | full", Topology, FullyConnected)
 public:
  std::string toString() const override {
    return "fully_connected topology";
  }

  virtual ~FullyConnected() {}

  FullyConnected(SST::Params& params);

  int diameter() const override {
    return 1;
  }

  int maxNumPorts() const override {
    return size_ + concentration();
  }

  SwitchId numLeafSwitches() const override {
    return size_;
  }

  int minimalDistance(SwitchId src, SwitchId dst) const {
    return 1;
  }

  int numHopsToNode(NodeId src, NodeId dst) const override {
    return 1;
  }

  void endpointsConnectedToInjectionSwitch(SwitchId swid,
                          std::vector<injection_port>& nodes) const override;

  bool uniformSwitchPorts() const override {
    return true;
  }

  bool uniformSwitches() const override {
    return true;
  }

  void configureIndividualPortParams(SwitchId src,
        SST::Params& switch_params) const override;

  void connectedOutports(SwitchId src,
       std::vector<connection>& conns) const override;

  SwitchId numSwitches() const override {
    return size_;
  }

 private:
  SwitchId size_;

};

}
} //end of namespace sstmac

#endif
