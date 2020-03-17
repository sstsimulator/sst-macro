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

#ifndef SSTMAC_HARDWARE_NETWORK_SWITCHES_REGULAR_TOPOLOGY_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_SWITCHES_REGULAR_TOPOLOGY_H_INCLUDED

#include <sstmac/hardware/topology/topology.h>

namespace sstmac {
namespace hw {

/**
Encapsulates a topology like torus, fat tree, butterfly which
has a regular, well-defined structure.  Routing on these topologies
is regular enough to be done with simple math computations without
resorting to a large routing table.  This contrasts with unstructured
topologies like #top_from_file where there is no regular structure
and routing must be done via lookup table.

The major defining characteristic of a structured topology is
being able to define a coordinate system.  Each switch in the
topology has a unique number and that number can be mapped
to a unique set of coordinates.  A torus has an obvious
mapping of index to X,Y,Z coordinates.  A butterfly or fat tree
also has a well-defined coordinate system, but is slightly less intuitive.
@class structured_topology
*/
class StructuredTopology : public Topology
{
 public:
  ~StructuredTopology() override {}

  /**** BEGIN PURE VIRTUAL INTERFACE *****/
  /**
     Structured topologies can be direct (torus) or indirect (fat tree).
     We therefore need to distinguish the total number of switches and
     the number of leaf switches - i.e. those directly connected to nodes.
     For direct topologies, num_switches and num_leaf_switches are the same.
     For indirect, num_leaf_switches < num_switches.
     @return The number of leaf switches directly connected to compute nodes
  */
  SwitchId numLeafSwitches() const override = 0;

  void endpointsConnectedToEjectionSwitch(SwitchId swid,
                       std::vector<InjectionPort>& nodes) const override;

  int concentration() const {
    return concentration_;
  }

  NodeId numNodes() const override {
    return concentration_ * numLeafSwitches();
  }

  SwitchId endpointToSwitch(NodeId nid) const override {
    return nid / concentration_;
  }

  SwitchId maxSwitchId() const override {
    return numSwitches();
  }

  NodeId maxNodeId() const override {
    return numNodes();
  }

  virtual int diameter() const = 0;
  /**** END PURE VIRTUAL INTERFACE *****/

 protected:
  StructuredTopology(SST::Params& params);

 protected:
  int concentration_;

  int injection_redundancy_;
};

}
}

#endif
