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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_torus_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_torus_H_INCLUDED

#include <sstmac/hardware/topology/cartesian_topology.h>

namespace sstmac {
namespace hw {

/**
 * @class torus
 * Implements a high dimensional torus network.
 */

class Torus : public CartesianTopology
{
  FactoryRegister("torus", Topology, Torus,
              "torus implements a high-dimension torus with an arbitrary number of dimensions")
 public:
  typedef enum {
    same_path,
    wrapped_around,
    new_dimension
  } route_type_t ;

  struct torus_routing_header {
    char crossed_timeline : 1;
  };

  Torus(SST::Params& params);

  typedef enum {
    pos = 0,
    neg = 1
  } direction_t;

  virtual std::string toString() const override {
    return "torus";
  }

  virtual ~Torus() {}

  int diameter() const override {
    return diameter_;
  }

  int maxNumPorts() const override {
    return 2*dimensions_.size() + concentration();
  }

  void endpointsConnectedToInjectionSwitch(SwitchId swaddr,
         std::vector<injection_port>& nodes) const override;

  /// Returns the vector giving each dimension of the torus.
  const std::vector<int>& dimensions() const {
    return dimensions_;
  }

  coordinates neighborAtPort(SwitchId sid, int port);

  SwitchId numSwitches() const override {
    return num_switches_;
  }

  SwitchId numLeafSwitches() const override {
    return numSwitches();
  }

  bool uniformSwitchPorts() const override {
    return false;
  }

  bool uniformSwitches() const override {
    return true;
  }

  void connectedOutports(SwitchId src, std::vector<connection>& conns) const override;

  void configureIndividualPortParams(SwitchId src,
            SST::Params& switch_params) const override;

  int minimalDistance(SwitchId sid, SwitchId dst) const;

  int numHopsToNode(NodeId src, NodeId dst) const override {
    return minimalDistance(src / concentration_, dst / concentration_);
  }

  coordinates switchCoords(SwitchId) const override;

  SwitchId switchAddr(const coordinates &coords) const override;

  vtk_switch_geometry getVtkGeometry(SwitchId sid) const override;

  int convertToPort(int dim, int dir) const {
    return 2*dim + dir;
  }

  int shortestDistance(int dim, int src, int dst) const;

  bool shortestPathPositive(int dim, int src, int dst) const;

 protected: //must be visible to hypercube
  int diameter_;
  SwitchId num_switches_;

};

}
} //end of namespace sstmac

#endif
