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

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FILE_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_FILE_H_INCLUDED

#include <utility>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/libraries/nlohmann/json.hpp>

namespace sstmac {
namespace hw {

/**
 *  @class file
 *  The file topology generates a network by reading from a file.
 */
class FileTopology : public Topology
{
 public:
  SPKT_REGISTER_DERIVED(
    Topology,
    FileTopology,
    "macro",
    "file",
    "topology based on JSON description")

  std::string toString() const override {
    return "file topology";
  }

  virtual ~FileTopology() {}

  FileTopology(SST::Params& params);

  int maxNumPorts() const override {
    return maxNumPorts_;
  }

  SwitchId maxSwitchId() const override {
    return num_switches_ - 1;
  }

  NodeId maxNodeId() const override {
    return num_nodes_ - 1;
  }

  SwitchId endpointToSwitch(NodeId) const override {
    spkt_abort_printf("endpointToSwitch() not implemented");
    return SwitchId(0);
  }

  SwitchId numLeafSwitches() const override {
    return num_leaf_switches_;
  }

  int minimalDistance(SwitchId src, SwitchId dst) const {
    spkt_abort_printf("minimalDistance() not implemented");
    return 0;
  }

  int numHopsToNode(NodeId src, NodeId dst) const override {
    // extremely approximate
    return num_hops_;
  }

  void endpointsConnectedToEjectionSwitch(
      SwitchId swaddr,
      std::vector<InjectionPort>& nodes) const override {
    endpointsConnectedToInjectionSwitch(swaddr,nodes);
  }

  void endpointsConnectedToInjectionSwitch(SwitchId swaddr,
               std::vector<InjectionPort>& nodes) const override;

  void connectedOutports(SwitchId src,
       std::vector<Connection>& conns) const override;

  SwitchId numSwitches() const override {
    return num_switches_;
  }

  NodeId numNodes() const override {
    return num_nodes_;
  }

//  NodeId nodeNameToId(std::string name) const override {
//    auto it = node_name_map_.find(name);
//    if( it == node_name_map_.end())
//      spkt_throw_printf(sprockit::input_error,
//        "file topology: can't find node id for %s", name.c_str());
//    return NodeId(it->second);
//  }

  SwitchId switchNameToId(std::string name) const override {
    auto it = switch_name_map_.find(name);
    if( it == switch_name_map_.end())
      spkt_throw_printf(sprockit::InputError,
        "file topology: can't find switch id for %s", name.c_str());
    return SwitchId(it->second);
  }

//  std::string nodeIdToName(NodeId id) const override {
//    // find switch name
//    std::string key;
//    for (auto &i : node_name_map_) {
//       if (i.second == id) {
//          key = i.first;
//          break;
//       }
//    }
//    return key;
//  }

  std::string switchIdToName(SwitchId id) const override {
    // find switch name
    std::string key;
    for (auto &i : switch_name_map_) {
       if (i.second == id) {
          key = i.first;
          break;
       }
    }
    return key;
  }

 private:
  void initHostnameMap(SST::Params& params) override;

  int num_nodes_;
  int num_switches_;
  int num_leaf_switches_;
  int maxNumPorts_;
  int num_hops_;
  nlohmann::json json_;
  nlohmann::json switches_;
  nlohmann::json nodes_;
  std::map<std::string,int> node_name_map_;
  std::map<std::string,int> switch_name_map_;
};

}
} //end of namespace sstmac

#endif
