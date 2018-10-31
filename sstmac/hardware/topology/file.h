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

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/libraries/nlohmann/json.hpp>

namespace sstmac {
namespace hw {

/**
 *  @class file
 *  The file topology generates a network by reading from a file.
 */
class file : public topology
{
  FactoryRegister("file", topology, file)
 public:
  std::string to_string() const override {
    return "file topology";
  }

  virtual ~file() {}

  file(sprockit::sim_parameters* params);

  int max_num_ports() const override {
    spkt_abort_printf("max_num_ports() not implemented");
  }

  switch_id max_switch_id() const override {
    return num_switches_ - 1;
  }

  node_id max_node_id() const override {
    return num_nodes_ - 1;
  }

  switch_id endpoint_to_switch(node_id) const override {
    spkt_abort_printf("endpoint_to_switch() not implemented");
  }

  switch_id num_leaf_switches() const override {
    return 0;
  }

  int minimal_distance(switch_id src, switch_id dst) const {
    spkt_abort_printf("minimal_distance() not implemented");
  }

  int num_hops_to_node(node_id src, node_id dst) const override {
    spkt_abort_printf("num_hops_to_node() not implemented");
  }

  void endpoints_connected_to_ejection_switch(
      switch_id swaddr,
      std::vector<injection_port>& nodes) const override {
    endpoints_connected_to_injection_switch(swaddr,nodes);
  }

  void endpoints_connected_to_injection_switch(switch_id swaddr,
               std::vector<injection_port>& nodes) const override;

  bool uniform_network_ports() const override {
    return false;
  }

  bool uniform_switches_non_uniform_network_ports() const override {
    return false;
  }

  bool uniform_switches() const override {
    return false;
  }

  void configure_individual_port_params(switch_id src,
        sprockit::sim_parameters *switch_params) const override;

  void connected_outports(switch_id src,
       std::vector<connection>& conns) const override;

  switch_id num_switches() const override {
    return num_switches_;
  }

  node_id num_nodes() const override {
    return num_nodes_;
  }

private:
  int num_nodes_;
  int num_switches_;
  nlohmann::json json_;

};

}
} //end of namespace sstmac

#endif
