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

#include <sstream>
#include <algorithm>
#include <sstmac/hardware/topology/file.h>
#include <sprockit/sim_parameters.h>

using namespace nlohmann;
using namespace std;

namespace sstmac {
namespace hw {

file::file(sprockit::sim_parameters* params) :
  topology(params)
{
  std::string fname = params->get_param("filename");
  std::ifstream in(fname);
  if( in.fail() )
    spkt_throw_printf(sprockit::input_error,
      "file topology: failed to open file %s", fname.c_str());
  in >> json_;

  num_hops_ = json_.at("avg_num_hops");

  // index the nodes
  nodes_ = json_.at("nodes");
  num_nodes_ = nodes_.size();
  int i=0;
  for (auto it = nodes_.begin(); it != nodes_.end(); ++it, ++i) {
    top_debug("file topology: mapping node %s to %d", it.key().c_str(), i);
    node_name_map_[it.key()] = i;
  }
    

  // index the switches
  switches_ = json_.at("switches");
  num_switches_ = switches_.size();
  i=0;
  for (auto it = switches_.begin(); it != switches_.end(); ++it, ++i)
    switch_name_map_[it.key()] = i;

  // loop through and analyze switch ports
  std::set<int> leafs;
  max_num_ports_ = 0;
  int max_node_ports = 0;
  for (auto it = switches_.begin(); it != switches_.end(); ++it) {

    int sid = switch_name_map_[it.key()];
    int node_ports = 0;
    json outports = it->at("outports");
    for (auto prt = outports.begin(); prt != outports.end(); ++prt) {

      // determine leaf switches and max node ports
      auto nd = node_name_map_.find(prt->at("destination"));
      if( nd != node_name_map_.end() ) {
        leafs.insert(sid);
        ++node_ports;
      }

      // update max switch ports
      if( switch_name_map_.find(prt->at("destination")) != switch_name_map_.end() ) {
        max_num_ports_ = max(max_num_ports_,stoi(prt.key()));
        max_num_ports_ = max(max_num_ports_,int(prt->at("inport")));
      }
    }

    max_node_ports = max(max_node_ports,node_ports);
  }

  num_leaf_switches_ = leafs.size();

  // compute max number of ports (switch ports + node ports)
  // +2 because we start indexing at zero
  max_num_ports_ += max_node_ports + 2;

}

void
file::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  conns.clear();

  // find switch name
  string key;
  for (auto &i : switch_name_map_) {
     if (i.second == src) {
        key = i.first;
        break;
     }
  }

  json outports = switches_.at(key).at("outports");
  for (auto prt = outports.begin(); prt != outports.end(); ++prt) {
    auto it = switch_name_map_.find(prt->at("destination"));
    if( it != switch_name_map_.end()  ) {
      connection c;
      c.src = src;
      c.src_outport = stoi(prt.key());
      int dst_id = it->second;
      c.dst = dst_id;
      c.dst_inport = prt->at("inport");
      conns.push_back(c);
    }
  }
}

void
file::endpoints_connected_to_injection_switch(switch_id swaddr,
                                   std::vector<injection_port>& nodes) const
{
  nodes.clear();

  // find switch name
  string key;
  for (auto &i : switch_name_map_) {
     if (i.second == swaddr) {
        key = i.first;
        break;
     }
  }

  json outports = switches_.at(key).at("outports");
  for (auto prt = outports.begin(); prt != outports.end(); ++prt) {
    auto it = node_name_map_.find(prt->at("destination"));
    if( it != node_name_map_.end() ) {
      injection_port ip;
      ip.nid = it->second;
      ip.ep_port = prt->at("inport");
      ip.switch_port = stoi(prt.key());
      nodes.push_back(ip);
    }
  }
}

void
file::configure_individual_port_params(
    switch_id src, sprockit::sim_parameters *switch_params) const
{
  spkt_abort_printf("configure_individual_port_params() not implemented");
}

}
} //end of namespace sstmac
