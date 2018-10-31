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
#include <sstmac/hardware/topology/file.h>
#include <sprockit/sim_parameters.h>

using namespace nlohmann;

namespace sstmac {
namespace hw {

file::file(sprockit::sim_parameters* params) :
  topology(params)
{
  std::string fname = params->get_param("filename");
  std::ifstream in(fname);
  in >> json_;
  //std::cout << json_;

  num_nodes_ = json_.at("num_nodes");
  std::cout << "\nnum_nodes: " << num_nodes_ << "\n";
  num_switches_ = json_.at("num_switches");
  std::cout << "\nnum_switches: " << num_switches_ << "\n";
}

void
file::connected_outports(switch_id src, std::vector<connection>& conns) const
{
  conns.clear();

  json links = json_.at("switch_to_switch_links");

  for(auto it = links.begin(); it != links.end(); it++ ) {
      connection c;
      if (src == it->at("switch1")) {
          c.src = src;
          c.src_outport = it->at("switch1_port");
          c.dst = it->at("switch2");
          c.dst_inport = it->at("switch2_port");
          conns.push_back(c);
        }
      else if (src == it->at("switch2")) {
          c.src = src;
          c.src_outport = it->at("switch1_port");
          c.dst = it->at("switch2");
          c.dst_inport = it->at("switch2_port");
          conns.push_back(c);
        }
    }

}

void
file::endpoints_connected_to_injection_switch(switch_id swaddr,
                                   std::vector<injection_port>& nodes) const
{
  nodes.clear();
  json links = json_.at("node_to_switch_links");
  for(auto it = links.begin(); it != links.end(); it++ ) {
      if (swaddr == it->at("switch")) {
          injection_port ip;
          ip.nid = it->at("node");
          ip.ep_port = it->at("node_port");
          ip.switch_port = it->at("switch_port");
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
