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

#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/file.h>
#include <sstmac/libraries/nlohmann/json.hpp>
#include <sstream>
#include <fstream>


namespace sstmac {
namespace hw {

class TableRouter : public Router {
 private:
  struct Port {
    Port(const nlohmann::json& pch) :
      rotater(0)
    {
      for (auto p : pch.at("ports")){
        ports.push_back(p);
      }
    }

    Port(int port) :
      ports(1, port),
      rotater(0)
    {
    }

    Port() : rotater(0), ports(0) {}

    int nextPort(){
      int ret = ports[rotater];
      rotater = (rotater + 1) % ports.size();
      return ret;
    }

    std::vector<int> ports;
    int rotater;
  };

 public:
  SST_ELI_REGISTER_DERIVED(
    Router,
    TableRouter,
    "macro",
    "table",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "router implementing table-based routing")

  TableRouter(SST::Params& params, Topology* top, NetworkSwitch* sw) :
    Router(params, top, sw),
    table_(top->numNodes())
  {
    std::string fname = params.find<std::string>("filename");
    std::ifstream in(fname);
    nlohmann::json jsn;
    in >> jsn;
    FileTopology* file_topo = dynamic_cast<FileTopology*>(top);

    std::string myName = top->switchIdToName(my_addr_);
    nlohmann::json routes =
        jsn.at("switches").at(myName).at("routes");
    nlohmann::json port_channels;
    if (file_topo){
      nlohmann::json switch_ports = file_topo->getSwitchJson(myName);
      auto pch_it = switch_ports.find("port_channels");
      if (pch_it != switch_ports.end()){
        port_channels = *pch_it;
      }
    }

    for (auto it = routes.begin(); it != routes.end(); ++it){
      NodeId dest_nid = top->nodeNameToId(it.key());
      if (it.value().is_number()){
        //this is a single port
        table_[dest_nid] = Port(int(it.value()));
      } else {
        std::string pch_name = it.value();
        table_[dest_nid] = Port(port_channels.at(pch_name));
      }
    }


    int size = table_.size();
    for (int i=0; i < size; ++i){
      if (table_[i].ports.size() == 0){
        spkt_abort_printf("No port specified on switch %d to destination %d",
                          my_addr_, i);
      }
    }

  }

  int numVC() const override {
    return 1;
  }

  std::string toString() const override {
    return "table-based router";
  }

  void route(Packet *pkt) override {
    int port = table_[pkt->toaddr()].nextPort();
    pkt->setEdgeOutport(port);
    //for now only valid on topologies with minimal/no vcs
    pkt->setDeadlockVC(0);
    rter_debug("packet to %d sent to port %d", pkt->toaddr(), port);
  }

 private:
  std::vector<Port> table_;
};

}
}
