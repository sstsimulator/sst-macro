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

#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/topology.h>
//#include <sstmac/hardware/topology/multipath_topology.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/fat_tree.h>
//#include <sstmac/hardware/topology/butterfly.h>
#include <sstmac/hardware/topology/fully_connected.h>
#include <sstmac/libraries/nlohmann/json.hpp>
#include <sstream>
#include <fstream>

RegisterKeywords(
{ "fileroot", "the file prefix for reading in tables" },
);

namespace sstmac {
namespace hw {

class TableRouter : public Router {
 public:
  FactoryRegister("table",
              Router, TableRouter,
              "router implementing table-based routing")

  TableRouter(SST::Params& params, Topology* top, NetworkSwitch* sw) :
    Router(params, top, sw),
    table_(top->numNodes(), -1)
  {
    std::string fname = params.find<std::string>("filename");
    std::ifstream in(fname);
    nlohmann::json jsn;
    in >> jsn;

    nlohmann::json routes =
        jsn.at("switches").at( top->switchIdToName(my_addr_) ).at("routes");
    int size = table_.size();
    for (auto it = routes.begin(); it != routes.end(); ++it)
      table_[top->nodeNameToId(it.key())] = it.value();

    for (int i=0; i < table_.size(); ++i){
      if (table_[i] == -1){
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
    int port = table_[pkt->toaddr()];
    pkt->setEdgeOutport(port);
    //for now only valid on topologies with minimal/no vcs
    pkt->setDeadlockVC(0);
    rter_debug("packet to %d sent to port %d", pkt->toaddr(), port);
  }

 private:
  std::vector<int> table_;
};

}
}
