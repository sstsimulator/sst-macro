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
#include <sstmac/hardware/topology/multipath_topology.h>
#include <sprockit/util.h>
#include <sprockit/delete.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/hardware/topology/butterfly.h>
#include <sstmac/hardware/topology/fully_connected.h>
#include <sstream>
#include <fstream>


RegisterKeywords(
{ "fileroot", "the file prefix for reading in tables" },
);

namespace sstmac {
namespace hw {

class table_router : public router {
 public:
  FactoryRegister("table",
              router, table_router,
              "router implementing table-based routing")

  table_router(sprockit::sim_parameters* params, topology* top, network_switch* sw) :
    router(params, top, sw),
    table_(top->num_nodes(), -1)
  {
    std::string fileprefix = params->get_param("fileroot");
    std::stringstream sstr;
    sstr << fileprefix << "." << my_addr_;
    std::string fname = sstr.str();
    std::ifstream ifs(fname);
    if (!ifs.good()){
      spkt_abort_printf("Table for router %d could not be found at %s",
                        my_addr_, fname.c_str())
    }

    std::string line;
    while (ifs.good()){
      std::getline(ifs, line);
      std::istringstream istr(line);
      int sid, port;
      istr >> sid;
      istr >> port;
      table_[sid] = port;
    }

    for (int i=0; i < table_.size(); ++i){
      if (table_[i] == -1){
        spkt_abort_printf("No port specified on switch %d to destination %d",
                          my_addr_, i);
      }
    }

  }

  int num_vc() const override {
    return 1;
  }

  std::string to_string() const override {
    return "table-based router";
  }

  void route(packet *pkt) override {
    uint16_t dir;
    switch_id ej_addr = top_->node_to_ejection_switch(pkt->toaddr(), dir);
    if (ej_addr == my_addr_){
      pkt->current_path().outport() = dir;
      pkt->current_path().vc = 0;
      return;
    }

    int port = table_[pkt->toaddr()];
    pkt->current_path().set_outport(port);
    //for now only valid on topologies with minimal/no vcs
    pkt->current_path().vc = 0;
    rter_debug("packet to %d sent to port %d", pkt->toaddr(), port);
  }

 private:
  std::vector<int> table_;
};

}
}
