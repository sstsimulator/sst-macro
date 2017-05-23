/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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

#include <sst/core/linkMap.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sstmac/sst_core/integrated_core.h>
#include <sstmac/sst_core/connectable_wrapper.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/output.h>

namespace sstmac {

SSTIntegratedComponent::SSTIntegratedComponent(
  sprockit::sim_parameters* params,
  uint64_t id) :
  SST::Component(SST::ComponentId_t(id))
{
  sprockit::output::init_out0(&std::cout);
  sprockit::output::init_err0(&std::cerr);
  sprockit::output::init_outn(&std::cout);
  sprockit::output::init_errn(&std::cerr);

  link_map_ = SST::Simulation::getSimulation()->getComponentLinkMap(id);
}

void
SSTIntegratedComponent::init_links(sprockit::sim_parameters *params)
{
  //loop all the links in our map and determine what we need to do with them
  for (auto& pair : link_map_->getLinkMap()){
    SST::Link* link = pair.second;
    //extract link info from the port name
    std::istringstream istr(pair.first);
    std::string port_type;
    int src_outport, dst_inport;
    istr >> port_type;
    istr >> src_outport;
    istr >> dst_inport;
    sprockit::sim_parameters* port_params = hw::topology::get_port_params(params, src_outport);
    link_wrapper* wrapper = new link_wrapper(link);

    if (port_type == "input"){
      //I will receive incoming payloads on this link
      configureLink(pair.first, payload_handler(dst_inport));
      //setup up the link for sending credits back to source
      connect_input(port_params, src_outport, dst_inport, wrapper);
    } else if (port_type == "output"){
      //I will receive credits back after sending out payloads
      configureLink(pair.first, credit_handler(src_outport));
      //setup the link for sending output payloads to destination
      connect_output(port_params, src_outport, dst_inport, wrapper);
    } else if (port_type == "in-out"){
      //no credits involved here - just setting up output handlers
      connect_output(port_params, src_outport, dst_inport, wrapper);
      configureLink(pair.first, payload_handler(src_outport));
    } else {
      //other special type of link I don't need to process
    }
  }
}

}