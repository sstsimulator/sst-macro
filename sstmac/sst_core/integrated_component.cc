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

#include <sst/core/linkMap.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sstmac/sst_core/integrated_core.h>
#include <sstmac/sst_core/connectable_wrapper.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/output.h>

namespace sstmac {

SSTIntegratedComponent::SSTIntegratedComponent(
  SST::Params&  /*params*/,
  uint32_t id) :
  SST::Component(SST::ComponentId_t(id))
{
  sprockit::output::init_out0(&std::cout);
  sprockit::output::init_err0(&std::cerr);
  sprockit::output::init_outn(&std::cout);
  sprockit::output::init_errn(&std::cerr);

  link_map_ = SST::Simulation::getSimulation()->getComponentLinkMap(id);
  TimeDelta::initStamps(100); //100 as per tick
}

void
SSTIntegratedComponent::initLinks(SST::Params&  /*params*/)
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
    EventLink::ptr ev_link{new EventLink(pair.first, TimeDelta(), link)};

    if (port_type == "input"){
      //setup up the link for sending credits back to source
      connectInput(src_outport, dst_inport, std::move(ev_link));
      //I will receive incoming payloads on this link
      configureLink(pair.first, EventScheduler::timeConverter(), payloadHandler(dst_inport));
    } else if (port_type == "output"){
      //setup the link for sending output payloads to destination
      connectOutput(src_outport, dst_inport, std::move(ev_link));
      //I will receive credits back after sending out payloads
      configureLink(pair.first, EventScheduler::timeConverter(), creditHandler(src_outport));
    } else if (port_type == "in-out"){
      //no credits involved here - just setting up output handlers
      connectOutput(src_outport, dst_inport, std::move(ev_link));
      configureLink(pair.first, EventScheduler::timeConverter(), payloadHandler(src_outport));
    } else {
      //other special type of link I don't need to process
    }

  }
}

}
