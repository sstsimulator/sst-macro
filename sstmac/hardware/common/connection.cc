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

#include <sstmac/hardware/common/connection.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/debug.h>

DeclareDebugSlot(timestamp)

namespace sstmac {
namespace hw {

#if !SSTMAC_INTEGRATED_SST_CORE

static bool checked_prefix_fxn = false;

class TimestampPrefixFxn :
  public sprockit::DebugPrefixFxn
{
 public:
  TimestampPrefixFxn(SST::Params& params, MacroBaseComponent* comp) :
    comp_(comp)
  {
    units_ = params.find<std::string>("timestamp_print_units", "s");
    if (units_ == "ns"){
      mult_ = 1e9;
    } else if (units_ == "us"){
      mult_ = 1e6;
    } else if (units_ == "ms"){
      mult_ = 1e3;
    } else if (units_ == "s"){
      mult_ = 1;
    } else {
      spkt_abort_printf("invalid timestamp units for printing function: %s", units_.c_str());
    }
  }

  std::string str() override {
    double t = comp_->now().sec() * mult_;
    return sprockit::sprintf("T=%14.8f %s:", t, units_.c_str());
  }

 private:
  MacroBaseComponent* comp_;
  std::string units_;
  double mult_;

};
#endif

ConnectableComponent::ConnectableComponent(uint32_t cid, SST::Params& params)
  : Component(cid, params)
{
#if !SSTMAC_INTEGRATED_SST_CORE
  if (!checked_prefix_fxn){
    if (sprockit::Debug::slotActive(sprockit::dbg::timestamp)){
      sprockit::Debug::prefix_fxn = std::unique_ptr<sprockit::DebugPrefixFxn>(new TimestampPrefixFxn(params, this));
    }
    checked_prefix_fxn = true;
  }
#endif
}

void
ConnectableSubcomponent::initInputLink(int src_outport, int dst_inport)
{
#if SSTMAC_INTEGRATED_SST_CORE
  std::string name = sprockit::sprintf("input%d", dst_inport);
  SST::Link* link = configureLink(name, SharedBaseComponent::timeConverter(), payloadHandler(dst_inport));
  if (!link){
    spkt_abort_printf("cannot find input link for port %s on connectable component", name.c_str());
  }
  EventLink::ptr ev_link{new EventLink(name, TimeDelta(), link)};
  connectInput(src_outport, dst_inport, std::move(ev_link));
#endif
}

void
ConnectableSubcomponent::initOutputLink(int src_outport, int dst_inport)
{
#if SSTMAC_INTEGRATED_SST_CORE
  std::string name = sprockit::sprintf("output%d", src_outport);
  SST::Link* link = configureLink(name, SharedBaseComponent::timeConverter(), creditHandler(src_outport));
  if (!link){
    spkt_abort_printf("cannot find output link for port %s on connectable component", name.c_str());
  }
  EventLink::ptr ev_link{new EventLink(name, TimeDelta(), link)};
  connectOutput(src_outport, dst_inport, std::move(ev_link));
#endif
}


void
ConnectableComponent::initInputLink(int src_outport, int dst_inport)
{
#if SSTMAC_INTEGRATED_SST_CORE
  std::string name = sprockit::sprintf("input%d", dst_inport);
  SST::Link* link = configureLink(name, SharedBaseComponent::timeConverter(), payloadHandler(dst_inport));
  if (!link){
    spkt_abort_printf("cannot find input link for port %s on connectable component", name.c_str());
  }
  EventLink::ptr ev_link{new EventLink(name, TimeDelta(), link)};
  connectInput(src_outport, dst_inport, std::move(ev_link));
#endif
}

void
ConnectableComponent::initOutputLink(int src_outport, int dst_inport)
{
#if SSTMAC_INTEGRATED_SST_CORE
  std::string name = sprockit::sprintf("output%d", src_outport);
  SST::Link* link = configureLink(name, SharedBaseComponent::timeConverter(), creditHandler(src_outport));
  if (!link){
    spkt_abort_printf("cannot find output link for port %s on connectable component", name.c_str());
  }
  EventLink::ptr ev_link{new EventLink(name, TimeDelta(), link)};
  connectOutput(src_outport, dst_inport, std::move(ev_link));
#endif
}


}
}
