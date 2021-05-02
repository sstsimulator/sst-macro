/**
Copyright 2009-2021 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2021, NTESS

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

#ifndef SnapprSwitch_h
#define SnapprSwitch_h

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/snappr/snappr.h>
#include <sstmac/hardware/snappr/snappr_inport.h>
#include <sstmac/hardware/snappr/snappr_outport.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/stats/ftq_fwd.h>
#include <queue>

namespace sstmac {
namespace hw {

/**
 @class SnapprSwitch
 A switch in the network that arbitrates/routes
 to the next link in the network
 */
class SnapprSwitch :
  public NetworkSwitch
{

 public:
  SST_ELI_REGISTER_DERIVED_COMPONENT(
    NetworkSwitch,
    SnapprSwitch ,
    "macro",
    "snappr_switch",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "A network switch implementing the snappr model",
    COMPONENT_CATEGORY_NETWORK)

  SST_ELI_DOCUMENT_PORTS(SSTMAC_VALID_PORTS)

  SST_ELI_DOCUMENT_SUBCOMPONENT_SLOTS(
      {"outport%(num_ports)d", "The output ports for the Switch", "sstmac::SnapprOutport"},
  )

  SST_ELI_DOCUMENT_STATISTICS(
    {"traffic_intensity",    "Count the traffic on a port", "unit of traffic", 1},
    {"xmit_stall", "congestion statistic", "cycles", 1}, // Name, Desc, Units, Enable Level
    {"xmit_active", "activity statistic", "cycles", 1}, // Name, Desc, Units, Enable Level
    {"xmit_idle", "idle statistic", "cycles", 1}, // Name, Desc, Units, Enable Level
    {"bytes_sent", "data sent on port", "bytes", 1}
  )

  SnapprSwitch(uint32_t id, SST::Params& params);

  ~SnapprSwitch();

  int queueLength(int port, int vc) const override;

  void connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  void connectInput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  LinkHandler* creditHandler(int port) override;

  LinkHandler* payloadHandler(int port) override;

  std::string toString() const override;

  void deadlockCheck() override;

 private:
  friend struct SnapprInPort;

  void handlePayload(SnapprPacket* ev, int port);

  std::vector<SnapprOutPort*> outports_;

  std::vector<SnapprInPort> inports_;

  void deadlockCheck(int vl);

  std::vector<Router*> routers_;

  int qos_levels_;

  int num_vc_;
  int num_vl_;


};

}
}

#endif
