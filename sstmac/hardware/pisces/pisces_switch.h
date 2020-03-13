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

#ifndef PACKETFLOW_SWITCH_H
#define PACKETFLOW_SWITCH_H

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/pisces/pisces_buffer.h>
#include <sstmac/hardware/pisces/pisces_crossbar.h>
#include <sstmac/hardware/pisces/pisces_arbitrator.h>

namespace sstmac {
namespace hw {

class PiscesAbstractSwitch :
  public NetworkSwitch
{
 public:
  Router* router() const {
    return router_;
  }

 protected:
  PiscesAbstractSwitch(uint32_t id, SST::Params& params);

  virtual ~PiscesAbstractSwitch();

  std::string arbType_;
  Router* router_;
};

/**
 @class PiscesSwitch
 A switch in the network that arbitrates/routes packets
 to the next link in the network
 */
class PiscesSwitch :
  public PiscesAbstractSwitch
{
 public:
  SST_ELI_REGISTER_DERIVED_COMPONENT(
    NetworkSwitch,
    PiscesSwitch,
    "macro",
    "pisces_switch",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "A network switch implementing the packet flow congestion model",
    COMPONENT_CATEGORY_NETWORK)

  SST_ELI_DOCUMENT_PORTS(SSTMAC_VALID_PORTS)

  SST_ELI_DOCUMENT_STATISTICS(
    {"xmit_bytes", "number of bytes transmitted on a port", "bytes", 1},
    {"recv_bytes", "number of bytes receive on a port", "bytes", 1},
    {"xmit_wait", "stalled cycles with data but no credits", "nanoseconds", 1}
  )

  PiscesSwitch(uint32_t id, SST::Params& params);

  virtual ~PiscesSwitch();

  int queueLength(int port, int vc) const override;

  virtual void connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  virtual void connectInput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  LinkHandler* creditHandler(int port) override;

  LinkHandler* payloadHandler(int port) override;

  void setup() override;

  void init(unsigned int phase) override;

  PiscesCrossbar* xbar() const {
    return xbar_;
  }

  virtual std::string toString() const override;

 private:
  void get_buffer(int outport);

  struct InputPort {
    PiscesSwitch* parent;
    SST::Statistics::Statistic<uint64_t>* recv_bytes_;
    int port;

    void handle(Event* ev);

    std::string toString() const {
      return parent->xbar()->toString();
    }
  };

  std::vector<PiscesBuffer*> out_buffers_;
  std::vector<InputPort> inports_;

  PiscesCrossbar* xbar_;

  int xbar_credits_;
  double link_bw_;
  int link_credits_;
  int mtu_;
};

}
}

#endif // PACKETFLOW_SWITCH_H
