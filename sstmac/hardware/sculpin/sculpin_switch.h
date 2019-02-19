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

#ifndef SculpinSwitch_h
#define SculpinSwitch_h

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/sculpin/sculpin.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/common/sstmac_config.h>
#if SSTMAC_VTK_ENABLED
#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/sst_types.h>
#include <sstmac/hardware/vtk/vtk_stats.h>
#else
#include <sstmac/hardware/vtk/vtk_stats.h>
#endif
#endif

namespace sstmac {
namespace hw {

/**
 @class SculpinSwitch
 A switch in the network that arbitrates/routes
 to the next link in the network
 */
class SculpinSwitch :
  public NetworkSwitch
{
  RegisterSSTComponent("sculpin", NetworkSwitch, SculpinSwitch,
         "macro", COMPONENT_CATEGORY_NETWORK,
         "A network switch implementing the sculpin model")

  SST_ELI_DOCUMENT_STATISTICS(
    { "traffic_intensity",    "Count the traffic on a port", "unit of traffic", 1}
  )

 public:
  SculpinSwitch(SST::Params& params, uint32_t id);

  virtual ~SculpinSwitch();

  int queueLength(int port, int vc) const override;

  Router* router() const override {
    return router_;
  }

  void connectOutput(int src_outport, int dst_inport, EventLink* link) override;

  void connectInput(int src_outport, int dst_inport, EventLink* link) override;

  LinkHandler* creditHandler(int port) override;

  LinkHandler* payloadHandler(int port) override;

  void handleCredit(Event* ev);

  void handlePayload(Event* ev);

  std::string toString() const override;

 private:
  struct priority_compare {
    bool operator()(SculpinPacket* l, SculpinPacket* r) const {
      if (l->priority() == r->priority()){
        if (l->arrival() == r->arrival()){
          return l->seqnum() < r->seqnum();
        } else {
          return l->arrival() < r->arrival();
        }
      } else {
        //zero is highest priority
        return l->priority() < r->priority();
      }
    }
  };

  struct Port {
    int id;
    int dst_port;
    GlobalTimestamp next_free;
    Timestamp byte_delay;
    uint32_t seqnum;
    std::set<SculpinPacket*, priority_compare> priority_queue;
    EventLink* link;
    Port() : link(nullptr){}
  };
  std::vector<Port> ports_;

  Router* router_;

#if SSTMAC_VTK_ENABLED
#if SSTMAC_INTEGRATED_SST_CORE
  std::vector<Statistic<traffic_event>* > traffic_intensity;
#else
  stat_vtk* vtk_;
#endif
#endif

  bool congestion_;

  StatHistogram* delay_hist_;

  std::set<NodeId> src_stat_filter_;
  std::set<NodeId> dst_stat_filter_;
  std::set<NodeId> src_stat_highlight_;
  std::set<NodeId> dst_stat_highlight_;

  double highlight_scale_;
  bool vtk_flicker_;

  double link_bw_;

 private:
  void send(Port& p, SculpinPacket* pkt, GlobalTimestamp now);

  void tryToSendPacket(SculpinPacket* pkt);

  void pullNext(int portnum);

  /**
   * @brief do_not_filter_packet
   * @param pkt
   * @return >0 scale factor for packet if allowed, <0 if packet should be filtered
   */
  double doNotFilterPacket(SculpinPacket* pkt);

};

}
}

#endif // PACKETFLOW_SWITCH_H
