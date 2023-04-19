/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

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

#ifndef pisces_MEMORY_MODEL_H
#define pisces_MEMORY_MODEL_H

#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/software/libraries/compute/compute_event_fwd.h>
#include <sstmac/hardware/pisces/pisces_arbitrator.h>
#include <sstmac/hardware/pisces/pisces_sender.h>
#include <sprockit/thread_safe_new.h>
#include <sprockit/allocator.h>

namespace sstmac {
namespace hw {

class PiscesMemoryModel : public MemoryModel
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_REGISTER_SUBCOMPONENT_DERIVED(
    PiscesMemoryModel,
    "macro",
    "pisces_memory",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "a memory model using pisces packet flow for contention",
    sstmac::hw::MemoryModel)
#else
  SST_ELI_REGISTER_DERIVED(
    MemoryModel,
    PiscesMemoryModel,
    "macro",
    "pisces",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "a memory model using pisces packet flow for contention")
#endif


  PiscesMemoryModel(uint32_t id, SST::Params& params, Node* parent);

  ~PiscesMemoryModel() override;

  std::string toString() const override {
    return "packet flow memory model";
  }

  void accessFlow(uint64_t bytes, TimeDelta min_byte_delay, Callback* cb) override;

  void accessRequest(int linkId, Request* req) override;

 private:
  void start(int channel, uint64_t size, TimeDelta byte_delay, Callback* cb);
  void channelFree(int channel);
  void dataArrived(int channel, uint32_t bytes);
  Timestamp access(int channel, uint32_t bytes, TimeDelta byte_delay, Callback* cb);
  Timestamp access(int channel, PiscesPacket* pkt, TimeDelta byte_delay, Callback* cb);
  void access(PiscesPacket* pkt, TimeDelta byte_delay, Callback* cb);

 private:
  struct ChannelRequest {
    uint64_t bytes_total;
    uint64_t bytes_arrived;
    TimeDelta byte_delay;
    ExecutionEvent* cb;
    PiscesPacket* pkt;

    ChannelRequest(uint64_t bytes, TimeDelta byt_delay, Callback* c) :
      bytes_total(bytes), bytes_arrived(0), byte_delay(byt_delay), cb(c), pkt(nullptr)
    {
    }

    ChannelRequest(TimeDelta byt_delay, Callback* c, PiscesPacket* p) :
      byte_delay(byt_delay), cb(c), pkt(p)
    {
    }

  };

  std::vector<int> channels_available_;
  std::vector<ChannelRequest> channel_requests_;
  std::list<ChannelRequest, sprockit::threadSafeAllocator<ChannelRequest>> stalled_requests_;

  int nchannels_;
  TimeDelta min_agg_byte_delay_;
  TimeDelta min_flow_byte_delay_;
  TimeDelta latency_;
  PiscesBandwidthArbitrator* arb_;
  int packet_size_;

};

}
} /* namespace sstmac */


#endif // pisces_MEMORY_MODEL_H
