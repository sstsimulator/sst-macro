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

#ifndef SNAPPR_MEM_H
#define SNAPPR_MEM_H

#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/software/libraries/compute/compute_event_fwd.h>
#include <sstmac/hardware/snappr/snappr.h>
#include <sprockit/thread_safe_new.h>
#include <sprockit/allocator.h>

namespace sstmac {
namespace hw {

class SnapprMemoryModel : public MemoryModel
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_REGISTER_SUBCOMPONENT_DERIVED(
    SnapprMemoryModel,
    "macro",
    "snappr_memory",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "a memory model using pisces packet flow for contention",
    sstmac::hw::MemoryModel)
#else
  SST_ELI_REGISTER_DERIVED(
    MemoryModel,
    SnapprMemoryModel,
    "macro",
    "snappr",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "a memory model using Snappr model for contention")
#endif


  SnapprMemoryModel(uint32_t id, SST::Params& params, Node* parent);

  ~SnapprMemoryModel() override{}

  std::string toString() const override {
    return "packet flow memory model";
  }

  void accessFlow(uint64_t bytes, TimeDelta min_byte_delay, Callback* cb) override;

  void accessRequest(int linkId, Request* req) override;

  void flowRequestResponse(Request* req);

 private:
  struct FlowRequest : public Request {
    uint32_t flowId;
  };

  struct Flow {
    ExecutionEvent* callback;
    uint64_t bytesLeft;
    TimeDelta byteRequestDelay;
  };

  struct ChannelQueue {
    TimeDelta byte_delay;
    std::vector<Request*> reqs;
    Timestamp next_free;
  };

  TimeDelta channel_byte_delay_;
  std::unordered_map<uint32_t, Flow> flows_;
  std::vector<ChannelQueue> channels_;
  uint32_t flowId_;
  uint32_t channelInterleaver_;
  uint32_t mtu_;
  uint32_t flow_mtu_;
  int flow_rsp_id_;


};

}
} /* namespace sstmac */


#endif // SNAPPR_MEM_H
