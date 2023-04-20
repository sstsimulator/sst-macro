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

#ifndef pisces_BUFFER_H
#define pisces_BUFFER_H

#include <sstmac/hardware/pisces/pisces_crossbar.h>
#include <sstmac/hardware/pisces/pisces_sender.h>
#include <sstmac/common/timestamp.h>

namespace sstmac {
namespace hw {


class PiscesBuffer :
  public PiscesSender
{
 public:

  ~PiscesBuffer() override;

  void setOutput(int this_outport, int dst_inport, EventLink::ptr&& link, int credits) override;

  void setInput(int this_inport, int src_outport, EventLink::ptr&& link) override;

  void handleCredit(Event* ev) override;

  void handlePayload(Event* ev) override;

  Timestamp sendPayload(PiscesPacket* pkt);

  void collectIdleTicks();

  bool spaceToSend(int vc, int bytes){
    return credits_[vc] >= bytes;
  }

  int numCredit(int vc) const {
    return credits_[vc];
  }

  std::string piscesName() const override {
    return input_.link ? "buffer" : "injection";
  }

  int queueLength(int vc) const;

  PiscesBuffer(SST::Params& params, const std::string& selfname, uint32_t id,
               const std::string& arb, double bw, int packet_size,
               SST::Component* parent, int numVC);

 private:
  Input input_;
  Output output_;
  uint32_t bytes_delayed_;

  int num_vc_;
  std::vector<PayloadQueue> queues_;
  std::vector<int> credits_;
  std::vector<int> initial_credits_;

  PiscesBandwidthArbitrator* arb_;
  std::set<int> deadlocked_channels_;
  std::map<int, std::list<PiscesPacket*> > blocked_messages_;
  int packet_size_;
  Timestamp last_tail_left_;
  Statistic<double>* xmit_wait_;
  Statistic<uint64_t>* xmit_bytes_;

};

}
}


#endif // BUFFER_H
