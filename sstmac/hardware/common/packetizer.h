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

#ifndef PACKETIZER_H
#define PACKETIZER_H

#include <sprockit/factories/factory.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/hardware/common/packet.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/hardware/common/recv_cq.h>
#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/integrated_component.h>
#include <sst/core/interfaces/simpleNetwork.h>
#endif

DeclareDebugSlot(packetizer)

namespace sstmac {
namespace hw {

class packetizer_callback
{
 public:
  virtual void notify(int vn, message* msg) = 0;

  virtual ~packetizer_callback(){}
};

class packetizer :
  public event_subcomponent
{
  DeclareFactory(packetizer, event_component*)
 public:
  virtual ~packetizer();

  void start(int vn, message* payload);

  void packetArrived(int vn, packet* pkt);

  void sendWhatYouCan(int vn);

  void setArrivalNotify(packetizer_callback* handler){
    notifier_ = handler;
  }

  void deadlock_check();

  void setInjectionAcker(event_handler* handler){
    acker_ = handler;
  }

  int packetSize() const {
    return packet_size_;
  }

  virtual link_handler* new_payload_handler() const = 0;
  virtual link_handler* new_credit_handler() const = 0;

 private:
  virtual void
  inject(int vn, long bytes, long byte_offset, message* payload) = 0;

  virtual bool
  spaceToSend(int vn, int num_bits) = 0;

 private:
  recv_cq completion_queue_;

  struct pending_send{
    message* msg;
    long bytes_left;
    long offset;
  };

  std::map<int, std::list<pending_send> > pending_;

  int packet_size_;

  double inv_bw_;

  packetizer_callback* notifier_;
  event_handler* acker_;

 protected:
  packetizer(sprockit::sim_parameters* params,
             event_scheduler* parent);

  void bytesArrived(int vn, uint64_t unique_id, int bytes, message* parent);

};

}
}

#endif // PACKETIZER_H