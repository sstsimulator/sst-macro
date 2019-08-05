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

#ifndef sumi_api_sim_TRANSPORT_H
#define sumi_api_sim_TRANSPORT_H

#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/common/event_scheduler_fwd.h>
#include <sstmac/software/api/api.h>
#include <sstmac/software/launch/task_mapping.h>
#include <sstmac/software/libraries/service.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/progress_queue.h>
#include <sstmac/hardware/network/network_message_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>

#include <sumi/message_fwd.h>
#include <sumi/collective.h>
#include <sumi/comm_functions.h>
#include <sumi/transport.h>
#include <sumi/collective_message.h>
#include <sumi/collective.h>
#include <sumi/comm_functions.h>
#include <sumi/options.h>
#include <sumi/communicator_fwd.h>

#include <sprockit/debug.h>
#include <sprockit/factory.h>
#include <sprockit/util.h>

#include <unordered_map>
#include <queue>

namespace sumi {

class QoSAnalysis {

 public:
  SST_ELI_DECLARE_BASE(QoSAnalysis)
  SST_ELI_DECLARE_DEFAULT_INFO()
  SST_ELI_DECLARE_CTOR(SST::Params&)

  QoSAnalysis(SST::Params& params){}

  virtual ~QoSAnalysis(){}

  virtual int selectQoS(Message* m) = 0;

  virtual void logDelay(sstmac::TimeDelta delay, Message* m) = 0;

};

class SimTransport : public Transport, public sstmac::sw::API {

 public:
  SST_ELI_REGISTER_DERIVED(
    API,
    SimTransport,
    "macro",
    "sumi",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "provides the SUMI transport API")

  using DefaultProgressQueue = sstmac::sw::MultiProgressQueue<Message>;

  SimTransport(SST::Params& params, sstmac::sw::App* parent, SST::Component* comp);

  sstmac::sw::SoftwareId sid() const {
    return Transport::sid();
  }

  void init() override;

  void finish() override;

  virtual ~SimTransport();

  sstmac::NodeId rankToNode(int rank) const override {
    return rank_mapper_->rankToNode(rank);
  }

  /**
   * @brief smsg_send_response After receiving a short message m, use that message object to return a response
   * This function "reverses" the sender/recever
   * @param m
   * @param loc_buffer
   * @param local_cq
   * @param remote_cq
   * @return
   */
  void smsgSendResponse(Message* m, uint64_t sz, void* loc_buffer, int local_cq, int remote_cq) override;

  /**
   * @brief rdma_get_response After receiving a short message payload coordinating an RDMA get,
   * use that message object to send an rdma get
   * @param m
   * @param loc_buffer
   * @param remote_buffer
   * @param local_cq
   * @param remote_cq
   * @return
   */
  void rdmaGetRequestResponse(Message* m, uint64_t sz, void* loc_buffer, void* remote_buffer,
                                 int local_cq, int remote_cq) override;

  void rdmaGetResponse(Message* m, uint64_t sz, int local_cq, int remote_cq) override;

  /**
   * @brief rdma_put_response
   * @param m
   * @param loc_buffer
   * @param remote_buffer
   * @param local_cq
   * @param remote_cq
   * @return
   */
  void rdmaPutResponse(Message* m, uint64_t payload_bytes,
             void* loc_buffer, void* remote_buffer, int local_cq, int remote_cq) override;

  double wallTime() const override {
    return now().sec();
  }

  sstmac::Timestamp now() const override;

  void* allocatePublicBuffer(uint64_t size) override {
    return ::malloc(size);
  }

  void* makePublicBuffer(void* buffer, uint64_t size) override {
    pinRdma(size);
    return buffer;
  }

  void freePublicBuffer(void* buf, uint64_t size) override {
    ::free(buf);
  }

  void memcopy(uint64_t bytes) override;

  int* nidlist() const override;

  void incomingEvent(sstmac::Event *ev);

  void compute(sstmac::TimeDelta t);

  void incomingMessage(Message* msg);

  void shutdownServer(int dest_rank, sstmac::NodeId dest_node, int dest_app);

  void pinRdma(uint64_t bytes);
  
  /**
   Check if a message has been received on a specific completion queue.
   Message returned is removed from the internal queue.
   Successive calls to the function do NOT return the same message.
   @param blocking Whether to block until a message is received
   @param cq_id The specific completion queue to check
   @param timeout  An optional timeout - only valid with blocking
   @return    The next message to be received, null if no messages
  */
  Message* poll(bool blocking, int cq_id, double timeout = -1) override {
    return default_progress_queue_.find(cq_id, blocking, timeout);
  }

  /**
   Check all completion queues if a message has been received.
   Message returned is removed from the internal queue.
   Successive calls to the function do NOT return the same message.
   @param blocking Whether to block until a message is received
   @param timeout  An optional timeout - only valid with blocking
   @return    The next message to be received, null if no messages
  */
  Message* poll(bool blocking, double timeout = -1) override {
    return default_progress_queue_.find_any(blocking, timeout);
  }

  /**
   Block until a message is received.
   Returns immediately if message already waiting.
   Message returned is removed from the internal queue.
   Successive calls to the function do NOT return the same message.
   @param cq_id The specific completion queue to check
   @param timeout   Timeout in seconds
   @return          The next message to be received. Message is NULL on timeout
  */
  Message* blockingPoll(int cq_id, double timeout = -1) override {
    return poll(true, cq_id, timeout);
  }

  int allocateCqId() override {
    if (free_cq_ids_.empty()){
      int id = completion_queues_.size();
      completion_queues_.emplace_back();
      return id;
    } else {
      int id = free_cq_ids_.front();
      free_cq_ids_.pop();
      return id;
    }
  }

  int allocateDefaultCq() override {
    int id = allocateCqId();
    allocateCq(id, std::bind(&DefaultProgressQueue::incoming,
                          &default_progress_queue_,
                          id, std::placeholders::_1));
    return id;
  }

  void allocateCq(int id, std::function<void(Message*)>&& f);

 private:      
  void send(Message* m) override;

  uint64_t allocateFlowId() override;

  std::vector<std::function<void(Message*)>> completion_queues_;

  std::function<void(Message*)> null_completion_notify_;

  sstmac::TimeDelta post_rdma_delay_;

  sstmac::TimeDelta post_header_delay_;

  sstmac::TimeDelta poll_delay_;

  sstmac::StatSpyplot<int,int,uint64_t>* spy_bytes_;

  sstmac::TimeDelta rdma_pin_latency_;
  sstmac::TimeDelta rdma_page_delay_;
  int page_size_;
  bool pin_delay_;

 protected:
  void registerNullHandler(std::function<void(Message*)> f){
    null_completion_notify_ = f;
  }

  bool smp_optimize_;

  std::map<int,std::list<Message*>> held_;

  std::queue<int> free_cq_ids_;

  sstmac::sw::App* parent_app_;

  sstmac::sw::TaskMapping::ptr rank_mapper_;

  DefaultProgressQueue default_progress_queue_;

  std::function<void(sstmac::hw::NetworkMessage*)> nic_ioctl_;

  QoSAnalysis* qos_analysis_;

 private:
  void drop(Message* m){}
};


class TerminateException : public std::exception
{
};

}



#endif // TRANSPORT_H
