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

#ifndef sumi_api_TRANSPORT_H
#define sumi_api_TRANSPORT_H

#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/libraries/service.h>
#include <sstmac/software/api/api.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/progress_queue.h>
#include <sstmac/hardware/network/network_message_fwd.h>

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
#include <sprockit/factories/factory.h>
#include <sprockit/util.h>

#include <unordered_map>
#include <queue>

DeclareDebugSlot(sumi);

#define CHECK_IF_I_AM_DEAD(x) if (is_dead_) x

#define print_size(x) printf("%s %d: sizeof(%s)=%d\n", __FILE__, __LINE__, #x, sizeof(x))

namespace sumi {

struct enum_hash {
  template <typename T>
  inline typename std::enable_if<std::is_enum<T>::value, std::size_t>::type
  operator()(T const value) const {
    return static_cast<std::size_t>(value);
  }
};


class Transport : public sstmac::sw::API {
  DeclareFactory(Transport)
 public:
  RegisterAPI("sumi_transport", Transport)

  using DefaultProgressQueue = sstmac::sw::MultiProgressQueue<Message>;

  Transport(sprockit::sim_parameters::ptr& params,
            sstmac::sw::SoftwareId sid,
            sstmac::sw::OperatingSystem* os);

  void init();

  void finish();

  ~Transport();

  /**
   * @brief smsg_send_response After receiving a short message m, use that message object to return a response
   * This function "reverses" the sender/recever
   * @param m
   * @param loc_buffer
   * @param local_cq
   * @param remote_cq
   * @return
   */
  void smsgSendResponse(Message* m, uint64_t sz, void* loc_buffer, int local_cq, int remote_cq);

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
                                 int local_cq, int remote_cq);

  void rdmaGetResponse(Message* m, uint64_t sz, int local_cq, int remote_cq);

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
                         void* loc_buffer, void* remote_buffer, int local_cq, int remote_cq);

  template <class T, class... Args>
  uint64_t rdmaGet(int remote_proc, uint64_t byte_length, void* local_buffer, void* remote_buffer,
                    int local_cq, int remote_cq, Message::class_t cls, Args&&... args){
    uint64_t flow_id = allocateFlowId();
    bool needs_ack = remote_cq != Message::no_ack;
    T* t = new T(std::forward<Args>(args)...,
                 rank_, remote_proc, remote_cq, local_cq, cls,
                 flow_id, serverLibname(), sid().app_,
                 rank_mapper_->rankToNode(remote_proc), addr(),
                 byte_length, needs_ack, local_buffer, remote_buffer, Message::rdma_get{});
    send(t);
    return flow_id;
  }

  template <class T, class... Args>
  uint64_t rdmaPut(int remote_proc, uint64_t byte_length, void* local_buffer, void* remote_buffer,
                    int local_cq, int remote_cq, Message::class_t cls, Args&&... args){
    uint64_t flow_id = allocateFlowId();
    bool needs_ack = local_cq != Message::no_ack;
    T* t = new T(std::forward<Args>(args)...,
                 rank_, remote_proc, local_cq, remote_cq, cls,
                 flow_id, serverLibname(), sid().app_,
                 rank_mapper_->rankToNode(remote_proc), addr(),
                 byte_length, needs_ack, local_buffer, remote_buffer, Message::rdma_put{});
    send(t);
    return flow_id;
  }

  template <class T, class... Args>
  uint64_t smsgSend(int remote_proc, uint64_t byte_length, void* buffer,
                     int local_cq, int remote_cq, Message::class_t cls, Args&&... args){
    uint64_t flow_id = allocateFlowId();
    bool needs_ack = local_cq != Message::no_ack;
    T* t = new T(std::forward<Args>(args)...,
                 rank_, remote_proc, local_cq, remote_cq, cls,
                 flow_id, serverLibname(), sid().app_,
                 rank_mapper_->rankToNode(remote_proc), addr(),
                 byte_length, needs_ack, buffer, Message::header{});
    send(t);
    return flow_id;
  }

  void incomingEvent(sstmac::Event *ev);

  void compute(sstmac::Timestamp t);

  double wallTime() const;

  sumi::Message* pollPendingMessages(bool blocking, double timeout = -1);

  void incomingMessage(Message* msg);

  void shutdownServer(int dest_rank, sstmac::NodeId dest_node, int dest_app);

  std::string serverLibname() const {
    return server_libname_;
  }

  sstmac::EventScheduler* desScheduler() const;

  void memcopy(uint64_t bytes);

  void pinRdma(uint64_t bytes);

  void* allocatePublicBuffer(uint64_t size) {
    return ::malloc(size);
  }

  void* makePublicBuffer(void* buffer, uint64_t size) {
    pinRdma(size);
    return buffer;
  }

  void unmakePublicBuffer(void* buf, uint64_t size) {}

  void freePublicBuffer(void* buf, uint64_t size) {
    ::free(buf);
  }

  int* nidlist() const;
  
  /**
   Check if a message has been received on a specific completion queue.
   Message returned is removed from the internal queue.
   Successive calls to the function do NOT return the same message.
   @param blocking Whether to block until a message is received
   @param cq_id The specific completion queue to check
   @param timeout  An optional timeout - only valid with blocking
   @return    The next message to be received, null if no messages
  */
  Message* poll(bool blocking, int cq_id, double timeout = -1){
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
  Message* poll(bool blocking, double timeout = -1){
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
  Message* blockingPoll(int cq_id, double timeout = -1){
    return poll(true, cq_id, timeout);
  }

  int allocateCqId(){
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

  int allocateDefaultCq(){
    int id = allocateCqId();
    allocateCq(id, std::bind(&DefaultProgressQueue::incoming,
                          &default_progress_queue_,
                          id, std::placeholders::_1));
    return id;
  }

  void allocateCq(int id, std::function<void(Message*)>&& f);

  CollectiveEngine* engine() const {
    return engine_;
  }

  int rank() const {
    return rank_;
  }

  int nproc() const {
    return nproc_;
  }

  void makeEngine();

 protected:
  Transport(sprockit::sim_parameters::ptr& params,
           const char* prefix,
           sstmac::sw::SoftwareId sid,
           sstmac::sw::OperatingSystem* os);

  Transport(sprockit::sim_parameters::ptr& params,
           sstmac::sw::SoftwareId sid,
           sstmac::sw::OperatingSystem* os,
           const std::string& prefix,
           const std::string& server_name);

  /**
   * @brief sumi_transport Ctor with strict library name. We do not create a server here.
   * Since this has been explicitly named, messages will be directly to a named library.
   * @param params
   * @param libname
   * @param sid
   * @param os
   */
  Transport(sprockit::sim_parameters::ptr& params,
           const std::string& libname,
           sstmac::sw::SoftwareId sid,
           sstmac::sw::OperatingSystem* os,
           const std::string& server_name = std::string("sumi_server"));

 private:    
  Transport(sprockit::sim_parameters::ptr& params);
  
  void validateApi();

  void send(Message* m);

  uint64_t allocateFlowId();

  std::vector<std::function<void(Message*)>> completion_queues_;

  uint32_t componentId_;

  sstmac::Timestamp post_rdma_delay_;

  sstmac::Timestamp post_header_delay_;

  sstmac::Timestamp poll_delay_;

  sstmac::sw::LibComputeTime* user_lib_time_;

  sstmac::StatSpyplot* spy_num_messages_;

  sstmac::StatSpyplot* spy_bytes_;

  sstmac::Timestamp rdma_pin_latency_;
  sstmac::Timestamp rdma_page_delay_;
  int page_size_;
  bool pin_delay_;

 protected:
  bool inited_;

  bool finalized_;

  int rank_;

  int nproc_;

  std::map<int,std::list<Message*>> held_;

  std::queue<int> free_cq_ids_;

  CollectiveEngine* engine_;

  std::string server_libname_;

  sstmac::sw::TaskMapping::ptr rank_mapper_;

  DefaultProgressQueue default_progress_queue_;

  std::function<void(sstmac::hw::NetworkMessage*)> nic_ioctl_;

};

class CollectiveEngine
{
 public:
  CollectiveEngine(sprockit::sim_parameters::ptr& params,
                    Transport* tport);

  ~CollectiveEngine();

  CollectiveDoneMessage* incoming(Message* m);

  int allocateGlobalCollectiveTag(){
    system_collective_tag_--;
    if (system_collective_tag_ >= 0)
      system_collective_tag_ = -1;
    return system_collective_tag_;
  }

  Communicator* globalDom() const {
    return global_domain_;
  }

  /**
   * The cutoff for message size in bytes
   * for switching between an eager protocol and a rendezvous RDMA protocol
   * @return
   */
  uint32_t eagerCutoff() const {
    return eager_cutoff_;
  }

  void notifyCollectiveDone(int rank, Collective::type_t ty, int tag);

  bool useEagerProtocol(uint64_t byte_length) const {
    return byte_length < eager_cutoff_;
  }

  void setEagerCutoff(uint64_t bytes) {
    eager_cutoff_ = bytes;
  }

  bool usePutProtocol() const {
    return use_put_protocol_;
  }

  bool useGetProtocol() const {
    return !use_put_protocol_;
  }

  void setPutProtocol(bool flag) {
    use_put_protocol_ = flag;
  }

  CollectiveDoneMessage* blockUntilNext(int cq_id);

  /**
   * The total size of the input buffer in bytes is nelems*type_size*comm_size
   * @param dst  Buffer for the result. Can be NULL to ignore payloads.
   * @param src  Buffer for the input. Can be NULL to ignore payloads.
   *             Automatically memcpy from src to dst.
   * @param nelems The number of elements in the result buffer at the end
   * @param type_size The size of the input type, i.e. sizeof(int), sizeof(double)
   * @param tag A unique tag identifier for the collective
   * @param fxn The function that will actually perform the reduction
   * @param fault_aware Whether to execute in a fault-aware fashion to detect failures
   * @param context The context (i.e. initial set of failed procs)
   */
  CollectiveDoneMessage* reduceScatter(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
                                          int cq_id, Communicator* comm = nullptr);

  template <typename data_t, template <typename> class Op>
  CollectiveDoneMessage* reduceScatter(void* dst, void* src, int nelems, int tag,
                                          int cq_id, Communicator* comm = nullptr){
    typedef ReduceOp<Op, data_t> op_class_type;
    return reduceScatter(dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, cq_id, comm);
  }

  /**
   * The total size of the input/result buffer in bytes is nelems*type_size
   * @param dst  Buffer for the result. Can be NULL to ignore payloads.
   * @param src  Buffer for the input. Can be NULL to ignore payloads.
   *             Automatically memcpy from src to dst.
   * @param nelems The number of elements in the input and result buffer.
   * @param type_size The size of the input type, i.e. sizeof(int), sizeof(double)
   * @param tag A unique tag identifier for the collective
   * @param fxn The function that will actually perform the reduction
   * @param fault_aware Whether to execute in a fault-aware fashion to detect failures
   * @param context The context (i.e. initial set of failed procs)
   */
  CollectiveDoneMessage* allreduce(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
                                     int cq_id, Communicator* comm = nullptr);

  template <typename data_t, template <typename> class Op>
  CollectiveDoneMessage* allreduce(void* dst, void* src, int nelems, int tag,
                                     int cq_id, Communicator* comm = nullptr){
    typedef ReduceOp<Op, data_t> op_class_type;
    return allreduce(dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, cq_id, comm);
  }

  /**
   * The total size of the input/result buffer in bytes is nelems*type_size
   * @param dst  Buffer for the result. Can be NULL to ignore payloads.
   * @param src  Buffer for the input. Can be NULL to ignore payloads.
   *             Automatically memcpy from src to dst.
   * @param nelems The number of elements in the input and result buffer.
   * @param type_size The size of the input type, i.e. sizeof(int), sizeof(double)
   * @param tag A unique tag identifier for the collective
   * @param fxn The function that will actually perform the reduction
   * @param fault_aware Whether to execute in a fault-aware fashion to detect failures
   * @param context The context (i.e. initial set of failed procs)
   */
  CollectiveDoneMessage* scan(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
                                int cq_id, Communicator* comm = nullptr);

  template <typename data_t, template <typename> class Op>
  CollectiveDoneMessage* scan(void* dst, void* src, int nelems, int tag, int cq_id, Communicator* comm = nullptr){
    typedef ReduceOp<Op, data_t> op_class_type;
    return scan(dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, cq_id, comm);
  }

  CollectiveDoneMessage* reduce(int root, void* dst, void* src, int nelems, int type_size, int tag,
                                  reduce_fxn fxn, int cq_id, Communicator* comm = nullptr);

  template <typename data_t, template <typename> class Op>
  CollectiveDoneMessage* reduce(int root, void* dst, void* src, int nelems, int tag, int cq_id, Communicator* comm = nullptr){
    typedef ReduceOp<Op, data_t> op_class_type;
    return reduce(root, dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, cq_id, comm);
  }

  Transport* tport() const {
    return tport_;
  }

  /**
   * The total size of the input/result buffer in bytes is nelems*type_size
   * @param dst  Buffer for the result. Can be NULL to ignore payloads.
   * @param src  Buffer for the input. Can be NULL to ignore payloads. This need not be public! Automatically memcpy from src to public facing dst.
   * @param nelems The number of elements in the input and result buffer.
   * @param type_size The size of the input type, i.e. sizeof(int), sizeof(double)
   * @param tag A unique tag identifier for the collective
   * @param fault_aware Whether to execute in a fault-aware fashion to detect failures
   * @param context The context (i.e. initial set of failed procs)
   */
  CollectiveDoneMessage* allgather(void* dst, void* src, int nelems, int type_size, int tag,
                                     int cq_id, Communicator* comm = nullptr);

  CollectiveDoneMessage* allgatherv(void* dst, void* src, int* recv_counts, int type_size, int tag,
                                      int cq_id, Communicator* comm = nullptr);

  CollectiveDoneMessage* gather(int root, void* dst, void* src, int nelems, int type_size, int tag,
                                  int cq_id, Communicator* comm = nullptr);

  CollectiveDoneMessage* gatherv(int root, void* dst, void* src, int sendcnt, int* recv_counts, int type_size, int tag,
                                   int cq_id, Communicator* comm = nullptr);

  CollectiveDoneMessage* alltoall(void* dst, void* src, int nelems, int type_size, int tag,
                                    int cq_id, Communicator* comm = nullptr);
  CollectiveDoneMessage* alltoallv(void* dst, void* src, int* send_counts, int* recv_counts, int type_size, int tag,
                                     int cq_id, Communicator* comm = nullptr);

  CollectiveDoneMessage* scatter(int root, void* dst, void* src, int nelems, int type_size, int tag,
                                   int cq_id, Communicator* comm = nullptr);

  CollectiveDoneMessage* scatterv(int root, void* dst, void* src, int* send_counts, int recvcnt, int type_size, int tag,
                                    int cq_id, Communicator* comm = nullptr);

  void waitBarrier(int tag);

  /**
   * Essentially just executes a zero-byte allgather.
   * @param tag
   * @param fault_aware
   */
  CollectiveDoneMessage* barrier(int tag, int cq_id, Communicator* comm = nullptr);

  CollectiveDoneMessage* bcast(int root, void* buf, int nelems, int type_size, int tag,
                                 int cq_id, Communicator* comm = nullptr);

  void cleanUp();

  void deadlockCheck();

 private:
  CollectiveDoneMessage* skipCollective(Collective::type_t ty,
                        int cq_id, Communicator* comm,
                        void* dst, void *src,
                        int nelems, int type_size,
                        int tag);

  void finishCollective(Collective* coll, int rank, Collective::type_t ty, int tag);

  CollectiveDoneMessage* startCollective(Collective* coll);

  void validateCollective(Collective::type_t ty, int tag);

  CollectiveDoneMessage* deliverPending(Collective* coll, int tag, Collective::type_t ty);

 private:
  Transport* tport_;
  static sstmac::sw::FTQTag sumi_transport_tag;
  static sstmac::sw::FTQTag poll_delay_tag;

  template <typename Key, typename Value>
  using spkt_enum_map = std::unordered_map<Key, Value, enum_hash>;

  typedef std::unordered_map<int,Collective*> tag_to_collective_map;
  typedef spkt_enum_map<Collective::type_t, tag_to_collective_map> collective_map;
  collective_map collectives_;

  typedef std::unordered_map<int,std::list<CollectiveWorkMessage*>> tag_to_pending_map;
  typedef spkt_enum_map<Collective::type_t, tag_to_pending_map> pending_map;
  pending_map pending_collective_msgs_;

  std::list<Collective*> todel_;

  Communicator* global_domain_;

  uint32_t eager_cutoff_;

  bool use_put_protocol_;

  int system_collective_tag_;

};



class terminate_exception : public std::exception
{
};

static void* sumi_null_ptr = ((void*)0x123);

static inline bool isNonNull(void* buf){
  return buf && buf != sumi_null_ptr;
}

static inline bool isNull(void* buf){
  return !(sumi::isNonNull(buf));
}


}



#endif // TRANSPORT_H
