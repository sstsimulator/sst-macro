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

#include <sstmac/hardware/network/network_message_fwd.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/common/timestamp.h>
#include <sumi/message_fwd.h>
#include <sumi/collective.h>
#include <sumi/comm_functions.h>
#include <sumi/collective_message.h>
#include <sumi/collective.h>
#include <sumi/comm_functions.h>
#include <sumi/options.h>
#include <sumi/communicator_fwd.h>
#include <sprockit/debug.h>
#include <sprockit/sim_parameters_fwd.h>

#include <unordered_map>
#include <queue>

DeclareDebugSlot(sumi);

namespace sumi {

struct enum_hash {
  template <typename T>
  inline typename std::enable_if<std::is_enum<T>::value, std::size_t>::type
  operator()(T const value) const {
    return static_cast<std::size_t>(value);
  }
};

/**
 * @brief The Transport class
 * Base class that can be included by an application. std library
 * replacement headers can be activated with this file. No simulator internals
 * are referenced assign from integer typedefs.
 */
class Transport {

 public:
  static Transport* get();

  sstmac::sw::SoftwareId sid() const {
    return sid_;
  }

  sstmac::NodeId addr() const {
    return nid_;
  }

  int rank() const {
    return rank_;
  }

  int nproc() const {
    return nproc_;
  }

  std::string serverLibname() const {
    return server_libname_;
  }

  virtual void init() = 0;

  virtual void finish() = 0;

  virtual ~Transport();

  virtual sstmac::NodeId rankToNode(int rank) const = 0;

  /**
   * @brief smsg_send_response After receiving a short message m, use that message object to return a response
   * This function "reverses" the sender/recever
   * @param m
   * @param loc_buffer
   * @param local_cq
   * @param remote_cq
   * @return
   */
  virtual void smsgSendResponse(Message* m, uint64_t sz, void* loc_buffer, int local_cq, int remote_cq) = 0;

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
  virtual void rdmaGetRequestResponse(Message* m, uint64_t sz, void* loc_buffer, void* remote_buffer,
                                      int local_cq, int remote_cq) = 0;

  virtual void rdmaGetResponse(Message* m, uint64_t sz, int local_cq, int remote_cq) = 0;

  /**
   * @brief rdma_put_response
   * @param m
   * @param loc_buffer
   * @param remote_buffer
   * @param local_cq
   * @param remote_cq
   * @return
   */
  virtual void rdmaPutResponse(Message* m, uint64_t payload_bytes,
                         void* loc_buffer, void* remote_buffer, int local_cq, int remote_cq) = 0;

  template <class T, class... Args>
  T* rdmaGet(int remote_proc, uint64_t byte_length, void* local_buffer, void* remote_buffer,
                    int local_cq, int remote_cq, Message::class_t cls, Args&&... args){
    uint64_t flow_id = allocateFlowId();
    bool needs_ack = remote_cq != Message::no_ack;
    T* t = new T(std::forward<Args>(args)...,
                 rank_, remote_proc, remote_cq, local_cq, cls,
                 flow_id, serverLibname(), sid().app_,
                 rankToNode(remote_proc), addr(),
                 byte_length, needs_ack, local_buffer, remote_buffer, Message::rdma_get{});
    send(t);
    return t;
  }

  template <class T, class... Args>
  T* rdmaPut(int remote_proc, uint64_t byte_length, void* local_buffer, void* remote_buffer,
                    int local_cq, int remote_cq, Message::class_t cls, Args&&... args){
    uint64_t flow_id = allocateFlowId();
    bool needs_ack = local_cq != Message::no_ack;
    T* t = new T(std::forward<Args>(args)...,
                 rank_, remote_proc, local_cq, remote_cq, cls,
                 flow_id, serverLibname(), sid().app_,
                 rankToNode(remote_proc), addr(),
                 byte_length, needs_ack, local_buffer, remote_buffer, Message::rdma_put{});
    send(t);
    return t;
  }

  template <class T, class... Args>
  T* smsgSend(int remote_proc, uint64_t byte_length, void* buffer,
              int local_cq, int remote_cq, Message::class_t cls, Args&&... args){
    uint64_t flow_id = allocateFlowId();
    bool needs_ack = local_cq != Message::no_ack;
    T* t = new T(std::forward<Args>(args)...,
                 rank_, remote_proc, local_cq, remote_cq, cls,
                 flow_id, serverLibname(), sid().app_,
                 rankToNode(remote_proc), addr(),
                 byte_length, needs_ack, buffer, Message::header{});
    send(t);
    return t;
  }

  virtual void memcopy(uint64_t bytes) = 0;

  virtual double wallTime() const = 0;

  virtual sstmac::Timestamp now() const = 0;

  virtual void* allocatePublicBuffer(uint64_t size) = 0;

  virtual void* makePublicBuffer(void* buffer, uint64_t size) = 0;

  virtual void unmakePublicBuffer(void* buf, uint64_t size){}

  virtual void freePublicBuffer(void* buf, uint64_t size) = 0;

  virtual int* nidlist() const = 0;
  
  /**
   Check if a message has been received on a specific completion queue.
   Message returned is removed from the internal queue.
   Successive calls to the function do NOT return the same message.
   @param blocking Whether to block until a message is received
   @param cq_id The specific completion queue to check
   @param timeout  An optional timeout - only valid with blocking
   @return    The next message to be received, null if no messages
  */
  virtual Message* poll(bool blocking, int cq_id, double timeout = -1) = 0;

  /**
   Check all completion queues if a message has been received.
   Message returned is removed from the internal queue.
   Successive calls to the function do NOT return the same message.
   @param blocking Whether to block until a message is received
   @param timeout  An optional timeout - only valid with blocking
   @return    The next message to be received, null if no messages
  */
  virtual Message* poll(bool blocking, double timeout = -1) = 0;

  /**
   Block until a message is received.
   Returns immediately if message already waiting.
   Message returned is removed from the internal queue.
   Successive calls to the function do NOT return the same message.
   @param cq_id The specific completion queue to check
   @param timeout   Timeout in seconds
   @return          The next message to be received. Message is NULL on timeout
  */
  virtual Message* blockingPoll(int cq_id, double timeout = -1) = 0;

  virtual uint64_t allocateFlowId() = 0;

  virtual int allocateCqId() = 0;

  virtual int allocateDefaultCq() = 0;

  CollectiveEngine* engine() const {
    return engine_;
  }

  virtual void logMessageDelay(sstmac::Timestamp wait_start, Message* msg){}

  virtual void startCollectiveMessageLog(){}

 private:      
  virtual void send(Message* m) = 0;

 protected:
  Transport(const std::string& server_name,
            sstmac::sw::SoftwareId sid,
            sstmac::NodeId nid) :
    server_libname_(server_name),
    sid_(sid),
    nid_(nid),
    rank_(sid.task_),
    nproc_(-1),
    engine_(nullptr)
  {
  }

  std::set<int> smp_neighbors_;

  std::string server_libname_;

  sstmac::sw::SoftwareId sid_;

  sstmac::NodeId nid_;

  int rank_;

  int nproc_;

  CollectiveEngine* engine_;
};

class CollectiveEngine
{
 public:
  CollectiveEngine(SST::Params& params,
                    Transport* tport);

  ~CollectiveEngine();

  CollectiveDoneMessage* incoming(Message* m);

  void initSmp(const std::set<int>& neighbors);

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

  std::string alltoall_type_;
  std::string allgather_type_;
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
