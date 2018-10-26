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
#include <sstmac/hardware/network/network_message_fwd.h>


#include <sumi/message_fwd.h>
#include <sumi/collective.h>
#include <sumi/comm_functions.h>
#include <sumi/transport.h>
#include <sumi/collective_message.h>
#include <sumi/collective.h>
#include <sumi/comm_functions.h>
#include <sumi/options.h>
#include <sumi/ping.h>
#include <sumi/communicator_fwd.h>

#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>
#include <sprockit/util.h>

#include <unordered_map>

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

class collective_engine;

class transport : public sstmac::sw::api {
  DeclareFactory(transport)
 public:
  RegisterAPI("sumi_transport", transport)

  transport(sprockit::sim_parameters* params,
            sstmac::sw::software_id sid,
            sstmac::sw::operating_system* os);

  void init();

  void finish();

  ~transport();

  /**
   * @brief smsg_send_response After receiving a short message m, use that message object to return a response
   * This function "reverses" the sender/recever
   * @param m
   * @param loc_buffer
   * @param local_cq
   * @param remote_cq
   * @return
   */
  void smsg_send_response(message* m, uint64_t sz, void* loc_buffer, int local_cq, int remote_cq);

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
  void rdma_get_request_response(message* m, uint64_t sz, void* loc_buffer, void* remote_buffer,
                                 int local_cq, int remote_cq);

  void rdma_get_response(message* m, uint64_t sz, int local_cq, int remote_cq);

  /**
   * @brief rdma_put_response
   * @param m
   * @param loc_buffer
   * @param remote_buffer
   * @param local_cq
   * @param remote_cq
   * @return
   */
  void rdma_put_response(message* m, uint64_t payload_bytes,
                         void* loc_buffer, void* remote_buffer, int local_cq, int remote_cq);

  template <class T, class... Args>
  uint64_t rdma_get(int remote_proc, uint64_t byte_length, void* local_buffer, void* remote_buffer,
                    int local_cq, int remote_cq, message::class_t cls, Args&&... args){
    uint64_t flow_id = allocate_flow_id();
    bool needs_ack = remote_cq != message::no_ack;
    T* t = new T(std::forward<Args>(args)...,
                 rank_, remote_proc, remote_cq, local_cq, cls,
                 flow_id, server_libname(), sid().app_,
                 rank_mapper_->rank_to_node(remote_proc), addr(),
                 byte_length, needs_ack, local_buffer, remote_buffer, message::rdma_get{});
    send(t);
    return flow_id;
  }

  template <class T, class... Args>
  uint64_t rdma_put(int remote_proc, uint64_t byte_length, void* local_buffer, void* remote_buffer,
                    int local_cq, int remote_cq, message::class_t cls, Args&&... args){
    uint64_t flow_id = allocate_flow_id();
    bool needs_ack = local_cq != message::no_ack;
    T* t = new T(std::forward<Args>(args)...,
                 rank_, remote_proc, local_cq, remote_cq, cls,
                 flow_id, server_libname(), sid().app_,
                 rank_mapper_->rank_to_node(remote_proc), addr(),
                 byte_length, needs_ack, local_buffer, remote_buffer, message::rdma_put{});
    send(t);
    return flow_id;
  }

  template <class T, class... Args>
  uint64_t smsg_send(int remote_proc, uint64_t byte_length, void* buffer,
                     int local_cq, int remote_cq, message::class_t cls, Args&&... args){
    uint64_t flow_id = allocate_flow_id();
    bool needs_ack = local_cq != message::no_ack;
    T* t = new T(std::forward<Args>(args)...,
                 rank_, remote_proc, local_cq, remote_cq, cls,
                 flow_id, server_libname(), sid().app_,
                 rank_mapper_->rank_to_node(remote_proc), addr(),
                 byte_length, needs_ack, buffer, message::header{});
    send(t);
    return flow_id;
  }

  void incoming_event(sstmac::event *ev);

  void compute(sstmac::timestamp t);

  double wall_time() const;

  sumi::message* poll_pending_messages(bool blocking, double timeout = -1);

  void incoming_message(message* msg);

  void shutdown_server(int dest_rank, sstmac::node_id dest_node, int dest_app);

  std::string server_libname() const {
    return server_libname_;
  }

  sstmac::event_scheduler* des_scheduler() const;

  void memcopy(uint64_t bytes);

  void pin_rdma(uint64_t bytes);

  void* allocate_public_buffer(uint64_t size) {
    return ::malloc(size);
  }

  void* make_public_buffer(void* buffer, uint64_t size) {
    pin_rdma(size);
    return buffer;
  }

  void unmake_public_buffer(void* buf, uint64_t size) {}

  void free_public_buffer(void* buf, uint64_t size) {
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
  message* poll(bool blocking, int cq_id, double timeout = -1);

  /**
   Check all completion queues if a message has been received.
   Message returned is removed from the internal queue.
   Successive calls to the function do NOT return the same message.
   @param blocking Whether to block until a message is received
   @param timeout  An optional timeout - only valid with blocking
   @return    The next message to be received, null if no messages
  */
  message* poll(bool blocking, double timeout = -1);

  /**
   Block until a message is received.
   Returns immediately if message already waiting.
   Message returned is removed from the internal queue.
   Successive calls to the function do NOT return the same message.
   @param cq_id The specific completion queue to check
   @param timeout   Timeout in seconds
   @return          The next message to be received. Message is NULL on timeout
  */
  message* blocking_poll(int cq_id, double timeout = -1){
    return poll(true, cq_id, timeout);
  }

  int allocate_cq();

  collective_engine* engine() const {
    return engine_;
  }

  int rank() const {
    return rank_;
  }

  int nproc() const {
    return nproc_;
  }

  void make_engine();

 protected:
  transport(sprockit::sim_parameters* params,
           const char* prefix,
           sstmac::sw::software_id sid,
           sstmac::sw::operating_system* os);

  transport(sprockit::sim_parameters* params,
           sstmac::sw::software_id sid,
           sstmac::sw::operating_system* os,
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
  transport(sprockit::sim_parameters* params,
           const std::string& libname,
           sstmac::sw::software_id sid,
           sstmac::sw::operating_system* os,
           const std::string& server_name = std::string("sumi_server"));

 private:    
  transport(sprockit::sim_parameters* params);

  void block(std::list<sstmac::sw::thread*>& threads, double timeout);

  message* find_message();
  
  void validate_api();

  void send(message* m);

  uint64_t allocate_flow_id();

  std::vector<std::list<message*>> completion_queues_;

  std::vector<std::list<sstmac::sw::thread*>> cq_blocked_threads_;

  std::list<sstmac::sw::thread*> blocked_threads_;

  uint32_t component_id_;

  sstmac::timestamp post_rdma_delay_;

  sstmac::timestamp post_header_delay_;

  sstmac::timestamp poll_delay_;

  sstmac::sw::lib_compute_time* user_lib_time_;

  sstmac::stat_spyplot* spy_num_messages_;

  sstmac::stat_spyplot* spy_bytes_;

  sstmac::timestamp rdma_pin_latency_;
  sstmac::timestamp rdma_page_delay_;
  int page_size_;
  bool pin_delay_;

 protected:
  bool inited_;

  bool finalized_;

  int rank_;

  int nproc_;

  std::map<int,std::list<message*>> held_;

  collective_engine* engine_;

  std::string server_libname_;

  sstmac::sw::task_mapping::ptr rank_mapper_;

};

class collective_engine
{
 public:
  collective_engine(sprockit::sim_parameters* params,
                    transport* tport);

  ~collective_engine();

  collective_done_message* incoming(message* m);

  int allocate_global_collective_tag(){
    system_collective_tag_--;
    if (system_collective_tag_ >= 0)
      system_collective_tag_ = -1;
    return system_collective_tag_;
  }

  communicator* global_dom() const {
    return global_domain_;
  }

  /**
   * The cutoff for message size in bytes
   * for switching between an eager protocol and a rendezvous RDMA protocol
   * @return
   */
  uint32_t eager_cutoff() const {
    return eager_cutoff_;
  }

  void notify_collective_done(int rank, collective::type_t ty, int tag);

  bool use_eager_protocol(uint64_t byte_length) const {
    return byte_length < eager_cutoff_;
  }

  void set_eager_cutoff(uint64_t bytes) {
    eager_cutoff_ = bytes;
  }

  bool use_put_protocol() const {
    return use_put_protocol_;
  }

  bool use_get_protocol() const {
    return !use_put_protocol_;
  }

  void set_put_protocol(bool flag) {
    use_put_protocol_ = flag;
  }

  collective_done_message* block_until_next(int cq_id);

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
  collective_done_message* reduce_scatter(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
                                          int cq_id, communicator* comm = nullptr);

  template <typename data_t, template <typename> class Op>
  collective_done_message* reduce_scatter(void* dst, void* src, int nelems, int tag,
                                          int cq_id, communicator* comm = nullptr){
    typedef ReduceOp<Op, data_t> op_class_type;
    return reduce_scatter(dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, cq_id, comm);
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
  collective_done_message* allreduce(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
                                     int cq_id, communicator* comm = nullptr);

  template <typename data_t, template <typename> class Op>
  collective_done_message* allreduce(void* dst, void* src, int nelems, int tag,
                                     int cq_id, communicator* comm = nullptr){
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
  collective_done_message* scan(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
                                int cq_id, communicator* comm = nullptr);

  template <typename data_t, template <typename> class Op>
  collective_done_message* scan(void* dst, void* src, int nelems, int tag, int cq_id, communicator* comm = nullptr){
    typedef ReduceOp<Op, data_t> op_class_type;
    return scan(dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, cq_id, comm);
  }

  collective_done_message* reduce(int root, void* dst, void* src, int nelems, int type_size, int tag,
                                  reduce_fxn fxn, int cq_id, communicator* comm = nullptr);

  template <typename data_t, template <typename> class Op>
  collective_done_message* reduce(int root, void* dst, void* src, int nelems, int tag, int cq_id, communicator* comm = nullptr){
    typedef ReduceOp<Op, data_t> op_class_type;
    return reduce(root, dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, cq_id, comm);
  }

  transport* tport() const {
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
  collective_done_message* allgather(void* dst, void* src, int nelems, int type_size, int tag,
                                     int cq_id, communicator* comm = nullptr);

  collective_done_message* allgatherv(void* dst, void* src, int* recv_counts, int type_size, int tag,
                                      int cq_id, communicator* comm = nullptr);

  collective_done_message* gather(int root, void* dst, void* src, int nelems, int type_size, int tag,
                                  int cq_id, communicator* comm = nullptr);

  collective_done_message* gatherv(int root, void* dst, void* src, int sendcnt, int* recv_counts, int type_size, int tag,
                                   int cq_id, communicator* comm = nullptr);

  collective_done_message* alltoall(void* dst, void* src, int nelems, int type_size, int tag,
                                    int cq_id, communicator* comm = nullptr);
  collective_done_message* alltoallv(void* dst, void* src, int* send_counts, int* recv_counts, int type_size, int tag,
                                     int cq_id, communicator* comm = nullptr);

  collective_done_message* scatter(int root, void* dst, void* src, int nelems, int type_size, int tag,
                                   int cq_id, communicator* comm = nullptr);

  collective_done_message* scatterv(int root, void* dst, void* src, int* send_counts, int recvcnt, int type_size, int tag,
                                    int cq_id, communicator* comm = nullptr);

  void wait_barrier(int tag);

  /**
   * Essentially just executes a zero-byte allgather.
   * @param tag
   * @param fault_aware
   */
  collective_done_message* barrier(int tag, int cq_id, communicator* comm = nullptr);

  collective_done_message* bcast(int root, void* buf, int nelems, int type_size, int tag,
                                 int cq_id, communicator* comm = nullptr);

  void clean_up();

  void deadlock_check();

 private:
  collective_done_message* skip_collective(collective::type_t ty,
                        int cq_id, communicator* comm,
                        void* dst, void *src,
                        int nelems, int type_size,
                        int tag);

  void finish_collective(collective* coll, int rank, collective::type_t ty, int tag);

  collective_done_message* start_collective(collective* coll);

  void validate_collective(collective::type_t ty, int tag);

  collective_done_message* deliver_pending(collective* coll, int tag, collective::type_t ty);

 private:
  transport* tport_;
  static sstmac::sw::ftq_tag sumi_transport_tag;
  static sstmac::sw::ftq_tag poll_delay_tag;

  template <typename Key, typename Value>
  using spkt_enum_map = std::unordered_map<Key, Value, enum_hash>;

  typedef std::unordered_map<int,collective*> tag_to_collective_map;
  typedef spkt_enum_map<collective::type_t, tag_to_collective_map> collective_map;
  collective_map collectives_;

  typedef std::unordered_map<int,std::list<collective_work_message*>> tag_to_pending_map;
  typedef spkt_enum_map<collective::type_t, tag_to_pending_map> pending_map;
  pending_map pending_collective_msgs_;

  std::list<collective*> todel_;

  communicator* global_domain_;

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
