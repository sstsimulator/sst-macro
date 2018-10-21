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
#include <sstmac/software/launch/job_launcher_fwd.h>
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

  void send(uint64_t byte_length,
    int dest_rank,
    sstmac::node_id dest_node,
    int dest_app,
    sumi::message* msg,
    int ty);

  void process(transport_message* msg);

  int pt2pt_cq_id() const {
    return pt2pt_cq_id_;
  }

  int collective_cq_id() const {
    return collective_cq_id_;
  }

  void incoming_event(sstmac::event *ev);

  void compute(sstmac::timestamp t);

  void client_server_send(
    int dest_rank,
    sstmac::node_id dest_node,
    int dest_app,
    sumi::message* msg);

  void client_server_rdma_put(
    int dest_rank,
    sstmac::node_id dest_node,
    int dest_app,
    sumi::message* msg);

  /**
   * Block on a collective of a particular type and tag
   * until that collective is complete
   * @param ty
   * @param tag
   * @return
   */
  sumi::collective_done_message* collective_block(
      sumi::collective::type_t ty, int tag, int cq_id = 0);

  double wall_time() const;

  sumi::transport_message* poll_pending_messages(bool blocking, double timeout = -1);

  /**
   * @brief send Intra-app. Send within the same process launch (i.e. intra-comm MPI_COMM_WORLD). This contrasts
   *  with client_server_send which exchanges messages between different apps
   * @param byte_length
   * @param msg
   * @param ty
   * @param dst
   * @param needs_ack
   */
  void send(uint64_t byte_length, sumi::message* msg, int ty, int dst);

  void incoming_message(transport_message* msg);

  void shutdown_server(int dest_rank, sstmac::node_id dest_node, int dest_app);

  std::string server_libname() const {
    return server_libname_;
  }

  sstmac::event_scheduler* des_scheduler() const;

  void memcopy(uint64_t bytes);

  void pin_rdma(uint64_t bytes);

  void smsg_send(int dst, sumi::message* msg);

  void rdma_put(int dst, sumi::message* msg);

  void rdma_get(int src, sumi::message* msg);

  void* make_public_buffer(void* buffer, uint64_t size) {
    pin_rdma(size);
    return buffer;
  }

  void unmake_public_buffer(void* buf, uint64_t size) {}

  void free_public_buffer(void* buf, uint64_t size) {
    ::free(buf);
  }

  int* nidlist() const;

  void deadlock_check();

  int allocate_cq();

  /**
   Send a message directly to a destination node.
   This assumes there is buffer space available at the destination to eagerly receive.
   * @param dst         The destination of the message
   * @param ev          An enum for the type of payload carried
   * @param needs_ack   Whether an ack should be generated notifying that the message was fully injected
   * @param msg         The message to send (full message at the MTL layer)
   */
  void smsg_send(int dst,
     message::payload_type_t ev,
     message* msg,
     int send_cq, int recv_cq);

  /**
   Helper function for #smsg_send. Directly send a message header.
   * @param dst
   * @param msg
   */
  void send_header(int dst, message* msg, int send_cq, int recv_cq);

  /**
   Helper function for #smsg_send. Directly send an actual data payload.
   * @param dst
   * @param msg
   */
  void send_payload(int dst, message* msg, int send_cq, int recv_cq);

  /**
   Put a message directly to the destination node.
   This assumes the application has properly configured local/remote buffers for the transfer.
   * @param dst      Where the data is being put (destination)
   * @param msg      The message to send (full message at the MTL layer).
   *                 Should have local/remote buffers configured for RDMA.
   * @param needs_send_ack  Whether an ack should be generated send-side when the message is fully injected
   * @param needs_recv_ack  Whether an ack should be generated recv-side when the message is fully received
   */
  void rdma_put(int dst, message* msg, int send_cq, int recv_cq);

  /**
   Get a message directly from the source node.
   This assumes the application has properly configured local/remote buffers for the transfer.
   * @param src      Where the data is being put (destination)
   * @param msg      The message to send (full message at the MTL layer).
   *                 Should have local/remote buffers configured for RDMA.
   * @param send_cq  Where an ack should be generated send-side (source) when the message is fully injected
   * @param recv_cq  Where an ack should be generated recv-side when the message is fully received
   */
  void rdma_get(int src, message* msg, int send_cq, int recv_cq);
  
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
  message* blocking_poll(int cq_id, double timeout = -1);

  template <class T> T*
  poll(const char* file, int line, const char* cls) {
    message* msg = poll(true);
    T* result = dynamic_cast<T*>(msg);
    if (!result){
      poll_cast_error(file, line, cls, msg);
    }
    return result;
  }

  template <class T> T*
  poll(double timeout, const char* file, int line, const char* cls) {
    message* msg = poll(true, timeout);
    T* result = dynamic_cast<T*>(msg);
    if (msg && !result){
      poll_cast_error(file, line, cls, msg);
    }
    return result;
  }

#define SUMI_POLL(tport, msgtype) tport->poll<msgtype>(__FILE__, __LINE__, #msgtype)
#define SUMI_POLL_TIME(tport, msgtype, to) tport->poll<msgtype>(to, __FILE__, __LINE__, #msgtype)

  void cq_push_back(int cq_id, message* msg);

  std::list<message*>::iterator cq_begin(int cq_id){
    return completion_queues_[cq_id].begin();
  }

  std::list<message*>::iterator cq_end(int cq_id){
    return completion_queues_[cq_id].end();
  }

  message* pop(int cq_id, std::list<message*>::iterator it){
    message* msg = *it;
    completion_queues_[cq_id].erase(it);
    return msg;
  }

  message* poll(message::payload_type_t ty, bool blocking, int cq_id, double timeout = -1);

  message* poll_new(message::payload_type_t ty, bool blocking, int cq_id, double timeout);

  message* poll_new(bool blocking, int cq_id, double timeout);

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

  void send_self_terminate();
  
  /**
   * The total size of the input/result buffer in bytes is nelems*type_size
   * This always run in a fault-tolerant fashion
   * This uses a dynamic tree structure that reconnects partners when failures are detected
   * @param vote The vote (currently restricted to integer) from this process
   * @param nelems The number of elements in the input and result buffer.
   * @param tag A unique tag identifier for the collective
   * @param fxn The function that merges vote, usually AND, OR, MAX, MIN
   * @param context The context (i.e. initial set of failed procs)
   */
  void dynamic_tree_vote(int vote, int tag, vote_fxn fxn, collective::config cfg = collective::cfg());

  template <template <class> class VoteOp>
  void vote(int vote, int tag, collective::config cfg = collective::cfg()){
    typedef VoteOp<int> op_class_type;
    dynamic_tree_vote(vote, tag, &op_class_type::op, cfg);
  }

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
  void reduce_scatter(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
                     collective::config cfg = collective::cfg());

  template <typename data_t, template <typename> class Op>
  void reduce_scatter(void* dst, void* src, int nelems, int tag, collective::config cfg = collective::cfg()){
    typedef ReduceOp<Op, data_t> op_class_type;
    reduce_scatter(dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, cfg);
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
  void allreduce(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
                         collective::config cfg = collective::cfg());

  template <typename data_t, template <typename> class Op>
  void allreduce(void* dst, void* src, int nelems, int tag, collective::config cfg = collective::cfg()){
    typedef ReduceOp<Op, data_t> op_class_type;
    allreduce(dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, cfg);
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
  void scan(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
        collective::config cfg = collective::cfg());

  template <typename data_t, template <typename> class Op>
  void scan(void* dst, void* src, int nelems, int tag, collective::config cfg = collective::cfg()){
    typedef ReduceOp<Op, data_t> op_class_type;
    scan(dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, cfg);
  }

  void reduce(int root, void* dst, void* src, int nelems, int type_size, int tag,
    reduce_fxn fxn, collective::config cfg = collective::cfg());

  template <typename data_t, template <typename> class Op>
  void reduce(int root, void* dst, void* src, int nelems, int tag, collective::config cfg = collective::cfg()){
    typedef ReduceOp<Op, data_t> op_class_type;
    reduce(root, dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, cfg);
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
  void allgather(void* dst, void* src, int nelems, int type_size, int tag,
                         collective::config = collective::cfg());

  void allgatherv(void* dst, void* src, int* recv_counts, int type_size, int tag,
                          collective::config = collective::cfg());

  void gather(int root, void* dst, void* src, int nelems, int type_size, int tag,
                      collective::config = collective::cfg());

  void gatherv(int root, void* dst, void* src, int sendcnt, int* recv_counts, int type_size, int tag,
                       collective::config = collective::cfg());

  void alltoall(void* dst, void* src, int nelems, int type_size, int tag,
                        collective::config = collective::cfg());

  void alltoallv(void* dst, void* src, int* send_counts, int* recv_counts, int type_size, int tag,
                         collective::config = collective::cfg());

  void scatter(int root, void* dst, void* src, int nelems, int type_size, int tag,
                 collective::config = collective::cfg());

  void scatterv(int root, void* dst, void* src, int* send_counts, int recvcnt, int type_size, int tag,
                        collective::config = collective::cfg());

  void wait_barrier(int tag);

  /**
   * Essentially just executes a zero-byte allgather.
   * @param tag
   * @param fault_aware
   */
  void barrier(int tag, collective::config = collective::cfg());

  void bcast(int root, void* buf, int nelems, int type_size, int tag, collective::config cfg = collective::cfg());
  
  void system_bcast(message* msg);

  int rank() const {
    return rank_;
  }

  int allocate_global_collective_tag(){
    system_collective_tag_--;
    if (system_collective_tag_ >= 0)
      system_collective_tag_ = -1;
    return system_collective_tag_;
  }

  int nproc() const {
    return nproc_;
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

  void notify_collective_done(collective_done_message* msg);  

  /**
   * @brief handle Receive some version of point-2-point message.
   *  Return either the original message or a collective done message
   *  if the message is part of a collective and the collective is done
   * @param msg A point to point message that might be part of a collective
   * @return Null, if collective message and collective is not done
   */
  message* handle(message* msg);

  void* allocate_public_buffer(uint64_t size) {
    return ::malloc(size);
  }

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


#if SSTMAC_COMM_SYNC_STATS
 public:
  virtual void collect_sync_delays(double wait_start, message* msg){}

  virtual void start_collective_sync_delays(){}
#endif

 private:  
  void finish_collective(collective* coll, collective_done_message* dmsg);

  void start_collective(collective* coll);

  void validate_collective(collective::type_t ty, int tag);

  void deliver_pending(collective* coll, int tag, collective::type_t ty);
  
  transport(sprockit::sim_parameters* params);
  
  void validate_api();

  void configure_send(int dst, message::payload_type_t ev, message* msg);

  void poll_cast_error(const char* file, int line, const char* cls, message* msg){
    spkt_throw_printf(sprockit::value_error,
       "Could not cast incoming message to type %s\n"
       "Got %s\n"
       "%s:%d",
       cls, msg->to_string().c_str(),
       file, line);
  }

  /**
   * @brief poll_new Return a new message not already in the completion queue
   * @param blocking
   * @param timeout
   * @return A message if found, null message if non-blocking or timed out
   */
  message* poll_new(bool blocking, double timeout = -1);

  /**
   * Helper function for doing operations necessary to close out a heartbeat
   * @param dmsg
   */
  void vote_done(int context, collective_done_message* dmsg);

  bool skip_collective(collective::type_t ty,
    collective::config& cfg,
    void* dst, void *src,
    int nelems, int type_size,
    int tag);

  void clean_up();

 protected:
  std::vector<std::list<message*>> completion_queues_;

  bool inited_;

  bool finalized_;

  int rank_;

  int nproc_;

 private:
  template <typename Key, typename Value>
  using spkt_enum_map = std::unordered_map<Key, Value, enum_hash>;

  typedef std::unordered_map<int,collective*> tag_to_collective_map;
  typedef spkt_enum_map<collective::type_t, tag_to_collective_map> collective_map;
  collective_map collectives_;

  typedef std::unordered_map<int,std::list<collective_work_message*>> tag_to_pending_map;
  typedef spkt_enum_map<collective::type_t, tag_to_pending_map> pending_map;
  pending_map pending_collective_msgs_;

  std::list<collective*> todel_;
  
  uint32_t eager_cutoff_;

  bool use_put_protocol_;

  communicator* global_domain_;

  int system_collective_tag_;

  static collective_algorithm_selector* allgather_selector_;
  static collective_algorithm_selector* alltoall_selector_;
  static collective_algorithm_selector* alltoallv_selector_;
  static collective_algorithm_selector* allreduce_selector_;
  static collective_algorithm_selector* reduce_scatter_selector_;
  static collective_algorithm_selector* scan_selector_;
  static collective_algorithm_selector* allgatherv_selector_;
  static collective_algorithm_selector* bcast_selector_;
  static collective_algorithm_selector* gather_selector_;
  static collective_algorithm_selector* gatherv_selector_;
  static collective_algorithm_selector* reduce_selector_;
  static collective_algorithm_selector* scatter_selector_;
  static collective_algorithm_selector* scatterv_selector_;
  static sstmac::sw::ftq_tag sumi_transport_tag;
  static sstmac::sw::ftq_tag poll_delay_tag;

  std::string server_libname_;

  sstmac::sw::task_mapping_ptr rank_mapper_;

  std::list<transport_message*> pending_messages_;

  std::list<sstmac::sw::thread*> blocked_threads_;

  uint32_t component_id_;

  sstmac::timestamp post_rdma_delay_;

  sstmac::timestamp post_header_delay_;

  sstmac::timestamp poll_delay_;

  sstmac::sw::lib_compute_time* user_lib_time_;

  sstmac::stat_spyplot* spy_num_messages_;

  sstmac::stat_spyplot* spy_bytes_;

  int collective_cq_id_;

  int pt2pt_cq_id_;

  sstmac::timestamp rdma_pin_latency_;
  sstmac::timestamp rdma_page_delay_;
  int page_size_;
  bool pin_delay_;

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
