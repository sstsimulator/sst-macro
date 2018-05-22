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

#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>
#include <unordered_map>
#include <sprockit/util.h>
#include <sumi/collective_message.h>
#include <sumi/collective.h>
#include <sumi/comm_functions.h>
#include <sumi/options.h>
#include <sumi/ping.h>
#include <sumi/rdma.h>
#include <sumi/communicator_fwd.h>

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

class transport {
  DeclareFactory(transport)
 public:
  virtual ~transport();

  virtual void init();
  
  virtual void finish();

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

  void nvram_get(int src, message* msg);
  
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

  /**
   * @brief poll_pending_messages
   * @param blocking  Whether to block if no messages are available
   * @param timeout   How long to block if no messages found
   * @return A transport message carrying a message
   */
  virtual transport_message* poll_pending_messages(bool blocking, double timeout) = 0;

  virtual void send_terminate(int dst) = 0;

  virtual double wall_time() const = 0;

  virtual int* nidlist() const = 0;

  /**
   * Block on a collective of a particular type and tag
   * until that collective is complete
   * @param ty
   * @param tag
   * @return
   */
  virtual collective_done_message* collective_block(
      collective::type_t ty, int tag, int cq_id = 0) = 0;

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

  void set_use_hardware_ack(bool flag);

  virtual bool supports_hardware_ack() const {
    return false;
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

  virtual void reduce(int root, void* dst, void* src, int nelems, int type_size, int tag,
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

  virtual public_buffer allocate_public_buffer(int size) {
    return public_buffer(::malloc(size));
  }

  virtual public_buffer make_public_buffer(void* buffer, int size) = 0;

  virtual void unmake_public_buffer(public_buffer buf, int size) = 0;

  virtual void free_public_buffer(public_buffer buf, int size) = 0;

  virtual void memcopy(uint64_t bytes) = 0;

 protected:
  void clean_up();

  void configure_send(int dst,
    message::payload_type_t ev,
    message* msg);

  virtual void do_smsg_send(int dst, message* msg) = 0;

  virtual void do_rdma_put(int dst, message* msg) = 0;

  virtual void do_rdma_get(int src, message* msg) = 0;

  virtual void do_nvram_get(int src, message* msg) = 0;

  virtual void go_die() = 0;

  virtual void go_revive() = 0;

 private:  
  void finish_collective(collective* coll, collective_done_message* dmsg);

  void start_collective(collective* coll);

  void validate_collective(collective::type_t ty, int tag);

  void deliver_pending(collective* coll, int tag, collective::type_t ty);
  
 protected:
  transport(sprockit::sim_parameters* params);
  
  void validate_api();

  /**
   * @brief poll_new Return a new message not already in the completion queue
   * @param blocking
   * @param timeout
   * @return A message if found, null message if non-blocking or timed out
   */
  message* poll_new(bool blocking, double timeout = -1);

 private:
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

 protected:
  bool inited_;
  
  bool finalized_;
  
  int rank_;

  int nproc_;
  
  uint32_t eager_cutoff_;

  std::vector<std::list<message*>> completion_queues_;

  bool use_put_protocol_;

  bool use_hardware_ack_;

  communicator* global_domain_;

  int system_collective_tag_;

 private:
  void poll_cast_error(const char* file, int line, const char* cls, message* msg){
    spkt_throw_printf(sprockit::value_error,
       "Could not cast incoming message to type %s\n"
       "Got %s\n"
       "%s:%d",
       cls, msg->to_string().c_str(),
       file, line);
  }

 private:
  static collective_algorithm_selector* allgather_selector_;
  static collective_algorithm_selector* alltoall_selector_;
  static collective_algorithm_selector* alltoallv_selector_;
  static collective_algorithm_selector* allreduce_selector_;
  static collective_algorithm_selector* scan_selector_;
  static collective_algorithm_selector* allgatherv_selector_;
  static collective_algorithm_selector* bcast_selector_;
  static collective_algorithm_selector* gather_selector_;
  static collective_algorithm_selector* gatherv_selector_;
  static collective_algorithm_selector* reduce_selector_;
  static collective_algorithm_selector* scatter_selector_;
  static collective_algorithm_selector* scatterv_selector_;


#if SSTMAC_COMM_SYNC_STATS
 public:
  virtual void collect_sync_delays(double wait_start, message* msg){}

  virtual void start_collective_sync_delays(){}
#endif

#ifdef FEATURE_TAG_SUMI_RESILIENCE
 public:
  int nproc_alive() const {
    return nproc_ - failed_ranks_.size();
  }

  int nproc_failed() const {
    return failed_ranks_.size();
  }

  virtual void schedule_ping_timeout(pinger* pnger, double to) = 0;

  virtual void send_ping_request(int dst) = 0;

  /**
   * Get the set of failed ranks associated with a given context
   * @param context The context for which you want to know the set of failed procs,
   *                default parameter is "default_context" which is the beginning, i.e. no failed procs
   * @throw If the context is unknown - i.e. no vote has executed with that tag number
   * @return The set of failed procs
   */
  const thread_safe_set<int>& failed_ranks(int context) const;

  const thread_safe_set<int>& failed_ranks() const {
    return failed_ranks_;
  }

  void clear_failures() {
    failed_ranks_.clear();
  }

  void declare_failed(int rank) {
    failed_ranks_.insert(rank);
  }

  bool is_failed(int rank) const {
    return failed_ranks_.count(rank);
  }

  bool is_alive(int rank) const {
    return failed_ranks_.count(rank) == 0;
  }

  void die();

  void revive();

  bool is_dead() const {
    return is_dead_;
  }

  /**
   * Cancel a currently active ping
   * @param dst
   * @param func
   */
  void cancel_ping(int dst, timeout_function* func);

  /**
    @param dst The neighbor
    @param func The timeout function to be invoked if no response
                or NACK received
    @return True if the dst node is already known to be failed. False if not.
  */
  bool ping(int dst, timeout_function* func);

  /**
   * @brief watch Wait on heartbeat notifcations for failures on this node.
   * Do NOT actively ping.  Wait instead for failure notifications.
   * @param dst
   * @param func
   * @return True if the dst node is already known to be failed. False if not.
   */
  bool start_watching(int dst, timeout_function* func);

  void stop_watching(int dst, timeout_function* func);

  void renew_pings();

  /**
   * Start regular heartbeat (i.e. vote) collectives going
   * This informs the application at regular intervals of any new process failures
   * @param interval The time in seconds at which to regularly execute heartbeats
   */
  virtual void start_heartbeat(double interval);

  /**
   * Let the current heartbeat finish and then stop them executing.
   */
  virtual void stop_heartbeat();

 protected:
  double heartbeat_interval_;

  void next_heartbeat();

 private:
  bool lazy_watch_;

  activity_monitor* monitor_;

  bool heartbeat_active_;

  bool heartbeat_running_;

  bool is_heartbeat(collective_done_message* dmsg) const {
    return dmsg->tag() >= heartbeat_tag_start_ && dmsg->tag() <= heartbeat_tag_stop_;
  }

  int nspares_;

  thread_safe_int<uint32_t> recovery_lock_;

  struct vote_result {
    int vote;
    thread_safe_set<int> failed_ranks;
    vote_result(int v, const thread_safe_set<int>& f) :
      vote(v), failed_ranks(f)
    {
    }
    vote_result() : vote(0) {}
  };
  typedef std::map<int, vote_result> vote_map;
  vote_map votes_done_;

  bool is_dead_;

  int heartbeat_tag_;
  thread_safe_set<int> failed_ranks_;

  int heartbeat_tag_start_;

  int heartbeat_tag_stop_;

  typedef std::map<int, function_set> watcher_map;
  watcher_map watchers_;

  void fail_watcher(int dst);

  void start_recovery();

  void end_recovery();

  virtual void schedule_next_heartbeat() = 0;

  /**
   * Actually do the work of executing a heartbeat
   * @param prev_context The context number for the last successful heartbeat
   */
  void do_heartbeat(int prev_context);
#endif
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
