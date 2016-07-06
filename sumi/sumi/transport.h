#ifndef sumi_api_TRANSPORT_H
#define sumi_api_TRANSPORT_H

#include <sumi/collective_message.h>
#include <sumi/collective.h>
#include <sumi/comm_functions.h>
#include <sumi/options.h>
#include <sumi/ping.h>
#include <sumi/rdma.h>
#include <sumi/domain_fwd.h>
#include <sumi/thread_safe_int.h>
#include <sumi/thread_safe_list.h>
#include <sumi/thread_safe_set.h>
#include <sumi/thread_lock.h>
#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>
#include <sprockit/unordered.h>
#include <sprockit/util.h>

DeclareDebugSlot(sumi);

#define CHECK_IF_I_AM_DEAD(x) if (is_dead_) x

#define print_size(x) printf("%s %d: sizeof(%s)=%d\n", __FILE__, __LINE__, #x, sizeof(x))

namespace sumi {

class transport :
  virtual public sprockit::factory_type
{

 public:
  virtual ~transport(){}

  virtual void
  init();
  
  virtual void
  finalize();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  void
  deadlock_check();

  /**
   Send a message directly to a destination node.
   This assumes there is buffer space available at the destination to eagerly receive.
   * @param dst         The destination of the message
   * @param ev          An enum for the type of payload carried
   * @param needs_ack   Whether an ack should be generated notifying that the message was fully injected
   * @param msg         The message to send (full message at the MTL layer)
   */
  void
  smsg_send(int dst,
     message::payload_type_t ev,
     const message::ptr& msg,
     bool needs_ack = false);

  /**
   Helper function for #smsg_send. Directly send a message header.
   * @param dst
   * @param msg
   */
  void
  send_header(int dst, const message::ptr& msg, bool needs_ack = false);

  /**
   Helper function for #smsg_send. Directly send an actual data payload.
   * @param dst
   * @param msg
   */
  void
  send_payload(int dst, const message::ptr& msg, bool needs_ack = false);

  /**
   Put a message directly to the destination node.
   This assumes the application has properly configured local/remote buffers for the transfer.
   * @param dst      Where the data is being put (destination)
   * @param msg      The message to send (full message at the MTL layer).
   *                 Should have local/remote buffers configured for RDMA.
   */
  void
  rdma_put(int dst, const message::ptr& msg);

  /**
   Put a message directly to the destination node.
   This assumes the application has properly configured local/remote buffers for the transfer.
   * @param dst      Where the data is being put (destination)
   * @param msg      The message to send (full message at the MTL layer).
   *                 Should have local/remote buffers configured for RDMA.
   * @param needs_send_ack  Whether an ack should be generated send-side when the message is fully injected
   * @param needs_recv_ack  Whether an ack should be generated recv-side when the message is fully received
   */
  void
  rdma_put(
    int dst,
    const message::ptr &msg,
    bool needs_send_ack,
    bool needs_recv_ack);

  /**
   Get a message directly from the source node.
   This assumes the application has properly configured local/remote buffers for the transfer.
   * @param src      Where the data resides and is being fetched from
   * @param msg      The message to get (full message at the MTL layer).
   *                 Should have local/remote buffers configured for RDMA.
   */
  void
  rdma_get(int src, const message::ptr& msg);

  /**
   Get a message directly from the source node.
   This assumes the application has properly configured local/remote buffers for the transfer.
   * @param src      Where the data is being put (destination)
   * @param msg      The message to send (full message at the MTL layer).
   *                 Should have local/remote buffers configured for RDMA.
   * @param needs_send_ack  Whether an ack should be generated send-side (source) when the message is fully injected
   * @param needs_recv_ack  Whether an ack should be generated recv-side when the message is fully received
   */
  void
  rdma_get(
    int src,
    const message::ptr &msg,
    bool needs_send_ack,
    bool needs_recv_ack);

  void
  nvram_get(int src, const message::ptr& msg);
  
  /**
   Block until a message is received.
   Returns immediately if message already waiting.
   Message returned is removed from the internal queue.
   Successive calls to the function do NOT return the same message.
   @return    The next message to be received
  */
  message::ptr
  blocking_poll();

  /**
   Block until a message is received.
   Returns immediately if message already waiting.
   Message returned is removed from the internal queue.
   Successive calls to the function do NOT return the same message.
   @param timeout   Timeout in seconds
   @return          The next message to be received. Message is NULL on timeout
  */
  message::ptr
  blocking_poll(double timeout);

  template <class T>
  typename T::ptr
  poll(const char* file, int line, const char* cls) {
    message::ptr msg = blocking_poll();
    typename T::ptr result = ptr_safe_cast(T, msg);
    if (!result){
      poll_cast_error(file, line, cls, msg);
    }
    return result;
  }

  template <class T>
  typename T::ptr
  poll(double timeout, const char* file, int line, const char* cls) {
    message::ptr msg = blocking_poll(timeout);
    typename T::ptr result = ptr_test_cast(T, msg);
    if (msg && !result){
      poll_cast_error(file, line, cls, msg);
    }
    return result;
  }

#define SUMI_POLL(tport, msgtype) tport->poll<msgtype>(__FILE__, __LINE__, #msgtype)
#define SUMI_POLL_TIME(tport, msgtype, to) tport->poll<msgtype>(to, __FILE__, __LINE__, #msgtype)


  message::ptr
  blocking_poll(message::payload_type_t);
  
  virtual message::ptr
  block_until_message() = 0;

  virtual message::ptr
  block_until_message(double timeout) = 0;

  bool
  use_eager_protocol(long byte_length) const {
    return byte_length < eager_cutoff_;
  }

  void
  set_eager_cutoff(long bytes) {
    eager_cutoff_ = bytes;
  }

  bool
  use_put_protocol() const {
    return use_put_protocol_;
  }

  bool
  use_get_protocol() const {
    return !use_put_protocol_;
  }

  void
  set_put_protocol(bool flag) {
    use_put_protocol_ = flag;
  }

  void
  set_use_hardware_ack(bool flag);

  virtual bool
  supports_hardware_ack() const {
    return false;
  }
  
  virtual void
  init_spares(int nspares);

  /**
   * Cancel a currently active ping
   * @param dst
   * @param func
   */
  void
  cancel_ping(int dst, timeout_function* func);

  /**
    @param dst The neighbor
    @param func The timeout function to be invoked if no response
                or NACK received
    @return True if the dst node is already known to be failed. False if not.
  */
  bool
  ping(int dst, timeout_function* func);

  /**
   * @brief watch Wait on heartbeat notifcations for failures on this node.
   * Do NOT actively ping.  Wait instead for failure notifications.
   * @param dst
   * @param func
   * @return True if the dst node is already known to be failed. False if not.
   */
  bool
  start_watching(int dst, timeout_function* func);

  void
  stop_watching(int dst, timeout_function* func);

  void
  send_self_terminate();

  void
  send_terminate(int rank);

  virtual double
  wall_time() const = 0;

  void
  send_ping_request(int dst);

  void
  renew_pings();
  
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
  
  /**
   * Block on a collective of a particular type and tag
   * until that collective is complete
   * @param ty
   * @param tag
   * @return
   */
  virtual collective_done_message::ptr
  collective_block(collective::type_t ty, int tag) = 0;
  
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
  virtual void
  dynamic_tree_vote(int vote, int tag, vote_fxn fxn, int context = options::initial_context, domain* dom = 0);

  template <template <class> class VoteOp>
  void
  vote(int vote, int tag, int context = options::initial_context, domain* dom = 0){
    typedef VoteOp<int> op_class_type;
    dynamic_tree_vote(vote, tag, &op_class_type::op, context, dom);
  }

  /**
   * The total size of the input/result buffer in bytes is nelems*type_size
   * @param dst  Buffer for the result. Can be NULL to ignore payloads.
   * @param src  Buffer for the input. Can be NULL to ignore payloads. This need not be public! Automatically memcpy from src to public facing dst.
   * @param nelems The number of elements in the input and result buffer.
   * @param type_size The size of the input type, i.e. sizeof(int), sizeof(double)
   * @param tag A unique tag identifier for the collective
   * @param fxn The function that will actually perform the reduction
   * @param fault_aware Whether to execute in a fault-aware fashion to detect failures
   * @param context The context (i.e. initial set of failed procs)
   */
  virtual void
  allreduce(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn, bool fault_aware = false, int context = options::initial_context, domain* dom = 0);

  template <typename data_t, template <typename> class Op>
  void
  allreduce(void* dst, void* src, int nelems, int tag, bool fault_aware = false, int context = options::initial_context, domain* dom = 0){
    typedef ReduceOp<Op, data_t> op_class_type;
    allreduce(dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, fault_aware, context, dom);
  }

  virtual void
  reduce(int root, void* dst, void* src, int nelems, int type_size, int tag,
    reduce_fxn fxn, bool fault_aware = false, int context = options::initial_context, domain* dom = 0);

  template <typename data_t, template <typename> class Op>
  void
  reduce(int root, void* dst, void* src, int nelems, int tag, bool fault_aware = false, int context = options::initial_context, domain* dom = 0){
    typedef ReduceOp<Op, data_t> op_class_type;
    reduce(root, dst, src, nelems, sizeof(data_t), tag, &op_class_type::op, fault_aware, context, dom);
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
  virtual void
  allgather(void* dst, void* src, int nelems, int type_size, int tag,
            bool fault_aware = false, int context = options::initial_context, domain* dom = 0);

  virtual void
  allgatherv(void* dst, void* src, int* recv_counts, int type_size, int tag,
             bool fault_aware = false, int context = options::initial_context, domain* dom = 0);

  virtual void
  gather(int root, void* dst, void* src, int nelems, int type_size, int tag,
         bool fault_aware = false, int context = options::initial_context, domain* dom = 0);

  virtual void
  gatherv(int root, void* dst, void* src, int sendcnt, int* recv_counts, int type_size, int tag,
          bool fault_aware = false, int context = options::initial_context, domain* dom = 0);

  virtual void
  alltoall(void* dst, void* src, int nelems, int type_size, int tag,
             bool fault_aware = false, int context = options::initial_context, domain* dom = 0);

  virtual void
  alltoallv(void* dst, void* src, int* send_counts, int* recv_counts, int type_size, int tag,
             bool fault_aware = false, int context = options::initial_context, domain* dom = 0);

  virtual void
  scatter(int root, void* dst, void* src, int nelems, int type_size, int tag,
          bool fault_aware = false, int context = options::initial_context, domain* dom = 0);

  virtual void
  scatterv(int root, void* dst, void* src, int* send_counts, int recvcnt, int type_size, int tag,
          bool fault_aware = false, int context = options::initial_context, domain* dom = 0);

  /**
   * Essentially just executes a zero-byte allgather.
   * @param tag
   * @param fault_aware
   */
  void
  barrier(int tag, bool fault_aware = false, domain* dom = 0);

  void
  bcast(int root, void* buf, int nelems, int type_size, int tag, bool fault_aware, int context=options::initial_context, domain* dom=0);
  
  int 
  rank() const {
    return rank_;
  }

  int 
  nproc() const {
    return nproc_;
  }

  int 
  nproc_alive() const {
    return nproc_ - failed_ranks_.size();
  }

  int 
  nproc_failed() const {
    return failed_ranks_.size();
  }

  domain*
  global_dom() const;

  /**
   * The cutoff for message size in bytes
   * for switching between an eager protocol and a rendezvous RDMA protocol
   * @return
   */
  int
  eager_cutoff() const {
    return eager_cutoff_;
  }
  
  /**
   * Get the set of failed ranks associated with a given context
   * @param context The context for which you want to know the set of failed procs,
   *                default parameter is "default_context" which is the beginning, i.e. no failed procs
   * @throw If the context is unknown - i.e. no vote has executed with that tag number
   * @return The set of failed procs
   */
  const thread_safe_set<int>&
  failed_ranks(int context) const;

  const thread_safe_set<int>&
  failed_ranks() const {
    return failed_ranks_;
  }

  void
  clear_failures() {
    failed_ranks_.clear();
  }

  void
  declare_failed(int rank){
    failed_ranks_.insert(rank);
  }

  bool
  is_failed(int rank) const {
    return failed_ranks_.count(rank);
  }

  bool
  is_alive(int rank) const {
    return failed_ranks_.count(rank) == 0;
  }
  
  virtual void
  delayed_transport_handle(const message::ptr& msg) = 0;

  virtual void
  cq_notify() = 0;
  
  void
  die();

  void
  revive();

  bool
  is_dead() const {
    return is_dead_;
  }

  void
  notify_collective_done(const collective_done_message::ptr& msg);
  
  virtual void
  schedule_ping_timeout(pinger* pnger, double to) = 0;
  
  virtual void
  schedule_next_heartbeat() = 0;

  /**
   * Execute the next heartbeat. This figures out the next tag and prev context
   * and then calls #do_heartbeat
   */
  void 
  next_heartbeat();  

  void
  handle(const message::ptr& msg);

  virtual public_buffer
  allocate_public_buffer(int size) {
    return public_buffer(::malloc(size));
  }

  virtual public_buffer
  make_public_buffer(void* buffer, int size) {
    return public_buffer(buffer);
  }

  virtual void
  unmake_public_buffer(public_buffer buf, int size) {
    //nothing to do
  }

  virtual void
  free_public_buffer(public_buffer buf, int size) {
    ::free(buf.ptr);
  }

  /**
   * @brief Optimizations can be performed in some transport layers
   * If the header being sent is explicitly known to be coordinating an RDMA
   * transaction you can ACK the transaction via hardware.  Otherwise,
   * the transpor must perform a software-level ACK.
   * @param dst
   * @param msg
   */
  void
  send_rdma_header(int dst, const message::ptr& msg);

  void
  send(int dst, const message::ptr& msg);

  void
  send_unexpected_rdma(int dst, const message::ptr& msg);

 protected:
  void
  start_transaction(const message::ptr& msg);

  void
  clean_up();

  void
  handle_unexpected_rdma_header(const message::ptr& msg);

  message::ptr
  finish_transaction(int tid);

  void
  configure_send(int dst,
    message::payload_type_t ev,
    const message::ptr& msg);

  virtual void
  do_smsg_send(int dst, const message::ptr& msg) = 0;

  virtual void
  do_rdma_put(int dst, const message::ptr& msg) = 0;

  virtual void
  do_rdma_get(int src, const message::ptr& msg) = 0;

  virtual void
  do_nvram_get(int src, const message::ptr& msg) = 0;

  virtual void
  do_send_terminate(int dst) = 0;

  virtual void
  do_send_ping_request(int dst) = 0;

  virtual void
  go_die() = 0;

  virtual void
  go_revive() = 0;

  /* Has default behavior */
  void
  operation_done(const message::ptr& msg);

 private:  
  bool
  is_heartbeat(const collective_done_message::ptr& dmsg) const {
    return dmsg->tag() >= heartbeat_tag_start_ && dmsg->tag() <= heartbeat_tag_stop_;
  }

  void 
  finish_collective(collective* coll, const collective_done_message::ptr& dmsg);

  void 
  start_collective(collective* coll);

  /**
   * Helper function for doing operations necessary to close out a heartbeat
   * @param dmsg
   */
  void 
  vote_done(const collective_done_message::ptr& dmsg);

  void 
  validate_collective(collective::type_t ty, int tag);

  void 
  deliver_pending(collective* coll, int tag, collective::type_t ty);

  /**
   * Actually do the work of executing a heartbeat
   * @param prev_context The context number for the last successful heartbeat
   */
  void 
  do_heartbeat(int prev_context);  

  
 protected:
  transport();
  
  void
  validate_api();

  void
  fail_watcher(int dst);

  void lock();

  void unlock();

  void start_recovery();

  void end_recovery();

  void start_function();

  void end_function();

 private:  
  bool
  skip_collective(collective::type_t ty,
    domain*& dom,
    void* dst, void *src,
    int nelems, int type_size,
    int tag);

 private:
  int heartbeat_tag_;
  typedef spkt_unordered_map<int,collective*> tag_to_collective_map;
  typedef spkt_enum_map<collective::type_t, tag_to_collective_map> collective_map;
  collective_map collectives_;
  
  //I don't know a better way to do this and be clean
  //callback mechanism - if collective completes
  //it passes back a notification stored here
  collective_done_message::ptr collective_notification_;

  typedef spkt_unordered_map<int,std::list<collective_work_message_ptr> > tag_to_pending_map;
  typedef spkt_enum_map<collective::type_t, tag_to_pending_map> pending_map;
  pending_map pending_collective_msgs_;

  thread_safe_set<int> failed_ranks_;

  int heartbeat_tag_start_;

  int heartbeat_tag_stop_;

  typedef std::map<int, function_set> watcher_map;
  watcher_map watchers_;

  std::list<collective*> todel_;

 protected:
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

  bool inited_;
  
  bool finalized_;
  
  int rank_;

  int nproc_;
  
  int eager_cutoff_;

  bool lazy_watch_;

  activity_monitor* monitor_;

  bool heartbeat_active_;

  bool heartbeat_running_;

  double heartbeat_interval_;

  thread_safe_list<message::ptr> completion_queue_;

  int next_transaction_id_;
  int max_transaction_id_;
  std::map<int, message::ptr> transactions_;

  bool is_dead_;

  bool use_put_protocol_;

  bool use_hardware_ack_;

  domain* global_domain_;

  int nspares_;

#if SUMI_USE_SPINLOCK
  spin_thread_lock lock_;
#else
  mutex_thread_lock lock_;
#endif

  thread_safe_int<uint32_t> recovery_lock_;

 private:
  void
  poll_cast_error(const char* file, int line, const char* cls, const message::ptr& msg){
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
  static collective_algorithm_selector* allgatherv_selector_;
  static collective_algorithm_selector* bcast_selector_;
  static collective_algorithm_selector* gather_selector_;
  static collective_algorithm_selector* gatherv_selector_;
  static collective_algorithm_selector* reduce_selector_;
  static collective_algorithm_selector* scatter_selector_;
  static collective_algorithm_selector* scatterv_selector_;

};

DeclareFactory(transport)


class terminate_exception : public std::exception
{
};

}



#endif // TRANSPORT_H
