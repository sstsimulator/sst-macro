#ifndef sumi_api_COLLECTIVE_ACTOR_H
#define sumi_api_COLLECTIVE_ACTOR_H

#include <sumi/collective.h>
#include <sumi/collective_message.h>
#include <sumi/dense_rank_map.h>
#include <set>
#include <map>
#include <stdint.h>
#include <sumi/thread_safe_set.h>
#include <sumi/config.h>

namespace sumi {

struct action
{
  typedef enum { send=0, recv=1 } type_t;
  type_t type;
  int partner;
  int join_counter;
  int round;
  int offset;
  int nelems;
  uint32_t id;

  static const char*
  tostr(type_t ty){
    if (ty == send) return "send";
    else return "recv";
  }

  static const uint32_t max_round = 22;

  static uint32_t
  message_id(type_t ty, int r, int p){
    //factor of two is for send or receive
    return p*max_round*2 + r*2 + ty;
  }

  static void
  details(uint32_t round, type_t& ty, int& r, int& p){
    uint32_t remainder = round;
    p = remainder / max_round / 2;
    remainder -= p*max_round*2;

    r = remainder / 2;
    remainder -= r*2;

    ty = (type_t) remainder;
  }

 protected:
  action(type_t ty, int r, int p) :
    type(ty), round(r), partner(p),
    join_counter(0)
  {
    id = message_id(ty, r, p);
  }
};

struct recv_action : public action
{
  recv_action(int round, int partner) :
    action(recv, round, partner)
  {
  }
};

struct send_action : public action
{
  send_action(int round, int partner) :
    action(send, round, partner)
  {
  }
};

/**
 * @class collective_actor
 * Object that actually does the work (the actor)
 * in a collective. A separation (for now) is kept
 * between the actually collective and actors in the collective.
 * The actors are essentially virtual ranks allowing the collective
 * to run an algorith with a virtual number of ranks different from
 * the actual physical number.
 */
class collective_actor
{
 public:
  virtual std::string
  to_string() const = 0;

  virtual ~collective_actor();

  void
  partner_ping_failed(int global_rank);

  bool
  complete() const {
    return complete_;
  }
  int tag() const {
    return tag_;
  }

  std::string
  rank_str() const;

  void init(transport* my_api, domain* dom, int tag, int context, bool fault_aware);

 protected:
  collective_actor(transport* my_api, domain* dom, int tag, int context, bool fault_aware);

  collective_actor(){} //will be initialized later

  /**
   * Start pinging neighbor to make sure they are still alive
   * @param rank
   */
  bool
  ping_neighbor(int dense_rank);

  /**
   * Stop pinging neighbor. I am done with them.
   * They can be dead for all I care.
   * @param rank
   */
  void
  cancel_ping(int dense_rank);

  int
  global_rank(int dense_rank) const;

  /**
   * domain <= dense <= virtual
   * A job starts with 4 nodes {0,1,2}.
   * Node 1 dies. There are 2 live nodes.
   * A collective starts with 4 virtual actors {0,1,2,3}
   * 0 -> 0
   * 1 -> 0
   * 2 -> 2
   * 3 -> 2
   * Map a virtual actor rank to the actual domain rank.
   * @param virtual_rank
   * @return
   */
  virtual int
  domain_rank(int dense_rank) const;

  /**
   * Notification that a partner failed.
   * Here the partner is identified by dense rank.
   * See #dense_rank
   * @param dense_rank
   */
  virtual void
  dense_partner_ping_failed(int dense_rank) = 0;

  void
  send_header(int dense_dst, const message::ptr& msg);

  void
  send_payload(int dense_dst, const message::ptr& msg);

  void
  rdma_get(int dense_dst, const message::ptr& msg);

  void
  rdma_put(int dense_dst, const message::ptr& msg);

  std::string
  rank_str(int dense_rank) const;

 protected:
  std::string
  failed_proc_string() const;

  /**
   * Validation function to make sure all pings are cleared
   */
  void
  validate_pings_cleared();

  virtual void
  finalize(){}

  bool
  ping_domain_rank(int phys_rank, int dense_rank);

  bool is_failed(int dense_rank) const {
    return failed_ranks_.count(dense_rank);
  }

  bool is_alive(int dense_rank) const {
    return failed_ranks_.count(dense_rank) == 0;
  }
  virtual bool
  check_neighbor(int global_phys_rank);

  virtual void
  stop_check_neighbor(int global_phys_rank);

  bool
  failed() const {
    return !failed_ranks_.empty();
  }

  bool
  do_ping_neighbor(int dense_rank);

  int
  dense_to_global_dst(int dense_dst);

 protected:
  transport* my_api_;

  domain* dom_;

  int dense_me_;

  int dense_nproc_;

  int tag_;

  std::map<int, int> ping_refcounts_;

  dense_rank_map rank_map_;

  thread_safe_set<int> failed_ranks_;

  timeout_function* timeout_;

  bool fault_aware_;

  bool complete_;

};

/**
 * @class collective_actor
 * Object that actually does the work (the actor)
 * in a collective. A separation (for now) is kept
 * between the actually collective and actors in the collective.
 * The actors are essentially virtual ranks allowing the collective
 * to run an algorith with a virtual number of ranks different from
 * the actual physical number.
 */
class dag_collective_actor :
 public collective_actor
{
 public:
  virtual std::string
  to_string() const = 0;

  virtual ~dag_collective_actor();

  virtual void
  recv(const collective_work_message::ptr& msg);

  virtual void
  start();

  typedef enum {
    eager_protocol,
    put_protocol,
    get_protocol } protocol_t;

  protocol_t
  protocol_for_action(action* ac) const;

  void
  deadlock_check() const;

  void init(
    collective::type_t type,
    transport* my_api,
    domain* dom,
    int nelems,
    int type_size,
    int tag,
    bool fault_aware,
    int context){
    collective_actor::init(my_api, dom, tag, context, fault_aware);
    type_ = type;
    nelems_ = nelems;
    type_size_ = type_size;
  }

  virtual void init_buffers(void* dst, void* src) = 0;
  virtual void finalize_buffers() = 0;
  virtual void init_dag() = 0;

 protected:
  void
  add_dependency(action* precursor, action* ac);

  void
  check_collective_done();

  void put_done_notification();

  void start_send(action* ac);
  void start_recv(action* ac);
  void start_send_nack_instead(action* ac);
  void start_recv_nack_instead(action* ac);
  void do_send(action* ac);
  void do_recv(action* ac);
  void send_failure_message(action* ac, collective_work_message::action_t ty);

  void start_action(action* ac);

  void send_eager_message(action* ac);
  void send_rdma_put_header(action* ac);
  void send_rdma_get_header(action* ac);

  void
  next_round_ready_to_put(action* ac,
    const collective_work_message::ptr& header);

  void
  next_round_ready_to_get(action* ac,
    const collective_work_message::ptr& header);

  void
  incoming_recv_message(action* ac, const collective_work_message::ptr& msg);

  void
  incoming_send_message(action* ac, const collective_work_message::ptr& msg);

 protected:
  void
  incoming_message(const collective_work_message::ptr& msg);

  void
  incoming_nack(action::type_t ty, const collective_work_message::ptr& msg);

  void
  data_recved(const collective_work_message::ptr& msg, void* recvd_buffer);

  void
  data_recved(action* ac, const collective_work_message::ptr &msg, void *recvd_buffer);

  void
  data_sent(const collective_work_message::ptr& msg);

  virtual void
  buffer_action(void* dst_buffer, void* msg_buffer, action* ac) = 0;

  void* message_buffer(void* buffer, int offset);

  public_buffer send_buffer(int offset);

  public_buffer recv_buffer(int round, int offset);

  collective_done_message::ptr
  done_msg() const;

  void add_initial_action(action* ac);

  void dense_partner_ping_failed(int dense_rank);

  bool
  out_of_place_round(int rnd) const {
    return out_of_place_rounds_.find(rnd) != out_of_place_rounds_.end();
  }

 private:
  typedef std::map<uint32_t, action*> active_map;
  typedef std::multimap<uint32_t, action*> pending_map;
  active_map active_sends_;
  active_map active_recvs_;
  pending_map pending_comms_;
  std::list<action*> completed_actions_;

  typedef std::multimap<uint32_t, collective_work_message::ptr> pending_msg_map;
  pending_msg_map pending_send_headers_;
  pending_msg_map pending_recv_headers_;

  void erase_pending(uint32_t id, pending_msg_map& m);
  void reput_pending(uint32_t id, pending_msg_map& m);

  /**
   * @brief Satisfy dependencies for any pending comms,
   *        Clean up pings, and check if collective done.
   *        This calls clear_action to do clean up.
   *        Should only be called for actions that became active
   * @param ac
   */
  void action_done(action* ac, active_map& m);

  /**
   * @brief Satisfy dependences and check if done.
   *        Unlike action_done, this can be called for actions
   *        that stopped early and never became active
   * @param ac
   * @param m
   */
  void clear_action(action* ac, active_map& m);

  void action_done(action::type_t ty, int round, int partner);

  void fail_actions(int dense_rank, active_map& m);

  std::list<action*> initial_actions_;

 protected:
  int type_size_;

  int nelems_;

  /**
   * @brief This where I send data from
   */
  public_buffer send_buffer_;
  /**
   * @brief This is where I directly receive data from neighbors
   */
  public_buffer recv_buffer_;
  /**
   * @brief This is where I accumulate or put results after a receive
   */
  public_buffer result_buffer_;

  collective::type_t type_;

  std::set<int> out_of_place_rounds_;
};

/**
 * @class virtual_rank_map
 * Maps a given number of live processors
 * onto a set of virtual ranks.
 * For example, if you have 5 procs you might
 * want to run a virtual collective with 8 ranks
 * so you have a nice, neat power of 2
 */
class virtual_rank_map
{
 public:
  int
  virtual_to_real(int rank) const;

  std::vector<int>
  real_to_virtual(int rank) const;

  int
  virtual_nproc() const {
    return virtual_nproc_;
  }

  int
  nproc() const {
    return nproc_;
  }

  virtual_rank_map(int nproc, int virtual_nproc);

 protected:
  int nproc_;

  int virtual_nproc_;

};



}


#endif // COLLECTIVE_ACTOR_H
