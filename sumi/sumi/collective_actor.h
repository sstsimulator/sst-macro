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

#ifndef sumi_api_COLLECTIVE_ACTOR_H
#define sumi_api_COLLECTIVE_ACTOR_H

#include <sumi/collective.h>
#include <sumi/collective_message.h>
#include <sumi/dense_rank_map.h>
#include <sumi/communicator.h>
#include <set>
#include <map>
#include <stdint.h>
#include <sumi/thread_safe_set.h>
#include <sumi/config.h>

DeclareDebugSlot(sumi_collective_buffer)

#define sumi_case(x) case x: return #x

#define do_sumi_debug_print(...)
// if (sprockit::debug::slot_active(sprockit::dbg::sumi_collective_buffer)) \
//   debug_print(__VA_ARGS__)

namespace sumi {

void
debug_print(const char* info, const std::string& rank_str,
            int partner, int round, int offset,
            int nelems, int type_size, const void* buffer);

struct action
{
  typedef enum { send=0, recv=1, shuffle=2, unroll=3, resolve=4, join=5 } type_t;
  type_t type;
  int partner;
  int phys_partner;
  int join_counter;
  int round;
  int offset;
  int nelems;
  uint32_t id;
  double start;

  static const char*
  tostr(type_t ty){
    switch(ty){
      sumi_case(send);
      sumi_case(recv);
      sumi_case(shuffle);
      sumi_case(unroll);
      sumi_case(resolve);
      sumi_case(join);
    }
  }

  std::string
  to_string() const;

  static const uint32_t max_round = 500;

  static uint32_t
  message_id(type_t ty, int r, int p){
    //factor of two is for send or receive
    const int num_enums = 6;
    return p*max_round*num_enums + r*num_enums + ty;
  }

  static void
  details(uint32_t round, type_t& ty, int& r, int& p){
    const int num_enums = 6;
    uint32_t remainder = round;
    p = remainder / max_round / num_enums;
    remainder -= p*max_round*num_enums;

    r = remainder / num_enums;
    remainder -= r*num_enums;

    ty = (type_t) remainder;
  }

  action(type_t ty, int r, int p) :
    type(ty), round(r),
    partner(p),
    join_counter(0)
  {
    id = message_id(ty, r, p);
  }
};

struct recv_action : public action
{
  typedef enum {
    in_place=0,
    reduce=1,
    packed_temp_buf=2,
    unpack_temp_buf=3
  } buf_type_t;

  buf_type_t buf_type;

  static const char*
  tostr(buf_type_t ty){
    switch(ty){
      sumi_case(in_place);
      sumi_case(reduce);
      sumi_case(packed_temp_buf);
      sumi_case(unpack_temp_buf);
    }
  }

  typedef enum {
    rdvz_in_place=0,
    rdvz_reduce=1,
    rdvz_packed_temp_buf=2,
    rdvz_unpack_temp_buf=3,
    eager_in_place=4,
    eager_reduce=5,
    eager_packed_temp_buf=6,
    eager_unpack_temp_buf=7
  } recv_type_t;

  static recv_type_t
  recv_type(bool eager, buf_type_t ty){
    int shift = eager ? 4 : 0;
    return recv_type_t(ty + shift);
  }

  recv_action(int round, int partner, buf_type_t bty) :
    action(recv, round, partner),
    buf_type(bty)
  {
  }
};

struct send_action : public action
{
  typedef enum {
    in_place=0,
    prev_recv=1,
    temp_send=2
  } buf_type_t;

  buf_type_t buf_type;

  static const char*
  tostr(buf_type_t ty){
    switch(ty){
      sumi_case(in_place);
      sumi_case(prev_recv);
      sumi_case(temp_send);
    }
  }

  send_action(int round, int partner, buf_type_t ty) :
    action(send, round, partner),
    buf_type(ty)
  {
  }

};

struct shuffle_action : public action
{
  shuffle_action(int round, int partner) :
    action(shuffle, round, partner)
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

  void init(transport* my_api, communicator* dom, int tag, int context, bool fault_aware);

 protected:
  collective_actor(transport* my_api, communicator* dom, int tag, int context, bool fault_aware);

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
   * @return
   */
  int
  comm_rank(int dense_rank) const;

  /**
   * Notification that a partner failed.
   * Here the partner is identified by dense rank.
   * See #dense_rank
   * @param dense_rank
   */
  virtual void
  dense_partner_ping_failed(int dense_rank) = 0;

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
  ping_rank(int phys_rank, int dense_rank);

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

  communicator* comm_;

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

class slicer {
 public:
  /**
   * @brief pack_in
   * @param packedBuf
   * @param unpackedBuf
   * @param offset
   * @param nelems
   * @return
   */
  virtual size_t
  pack_send_buf(void* packedBuf, void* unpackedObj,
                int offset, int nelems) const = 0;

  virtual void
  unpack_recv_buf(void* packedBuf, void* unpackedObj,
                  int offset, int nelems) const = 0;

  virtual void
  memcpy_packed_bufs(void* dst, void* src, int nelems) const = 0;

  virtual void
  unpack_reduce(void* packedBuf, void* unpackedObj,
            int offset, int nelems) const {
    spkt_throw(sprockit::unimplemented_error,
          "slicer for collective does not implement a reduce op");
  }

  virtual bool
  contiguous() const = 0;

  virtual int
  element_packed_size() const = 0;
};

class default_slicer :
 public slicer
{

 public:
  size_t
  pack_send_buf(void* packedBuf, void* unpackedObj, int offset, int nelems) const {
    char* dstptr = (char*) packedBuf;
    char* srcptr = (char*) unpackedObj + offset*type_size;
    ::memcpy(dstptr, srcptr, nelems*type_size);
    return nelems*type_size;
  }

  void
  unpack_recv_buf(void* packedBuf, void* unpackedObj, int offset, int nelems) const {
    char* dstptr = (char*) unpackedObj + offset*type_size;
    char* srcptr = (char*) packedBuf;
    ::memcpy(dstptr, srcptr, nelems*type_size);
  }

  virtual void
  memcpy_packed_bufs(void *dst, void *src, int nelems) const {
    ::memcpy(dst, src, nelems*type_size);
  }

  virtual void
  unpack_reduce(void *packedBuf, void *unpackedObj, int offset, int nelems) const {
    char* dstptr = (char*) unpackedObj + offset*type_size;
    (fxn)(dstptr, packedBuf, nelems);
  }

  int
  element_packed_size() const {
    return type_size;
  }

  bool
  contiguous() const {
    return true;
  }

  int type_size;
  reduce_fxn fxn;
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
 public collective_actor,
 public communicator::rank_callback
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
    communicator* dom,
    int nelems,
    int type_size,
    int tag,
    bool fault_aware,
    int context){
    collective_actor::init(my_api, dom, tag, context, fault_aware);
    type_ = type;
    nelems_ = nelems;
    type_size_ = type_size;
    slicer_ = new default_slicer;
    slicer_->type_size = type_size;
  }

  virtual void init_buffers(void* dst, void* src) = 0;
  virtual void finalize_buffers() = 0;
  virtual void init_dag() = 0;
  virtual void init_tree(){}

 private:
  typedef std::map<uint32_t, action*> active_map;
  typedef std::multimap<uint32_t, action*> pending_map;
  typedef std::multimap<uint32_t, collective_work_message::ptr> pending_msg_map;

 protected:
  dag_collective_actor() :
    slicer_(nullptr)
  {
  }

  void add_dependency(action* precursor, action* ac);
  void add_action(action* ac);

  void
  compute_tree(int& log2nproc, int& midpoint, int& nproc) const;

  static bool
  is_shared_role(int role, int num_roles, int* my_roles){
    for (int r=0; r < num_roles; ++r){
      if (role == my_roles[r]){
        return true;
      }
    }
    return false;
  }

 private:
  void add_comm_dependency(action* precursor, action* ac);
  void add_dependency_to_map(uint32_t id, action* ac);
  void rank_resolved(int global_rank, int comm_rank);

  void check_collective_done();

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

  /**
   * @brief set_send_buffer
   * @param ac  The action being performed (eager, rdma get, rdma put)
   * @param buf in/out parameter that will hold the correct buffer
   * @return The size of the buffer in bytes
   */
  size_t
  set_send_buffer(action* ac, public_buffer& buf);

  void
  set_recv_buffer(action* ac, public_buffer& buf);

  collective_work_message::ptr
  new_message(action* ac, collective_work_message::action_t act);

  collective_done_message::ptr
  done_msg() const;

  void dense_partner_ping_failed(int dense_rank);

  virtual void
  start_shuffle(action* ac);

  void erase_pending(uint32_t id, pending_msg_map& m);

  void reput_pending(uint32_t id, pending_msg_map& m);

  /**
   * @brief Satisfy dependencies for any pending comms,
   *        Clean up pings, and check if collective done.
   *        This calls clear_action to do clean up.
   *        Should only be called for actions that became active
   * @param ac
   */
  void comm_action_done(action* ac);

  /**
   * @brief Satisfy dependences and check if done.
   *        Unlike action_done, this can be called for actions
   *        that stopped early and never became active
   * @param ac
   * @param m
   */
  void clear_action(action* ac);

  void clear_dependencies(action* ac);

  action* comm_action_done(action::type_t ty, int round, int partner);

  void fail_actions(int dense_rank);

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

  default_slicer* slicer_;

 private:
  active_map active_comms_;
  pending_map pending_comms_;
  std::list<action*> completed_actions_;

  pending_msg_map pending_send_headers_;
  pending_msg_map pending_recv_headers_;

  struct action_compare {
    bool operator()(action* l, action* r) const {
      return l->id < r->id;
    }
  };

  std::set<action*, action_compare> initial_actions_;



};

class bruck_actor : public dag_collective_actor
{
 protected:
  void
  compute_tree(int& log2nproc, int& midpoint,
               int& num_rounds, int& nprocs_extra_round) const;

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

  /**
   * @brief real_to_virtual
   * @param rank
   * @param virtual_ranks An array large enough to hold the number of ranks
   * @return The number of virtual ranks
   */
  int
  real_to_virtual(int rank, int* virtual_ranks) const;

  int
  virtual_nproc() const {
    return virtual_nproc_;
  }

  int
  nproc() const {
    return nproc_;
  }

  void
  init(int nproc, int virtual_nproc) {
    nproc_ = nproc;
    virtual_nproc_ = virtual_nproc;
  }

  virtual_rank_map(int nproc, int virtual_nproc) :
    nproc_(nproc), virtual_nproc_(virtual_nproc)
  {
  }

  virtual_rank_map() : nproc_(-1), virtual_nproc_(-1)
  {
  }

 protected:
  int nproc_;

  int virtual_nproc_;

};



}


#endif // COLLECTIVE_ACTOR_H