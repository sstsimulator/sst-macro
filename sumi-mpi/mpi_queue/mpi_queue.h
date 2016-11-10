/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_H_INCLUDED


#include <sumi-mpi/mpi_message.h>
#include <sumi-mpi/mpi_request.h>
#include <sumi-mpi/mpi_comm/mpi_comm.h>
#include <sumi-mpi/mpi_types/mpi_type.h>

#include <sstmac/software/libraries/compute/lib_compute_memmove.h>
#include <sstmac/software/libraries/compute/lib_compute_time.h>

#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_scheduler_fwd.h>

#include <sprockit/factories/factory.h>

#include <sstmac/common/event_manager_fwd.h>
#include <sstmac/common/stats/stat_spyplot_fwd.h>

#include <sumi-mpi/mpi_api_fwd.h>
#include <sumi-mpi/mpi_status_fwd.h>
#include <sumi-mpi/mpi_protocol/mpi_protocol_fwd.h>

#include <sumi-mpi/mpi_queue/mpi_queue_recv_request_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_send_request_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_probe_request_fwd.h>

#include <queue>

namespace sumi {

using sstmac::event_manager;
using sstmac::event_handler;
using sstmac::sw::operating_system;
using sstmac::sw::software_id;
using sstmac::timestamp;
using sstmac::sw::task_id;
using sstmac::sw::app_id;

class mpi_queue
{

  /** temporary fix until I figure out a better way to do this */
  friend class mpi_protocol;
  friend class eager0;
  friend class eager1;
  friend class eager1_singlecpy;
  friend class eager1_doublecpy;
  friend class rendezvous_protocol;
  friend class rendezvous_get;
  friend class mpi_queue_send_request;
  friend class mpi_queue_recv_request;

 public:
  mpi_queue(sprockit::sim_parameters* params, sstmac::sw::software_id sid, mpi_api* api);

  /// Goodbye.
  ~mpi_queue() throw ();

  static void
  delete_statics();

  void
  send(mpi_request* key, int count, MPI_Datatype type,
       int dest, int tag, mpi_comm* comm,
       void* buffer);

  void
  recv(mpi_request* key, int count, MPI_Datatype type,
       int source, int tag, mpi_comm* comm,
       void* buffer = 0);

  void
  probe(mpi_request* key, mpi_comm* comm,
        int source, int tag);

  bool
  iprobe(mpi_comm* comm, int source, int tag, MPI_Status* stat);

  void
  incoming_progress_loop_message(const mpi_message::ptr& message);

  mpi_protocol*
  protocol(long bytes) const;

  mpi_api*
  api() const {
    return api_;
  }

  double
  now() const;

  sstmac::sw::lib_compute_memmove*
  user_lib_mem() const {
    return user_lib_mem_;
  }

  sstmac::sw::lib_compute_time*
  user_lib_time() const {
    return user_lib_time_;
  }

  void
  finalize_recv(const mpi_message::ptr& msg,
                mpi_queue_recv_request* req);

  timestamp
  progress_loop(mpi_request* req);

  void
  start_progress_loop(const std::vector<mpi_request*>& req);

  void
  start_progress_loop(const std::vector<mpi_request*>& req,
                      timestamp timeout);

  void
  finish_progress_loop(const std::vector<mpi_request*>& req);

  void forward_progress(double timeout);

  void
  buffer_unexpected(const mpi_message::ptr& msg);

  void
  post_rdma(const mpi_message::ptr& msg,
    bool needs_send_ack,
    bool needs_recv_ack);

  void
  post_header(const mpi_message::ptr& msg, bool needs_ack);

 private:
  struct sortbyseqnum {
    bool operator()(const mpi_message::ptr& a, const mpi_message::ptr&b) const;
  };

  typedef std::set<mpi_message::ptr, sortbyseqnum, std::allocator<mpi_message::ptr> >
  hold_list_t;

  typedef std::list<mpi_queue_recv_request*> pending_message_t;

  typedef std::list<mpi_message::ptr> need_recv_t;

  typedef std::list<mpi_queue_send_request*> send_needs_ack_t;

  typedef spkt_unordered_map<int, mpi_message::ptr> reorderlist_t;

  typedef std::map<mpi_message::id, mpi_queue_send_request*> ack_needed_t;

  typedef std::map<mpi_message::id, mpi_queue_recv_request*> pending_req_map;

  typedef std::list<mpi_queue_probe_request*> probelist_t;

 private:
  void handle_poll_msg(const sumi::message::ptr& msg);

  void
  handle_collective_done(const sumi::message::ptr& msg);

  void
  incoming_completion_ack(const mpi_message::ptr& message);

  void
  incoming_new_message(const mpi_message::ptr& message);

  void
  handle_nic_ack(const mpi_message::ptr& message);

  void
  complete_nic_ack(const mpi_message::ptr& message);

  void
  handle_new_message(const mpi_message::ptr& message);

  void
  notify_probes(const mpi_message::ptr& message);

  mpi_queue_recv_request*
  pop_matching_request(pending_message_t& pending, const mpi_message::ptr& message);

  mpi_queue_recv_request*
  pop_pending_request(const mpi_message::ptr& message,
                       bool set_need_recv = true);

  mpi_queue_recv_request*
  pop_waiting_request(const mpi_message::ptr& message);

  mpi_message::ptr
  find_matching_recv(mpi_queue_recv_request* req);

  void
  send_completion_ack(const mpi_message::ptr& message);

  mpi_message::ptr
  send_message(int count, MPI_Datatype type,
    int dst_rank, int tag, mpi_comm* comm);

  void
  configure_send_request(const mpi_message::ptr& mess, mpi_request* req);

  void clear_pending();

  bool
  at_least_one_complete(const std::vector<mpi_request*>& req);

 private:
  sstmac::stat_spyplot* spy_num_messages_;
  sstmac::stat_spyplot* spy_bytes_;

  /// The sequence number for our next outbound transmission.
  spkt_unordered_map<task_id, int> next_outbound_;

  /// The sequence number expected for our next inbound transmission.
  spkt_unordered_map<task_id, int> next_inbound_;

  /// Hold messages that arrived out of order.
  spkt_unordered_map<task_id, hold_list_t> held_;

  /// The (locally unique) id that will be given to the next message.
  mpi_message::id next_id_;

  pending_message_t pending_message_;

  pending_message_t waiting_message_;

  pending_message_t in_flight_messages_;

  /// Inbound messages waiting for a matching receive request.
  need_recv_t need_recv_;

  /// Save all sends so we can match up the nic acks that come back
  send_needs_ack_t send_needs_nic_ack_;
  send_needs_ack_t send_needs_eager_ack_;

  ack_needed_t send_needs_completion_ack_;

  /// Requests that sent an ack and are waiting for the rest of their data.
  pending_req_map recv_needs_payload_;

  /// Probe requests watching for a given envelope.
  probelist_t probelist_;

  task_id taskid_;

  app_id appid_;

  mpi_api* api_;

  /// A blocking memcpy library for buffered sends in which
  /// the memcpy happens in the application
  sstmac::sw::lib_compute_memmove* user_lib_mem_;
  sstmac::sw::lib_compute_time* user_lib_time_;

  sstmac::sw::operating_system* os_;

  int max_vshort_msg_size_;
  int max_eager_msg_size_;

  timestamp post_rdma_delay_;

  timestamp post_header_delay_;

  timestamp poll_delay_;

};

}

#define mpi_queue_action_debug(rank, ...) \
  mpi_debug(rank, sprockit::dbg::mpi_queue, \
   " [queue] %s", sprockit::printf(__VA_ARGS__).c_str())

//for local use in mpi queue object
#define mpi_queue_debug(...) \
  mpi_queue_action_debug(int(taskid_), __VA_ARGS__)


#endif

