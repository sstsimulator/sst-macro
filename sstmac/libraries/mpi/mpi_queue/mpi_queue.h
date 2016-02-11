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


#include <sstmac/libraries/mpi/mpi_message.h>
#include <sstmac/libraries/mpi/mpi_request.h>
#include <sstmac/libraries/mpi/mpi_implementation/mpi_implementation.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_id.h>
#include <sstmac/libraries/mpi/rma/mpi_window.h>

#include <sstmac/software/libraries/compute/lib_compute_memmove.h>
#include <sstmac/software/libraries/compute/lib_compute_time.h>

#include <sstmac/common/event_handler.h>
#include <sstmac/common/messages/payload.h>

#include <sprockit/factories/factory.h>

#include <sstmac/common/event_manager_fwd.h>
#include <sstmac/common/stats/stat_spyplot_fwd.h>

#include <sstmac/libraries/mpi/mpi_api_fwd.h>
#include <sstmac/libraries/mpi/mpi_status_fwd.h>
#include <sstmac/libraries/mpi/mpi_server_fwd.h>
#include <sstmac/libraries/mpi/mpi_implementation/mpi_implementation_fwd.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol_fwd.h>

#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_recv_request_fwd.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_send_request_fwd.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_probe_request_fwd.h>
#include <sstmac/libraries/mpi/rma/mpi_queue_get_request_fwd.h>

#include <queue>

namespace sstmac {
namespace sw {

struct app_id;

class mpi_queue :
  public sprockit::factory_type
{

  /** temporary fix until I figure out a better way to do this */
  friend class mpi_server;
  friend class mpi_protocol;
  friend class eager0_protocol;
  friend class eager0_socket;
  friend class eager0_rdma;
  friend class eager0_mmap;
  friend class eager1_rdma;
  friend class eager1_rdma_singlecpy;
  friend class eager1_rdma_doublecpy;
  friend class rendezvous_protocol;
  friend class rendezvous_mmap;
  friend class rendezvous_socket;
  friend class rendezvous_rdma;
  friend class creme_mpiprotocol;
  friend class eager_ssend;
  friend class rendezvous_rma;
  friend class mpi_queue_send_request;
  friend class mpi_queue_recv_request;

 public:
  struct sendinfo {
    sendinfo() :
      rsend(false), ssend(false), onesided(false) {
    }
    bool rsend;
    bool ssend;
    bool onesided;
  };

 public:
  virtual std::string
  to_string() const {
    return "mpi queue";
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  /// Goodbye.
  virtual ~mpi_queue() throw ();

  mpi_implementation*
  implementation() const {
    return mpi_impl_;
  }

  static void
  delete_statics();

  void
  unregister_all_libs();

  /// Send data.  All sends are triggered at time nodeinterface::now().
  /// Upon completion, the completion object have an updated
  /// status object relating to the send operation.
  void
  send(mpi_request* key, int count, mpi_type_id type,
       mpi_id dest, mpi_tag tag, mpi_comm* comm,
       const sendinfo& sinfo, mpi_message::category message_type,
       event_handler* completion,
       const payload::const_ptr& content = payload::null());

  void
  send(mpi_request* key, int count, mpi_type_id type,
       mpi_id dest, mpi_tag tag, mpi_comm* comm,
       const sendinfo& sinfo, mpi_message::category message_type,
       event_handler* completion,
       void* buffer);

  void
  start_send(const mpi_message::ptr& msg);

  void
  get(mpi_request* key, int count, mpi_type_id type,
      mpi_id dest, mpi_comm* comm,
      mpi_window* win, const mpi_rma_message::op_info& op,
      event_handler* completion);

  void
  put(mpi_request* key, int count, mpi_type_id type,
      mpi_id dest, mpi_comm* comm,
      mpi_window* win, const mpi_rma_message::op_info& op,
      event_handler* completion,
      const payload::const_ptr& content = payload::null());

  void
  recv(mpi_request* key, int count, mpi_type_id type,
       mpi_id source, mpi_tag tag, mpi_comm* comm,
       mpi_message::category message_type, event_handler* completion,
       void* buffer = 0);

  void
  probe(mpi_request* key, mpi_comm* comm,
        mpi_id source, mpi_tag tag,
        event_handler* completion);

  bool
  iprobe(mpi_comm* comm, mpi_id source, mpi_tag tag, mpi_status* stat);

  /// CALLBACK used by mpiserver when this object has a message.
  virtual void
  incoming_message(const mpi_message::ptr& message) = 0;

  void
  set_event_parent(event_scheduler* m);

  virtual void
  init_param1(const software_id& id);

  virtual void
  finalize_init();

  void
  set_api(mpi_api* api){
    api_ = api;
  }

  void
  win_create(mpi_window* win) {
    windows_[win->winid()] = win;
  }

  void
  win_free(mpi_window* win) {
    windows_.erase(win->winid());
  }

  mpi_protocol*
  protocol(long bytes, bool intranode, bool rsend, bool onesided) const;

  mpi_api*
  api() const {
    return api_;
  }

  lib_compute_memmove*
  user_lib_mem() const {
    return user_lib_mem_;
  }

  lib_compute_time*
  user_lib_time() const {
    return user_lib_time_;
  }

  mpi_server*
  server() const {
    return server_;
  }

  void
  finalize_recv(const mpi_message::ptr& msg);

  virtual timestamp
  progress_loop(mpi_request* req) = 0;

  virtual void
  start_progress_loop(const std::vector<mpi_request*>& req) = 0;

  virtual void
  start_progress_loop(const std::vector<mpi_request*>& req,
                      timestamp timeout) = 0;

  virtual void
  finish_progress_loop(const std::vector<mpi_request*>& req) = 0;

  virtual event_handler*
  progress_done_handler(operating_system* os, mpi_request* req) = 0;

  void
  start_recv(mpi_queue_recv_request* req);

  virtual void
  buffered_send(const mpi_message::ptr& msg) = 0;

  virtual void
  buffered_recv(const mpi_message::ptr& msg, mpi_queue_recv_request* req) = 0;

  virtual void
  buffer_unexpected(const mpi_message::ptr& msg) = 0;

  virtual void
  post_rdma(const mpi_message::ptr& msg) = 0;

  virtual void
  post_header(const mpi_message::ptr& msg) = 0;

  void
  handle_put(const mpi_message::ptr& message);

  void
  set_mpi_server(mpi_server* server);

  virtual bool
  is_service_thread() const = 0;

 protected:
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

  typedef std::map<mpi_message::id, mpi_queue_get_request*> get_requests_t;

 protected:
  /// Hi there.
  mpi_queue();

  virtual void
  do_send(const mpi_message::ptr& mess) = 0;

  virtual void
  do_recv(mpi_queue_recv_request* req) = 0;

  /// The incoming_message method forwards to here when the message is
  /// a response to a handshake request.
  void
  incoming_rendezvous_ack(const mpi_message::ptr& message);

  void
  incoming_completion_ack(const mpi_message::ptr& message);

  /// The incoming_message method forwards to here when the message is
  /// a new message, whehter it's an eager send or a new handshake request.
  void
  incoming_new_message(const mpi_message::ptr& message);

  /// Called by the mpiserver when it figured out that the nic is sending
  /// back an ack that indicates that the message was sent
  void
  handle_nic_ack(const mpi_message::ptr& message);

  /// Called by the mpiserver when it figured out that the nic is sending
  /// back an ack that indicates that the message was sent
  void
  complete_nic_ack(const mpi_message::ptr& message);

  /// The incoming_new_message method calls here to complete an
  /// inbound message.  At this point, arrival ordering has been ensured.
  void
  handle_new_message(const mpi_message::ptr& message);

  void
  notify_probes(const mpi_message::ptr& message);

  mpi_queue_recv_request*
  find_request(pending_message_t& pending, const mpi_message::ptr& message);

  mpi_queue_recv_request*
  find_pending_request(const mpi_message::ptr& message,
                       bool set_need_recv = true);

  mpi_queue_recv_request*
  find_waiting_request(const mpi_message::ptr& message);

  mpi_message::ptr
  find_matching_recv(mpi_queue_recv_request* req);

  void
  send_rendezvous_ack(const mpi_message::ptr& message);

  void
  send_completion_ack(const mpi_message::ptr& message);

  std::string
  id_string() const;

  mpi_message::ptr
  send_message(int count, mpi_type_id type,
    mpi_id dst_rank, mpi_tag tag, mpi_comm* comm,
    const sendinfo &sinfo, mpi_message::category message_type);

  void
  configure_send_request(const mpi_message::ptr& mess, mpi_request* req, event_handler* completion);

 private:
  void
  send_debug_printf(const mpi_message::ptr& mess);

 protected:
  stat_spyplot* spy_num_messages_;
  stat_spyplot* spy_bytes_;

  /// Minimum transmission size.
  int64_t mintrans_;

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

  /// Inbound messages waiting for a matching receive request.
  need_recv_t need_recv_;

  /// Save all sends so we can match up the nic acks that come back
  send_needs_ack_t send_needs_nic_ack_;
  send_needs_ack_t send_needs_eager_ack_;

  /// Requests waiting for an ack.
  ack_needed_t send_needs_rendezvous_ack_;
  ack_needed_t send_needs_completion_ack_;

  /// Requests that sent an ack and are waiting for the rest of their data.
  pending_req_map recv_needs_payload_;

  /// Probe requests watching for a given envelope.
  probelist_t probelist_;

  task_id taskid_;

  app_id appid_;

  mpi_implementation* mpi_impl_;

  mpi_api* api_;

  spkt_unordered_map<int, mpi_window*> windows_;

  get_requests_t gets_waiting_for_data_;

  spkt_unordered_map<long, std::queue<mpi_message::ptr> >
  headers_waiting_epoch_;

  /// A blocking memcpy library for buffered sends in which
  /// the memcpy happens in the application
  lib_compute_memmove* user_lib_mem_;
  lib_compute_time* user_lib_time_;

  operating_system* os_;

  mpi_server* server_;

#if SSTMAC_ENABLE_MPI_TIMELINE
 public:
  void print_log();

 private:
  void
  log_completion(const mpi_message::ptr& m);

  struct message_log_data_t {
    timestamp start;
    timestamp finish;
    int bytes;
  };

  typedef std::map<uint64_t, std::list<message_log_data_t> > source_to_log_map;
  source_to_log_map pt2pt_logs_;
  source_to_log_map collective_logs_;

  void
  print_log(std::ostream& os, 
    std::list<message_log_data_t>& theList,
    int src);

  void
  print_log(source_to_log_map& theMap, const char* type);
#else
 public:
  void
  log_completion(const mpi_message::ptr& m){}
#endif

};

DeclareFactory1InitParam(mpi_queue, software_id);

}
} // end of namespace sstmac

#define mpi_queue_action_debug(rank, ...) \
  mpi_debug(rank, sprockit::dbg::mpi_queue, \
   " [queue] %s", sprockit::printf(__VA_ARGS__).c_str())

//for local use in mpi queue object
#define mpi_queue_debug(...) \
  mpi_queue_action_debug(int(taskid_), __VA_ARGS__)


#endif

