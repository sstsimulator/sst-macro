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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_MPIQUEUE_H_INCLUDED


#include <sumi-mpi/mpi_message.h>
#include <sumi-mpi/mpi_request.h>
#include <sumi-mpi/mpi_comm/mpi_comm.h>
#include <sumi-mpi/mpi_types/mpi_type.h>

#include <sstmac/common/event_scheduler_fwd.h>

#include <sprockit/factories/factory.h>

#include <sstmac/common/event_manager_fwd.h>

#include <sumi-mpi/mpi_api_fwd.h>
#include <sumi-mpi/mpi_status_fwd.h>
#include <sumi-mpi/mpi_protocol/mpi_protocol_fwd.h>

#include <sumi-mpi/mpi_queue/mpi_queue_recv_request_fwd.h>
#include <sumi-mpi/mpi_queue/mpi_queue_probe_request_fwd.h>

#include <queue>
#include <sstmac/common/timestamp.h>

namespace sumi {

class mpi_queue
{

  /** temporary fix until I figure out a better way to do this */
  friend class mpi_protocol;
  friend class eager0;
  friend class eager1;
  friend class rendezvous_get;
  friend class mpi_queue_send_request;
  friend class mpi_queue_recv_request;

 public:
  mpi_queue(sprockit::sim_parameters* params, int task_id, mpi_api* api);

  ~mpi_queue() throw ();

  static void delete_statics();

  void send(mpi_request* key, int count, MPI_Datatype type,
       int dest, int tag, mpi_comm* comm,
       void* buffer);

  void recv(mpi_request* key, int count, MPI_Datatype type,
       int source, int tag, mpi_comm* comm,
       void* buffer = 0);

  void probe(mpi_request* key, mpi_comm* comm,
        int source, int tag);

  bool iprobe(mpi_comm* comm, int source, int tag, MPI_Status* stat);

  mpi_api* api() const {
    return api_;
  }

  void memcopy(uint64_t bytes);

  double now() const;

  void finalize_recv(mpi_message* msg,
                mpi_queue_recv_request* req);

  sstmac::timestamp progress_loop(mpi_request* req);

  void nonblocking_progress();

  void start_progress_loop(const std::vector<mpi_request*>& req);

  void start_progress_loop(const std::vector<mpi_request*>& req,
                      sstmac::timestamp timeout);

  void finish_progress_loop(const std::vector<mpi_request*>& req);

  void forward_progress(double timeout);

  void buffer_unexpected(mpi_message* msg);

  void post_rdma(mpi_message* msg,
    bool needs_send_ack,
    bool needs_recv_ack);

  void incoming_new_message(mpi_message* message);

  int pt2pt_cq_id() const{
    return pt2pt_cq_;
  }

  int coll_cq_id() const {
    return coll_cq_;
  }

 private:
  struct sortbyseqnum {
    bool operator()(mpi_message* a, mpi_message*b) const;
  };

  typedef std::set<mpi_message*, sortbyseqnum> hold_list_t;

 private:
  /**
   * @brief incoming_pt2pt_message Message might be held up due to sequencing constraints
   * @param msg
   */
  void incoming_pt2pt_message(sumi::message* msg);

  /**
   * @brief handle_pt2pt_message Message is guaranteed to satisfy sequencing constraints
   * @param msg
   */
  void handle_pt2pt_message(mpi_message* msg);

  void incoming_collective_message(sumi::message* message);

  void incoming_message(sumi::message* message);

  void notify_probes(mpi_message* message);

  mpi_message* find_matching_recv(mpi_queue_recv_request* req);
  mpi_queue_recv_request* find_matching_recv(mpi_message* msg);

  void clear_pending();

  bool at_least_one_complete(const std::vector<mpi_request*>& req);

 private:
  /// The sequence number for our next outbound transmission.
  std::unordered_map<task_id, int> next_outbound_;

  /// The sequence number expected for our next inbound transmission.
  std::unordered_map<task_id, int> next_inbound_;

  /// Hold messages that arrived out of order.
  std::unordered_map<task_id, hold_list_t> held_;

  /// Inbound messages waiting for a matching receive request.
  std::list<mpi_message*> need_recv_match_;
  std::list<mpi_queue_recv_request*> need_send_match_;

  std::vector<mpi_protocol*> protocols_;

  /// Probe requests watching
  std::list<mpi_queue_probe_request*> probelist_;

  task_id taskid_;

  app_id appid_;

  mpi_api* api_;

  int max_vshort_msg_size_;
  int max_eager_msg_size_;

  int pt2pt_cq_;
  int coll_cq_;

};

}

#define mpi_queue_action_debug(rank, ...) \
  mpi_debug(rank, sprockit::dbg::mpi_queue, \
   " [queue] %s", sprockit::printf(__VA_ARGS__).c_str())

//for local use in mpi queue object
#define mpi_queue_debug(...) \
  mpi_queue_action_debug(int(taskid_), __VA_ARGS__)

#define mpi_queue_protocol_debug(...) \
  mpi_queue_action_debug(mpi_->rank(), __VA_ARGS__)


#endif
