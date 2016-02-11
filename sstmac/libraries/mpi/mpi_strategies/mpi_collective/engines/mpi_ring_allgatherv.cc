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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_ring_allgatherv.h>
#include <sprockit/errors.h>

#define mpi_allgatherv_debug(...) \
  mpi_coll_debug("MPI_Allgatherv ring algorithm", __VA_ARGS__)

namespace sstmac {
namespace sw {

//
// Hello.
//
mpi_ring_allgatherv::mpi_ring_allgatherv(mpi_request* thekey,
    mpi_queue* queue,
    int sendcnt, mpi_type_id sendtype,
    const std::vector<int> &recvcnt,
    mpi_type_id recvtype,
    mpi_tag tag,
    mpi_comm* comm,
    const payload::const_ptr& content,
    event_handler* completion) :
  mpi_collective(thekey, queue, tag, comm, completion), sendcnt_(sendcnt),
  sendtype_(sendtype),
  recvcnt_(recvcnt), recvtype_(recvtype),
  source_((int(comm->size()) + int(comm->rank()) - 1) % int(comm->size())),
  dest_((int(comm->rank()) + 1) % int(comm->size())),
  pending_sends_(0), pending_recvs_(0)
{
  std::vector<payload::const_ptr > vals;
  for (int i = 0; i < int(comm->size()); i++) {
    if (i == int(comm->rank())) {
      vals.push_back(content);
    }
    else {
      vals.push_back(payload::const_ptr());
    }
  }

  content_ = new mpi_collective_payload(vals);
}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_ring_allgatherv::send_complete(const mpi_message::ptr& msg)
{
  mpi_allgatherv_debug("send complete to %d, count=%d", int(msg->dest()), msg->count());
  --pending_sends_;
  if(pending_sends_ == 0 && pending_recvs_ == 0) {
    mpi_collective::complete(content_);
  }
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_ring_allgatherv::recv_complete(const mpi_message::ptr& msg)
{
  mpi_allgatherv_debug("recv complete from %d, count=%d", int(msg->source()), msg->count());

  --pending_recvs_;

  payload::const_ptr pay = msg->content();
  if (pay) {
    mpi_collective_payload::ptr other = new mpi_collective_payload(
        ptr_safe_cast(const mpi_collective_payload, pay->clone()));

    std::vector<payload::const_ptr > realload = other->get_content();

    std::vector<payload::const_ptr>::iterator it, end = realload.end();

    int i = 0;
    for (it = realload.begin(); it != end; it++) {
      if (*it) {
        mpi_allgatherv_debug("setting content for position %d", i);
        content_->set_content(*it, i);
      }
      i++;
    }
  }

  if(! iteration_.empty()) {
    // Next send/receive pair.
    srpair sr = iteration_.front();
    iteration_.pop_front();
    ++pending_sends_;
    ++pending_recvs_;
    this->start_recv(sr.recvcnt, recvtype_, tag_, source_);
    this->start_send(sr.sendcnt, sendtype_, tag_, dest_, content_);
  }
  if(pending_sends_ == 0 && pending_recvs_ == 0) {

    mpi_collective::complete(content_);
  }
}

//
// Goodbye.
//
mpi_ring_allgatherv::~mpi_ring_allgatherv() throw() {}

//
// Get busy child.
//
void mpi_ring_allgatherv::start()
{
  mpi_allgatherv_debug("starting");
  mpi_collective::start();
  // Catch situations where comm_->size() is 1.
  if(comm_->size().id_ <= 1) {
    mpi_collective::complete(content_);
  }
  else {
    // Set up the iteration_ queue.
    const int size = comm_->size();
    const int rank = comm_->rank();
    for(int offset = 1; offset < size; ++offset) {
      int receiving_data_owner = (size + rank - offset) % size;
      int sending = sendcnt_;
      int recving = recvcnt_.at(receiving_data_owner);
      iteration_.push_back(srpair(sending, recving));
    }
    // start the first send/receive pair.
    srpair first = iteration_.front();
    iteration_.pop_front();
    pending_sends_ = pending_recvs_ = 1;
    this->start_recv(first.recvcnt, recvtype_, tag_, source_);
    this->start_send(first.sendcnt, sendtype_, tag_, dest_, content_);
  }
}



}
} // end of namespace sstmac

