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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_direct_gatherv.h>
#include <sstmac/common/event_handler.h>
#include <sprockit/errors.h>
#include <deque>

#define mpi_gatherv_debug(...) \
  mpi_coll_debug("MPI_Gatherv direct", __VA_ARGS__)

namespace sstmac {
namespace sw {

//
// Hi.
//
mpi_direct_gatherv::mpi_direct_gatherv(mpi_request* thekey,
                                       mpi_queue* queue,
                                       int sendcount, mpi_type_id sendtype,
                                       const std::vector<int> &recvcount,
                                       mpi_type_id recvtype,
                                       mpi_id root, mpi_tag tag,
                                       mpi_comm* comm,
                                       const payload::const_ptr& content,
                                       event_handler* completion) :
  mpi_collective(thekey, queue, tag, comm, completion),
  sendcount_(sendcount), sendtype_(sendtype),
  recvcount_(recvcount), recvtype_(recvtype),
  root_(root),
  pending_sends_(0), pending_recvs_(0)
{

  bool is_root = comm_->rank() == root_;

  if(is_root) {

    std::vector<payload::const_ptr> vals;

    for(int i = 0; i < int(comm->size()); i++) {
      vals.push_back(payload::const_ptr());
    }

    vals[comm->rank()] = content;

    content_ = new mpi_collective_payload(vals);

  }
  else {
    send_content_ = content;
  }

}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_direct_gatherv::send_complete(const mpi_message::ptr& msg)
{

  mpi_gatherv_debug("send complete to %d, count=%d", int(msg->dest()), msg->count());

  --pending_sends_;
  if(! pending_sends_) {
    payload::const_ptr content;
    if((comm_->rank() == root_)) {

      content = content_;
    }
    mpi_collective::complete(content);
  }
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_direct_gatherv::recv_complete(const mpi_message::ptr& msg)
{
  mpi_gatherv_debug("recv complete from %d, count=%d", int(msg->source()), msg->count());

  payload::const_ptr load = msg->content();

  mpi_id src = msg->source();

  content_->set_content(load, src);

  --pending_recvs_;
  if(! pending_recvs_) {
    if((comm_->rank() == root_)) {
      mpi_collective::complete(content_);
    }
  }
}

//
// Goodbye.
//
mpi_direct_gatherv::~mpi_direct_gatherv() throw()
{
}

//
// Start this kernel.
//
void mpi_direct_gatherv::start()
{
  mpi_gatherv_debug("starting");
  mpi_collective::start();
  if(comm_->rank() == root_) {
    // Root node -- fire off a bunch of receives.
    mpi_id size = comm_->size();
    pending_recvs_ = int(comm_->size()) - 1;
    if(pending_recvs_ == 0) {
      payload::const_ptr content;
      if((comm_->rank() == root_)) {
        content = content_;
      }
      mpi_collective::complete(content);
    }
    else {
      for(mpi_id dest(0); dest < size; ++dest) {
        if(dest != root_) {
          this->start_recv(recvcount_.at(dest), recvtype_, tag_, dest);
        }
      }
    }
  }
  else {
    // Client node -- fire off a single send.
    pending_sends_ = 1;
    this->start_send(sendcount_, sendtype_, tag_, root_, send_content_);
  }
}

}
} // end of namespace sstmac

