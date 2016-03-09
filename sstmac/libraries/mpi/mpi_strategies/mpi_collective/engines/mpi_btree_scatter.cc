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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_btree_scatter.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/bt_scatter_iterator.h>

#include <sstmac/common/event_handler.h>
#include <sprockit/errors.h>

#define mpi_scatter_debug(...) \
  mpi_coll_debug("MPI_Scatter binary tree", __VA_ARGS__)


namespace sstmac {
namespace sw {

//
// Given an absolute rank, return the relative rank.
//
int
mpi_btree_scatter::relrank(mpi_id therank) const
{
  return ((therank.id_ - root_.id_ + comm_->size().id_) % comm_->size().id_);
}

//
// Given a relative rank, return the absolute rank.
//
mpi_id
mpi_btree_scatter::absrank(int therank) const
{
  return mpi_id((therank + root_.id_) % comm_->size().id_);
}

//
// Start our zero or one receive..
//
void
mpi_btree_scatter::init_recv()
{
  mpi_scatter_debug("init recv");
  // Figure out where we are receiving from.
  const int relative_rank = relrank(comm_->rank());
  if (relative_rank == 0) {
    // We are root -- charge straight to start_send.
    this->init_sends();
    return;
  }
  int keymask = 1;
  while (!(relative_rank & keymask)) {
    keymask <<= 1;
  }
  const int relative_source = relative_rank - keymask;
  int endrecv = relative_rank + keymask;
  if (endrecv > comm_->size()) {
    endrecv = comm_->size();
  }
  const int entries = endrecv - relative_rank;
  const int length = entries * recvcount_;
  ++pending_recvs_;
  this->start_recv(length, recvtype_, tag_, absrank(relative_source));
}

//
// Start our zero or more sends.
//
void
mpi_btree_scatter::init_sends()
{
  mpi_scatter_debug("init sends")

  // We should never get here with a pending receive.
  if (pending_recvs_) {
    spkt_throw(sprockit::illformed_error,
              "mpibtreescatter::init_sends: already have pending receive");
  }

  const int relative_rank = relrank(comm_->rank());
  const int size = comm_->size();
  int keymask = 1;
  if (relative_rank == 0) {
    // Root node -- we are interested in one-past MSB
    int v = size;
    int shift = 0;
    while (v) {
      ++shift;
      v >>= 1;
    }
    keymask = 1 << shift;
  }
  else {
    while (!(relative_rank & keymask)) {
      keymask <<= 1;
    }

    mpi_scatter_debug("relative rank %d has keymask %d",
        relative_rank, keymask);
  }
  // Figure out initial send/receive pair (if any).
  int localmask = keymask >> 1;
  int endsend = relative_rank + keymask;
  if (endsend > size) {
    endsend = size;
  }


  // And start.
  //compute the number of pending sends
  while (localmask > 0) {
    int relative_dest = relative_rank + localmask;
    if (relative_dest >= size) {
      localmask >>= 1;
      continue;
    }
    ++pending_sends_;
    localmask >>= 1;
  }

   //now actually do the sends
  localmask = keymask >> 1;
   while (localmask > 0) {
    int relative_dest = relative_rank + localmask;
    if (relative_dest >= size) {
      localmask >>= 1;
      continue;
    }
    const int entries = endsend - relative_dest;
    const int length = entries * sendcount_;
    mpi_id rrank = absrank(relative_dest);
    this->start_send(length, sendtype_, tag_, rrank, content_);
    endsend = relative_dest;
    localmask >>= 1;
  }


  // Some of the nodes are not doing any sends.
  if (pending_sends_ == 0 && pending_recvs_ == 0) {
    if (content_) {
      mpi_collective::complete(content_->get_content()[comm_->rank()]);
    }
    else {
      mpi_collective::complete(payload::const_ptr());
    }
  }

}

//
// Hi.
//
mpi_btree_scatter::mpi_btree_scatter(mpi_request* thekey,
                                     mpi_queue* queue, int sendcount,
                                     mpi_type_id sendtype, int recvcount, mpi_type_id recvtype,
                                     mpi_id root, mpi_tag tag, mpi_comm* comm,
                                     const std::vector<payload::const_ptr > &cc,
                                     event_handler* completion) :
  mpi_collective(thekey, queue, tag, comm, completion),
  sendcount_(sendcount), sendtype_(sendtype),
  recvcount_(recvcount), recvtype_(recvtype),
  root_(root),
  pending_sends_(0), pending_recvs_(0)
{
  if (cc.size()) {
    content_ = new mpi_collective_payload(cc);
  }
#if SSTMAC_SANITY_CHECK
  if (sendtype_ == -1 || recvtype_ == -1){
    spkt_throw(sprockit::value_error,
        "mpi_btree_scatter::invalid recv/send types");
  }
#endif
}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_btree_scatter::send_complete(mpi_message* msg)
{
  mpi_scatter_debug("send complete to %d, count=%d", int(msg->dest()), msg->count());
  --pending_sends_;
  if (!pending_sends_) {
    if (content_) {
      mpi_collective::complete(content_->get_content()[comm_->rank()]);
    }
    else {
      mpi_collective::complete(content_);
    }
  }
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_btree_scatter::recv_complete(mpi_message* msg)
{
  mpi_scatter_debug("recv complete from %d, count=%d", int(msg->source()), msg->count());
  if (content_) {
    mpi_collective_payload::const_ptr incoming = ptr_safe_cast(const mpi_collective_payload, msg->content());
    content_ = new mpi_collective_payload(incoming);
  }

  --pending_recvs_;
  if (!pending_recvs_) {
    this->init_sends();
  }
}

//
// Goodbye.
//
mpi_btree_scatter::~mpi_btree_scatter() throw ()
{
}

//
// Start this kernel.
//
void
mpi_btree_scatter::start()
{
  mpi_scatter_debug("starting");
  mpi_collective::start();
  this->init_recv();
}

}
} // end of namespace sstmac

