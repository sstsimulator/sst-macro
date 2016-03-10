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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_btree_gather.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/bt_scatter_iterator.h>

#include <sstmac/common/event_handler.h>
#include <sstmac/common/messages/vector_payload.h>
#include <sprockit/errors.h>

#include <deque>

namespace sstmac {
namespace sw {


#define mpi_gather_debug(...) \
  mpi_coll_debug("MPI_Gather binary tree", __VA_ARGS__)

//
// Given an absolute rank, return the relative rank.
//
int
mpi_btree_gather::relrank(mpi_id therank) const
{
  return ((therank.id_ - root_.id_ + comm_->size().id_) % comm_->size().id_);
}

//
// Given a relative rank, return the absolute rank.
//
mpi_id
mpi_btree_gather::absrank(int therank) const
{
  return mpi_id((therank + root_.id_) % comm_->size().id_);
}

//
// Fire off any and all receives concurrently.
//
void
mpi_btree_gather::init_recvs()
{
  const int relative_rank = relrank(comm_->rank());
  const int size = comm_->size();
  int keymask = 1;
  if (relative_rank == 0) {
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
  }
  // Figure out the sources for this node.
  int localsrc = 1;
  std::vector<mpisource> sources;
  while (localsrc < keymask) {
    const int source = relative_rank + localsrc;
    if (source >= size) {
      break;
    }
    const int entries = ((source + localsrc <= size) ? localsrc : size
                         - source);
    const int length = entries * recvcount_;
    ++pending_recvs_;
    sources.push_back(mpisource(absrank(source), length));
    localsrc <<= 1;
  }
  mpi_gather_debug("init with %d pending recvs", pending_recvs_);

  if (pending_recvs_) {
    // Fire off all receives at once.
    for (size_t i = 0; i < sources.size(); ++i) {
      mpi_gather_debug("start recv from rank %d", int(sources[i].source));
      this->start_recv(sources[i].count, recvtype_, tag_, sources[i].source);
    }
  }
  else {
    // Leaf node -- no receives, just sends.
    init_send();
  }
}

//
// Fire off the send operation (if any).  Can be called from root.
//
void
mpi_btree_gather::init_send()
{
  // We should only be here when all receives are done.
  if (pending_recvs_) {
    spkt_throw(sprockit::illformed_error,
        "mpi_btree_gather::init_send: pending receives already");
  }
  const int relative_rank = relrank(comm_->rank());
  const int size = comm_->size();
  if (relative_rank != 0) {
    // We need to do a send.
    int keymask = 1;
    while (!(relative_rank & keymask)) {
      keymask <<= 1;
    }
    const int localdest = relative_rank - keymask;
    const int entries = ((relative_rank + keymask) <= size ? keymask : size
                         - relative_rank);
    const int length = entries * sendcount_;
    ++pending_sends_;
    mpi_gather_debug("init send to rank %d", int(absrank(localdest)));
    this->start_send(length, sendtype_, tag_, absrank(localdest), content_);
  }
  else if (pending_sends_ <= 0 && pending_recvs_ <= 0) {
    mpi_collective::complete(content_);
  }
}

//
// Hi.
//
mpi_btree_gather::mpi_btree_gather(mpi_request* thekey,
                                   mpi_queue* queue,
                                   int sendcount, mpi_type_id sendtype,
                                   int recvcount, mpi_type_id recvtype,
                                   mpi_id root, mpi_tag tag, mpi_comm* comm,
                                   const payload::const_ptr& content,
                                   event_handler* completion) :
  mpi_collective(thekey, queue, tag, comm, completion), sendcount_(sendcount),
  sendtype_(sendtype), recvcount_(recvcount), recvtype_(recvtype),
  root_(root), tag_(tag), pending_sends_(0), pending_recvs_(0)
{
  if (content) {
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
}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_btree_gather::send_complete(mpi_message* msg)
{
  mpi_gather_debug("send complete to %d, count=%d", int(msg->dest()), msg->count());
  --pending_sends_;
  if (!pending_sends_) {
    mpi_collective::complete(content_);
  }
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_btree_gather::recv_complete(mpi_message* msg)
{
  mpi_gather_debug("recv complete from %d, count=%d", int(msg->source()), msg->count());

  payload::const_ptr pay = msg->content();
  if (pay) {
    mpi_collective_payload::const_ptr other = new mpi_collective_payload(
          ptr_safe_cast(const mpi_collective_payload, pay->clone()));

    std::vector<payload::const_ptr> realload = other->get_content();

    std::vector<payload::const_ptr >::iterator it, end = realload.end();

    int i = 0;
    for (it = realload.begin(); it != end; it++) {
      if (*it) {
        mpi_gather_debug("setting content for position %d", i);
        content_->set_content(*it, i);
      }
      i++;
    }
  }

  --pending_recvs_;
  if (!pending_recvs_) {
    this->init_send();
  }
}


//
// Goodbye.
//
mpi_btree_gather::~mpi_btree_gather() throw ()
{
}

//
// Start this kernel.
//
void
mpi_btree_gather::start()
{
  mpi_gather_debug("starting");
  mpi_collective::start();
  this->init_recvs();
}

}
} // end of namespace sstmac


