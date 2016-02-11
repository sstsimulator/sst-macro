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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_bcast_engine.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/topology_iterator.h>
#include <sprockit/errors.h>


#define mpi_bcast_debug(...) \
  mpi_coll_debug("MPI_Bcast", __VA_ARGS__)

namespace sstmac {
namespace sw {

// A utility type for sendrecv
// Although this is very unlikely to happen for bcast, there is the
// possibility of ending up with recursive calls to sendrecv (and an
// invalid iterator) if we don't separate iteration from communications
struct sendrecvop {
  bool sending;
  mpi_id partner;
};

//
// Start next cycle of sends and receives.
//
void
mpi_bcast_engine::sendrecv()
{
  if (pending_recvs_) {
    spkt_throw(sprockit::illformed_error, "mpibcastengine::sendrecvs: pending receive");
  }
  std::vector<sendrecvop> ops;
  while (iter_->active()) {
    if (iter_->sending()) {
      sendrecvop op =
      { true, mpi_id(iter_->dest()) };
      ops.push_back(op);
    }
    if (iter_->receiving()) {
      sendrecvop op =
      { false, mpi_id(iter_->source()) };
      ops.push_back(op);
      // We can only safely handle one receive at a time.
      // (a bcast will generally receive zero or one time,
      // followed by zero or more sends).
      iter_->next();
      break;
    }
    iter_->next();
  }

  mpi_bcast_debug("firing off %d send/recvs on tag", ops.size(), int(tag_));

  // Do any sends/receives.
  for (size_t it = 0; it < ops.size(); ++it) {
    if (!ops[it].sending) {
      ++pending_recvs_;
      this->start_recv(count_, type_, tag_, ops[it].partner);
    }
    else {
      ++pending_sends_;
      this->start_send(count_, type_, tag_, ops[it].partner, content_);
    }
  }
  // If we have no pending sends or receives, we are done.
  if (!completing_ && pending_sends_ == 0 && pending_recvs_ == 0) {
    completing_ = true;
    mpi_collective::complete(content_);
  }
}

//
// Hi.
//
mpi_bcast_engine::mpi_bcast_engine(topology_iterator* iter,
                                   mpi_request* thekey, mpi_queue* queue, int count,
                                   mpi_type_id type, mpi_id root, mpi_tag tag,
                                   mpi_comm* comm, const payload::const_ptr& content,
                                   event_handler* completion) :
  mpi_collective(thekey, queue, tag, comm, completion), iter_(iter),
  count_(count), type_(type), rank_(comm->rank()),
  pending_sends_(0), pending_recvs_(0)
{
  completing_ = false;
  is_root_ = (comm->rank() == root);
  mpi_coll_debug("MPI_Bcast", "constructing enginge with root %d", int(root));
  if (comm->rank() == root) {
    if (content) {
      content_ = content;
    }
  }
}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_bcast_engine::send_complete(const mpi_message::ptr& msg)
{
  mpi_bcast_debug("send complete to %d, count=%d", msg->count(), int(msg->dest()));
  --pending_sends_;
  if (!pending_recvs_) {
    this->sendrecv();
  }
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_bcast_engine::recv_complete(const mpi_message::ptr& msg)
{
  mpi_bcast_debug("recv complete from %d, count=%d", msg->count(), int(msg->source()));

  if (!is_root_) {
    content_ = msg->content();
  }

  --pending_recvs_;
  this->sendrecv();
}

//
// Goodbye.
//
mpi_bcast_engine::~mpi_bcast_engine() throw ()
{
  delete iter_;
}

//
// Start this kernel.
//
void
mpi_bcast_engine::start()
{
  mpi_collective::start();
  this->sendrecv();
}

}
} // end of namespace sstmac


