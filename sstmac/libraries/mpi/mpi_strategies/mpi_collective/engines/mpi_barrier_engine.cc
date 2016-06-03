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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_barrier_engine.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/topology_iterator.h>
#include <sstmac/common/messages/payload.h>
#include <sprockit/errors.h>

#define mpi_barrier_debug(...) \
  mpi_coll_debug("MPI_Barrier", __VA_ARGS__)

namespace sstmac {
namespace sw {

// Pointer types.


// A utility type for sendrecv
// I figured out that I can actually end up recursively inside sendrecv,
// which will then result in an invalid iterator and major screw-ups.

void
mpi_barrier_engine::maybe_do_sendrecvs()
{
  configure_sendrecv();

  mpi_barrier_debug("maybe do sendrecv: %d active", active_sendrecvs_);
  if (active_sendrecvs_ == 0){
    ++active_sendrecvs_;
    while (!sendrecv_ops_.empty()){
      fire_off(sendrecv_ops_.front());
      sendrecv_ops_.pop_front();
    }
    --active_sendrecvs_;
  }
}

void
mpi_barrier_engine::fire_off(const std::vector<mpi_barrier_engine::sendrecvop>& ops)
{
  mpi_barrier_debug("firing off %d send/recvs : %p", int(ops.size()), ops.data());
  // Fire off all sends and receives.
  for (size_t it = 0; it < ops.size(); ++it) { //receives first
    if (!ops[it].sending) {
      this->start_recv(1, mpi_type::mpi_byte->id, tag_, ops[it].partner);
    }
  }
  for (size_t it = 0; it < ops.size(); ++it) {
    if (ops[it].sending) {
      this->start_send(1, mpi_type::mpi_byte->id, tag_, ops[it].partner,
                       payload::null());
    }
  }
}

//
// Fire off the next set of send and receive operations.
//
void
mpi_barrier_engine::configure_sendrecv()
{
  mpi_barrier_debug("start sendrecv");
  if (pending_recvs_) {
    spkt_throw(sprockit::illformed_error,
              "mpibarrierengine::sendrecvs: already have pending receive");
  }

  std::vector<sendrecvop> ops;
  while (iter_->active()) {
    if (iter_->sending()) {
      ++pending_sends_;
      sendrecvop op =
      { true, mpi_id(iter_->dest()) };
      ops.push_back(op);
    }
    if (iter_->receiving()) {
      ++pending_recvs_;
      sendrecvop op =
      { false, mpi_id(iter_->source()) };
      ops.push_back(op);
      // We can only safely handle one receive at a time.
      // (a barrier will generally receive zero or one time,
      // followed by zero or more sends).
      iter_->next();
      break;
    }
    iter_->next();
  }

  if (!ops.empty()){
    mpi_barrier_debug("pushing back %d new ops", (int)ops.size());
    sendrecv_ops_.push_back(ops);
  } else {
    mpi_barrier_debug("no new ops added");
  }


  mpi_barrier_debug("now have %d pending sends, %d pending recvs", pending_sends_, pending_recvs_);
  --pending_sendrecvs_;

  // If we have no pending sends or receives, we are done.
}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_barrier_engine::send_complete(mpi_message* msg)
{
  mpi_barrier_debug("send complete to %d on tag %d", int(msg->source()), int(msg->tag()));

  --pending_sends_;
  if (pending_recvs_ == 0){
    maybe_do_sendrecvs();
  }

  if (pending_sends_ == 0 && pending_recvs_ == 0){
    mpi_barrier_debug("all done on tag %d", int(tag_));
    mpi_collective::complete(payload::const_ptr());
  }
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_barrier_engine::recv_complete(mpi_message* msg)
{
  mpi_barrier_debug("recv complete from %d on tag %d",
    int(msg->source()), int(msg->tag()));
  --pending_recvs_;
  if (pending_recvs_ == 0){
    maybe_do_sendrecvs();
  }

  if (pending_sends_ == 0 && pending_recvs_ == 0){
    mpi_barrier_debug("all done on tag %d", int(tag_));
    mpi_collective::complete(payload::const_ptr());
  }
}

//
// Hi.
//
mpi_barrier_engine::mpi_barrier_engine(topology_iterator* iter,
                                       mpi_request* the_key, mpi_queue* queue,
                                       mpi_tag tag, mpi_comm* comm,
                                       event_handler* completion) :
  mpi_collective(the_key, queue, tag, comm, completion), iter_(iter),
  pending_sends_(0), pending_recvs_(0), pending_sendrecvs_(0),
  active_sendrecvs_(0)
{
  if (tag_ == mpi::any_tag) {
    spkt_throw(sprockit::value_error,
              "mpibarrierengine: refusing to send using mpitag::any_tag");
  }
}

//
// Goodbye.
//
mpi_barrier_engine::~mpi_barrier_engine() throw ()
{
  delete iter_;
}

//
// Start this kernel.
//
void
mpi_barrier_engine::start()
{
  mpi_barrier_debug("starting");
  mpi_collective::start();
  maybe_do_sendrecvs();
}

}
} // end of namespace sstmac


