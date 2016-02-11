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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/topology_iterator.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_linear_barrier_engine.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/common/messages/payload.h>
#include <sprockit/errors.h>

#define mpi_barrier_debug(...) \
  mpi_coll_debug("MPI_Barrier linear algorithm", __VA_ARGS__)

namespace sstmac {
namespace sw {

//
// Fire off the next set of send and receive operations.
//
void
mpi_linear_barrier_engine::sendrecv(bool firsttime)
{
  mpi_barrier_debug("sendrecv");

  if (comm_->rank() == root_) {
    // Fire off all sends and receives.
    for (long it = 0; it < comm_->size().id_; ++it) {
      if (firsttime) {
        if (it != root_.id_) {
          ++pending_recvs_;
          this->start_recv(1, mpi_type::mpi_byte->id, tag_, mpi_id(it));
        }
      }
      else {
        if (it != root_.id_) {
          ++pending_sends_;
          this->start_send(1, mpi_type::mpi_byte->id, tag_, mpi_id(it),
                           payload::null());
        }
      }
    }
  }
  else {
    if (firsttime) {
      ++pending_recvs_;
      this->start_recv(1, mpi_type::mpi_byte->id, tag_, root_);

      ++pending_sends_;
      this->start_send(1, mpi_type::mpi_byte->id, tag_, root_,
                       payload::null());
    }
  }

  // If we have no pending sends or receives, we are done.
  if (pending_sends_ == 0 && pending_recvs_ == 0) {
    mpi_collective::complete(payload::const_ptr());
  }
}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_linear_barrier_engine::send_complete(const mpi_message::ptr& msg)
{
  mpi_barrier_debug("send complete to %d, count=%d", int(msg->dest()), msg->count());
  --pending_sends_;
  if (!pending_sends_) {
    if (comm_->rank() == root_) {
      mpi_collective::complete(payload::const_ptr());
    }
    else {
      this->sendrecv(false);
    }
  }
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_linear_barrier_engine::recv_complete(const mpi_message::ptr& msg)
{
  mpi_barrier_debug("recv complete from %d, count=%d", int(msg->source()), msg->count());
  --pending_recvs_;
  if (!pending_recvs_) {
    this->sendrecv(false);
  }
}

//
// Hi.
//
mpi_linear_barrier_engine::mpi_linear_barrier_engine(
  topology_iterator* iter, mpi_request* the_key,
  mpi_queue* queue, mpi_tag tag, mpi_comm* comm,
  const mpi_id& root, event_handler* completion) :
  mpi_collective(the_key, queue, tag, comm, completion), iter_(iter),
  pending_sends_(0), pending_recvs_(0), root_(root)
{
  if (tag_ == mpi::any_tag) {
    spkt_throw(sprockit::value_error,
              "mpilinearbarrierengine: refusing to send using mpitag::any_tag");
  }
}

//
// Goodbye.
//
mpi_linear_barrier_engine::~mpi_linear_barrier_engine() throw ()
{
}

//
// Start this kernel.
//
void
mpi_linear_barrier_engine::start()
{
  mpi_barrier_debug("starting");
  mpi_collective::start();
  this->sendrecv(true);
}

}
} // end of namespace sstmac


