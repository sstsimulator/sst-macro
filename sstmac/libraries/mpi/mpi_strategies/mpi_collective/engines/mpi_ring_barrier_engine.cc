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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_ring_barrier_engine.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/topology_iterator.h>

#include <sstmac/common/messages/payload.h>
#include <sprockit/errors.h>

#define mpi_barrier_debug(...) \
  mpi_coll_debug("MPI_Barrier ring algorithm", __VA_ARGS__)

namespace sstmac {
namespace sw {

//
// Fire off the next set of send and receive operations.
//
void
mpi_ring_barrier_engine::sendrecv(bool firsttime)
{
  mpi_barrier_debug("sendrecv");

  long size = comm_->size();
  if (firsttime) {
    if (int(comm_->rank()) == 0) {
      this->start_send(1, mpi_type::mpi_byte->id, tag_,
                       mpi_id((int(comm_->rank()) + 1) % size), payload::null());

      this->start_recv(1, mpi_type::mpi_byte->id, tag_,
                       mpi_id((int(comm_->rank()) - 1 + size) % size));
    }
    else {
      this->start_recv(1, mpi_type::mpi_byte->id, tag_,
                       mpi_id((int(comm_->rank()) - 1 + size) % size));
    }
  }

}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_ring_barrier_engine::send_complete(const mpi_message::ptr& msg)
{
  mpi_barrier_debug("send complete to %d, count=%d", int(msg->dest()), msg->count());
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_ring_barrier_engine::recv_complete(const mpi_message::ptr& msg)
{
  mpi_barrier_debug("recv complete from %d, count=%d", int(msg->source()), msg->count());

  long size = comm_->size();

  if (forward_) {
    forward_ = false;

    this->start_recv(1, mpi_type::mpi_byte->id, tag_,
                     mpi_id((int(comm_->rank()) + 1) % size));

    if (comm_->rank() == 0) {
      this->start_send(1, mpi_type::mpi_byte->id, tag_,
                       mpi_id((int(comm_->rank()) - 1 + size) % size), payload::null());
    }
    else {
      this->start_send(1, mpi_type::mpi_byte->id, tag_,
                       mpi_id((int(comm_->rank()) + 1) % size), payload::null());
    }
  }
  else {
    if (comm_->rank() != 0) {
      this->start_send(1, mpi_type::mpi_byte->id, tag_,
                       mpi_id((int(comm_->rank()) - 1 + size) % size), payload::null());
    }

    mpi_collective::complete(payload::const_ptr());
  }
}

//
// Hi.
//
mpi_ring_barrier_engine::mpi_ring_barrier_engine(
  topology_iterator* iter, mpi_request* the_key,
  mpi_queue* queue, mpi_tag tag, mpi_comm* comm,
  event_handler* completion) :
  mpi_collective(the_key, queue, tag, comm, completion), iter_(iter),
  forward_(true)
{
  if (tag_ == mpi::any_tag) {
    spkt_throw(sprockit::value_error,
              "mpiringbarrierengine: refusing to send using mpitag::any_tag");
  }
}

//
// Goodbye.
//
mpi_ring_barrier_engine::~mpi_ring_barrier_engine() throw ()
{
}

//
// Start this kernel.
//
void
mpi_ring_barrier_engine::start()
{
  mpi_barrier_debug("starting");
  mpi_collective::start();
  this->sendrecv(true);
}

}
} // end of namespace sstmac


