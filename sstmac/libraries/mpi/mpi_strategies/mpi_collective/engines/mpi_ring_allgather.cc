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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_ring_allgather.h>
#include <sprockit/errors.h>

#define mpi_allgather_debug(...) \
  mpi_coll_debug("MPI_Allgather ring algorithm", __VA_ARGS__)

namespace sstmac {
namespace sw {

//
// Hello.
//
mpi_ring_allgather::mpi_ring_allgather(mpi_request* thekey,
                                       mpi_queue* queue,
                                       int sendcnt, mpi_type_id sendtype,
                                       int recvcnt, mpi_type_id recvtype,
                                       mpi_tag tag,
                                       mpi_comm* comm,
                                       const payload::const_ptr& content,
                                       event_handler* completion) :
  mpi_collective(thekey, queue, tag, comm, completion), sendcnt_(sendcnt),
  sendtype_(sendtype), recvcnt_(recvcnt), recvtype_(recvtype),
  iteration_(0), target_iterations_(int(comm->size()) - 1),
  source_((int(comm->size()) + int(comm->rank()) - 1) % int(comm->size())),
  dest_((int(comm->rank()) + 1) % int(comm->size())), pending_sends_(0),
  pending_recvs_(0)
{

  if (content) {
    std::vector<payload::const_ptr> vals(int(comm->size()), payload::const_ptr());
    vals[int(comm->rank())] = content;
    content_ = new mpi_collective_payload(vals);
  }
  else {
    content_ = mpi_collective_payload::ptr();
  }

}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_ring_allgather::send_complete(const mpi_message::ptr& msg)
{
  mpi_allgather_debug("send complete to %d, count=%d", int(msg->dest()), msg->count());
  --pending_sends_;
  if (pending_sends_ == 0 && pending_recvs_ == 0) {
    mpi_collective::complete(content_);
  }
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_ring_allgather::recv_complete(const mpi_message::ptr& msg)
{
  mpi_allgather_debug("recv complete from %d, count=%d", int(msg->source()), msg->count());

  ++iteration_;

  payload::const_ptr pay = msg->content();
  if (pay) {
    mpi_collective_payload::const_ptr tmp = ptr_test_cast(const mpi_collective_payload, pay->clone());
    if (tmp) {
      mpi_collective_payload::ptr other = new mpi_collective_payload(tmp);

      std::vector<payload::const_ptr> realload = other->get_content();

      std::vector<payload::const_ptr>::iterator it, end = realload.end();

      int i = 0;
      for (it = realload.begin(); it != end; it++) {
        if (*it) {
          mpi_allgather_debug("setting content for position %d", i);
          content_->set_content(*it, i);
        }
        i++;
      }
    }
  }

  /** DEPRECATED JJW 01/09/2012 Do not chain recvs anymore.
   All sends/recvs are now started at the beginning. This
   generates a massive call stack that breaks the simulator
   */
  if (iteration_ < target_iterations_) {
    // Next send/receive pair.
    ++pending_recvs_;
    this->start_recv(recvcnt_, recvtype_, tag_, source_);
    /** JJW 01/09/2012 Even though we don't chain recvs,
     we should still chain sends. This won't generate
     a massive call stack */
    ++pending_sends_;
    this->start_send(sendcnt_, sendtype_, tag_, dest_, content_);
  }
  --pending_recvs_;

  if (pending_sends_ == 0 && pending_recvs_ == 0) {
    mpi_collective::complete(content_);
  }

}

//
// Goodbye.
//
mpi_ring_allgather::~mpi_ring_allgather() throw ()
{
}

//
// Get busy child.
//
void
mpi_ring_allgather::start()
{
  mpi_allgather_debug("starting");
  mpi_collective::start();
  // Catch situations where comm_->size() is 1.
  if (target_iterations_ == 0) {
    mpi_collective::complete(content_);
  }
  else {
    /** Start all the recvs at once to avoid
     a giant handle_recv recursive call stack that
     breaks the simulator */
    iteration_ = 0;
    pending_sends_ = pending_recvs_ = 0;

    ++pending_recvs_;
    this->start_recv(recvcnt_, recvtype_, tag_, source_);

    /** The payload content_ gets updated after each recv.  Thus,
     we do not want to start all sends at once.  Each successive
     recv will launch the next send */
    ++pending_sends_;
    this->start_send(sendcnt_, sendtype_, tag_, dest_, content_);
  }
}

}
} // end of namespace sstmac

