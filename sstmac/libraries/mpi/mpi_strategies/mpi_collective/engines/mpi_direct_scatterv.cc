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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_direct_scatterv.h>
#include <sprockit/errors.h>

#define mpi_scatterv_debug(...) \
  mpi_coll_debug("MPI_Scatterv direct", __VA_ARGS__)

namespace sstmac {
namespace sw {

//
// Hi.
//
mpi_direct_scatterv::mpi_direct_scatterv(mpi_request* thekey,
    mpi_queue* queue,
    const std::vector<int> &sendcount, mpi_type_id sendtype,
    int recvcount, mpi_type_id recvtype,
    mpi_id root, mpi_tag tag,
    mpi_comm* comm,
    const std::vector<payload::const_ptr >& cont,
    event_handler* completion) :
  mpi_collective(thekey, queue, tag, comm, completion),
  sendcount_(sendcount), sendtype_(sendtype),
  recvcount_(recvcount), recvtype_(recvtype),
  root_(root),
  pending_sends_(0), pending_recvs_(0)
{
  if(comm->rank() == root) {

    content_ = cont;
  }
}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_direct_scatterv::send_complete(mpi_message* msg)
{
  mpi_scatterv_debug("send complete to %d, count=%d", int(msg->dest()), msg->count());
  --pending_sends_;
  if(! pending_sends_) {
    mpi_collective::complete(content_[int(comm_->rank())]);
  }
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_direct_scatterv::recv_complete(mpi_message* msg)
{
  mpi_scatterv_debug("recv complete from %d, count=%d", int(msg->source()), msg->count());
  payload::const_ptr load = msg->content();

  --pending_recvs_;
  if(! pending_recvs_) {
    mpi_collective::complete(load);
  }
}

//
// Goodbye.
//
mpi_direct_scatterv::~mpi_direct_scatterv() throw()
{
}

//
// Start this kernel.
//
void mpi_direct_scatterv::start()
{
  mpi_scatterv_debug("starting");
  mpi_collective::start();
  if(comm_->rank() == root_) {
    // Root node -- fire off a bunch of sends.
    mpi_id size = comm_->size();
    pending_sends_ = int(comm_->size()) - 1;
    if(pending_sends_ == 0) {
      mpi_collective::complete(content_[int(comm_->rank())]);
    }
    else {
      for(mpi_id dest(0); dest < size; ++dest) {
        if(dest != root_) {
          this->start_send(sendcount_.at(dest), sendtype_, tag_, dest, content_.at(dest));
        }
      }
    }
  }
  else {
    // Client node -- fire off a single receive.
    pending_recvs_ = 1;
    this->start_recv(recvcount_, recvtype_, tag_, root_);
  }
}


}
} // end of namespace sstmac

