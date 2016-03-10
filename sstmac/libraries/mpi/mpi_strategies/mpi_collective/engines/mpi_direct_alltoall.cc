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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_direct_alltoall.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>
#include <sprockit/errors.h>

#define mpi_ata_debug(...) \
  mpi_coll_debug("MPI_Alltoall direct", __VA_ARGS__)

namespace sstmac {
namespace sw {

//
// Hello.
//
mpi_direct_alltoall::mpi_direct_alltoall(mpi_request* thekey,
    mpi_queue* queue, const std::vector<int> &sendcnt,
    mpi_type_id sendtype, const std::vector<int> &recvcnt,
    mpi_type_id recvtype, mpi_tag tag, mpi_comm* comm,
    const std::vector<payload::const_ptr >& content,
    event_handler* completion) :
  mpi_collective(thekey, queue, tag, comm, completion), sendcnt_(sendcnt),
  sendtype_(sendtype), recvcnt_(recvcnt), recvtype_(recvtype),
  max_concurrent_ops_(5), started_(false)
{
  bool wrong_send_cnt = sendcnt_.size() != size_t(comm_->size());
  bool wrong_recv_cnt = recvcnt_.size() != size_t(comm_->size());
  if (wrong_send_cnt || wrong_recv_cnt) {
    spkt_throw(sprockit::value_error,
              "mpidirectalltoall:  length of send- and/or recv arrays is not the same as communicator size.");
  }

  send_content_ = content;

  for (unsigned i = 0; i < send_content_.size(); i++) {
    recv_content_.push_back(payload::const_ptr());
  }

  recv_content_[comm->rank()] = send_content_[comm->rank()];
}

void
mpi_direct_alltoall::check_complete()
{
  if ((pending_sends_ == 0) && (pending_recvs_ == 0) 
      && pendrecv_.empty() && pendsend_.empty() && started_) {
    mpi_collective::complete(new mpi_collective_payload(recv_content_));
  }  
}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_direct_alltoall::send_complete(mpi_message* msg)
{
  mpi_ata_debug("send complete to %d, count=%d - now have %d pending sends, %d pending recvs on tag=%d",
    int(msg->dest()), msg->count(), pending_sends_, pending_recvs_, int(tag_));

  --pending_sends_;

  if (!pendsend_.empty()) {
    pendsend snd = pendsend_.front();
    pendsend_.pop_front();
    pending_sends_++;
    this->start_send(snd.cnt, sendtype_, tag_, snd.dest, send_content_[snd.dest]);
  }
  else {
    check_complete();
  }
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_direct_alltoall::recv_complete(mpi_message* msg)
{
  mpi_ata_debug("recv complete from %d, count=%d - now have %d pending sends, %d pending recvs on tag=%d",
    int(msg->source()), msg->count(), pending_sends_, pending_recvs_, int(tag_));

  payload::const_ptr other = ptr_test_cast(const payload, msg->content());
  int index = msg->source();
  recv_content_[index] = other;

  --pending_recvs_;

  /** DEPRECATED JJW 01/15/2012 Do not chain recvs. This generates a huge call stack
  if (!pendrecv_.empty())
    {
          stack that breaks the program.
      pendrecv rcv = pendrecv_.front();
      pendrecv_.pop_front();
      pending_recvs_++;
      this->start_recv(rcv.cnt, recvtype_, tag_, rcv.source);
    }
  else
    {

    }
   */
  check_complete();
}


//
// Goodbye.
//
mpi_direct_alltoall::~mpi_direct_alltoall() throw ()
{
}

//
// Set the maximum number of concrurrent sends.
//
void
mpi_direct_alltoall::set_max_concurrent_ops(int maxval)
{
  max_concurrent_ops_ = maxval;
}

// Utility routine.
template<typename T>
T
min(T a, T b)
{
  return (a < b ? a : b);
}

//
// Start this kernel.
//
void
mpi_direct_alltoall::start()
{
  mpi_ata_debug("starting");
  mpi_collective::start();
  // Prepare to fire off all my sends and receives.
  mpi_id rank = comm_->rank();
  mpi_id size = comm_->size();
  // Catch the case where comm_->size() was 1.
  if (size <= 1) {
    mpi_collective::complete(new mpi_collective_payload(send_content_));
  }
  else {
    pending_sends_ = 0;
    pending_recvs_ = 0;
    int total_num_sends = int(size) - 1;
    int max_num_active_sends = total_num_sends;
    if (max_concurrent_ops_ > 0){
      max_num_active_sends = std::min(max_concurrent_ops_, total_num_sends);
    }
    // Stagger the sends so not everybody hammers node 0 at the same time.
    for (int relrank = 1; relrank < int(size); ++relrank) {
      mpi_id peer((int(size)+ int(rank) - relrank) % int(size));
      if (peer != rank) {
        if (pending_sends_ < max_num_active_sends) {
          pending_sends_++;
          this->start_send(sendcnt_.at(peer), sendtype_, tag_,
                           peer, send_content_[peer]);
        }
        else {
          pendsend_.push_back(pendsend(sendcnt_.at(peer), peer));
        }
      }
    }
    // Stagger the receives in the opposite order from the sends
    // to avoid deadlock.
    for (int relrank = 1; relrank <= int(size); ++relrank) {
      mpi_id peer((int(size) + int(rank) + relrank) % int(size));
      if (peer != rank) {
        if (pending_recvs_ < max_num_active_sends) {
          pending_recvs_++;
          this->start_recv(sendcnt_.at(peer), sendtype_, tag_,
                           peer);
        }
        else {
          pendrecv_.push_back(pendrecv(recvcnt_.at(peer), peer));
        }
      }
    }

    /** DEPRECATED JJW 01/15/2012 Start all recvs at the beginning
    if (!pendrecv_.empty() && pending_recvs_ == 0) //this is possible, believe it or not
      {
        pendrecv rcv = pendrecv_.front();
        pendrecv_.pop_front();
        pending_recvs_++;
        this->start_recv(rcv.cnt, recvtype_, tag_, rcv.source);
      }
    */

    /** Start all the recvs at one to avoid generating a HUGE call stack from chained recvs
    */
    while (!pendrecv_.empty()) {
      pendrecv rcv = pendrecv_.front();
      pendrecv_.pop_front();
      pending_recvs_++;
      this->start_recv(rcv.cnt, recvtype_, tag_, rcv.source);
    }

  }
  started_ = true;
  check_complete();
}

}
} // end of namespace sstmac

