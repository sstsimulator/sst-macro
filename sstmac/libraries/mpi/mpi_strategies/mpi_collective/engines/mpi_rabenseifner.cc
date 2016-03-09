/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_rabenseifner.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_rank_map.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/common/messages/payload.h>
#include <sprockit/errors.h>
#include <sstmac/common/messages/value_payload.h>

#define mpi_reduce_debug(...) \
  if (allreduce_) { mpi_coll_debug("MPI_Allreduce rabenseifner", __VA_ARGS__); } \
  else { mpi_coll_debug("MPI_Reduce rabenseifner", __VA_ARGS__); }

namespace sstmac {
namespace sw {

// Nested sendrecv container.
mpi_rabenseifner::sendrecv::sendrecv() :
  partner(-1), send_count(0), recv_count(0)
{
}

mpi_rabenseifner::sendrecv::sendrecv(mpi_id pt, int sc, int rc, bool rd) :
  partner(pt), send_count(sc), recv_count(rc), recv_reduce(rd)
{
}

void
mpi_rabenseifner::do_sendrecv()
{
  pending_sendrecvs_++;
  complete_lock_ = true;
  mpi_reduce_debug("starting with %d pending sends, %d pending recvs and queue size %d on tag %d",
    pending_sends_, pending_recvs_, queue_.size(), int(tag_));

  while ((pending_recvs_ <= 0) && (!queue_.empty())) {
    // Pop of at least one sendreceive, and then keep popping them off
    // until we hit a receive.  Receives must be completed before next-level
    // send operations.
    sendrecv sr = queue_.front();
    if ((int(sr.partner) < 0) || (sr.partner >= comm_->size())) {
      spkt_throw(sprockit::illformed_error,
                "mpirabenseifner::do_sendrecv: partner %d out of range",
                sr.partner);
    }
    queue_.pop_front();

    if (sr.send_count >= 0) {
      ++pending_sends_;
    }
    if (sr.recv_count >= 0) {
      ++pending_recvs_;
    }

    if (sr.send_count >= 0) {
      mpi_reduce_debug("send to %d, count=%d, pending=%d on tag %d with content %s",
        int(sr.partner), sr.send_count, pending_sends_, int(tag_),
        content_str());
      payload::const_ptr load = content_ ? content_->clone() : payload::const_ptr();
      this->start_send(sr.send_count, type_, tag_, sr.partner, load);
    }
    if (sr.recv_count >= 0) {
      mpi_reduce_debug("recv from %d, count=%d, pending=%d on tag %d",
        int(sr.partner), sr.recv_count, pending_recvs_, int(tag_));
      reduce_this_recv_ = sr.recv_reduce;
      this->start_recv(sr.recv_count, type_, tag_, sr.partner);
    }
  }

  while (pending_sends_complete_.begin() != pending_sends_complete_.end()) {
    mpi_id id = pending_sends_complete_.front();
    pending_sends_complete_.erase(pending_sends_complete_.begin());
    do_send_complete(id);
  }

  while (pending_recvs_complete_.begin() != pending_recvs_complete_.end()) {
    std::list<pending_recv_t>::iterator tmp = pending_recvs_complete_.begin();
    pending_recv_t& entry = *tmp;
    do_recv_complete(entry.first, entry.second);
    pending_recvs_complete_.erase(tmp);
  }
  complete_lock_ = false;
  --pending_sendrecvs_;

  mpi_reduce_debug("finishing sendrecv with %d pending sends, %d pending recvs on tag %d",
    pending_sends_, pending_recvs_, int(tag_));

  if (queue_.empty() && pending_sends_ == 0 && pending_recvs_ == 0
      && pending_sendrecvs_ == 0) {
    payload::const_ptr result = content_;
    if (!got_result_) {
      got_result_ = true;
      mpi_collective::complete(result);
    }
  }
  //done here
  --ready_sendrecvs_;
}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_rabenseifner::send_complete(mpi_message* msg)
{
  if (complete_lock_) {
    pending_sends_complete_.push_back(msg->source());
  }
  else {
    do_send_complete(msg->dest());
  }
}

void
mpi_rabenseifner::maybe_start_sendrecvs()
{
  if (ready_sendrecvs_ == 0){
    ++ready_sendrecvs_;
    while (ready_sendrecvs_ > 0){
      do_sendrecv();
    }
  } else {
    ++ready_sendrecvs_;
  }
}

void
mpi_rabenseifner::do_send_complete(mpi_id id)
{
  mpi_reduce_debug("sending complete to %d on tag %d", int(id), int(tag_));
  --pending_sends_;
  maybe_start_sendrecvs();
}

void
mpi_rabenseifner::do_recv_complete(const payload::const_ptr& load, mpi_id source)
{
  --pending_recvs_;

  if (load){
    if (reduce_this_recv_){
      content_ = combine_content(content_, load, op_, comm_->rank_, source);
      mpi_reduce_debug("combined to content %s on tag %d",
        content_->to_string().c_str(), int(tag_));
    } else if (pending_recvs_ == 0) {
      api* a = mpi_collective::queue_->api();
      const_cast<payload*>(load.get())->recover(a);
      content_ = load;
      mpi_reduce_debug("received content %s on tag %d",
        content_->to_string().c_str(), int(tag_));
    }
  }

  mpi_reduce_debug("recv complete from %d, tag %d, pending=%d, content=%s, incoming=%s",
    int(source), int(tag_), pending_recvs_,
    content_str(), load ? load->to_string().c_str() : "null");

  maybe_start_sendrecvs();
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_rabenseifner::recv_complete(mpi_message* msg)
{
  mpi_reduce_debug("start recv complete from %d, count=%d on tag %d: reduce? %d",
    int(msg->source()), msg->count(), int(tag_), reduce_this_recv_);
  if (complete_lock_) {
    pending_recvs_complete_.push_back(std::make_pair(msg->content(), msg->source()));
  }
  else {
    do_recv_complete(msg->content(), msg->source());
  }
}

//
// Hello.
//
mpi_rabenseifner::mpi_rabenseifner(bool allreduce,
                                   mpi_request* thekey, mpi_queue* queue,
                                   int count, mpi_type_id datatype, mpi_op* op, mpi_id root,
                                   mpi_tag tag, mpi_comm* comm,
                                   const payload::const_ptr& content,
                                   event_handler* completion) :
  mpi_collective(thekey, queue, tag, comm, completion), allreduce_(allreduce),
  count_(count), type_(datatype), op_(op), root_(root),
  pending_sends_(0), pending_recvs_(0), pending_sendrecvs_(0),
  reduce_this_recv_(0),
  ready_sendrecvs_(0),
  complete_lock_(false)
{

  if ((!allreduce) && (root < mpi_id(0) || root >= comm_->size()))
    spkt_throw(sprockit::value_error, "mpirabenseifner:  Reduce operation requested with "
                      "a root index that's out of range.");
  if (count < 0) {
    spkt_throw(sprockit::value_error,"mpirabenseifner:  Won't send negative element counts.");
  }
  if (allreduce) {
    root_ = mpi_id(-1);
  }
  get_result_ = (allreduce ? true : (comm_->rank() == root));
  got_result_ = false;

  content_ = content;
}

//
// Goodbye.
//
mpi_rabenseifner::~mpi_rabenseifner() throw ()
{
}

//
// Get busy child.
//
void
mpi_rabenseifner::start()
{
  mpi_reduce_debug("starting");
  // Precompute the queue.
  queue_.clear();
  mpi_rank_map rmap(root_, comm_);
  // Donor/acceptor exchange to shrink data.
  if (rmap.donor() || rmap.acceptor()) {
    int lowerhalf = count_ / 2;
    int upperhalf = count_ - lowerhalf;
    mpi_id partner = rmap.exchangewith();
    if (rmap.donor()) {
      mpi_reduce_debug("starting as donor");
      queue_.push_back(sendrecv(partner, lowerhalf, upperhalf, true));
      queue_.push_back(sendrecv(partner, upperhalf, -1, false));
    }
    else if (rmap.acceptor()) {
      mpi_reduce_debug("starting as acceptor");
      queue_.push_back(sendrecv(partner, lowerhalf, upperhalf, true));
      queue_.push_back(sendrecv(partner, -1, upperhalf, false));
    }
  }
  // Recursive exchanges with progressively larger stride and less data.
  std::vector<int> evensize(rmap.log2(), -1);
  std::vector<int> oddsize(rmap.log2(), -1);
  int effrank = rmap.effective_rank();
  if (!rmap.donor()) {
    int x_count = count_;
    for (int logdist = 0; logdist < rmap.log2(); ++logdist) {
      evensize.at(logdist) = x_count / 2;
      oddsize.at(logdist) = x_count - evensize[logdist];
      int dist = 1 << logdist;
      mpi_reduce_debug("fan-out with range %d", dist);
      mpi_id partner;
      int sendcount = -1, recvcount = -1;
      if ((effrank & dist) == 0) {
        // "even" rank w.r.t. current stride
        partner = rmap.real_rank(effrank + dist);
        x_count = evensize.at(logdist);
        sendcount = oddsize.at(logdist);
        recvcount = evensize.at(logdist);
      }
      else {
        // "odd" rank w.r.t. current strie
        partner = rmap.real_rank(effrank - dist);
        x_count = oddsize.at(logdist);
        sendcount = evensize.at(logdist);
        recvcount = oddsize.at(logdist);
      }
      queue_.push_back(sendrecv(partner, sendcount, recvcount, true));
    }
  }
  // Recursive exchanges with progressively smaller stride and more data.
  // This is where we do things differently for reduce or allreduce.
  if (allreduce_) {
    // Reduce data onto all nodes except donor nodes
    if (!rmap.donor()) {
      for (int logdist = rmap.log2() - 1; logdist >= 0; --logdist) {
        int dist = 1 << logdist;
        mpi_reduce_debug("fan-in with range %d", dist);
        mpi_id partner(-1);
        int sendcount = -1, recvcount = -1;
        if ((effrank & dist) == 0) {
          // "even" w.r.t. current stride
          mpi_reduce_debug("even node with relative partner %d", effrank + dist);
          partner = rmap.real_rank(effrank + dist);
          sendcount = evensize.at(logdist);
          recvcount = oddsize.at(logdist);
        }
        else {
          // "odd" rank w.r.t. current stride.
          mpi_reduce_debug("odd node with relative partner %d", effrank - dist);
          partner = rmap.real_rank(effrank - dist);
          sendcount = oddsize.at(logdist);
          recvcount = evensize.at(logdist);
        }
        queue_.push_back(sendrecv(partner, sendcount,   recvcount, false));
      }
    }
    // Update donor nodes
    if (rmap.donor()) {
      mpi_reduce_debug("donor node receiving updated data");
      queue_.push_back(sendrecv(rmap.exchangewith(), -1, count_, false));
    }
    else if (rmap.acceptor()) {
      mpi_reduce_debug("acceptor node sending updated data");
      queue_.push_back(sendrecv(rmap.exchangewith(), count_, -1, false));
    }
  }
  else {
    // not MPI_Allreduce -- just a regular MPI_Reduce
    bool remapped_root = false;
    int newroot = rmap.effective_rank(root_);
    if (rmap.donor(root_)) {
      mpi_reduce_debug("remap root");
      remapped_root = true;
      newroot = 0;
      if (comm_->rank() == root_) {
        // Compute the receive indices that the old node 0 has used to date.
        effrank = newroot;
        int x_count = count_;
        for (int logdist = 0; logdist < rmap.log2(); ++logdist) {
          evensize.at(logdist) = int(x_count) / 2;
          oddsize.at(logdist) = x_count - evensize.at(logdist);
          x_count /= 2;
        }
        // Set up to receive all data from the old node 0.
        queue_.push_back(sendrecv(mpi_id(0), -1, count_, false));
      }
      else if (comm_->rank() == mpi_id(0)) {
        // This node is swapping places with the remapped root.
        queue_.push_back(sendrecv(root_, count_, -1, false));
      }
    }
    // Now it's mostly a normal exchange, except we are excluding some nodes
    bool include_me = (comm_->rank() == root_ || !(rmap.donor()
                       || (remapped_root && comm_->rank() == mpi_id(0))));
    if (include_me) {
      for (int logdist = rmap.log2() - 1; logdist >= 0; --logdist) {
        int dist = 1 << logdist;

        mpi_reduce_debug("fan-in with range %d, effective_rank %d with root %d",
            dist, effrank, int(root_));

        int relative_partner;
        int transfer = -1;
        if ((effrank & dist) != (newroot & dist)) {
          // sender
          if ((effrank & dist) == 0) {
            // even
            relative_partner = effrank + dist;
            transfer = evensize.at(logdist);
          }
          else {
            // odd
            relative_partner = effrank - dist;
            transfer = oddsize.at(logdist);
          }
          mpi_id partner(-1);
          if (remapped_root && relative_partner == 0) {
            partner = root_;
          }
          else {
            partner = rmap.real_rank(relative_partner);
          }
          queue_.push_back(sendrecv(partner, transfer, -1, false));
        }
        else {
          // receiver
          if ((effrank & dist) == 0) {
            // even
            relative_partner = effrank + dist;
            transfer = oddsize.at(logdist);

            mpi_reduce_debug("starting with relative partner %d", relative_partner);
          }
          else {
            // odd
            relative_partner = effrank - dist;
            transfer = evensize.at(logdist);
            mpi_reduce_debug("starting with relative partner %d", relative_partner);
          }
          mpi_id partner = rmap.real_rank(relative_partner);
          mpi_reduce_debug("starting with real partner %d", int(partner));
          queue_.push_back(sendrecv(partner, -1, transfer, false));
        }
      }
    }
  }
  mpi_reduce_debug("starting");
  mpi_collective::start();

  //this is to avoid giant call stacks
  maybe_start_sendrecvs();
}

}
} // end of namespace sstmac

