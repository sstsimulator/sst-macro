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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_halving_reduce_scatter.h>
#include <sprockit/errors.h>

#define mpi_redscat_debug(...) \
  mpi_coll_debug("MPI_Reduce_scatter halving algorithm", __VA_ARGS__)

namespace sstmac {
namespace sw {

//
// A private method to perform data exchanges.
//
void mpi_halving_reduce_scatter::sendrecv()
{
  mpi_redscat_debug("send recv with %d pending sends, %d pending recvs",
    pending_sends_, pending_recvs_);

  if(pending_recvs_) { // We can't send anything until our receive is done.
    return;
  }
  if(! peers_.empty()) {
    commstep curr = peers_.front();
    peers_.pop_front();
    // Incrementing sends and receives separately - just so we don't
    // end up entering sendrecv too early.
    if(curr.sendbytes > 0) {
      ++pending_sends_;
    }
    if(curr.recvbytes > 0) {
      ++pending_recvs_;
    }
    if(curr.sendbytes > 0) {
      this->start_send(curr.sendbytes, type_, tag_, curr.peer);
    }
    if(curr.recvbytes > 0) {
      this->start_recv(curr.recvbytes, type_, tag_, curr.peer);
    }
  }
  else if(pending_sends_ == 0 && pending_recvs_ == 0) {
    // We're done.

    mpi_collective::complete(content_);
  }
}

//
// Hello.
//
mpi_halving_reduce_scatter::
mpi_halving_reduce_scatter(mpi_request* thekey,
                           mpi_queue* queue,
                           const std::vector<int> &recvcnts,
                           mpi_type_id recv_type, mpi_op* op,
                           mpi_tag tag, mpi_comm* comm,
                           const payload::const_ptr& content,
                           event_handler* completion) :
  mpi_collective(thekey, queue, tag, comm, completion),
  recvcnts_(recvcnts), type_(recv_type), op_(op),
  pending_sends_(0), pending_recvs_(0)
{
  // Clone the content if it is non-null
  std::vector<payload::const_ptr> vals;
  for (int i = 0; i < int(comm->size()); i++) {
    if (i == int(comm->rank())) {
      vals.push_back(content);
    }
    else {
      vals.push_back(payload::const_ptr());
    }
  }
  content_ = new mpi_collective_payload(vals);


  if(recvcnts_.size() != size_t(comm->size())) {
    spkt_throw(sprockit::value_error,
              "mpihalvingreducescatter: number of peers differs from the number of receive counts");
  }
  std::vector<int>::const_iterator it, end = recvcnts.end();
  for(it = recvcnts.begin(); it != end; ++it) {
    if(*it < 0) {
      spkt_throw(sprockit::value_error,
                "mpihalvingreducescatter:fFound negative receive count");
    }
  }
}

//
// Callback method to indicate that a send operation has completed.
//
void
mpi_halving_reduce_scatter::send_complete(mpi_message* msg)
{
  mpi_redscat_debug("send complete to %d, count=%d", int(msg->dest()), msg->count());
  --pending_sends_;
  this->sendrecv();
}

//
// Callback method to indicate that a receive operation has completed.
//
void
mpi_halving_reduce_scatter::recv_complete(mpi_message* msg)
{
  mpi_redscat_debug("recv complete from %d, count=%d", int(msg->source()), msg->count());

  --pending_recvs_;
  this->sendrecv();
}

//
// Goodbye.
//
mpi_halving_reduce_scatter::~mpi_halving_reduce_scatter() throw()
{
}

//
// Utility routine to find communication partner given a pivot.
//
inline int partner(int rank, int pivot)
{
  if(rank >= (pivot<<1)){
    spkt_throw(sprockit::value_error,
       "mpihalvingreducescatter::start::partner: rank out of valid range");
  }
  return (rank < pivot ? rank + pivot : rank - pivot);
}
//
// Utility routine to find the power-of-two rank for a given index.
//
inline int to_pof2(mpi_id rank, int extras)
{
  if(rank.id_ >= 2*extras) {
    return rank.id_-extras;
  }
  else {
    return (rank.id_%2 ? -1 : rank.id_/2);
  }
}
//
// Utility routine to convert a power-of-two rank to an MPI rank.
//
inline mpi_id from_pof2(int rank, int extras)
{
  if(rank < 0) {
    spkt_throw(sprockit::value_error,
              "mpihalfringreducescatter::start::from_pof2: negative rank.");
  }
  return mpi_id(rank > extras ? rank+extras : rank*2);
}
//
// Utility routine to find the greatest power of two not greater than value
//
inline int msb(int value)
{
  if(value < 0) {
    spkt_throw(sprockit::value_error,
              "mpihalvingreducescatter::start::msb: negative value");
  }
  int rv = 1;
  while(rv <= value) {
    rv <<= 1;
  }
  return (rv>>1);
}

//
// Get busy child.
//
void mpi_halving_reduce_scatter::start()
{
  mpi_redscat_debug("starting");
  mpi_collective::start();
  //
  // Figure out our entire pattern of sends and receives.
  const int size = msb(comm_->size());
  const int extras = comm_->size().id_ - size;
  const int rank = to_pof2(comm_->rank(), extras);
  // Set up mappings of how much we are sending/receiving.
  std::vector<int> sizes;
  sizes.reserve(size);
  int total_elements = 0;
  for(size_t it = 0; it < recvcnts_.size(); ++it) {
    total_elements += recvcnts_.at(it);
    if(to_pof2(mpi_id(it), extras) < 0) {
      sizes.back() += recvcnts_[it];
    }
    else {
      sizes.push_back(recvcnts_[it]);
    }
  }
  //
  // First, check whether we need to collapse this node.
  if(rank < extras) {
    if(rank < 0) {
      // This node needs to pre-send all it's data down.
      peers_.push_back(commstep(mpi_id(int(comm_->rank())-1), total_elements, 0));
    }
    else {
      peers_.push_back(commstep(mpi_id(int(comm_->rank())+1), 0, total_elements));
    }
  }
  //
  // Next, set up all the normal (power-of-two) data exchanges.
  if(rank >= 0) {
    int pivot = size;
    while(pivot > 1) {
      const int cap = pivot;
      pivot >>= 1;
      // Figure out where we're sending
      const int relrank = rank % cap;
      const int shift = rank - relrank;
      const int pal = partner(relrank, pivot) + shift;
      // Figure out what we're sending
      int lowframe[]  = { shift, pivot+shift };
      int highframe[] = { pivot+shift, cap+shift };
      int *recvframe = (relrank < pivot ? &lowframe[0]  : &highframe[0]);
      int *sendframe = (relrank < pivot ? &highframe[0] : &lowframe[0]);
      int sendamt = 0, recvamt = 0;
      for(int it = recvframe[0]; it < recvframe[1]; ++it) {
        sendamt += sizes.at(it);
      }
      for(int it = sendframe[0]; it < sendframe[1]; ++it) {
        recvamt += sizes.at(it);
      }
      // and add it to the queue of send/receive pairs.
      peers_.push_back(commstep(from_pof2(pal,extras), sendamt, recvamt));
    }
  }
  //
  // Finally, check whether we need to send back any data.
  if(rank < extras) {
    if(rank < 0) {
      // We need to get our data back from below.
      peers_.push_back(commstep(mpi_id(int(comm_->rank())-1),
                                0, recvcnts_.at(int(comm_->rank()))));
    }
    else {
      peers_.push_back(commstep(mpi_id(int(comm_->rank())+1),
                                recvcnts_.at(int(comm_->rank())+1), 0));
    }
  }
  // and now we can go.
  this->sendrecv();
}


}
} // end of namespace sstmac

