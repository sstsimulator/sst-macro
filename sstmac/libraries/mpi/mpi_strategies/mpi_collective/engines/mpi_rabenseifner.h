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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIRABENSEIFNER_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIRABENSEIFNER_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_op.h>
#include <deque>

namespace sstmac {
namespace sw {

/**
 * Rolf Rabenseifner's MPI reduce algorithm.
 *
 * Re-implemented from https://fs.hlrs.de/projects/par/mpi//myreduce.c
 * Copyright: Rolf Rabenseifner, 1997
 *            Computing Center University of Stuttgart
 *            rabenseifner@rus.uni-stuttgart.de
 *
 * Which has the license terms:
 *   The usage of this software is free,
 *   but this header must not be removed.
 */
class mpi_rabenseifner : public mpi_collective
{

 public:
  /// We pre-compute all the sends, receives, and reduces that
  /// will have to be done on our data.
  /// If either send_count or recv_count is <= 0, then send or receive
  /// is skipped in the current cycle.
  struct sendrecv {
    mpi_id partner;
    int send_count, recv_count;
    bool recv_reduce;
    sendrecv();
    sendrecv(mpi_id partner, int sendcnt, int recvcnt, bool reduce);
  };

  /// Hello.
  mpi_rabenseifner(bool allreduce,
                   mpi_request* thekey,
                   mpi_queue* queue, int count, mpi_type_id datatype,
                   mpi_op* op, mpi_id root, mpi_tag tag,
                   mpi_comm* comm, const payload::const_ptr& content,
                   event_handler* completion);

  std::string
  to_string() const {
    return sprockit::printf("mpirabenseifner(tag=%d)", int(tag_));
  }

  /// Goodbye.
  virtual
  ~mpi_rabenseifner() throw ();

  /// Get busy child.
  virtual void
  start();

  /// Private method that handles the actual send/receive exchanges.
  void
  do_sendrecv();

  void do_send_complete(mpi_id id);

  void do_recv_complete(const mpi_message::ptr& msg);

 protected:
  /// Callback method to indicate that a send operation has completed.
  virtual void
  send_complete(const mpi_message::ptr& msg);

  /// Callback method to indicate that a receive operation has completed.
  virtual void
  recv_complete(const mpi_message::ptr& msg);

 private:
  static const char*
  content_str(const payload::const_ptr& content) {
    return content ? content->to_string().c_str() : "null";
  }

  const char*
  content_str() const {
    return content_str(content_);
  }

  void
  maybe_start_sendrecvs();

 private:
  /// This will be the order of event.  Push back when populating,
  /// pop front when dispatching.
  std::deque<sendrecv> queue_;

  payload::const_ptr content_;

  /// Flag indicating whether we're doing an allreduce or a reduce.
  bool allreduce_;
  /// Number of elements in our complete data array.
  int count_;
  /// The datatype we're sending and receiving.
  mpi_type_id type_;
  /// The operation we're applying to our data.
  mpi_op* op_;
  /// The root node -- only used if allreduce_ is false.
  mpi_id root_;

  /// We can have any number of pending sends, but the collective op
  /// is not over until all the sends have completed.
  int pending_sends_;
  /// We will not dispatch the next operation (send or receive) until
  /// a pending receive is retired.  There should only be zero or one.
  int pending_recvs_;

  int pending_sendrecvs_;

  /// We have a one-deep stack telling us whether we need to reduce
  /// the results from the next receive or not.
  bool reduce_this_recv_;

  /// Keep track of whether this node will report back reduced data or not.
  bool get_result_;
  bool got_result_;

  bool complete_lock_;

  int ready_sendrecvs_;

  std::list<mpi_id> pending_sends_complete_;

  std::list<mpi_message::ptr> pending_recvs_complete_;


};

}
} // end of namespace sstmac

#endif

