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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIDIRECTALLTOALL_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIDIRECTALLTOALL_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <deque>

namespace sstmac {
namespace sw {

/**
 * Perform an alltoall (or alltoallv) using concurrent isend and irecv calls.
 * This may not provide the best scaling for all situations, but is probably
 * never outright horrible.
 */
class mpi_direct_alltoall : public mpi_collective
{

 public:
  std::string
  to_string() const {
    return sprockit::printf("mpidirectalltoall(tag=%d)", int(tag_));
  }

  /// Hello.
  mpi_direct_alltoall(mpi_request* thekey,
                      mpi_queue* queue,
                      const std::vector<int> &sendcnt, mpi_type_id sendtype,
                      const std::vector<int> &recvcnt, mpi_type_id recvtype,
                      mpi_tag tag, mpi_comm* comm,
                      const std::vector<payload::const_ptr >& content,
                      event_handler* completion);

  /// Goodbye.
  virtual ~mpi_direct_alltoall() throw();

  /// Get the maximum number of concurrent sends or receives.
  /// A value of zero or less indicates no maximum number.
  inline int max_concurrent_ops() {
    return max_concurrent_ops_;
  }

  /// Define the maximum number of concurrent sends or receives.
  /// Set to zero or less to have no maximum.
  void set_max_concurrent_ops(int max);

  /// Start this kernel.
  virtual void start();

 protected:
  /// Callback method to indicate that a send operation has completed.
  virtual void
  send_complete(const mpi_message::ptr& msg);

  /// Callback method to indicate that a receive operation has completed.
  virtual void
  recv_complete(const mpi_message::ptr& msg);
  
  void check_complete();

 protected:
  /// Private nested type to keep track of sends that haven't been posted yet
  struct pendsend {
    explicit pendsend(int sendcnt, mpi_id pal) :
      cnt(sendcnt), dest(pal) {
    }
    int cnt;
    mpi_id dest;
  };
  /// Private nested type for receives waiting to be posted.
  struct pendrecv {
    explicit pendrecv(int recvcnt, mpi_id pal) :
      cnt(recvcnt), source(pal) {
    }
    int cnt;
    mpi_id source;
  };

  /// The list of send lengths.
  std::vector<int> sendcnt_;
  /// The send type.
  mpi_type_id sendtype_;
  /// The list of receive lengths.
  std::vector<int> recvcnt_;
  /// The receive type.
  mpi_type_id recvtype_;
  /// The maximum number of concurrent sends and receives.
  int max_concurrent_ops_;
  /// Sends that have not been posted yet.
  std::deque<pendsend> pendsend_;
  /// Receives that have not been posted yet.
  std::deque<pendrecv> pendrecv_;
  /// The number of outstanding sends (sends already posted).
  int pending_sends_;
  /// The number of outstanding recvs.
  int pending_recvs_;

  bool started_;

  std::vector<payload::const_ptr > send_content_;
  std::vector<payload::const_ptr > recv_content_;
};

}
} // end of namespace sstmac

#endif

