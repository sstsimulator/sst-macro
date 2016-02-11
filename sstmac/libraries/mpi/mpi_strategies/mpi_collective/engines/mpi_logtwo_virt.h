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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPILOGTWOVIRT_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPILOGTWOVIRT_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_op.h>
#include <deque>

namespace sstmac {
namespace sw {

class mpi_logtwo_virt_allreduce : public mpi_collective
{

 public:
  mpi_logtwo_virt_allreduce(mpi_request* thekey,
                            mpi_queue* queue, int count, mpi_type_id datatype,
                            mpi_op* op, mpi_id root, mpi_tag tag,
                            mpi_comm* comm, const payload::const_ptr& content,
                            event_handler* completion);

  void start();

  virtual ~mpi_logtwo_virt_allreduce() throw () {}

  std::string
  to_string() const {
    return "mpilogtwovirtallreduce";
  }

 protected:
  /// Callback method to indicate that a send operation has completed.
  virtual void
  send_complete(const mpi_message::ptr& msg);

  /// Callback method to indicate that a receive operation has completed.
  virtual void
  recv_complete(const mpi_message::ptr& msg);

  static int two_to_power(int n);

  static int log_base_two(int n);

  int virtual_rank_to_actual_rank(int me);

  struct exchange {
    int partner;
    int count;
    exchange(int p, int c) :
      partner(p), count(c) {
    }
  };

  void do_run_round(int round, int me, int virtual_me,
                    std::list<exchange>& to_do);

  void configure_round(int me, int round,
                       int& count, int& partner);

  void run_next_round();

 private:
  int nproc_;

  int my_rank_;

  /// Number of elements in our complete data array.
  int count_;
  /// The datatype we're sending and receiving.
  mpi_type_id type_;
  /// The operation we're applying to our data.
  mpi_op op_;
  /// The root node -- only used if allreduce_ is false.
  mpi_id root_;

  /// We can have any number of pending sends, but the collective op
  /// is not over until all the sends have completed.
  int pending_sends_;
  /// We will not dispatch the next operation (send or receive) until
  /// a pending receive is retired.  There should only be zero or one.
  int pending_recvs_;

  int num_rounds_;

  int round_;

  /// We have a one-deep stack telling us whether we need to reduce
  /// the results from the next receive or not.
  bool reducing_round_;

  bool complete_lock_;

  std::set<int> my_roles_;

};
}
}
#endif // MPILOGTWOVIRT_H

