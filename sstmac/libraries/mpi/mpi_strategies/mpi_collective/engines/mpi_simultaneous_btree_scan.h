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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPISIMULATANEOUSBTREESCAN_H
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPISIMULATANEOUSBTREESCAN_H


#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_op.h>

namespace sstmac {
namespace sw {

/// Important types.
class mpi_queue;
class mpi_status;
class nodeinterface;
class topology_iterator;

/**
 * An mpikernel to performs an mpi_scan.  This uses the simultaneous binary tree algorithm
 * from "Parallel Prefix (Scan) Algorithms for MPI" in
 * Recent Advances in Parallel Virtual Machine and Message Passing Interface: 13th edition
 */

class mpi_simultaneous_btree_scan_engine : public mpi_collective
{

 public:
  /// Hi.
  mpi_simultaneous_btree_scan_engine(mpi_request* thekey,
                                     mpi_queue* queue,
                                     int count, mpi_type_id type,
                                     mpi_op* op,
                                     mpi_tag tag, mpi_comm* comm,
                                     const payload::const_ptr& content,
                                     event_handler* completion);

  /// Callback method to indicate that a send operation has completed.
  virtual void
  send_complete(mpi_message* msg);

  /// Callback method to indicate that a receive operation has completed.
  virtual void
  recv_complete(mpi_message* msg);

  /// Fire off the next set of send and receive operations.
  void sendrecv();

  void next_send();

  void next_recv();

  /// Goodbye.
  virtual ~mpi_simultaneous_btree_scan_engine() throw();

  /// Capture the 'start' method so we can set up our reducer.
  virtual void start();

  std::string
  to_string() const;

 private:
  void check_complete();
  
 protected:
  /// The lenght of our data array.
  int count_;

  /// The type we are sending and receiving.
  mpi_type_id type_;

  /// The operation we are applying to our data.
  mpi_op* op_;

  /// The data we are collecting.
  payload::const_ptr content_;

  int send_round_;

  int send_two_to_the_k_;

  int recv_round_;

  int recv_two_to_the_k_;

  int me_;

  int nproc_;

  /// The number of pending sends.
  int pending_sends_;

  /// The number of pending receives.
  int pending_recvs_;

  bool send_done_;

  bool recv_done_;
  
  bool in_send_recv_;
  
  

};

}
} // end of namespace sstmac

#endif // MPISIMULATANEOUSBTREESCAN_H

