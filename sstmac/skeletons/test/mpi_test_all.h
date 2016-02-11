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

#ifndef SSTMAC_SOFTWARE_SKELETONS_TEST_MPI_TESTALL_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_TEST_MPI_TESTALL_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_app.h>

DeclareDebugSlot(mpi_test_all)

namespace sstmac {
namespace sw {

class mpi_test_all : public mpi_app
{
 public:
  mpi_test_all(){}

  virtual void
  consume_params(sprockit::sim_parameters* params);

  /// Get a copy.
  virtual app*
  clone_type() {
    return new mpi_test_all;
  }

  virtual
  ~mpi_test_all();

  /// Go.
  void
  skeleton_main();

  virtual std::string
  to_string() const {
    return "mpi_testall";
  }

 protected:
  void
  test_sendrecv();

  void
  test_barrier();

  void
  test_asynch();

  void
  test_bcast();

  void
  test_scatter();

  void
  test_gather();

  void
  test_allgather();

  void
  test_reduce();

  void
  test_allreduce();

  void
  test_alltoall();

  void
  test_comms();

  void
  test_cartcomms();

  void
  test_wait();

  void
  test_reducescatter();

  void
  test_probe();

  void
  test_persistent();

  void
  test_scan();

 protected:
  timestamp sleep_time_;
  timestamp compute_time_;
  bool print_all_;



  bool stop_at_errors_;
};

}
} //end of namespace sstmac
#endif

