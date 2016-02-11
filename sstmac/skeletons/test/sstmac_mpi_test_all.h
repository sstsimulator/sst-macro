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

#ifndef SSTMAC_SOFTWARE_SKELETONS_TEST_SSTMPI_TESTALL_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_TEST_SSTMPI_TESTALL_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_app.h>
#include <sstmac/libraries/mpi/sstmac_mpi.h>

namespace sstmac {
namespace sw {

class sstmac_mpi_test_all : public mpi_app
{

 public:
  sstmac_mpi_test_all(){}

  virtual void
  consume_params(sprockit::sim_parameters* params);

  /// Get a copy.
  virtual app*
  clone_type() {
    return new sstmac_mpi_test_all;
  }

  virtual
  ~sstmac_mpi_test_all() { }

  /// Go.
  void
  skeleton_main();

  virtual std::string
  to_string() const {
    return "sstmac_mpi_testall";
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
  test_scan();

  void
  test_send();

  void
  test_isend();

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
  test_wait();

  void
  test_reducescatter();

  void
  test_probe();

  void
  test_persistent();

  void
  test_reduce_scatter();

 protected:
  static int errors_;

  bool usetopo_;
  MPI_Comm world_;





  bool stop_at_errors_;

};

}
} //end of namespace sstmac
#endif

