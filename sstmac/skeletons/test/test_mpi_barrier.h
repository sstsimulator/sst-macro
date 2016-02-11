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

#ifndef SSTMAC_SOFTWARE_SKELETONS_TEST_TEST_MPI_BARROER_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_TEST_TEST_MPI_BARROER_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_app.h>

namespace sstmac {
namespace sw {

/**
 * Basic MPI ping-pong.
 */
class test_mpi_barrier : public mpi_app
{
 public:
  test_mpi_barrier(){}

  /// Goodbye.
  virtual
  ~test_mpi_barrier() throw ();

  /// Get a copy.
  virtual app*
  clone_type(){
    return new test_mpi_barrier;
  }

  /// Go.
  void
  skeleton_main();

  virtual void
  consume_params(sprockit::sim_parameters* params);

  virtual std::string
  to_string() const {
    return "mpipingpong";
  }

 private:
  /// How many iterations we do.
  int iterations_;
};

}
} // end of namespace sstmac

#endif

