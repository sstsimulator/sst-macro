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

#ifndef SSTMAC_SOFTWARE_SKELETONS_TEST_MPIPAIRWISE_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_TEST_MPIPAIRWISE_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_app.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_id.h>

namespace sstmac {
namespace sw {

/**
 * Basic MPI pairwise send-receive.
 */
class mpi_pairwise : public mpi_app
{

 public:
  mpi_pairwise(){}

  /// Goodbye.
  virtual
  ~mpi_pairwise() throw ();

  /// Get a copy.
  app*
  clone_type(){
    return new mpi_pairwise;
  }

  /// Go.
  void
  skeleton_main();

  virtual void
  consume_params(sprockit::sim_parameters* params);

  virtual std::string
  to_string() const {
    return "mpi_pairwise";
  }

 private:
  /// How many iterations we do.
  int iterations_, count_;

  std::vector<mpi_id> from_;
  std::vector<mpi_id> to_;
  std::vector<timestamp> when_;

};

}
} //end of namespace sstmac

#endif

