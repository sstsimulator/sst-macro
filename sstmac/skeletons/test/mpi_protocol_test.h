
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

#ifndef SSTMAC_SOFTWARE_SKELETONS_TEST_MPI_PROTOCOL_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_TEST_MPI_PROTOCOL_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_app.h>

namespace sstmac {
namespace sw {

/**
 * Basic MPI ping-pong.
 */
class mpi_protocol_test : public mpi_app
{

 public:
  mpi_protocol_test(){}

  /// Goodbye.
  virtual
  ~mpi_protocol_test() throw ();

  /// Get a copy.
  virtual app*
  clone_type(){
    return new mpi_protocol_test;
  }

  /// Go.
  void
  skeleton_main();

  virtual void
  consume_params(sprockit::sim_parameters* params);

  virtual std::string
  to_string() const {
    return "mpiprotocol_test";
  }

 protected:
  bool print_all_;
};

}
} // end of namespace sstmac


#endif // MPIPROTOCOL_H

