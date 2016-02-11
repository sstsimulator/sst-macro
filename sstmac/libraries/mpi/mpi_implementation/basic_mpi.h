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

#ifndef SSTMAC_SOFTWARE_MPI_IMPLEMENTATION_BASICMPI_H
#define SSTMAC_SOFTWARE_MPI_IMPLEMENTATION_BASICMPI_H

#include <sstmac/libraries/mpi/mpi_implementation/mpi_implementation.h>

namespace sstmac {
namespace sw {

class basic_mpi_implementation
  : public mpi_implementation
{

 public:
  mpi_protocol*
  get_protocol(long bytes, bool intranode, bool rsend, bool onesided) const;


  virtual std::string
  to_string() const {
    return "basic mpi implentation";
  }

 protected:
  virtual void
  init_factory_params(sprockit::sim_parameters* params);

 protected:
  int64_t handsize_;


};
}
}


#endif


