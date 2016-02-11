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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_IMPLEMENTATION_H
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_IMPLEMENTATION_H

#include <sstmac/libraries/mpi/mpi_message.h>
#include <sprockit/factories/factory.h>

namespace sstmac {
namespace sw {

class mpi_implementation :
  public sprockit::factory_type
{

 public:
  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  virtual mpi_protocol*
  get_protocol(long bytes, bool intranode, bool ssend, bool onesided) const = 0;

  int64_t
  envelope_size() const {
    return envelope_;
  }

  virtual std::string
  to_string() const {
    return "mpi implentation";
  }

  virtual ~mpi_implementation(){}

 protected:
  int64_t envelope_;

};

DeclareFactory(mpi_implementation);

}
}

#endif // MPI_IMPLEMENTATION_H

