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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIAPP_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPIAPP_H_INCLUDED

#include <sstmac/software/process/app.h>
#include <sstmac/libraries/mpi/mpi_api_fwd.h>

namespace sstmac {
namespace sw {

/**
 * A base type for applications that can run via mpi.
 * Intended to be safe for multiple inheritance (for applications
 * that use MPI and some other form of communication).
 */
class mpi_app : public app
{

 public:
  /// Access the MPI API.
  mpi_api*
  mpi() const {
    return mpi_;
  }

  /// Goodbye.
  virtual ~mpi_app();

  virtual void kill();

  virtual std::string
  to_string() const {
    return "mpiapp";
  }

  virtual void
  init_os(operating_system* os);

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

 protected:
  mpi_app(){}

 protected:
  mpi_api* mpi_;


};

}
} // end of namespace sstmac.

#endif

