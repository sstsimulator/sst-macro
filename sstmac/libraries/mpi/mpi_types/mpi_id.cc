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

#include <sstmac/libraries/mpi/mpi_types.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_id.h>
#include <sprockit/errors.h>
#include <iostream>
#include <sstream>

namespace sstmac {
namespace sw {

namespace mpi {
const mpi_id any_source(MPI_ANY_SOURCE);
const mpi_id root(-10);
const mpi_id proc_null(MPI_PROC_NULL);
}

}
} // end of namespace sstmac

