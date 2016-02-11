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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_TYPES_MPIID_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_TYPES_MPIID_H_INCLUDED

#include <iosfwd>
#include <sprockit/opaque_typedef.h>

namespace sstmac {
namespace sw {

typedef_opaque_int(mpi_id, int);
implement_opaque_int(mpi_id)

namespace mpi {
extern const mpi_id any_source;
extern const mpi_id root;
extern const mpi_id proc_null;
}

}
} // end of namespace sstmac

#endif

