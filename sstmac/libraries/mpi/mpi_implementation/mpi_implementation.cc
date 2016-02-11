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

#include <sstmac/libraries/mpi/mpi_implementation/mpi_implementation.h>
#include <sprockit/sim_parameters.h>

ImplementFactory(sstmac::sw::mpi_implementation);

namespace sstmac {
namespace sw {

void
mpi_implementation::init_factory_params(sprockit::sim_parameters* params)
{
  envelope_ = params->get_optional_int_param("envelope", 24);
}


}
}


