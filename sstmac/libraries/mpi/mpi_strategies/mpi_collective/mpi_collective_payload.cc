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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sprockit/serializer.h>
#include <sprockit/util.h>

DeclareSerializable(sstmac::sw::mpi_collective_payload);

namespace sstmac {
namespace sw {

void
mpi_collective_payload::serialize_order(sprockit::serializer& ser)
{
  ser & (contents_);
}

void
mpi_collective_payload::recover(api* a)
{
  mpi_api* mpi = safe_cast(mpi_api, a);
  spkt_throw_printf(sprockit::unimplemented_error,
   "mpi_collective_payload::recover: %s does not implement recover",
   to_string().c_str());
}

}
}

