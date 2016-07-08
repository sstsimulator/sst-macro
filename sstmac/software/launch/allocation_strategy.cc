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

#include <algorithm>
#include <exception>
#include <iterator>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/software/launch/allocation_strategy.h>
#include <sprockit/errors.h>

ImplementFactory(sstmac::sw::allocation_strategy);
RegisterDebugSlot(allocation);

namespace sstmac {
namespace sw {


allocation_strategy::~allocation_strategy() throw ()
{
}

void
allocation_strategy::init_factory_params(sprockit::sim_parameters *params)
{
  STATIC_INIT_TOPOLOGY(params);
}


}
} //end of namespace sstmac

