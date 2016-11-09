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
#include <sstmac/software/launch/node_allocator.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/errors.h>

RegisterDebugSlot(allocation);

ImplementFactory(sstmac::sw::node_allocator);

namespace sstmac {
namespace sw {

node_allocator::~node_allocator() throw ()
{
}

node_allocator::node_allocator(sprockit::sim_parameters* params)
{
  rt_ = parallel_runtime::static_runtime(params);
  topology_ = sstmac::hw::topology::static_topology(params);
}

}
} //end of namespace sstmac

