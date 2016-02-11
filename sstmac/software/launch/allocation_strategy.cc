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
allocation_strategy::set_interconnect(hw::interconnect* interconn)
{
  interconn_ = interconn;
  set_topology(interconn->topol());
}

void
allocation_strategy::validate_num_nodes(int num_nodes, const char *type)
{
  int max_available = interconn_->available().size();
  if (num_nodes > max_available) {
    spkt_throw_printf(sprockit::value_error,
                     "%s::allocate: requested %d nodes, only %d are available",
                     type, num_nodes, max_available);
  }
  else if (num_nodes <= 0) {
    spkt_throw_printf(sprockit::value_error,
                     "%s::allocate: requested %d nodes, but I need a positive number."
                     "If you're running DUMPI or another tracer, launch_allocation parameter "
                     "needs to be hostname or dumpi",
                     type, num_nodes);
  }
}

void
allocation_strategy::init_factory_params(sprockit::sim_parameters *params)
{
  STATIC_INIT_TOPOLOGY(params);
}

void
allocation_strategy::release(const node_set& alloc)
{
  hw::interconnect::node_set& available = interconn_->available();
  hw::interconnect::node_set& allocated = interconn_->allocated();
  node_set::const_iterator it, end = alloc.end();
  for (it = alloc.begin(); it != end; it++) {
    allocated.erase(*it);
    available.insert(*it);
  }
}


}
} //end of namespace sstmac

