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

#include <sstmac/hardware/node/simple_node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/processor/processor.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/launch/launcher.h>
#include <sstmac/software/libraries/compute/compute_event.h>

#include <sprockit/errors.h>
#include <sprockit/util.h>

#include <iostream>

namespace sstmac {
namespace hw {

SpktRegister("simple", node, simple_node,
  "A basic endpoint node running SST/macro software stacks");

simple_node::~simple_node()
{
}

simple_node::simple_node(sprockit::sim_parameters *params,
                         uint64_t id,
                         event_manager *mgr)
  : node(params, id, mgr)
{
}

void
simple_node::execute(ami::COMP_FUNC func,
                            event* data,
                            callback* cb)
{
  node_debug("executing kernel %s on node %d",
             ami::tostr(func), my_addr_);
  switch (func) {
    case sstmac::ami::COMP_INSTR:
      proc_->compute(data, cb);
      break;
    case sstmac::ami::COMP_TIME: {
      sw::timed_compute_event* ev = safe_cast(sw::timed_compute_event, data);
      schedule_delay(ev->data(), cb);
      break;
    }
    default:
      spkt_throw_printf(sprockit::spkt_error,
            "simplenode: cannot process kernel %s",
            ami::tostr(func));
  }
}

}
} // end of namespace sstmac.

