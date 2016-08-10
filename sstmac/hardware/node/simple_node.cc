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
#include <sstmac/software/launch/machine_descriptor.h>
#include <sstmac/software/launch/launcher.h>
#include <sstmac/software/libraries/compute/compute_event.h>

#include <sprockit/errors.h>
#include <sprockit/util.h>

#include <iostream>

namespace sstmac {
namespace hw {

#if !SSTMAC_INTEGRATED_SST_CORE
SpktRegister("simple | simplenode", node, simple_node,
            "Simple node which implements basic OS/compute scheduling functionality");
#endif

ImplementIntegratedComponent(simple_node);

using namespace sstmac::sw;


simple_node::~simple_node()
{
}

void
simple_node::init_factory_params(sprockit::sim_parameters *params)
{
  node::init_factory_params(params);
}

void
simple_node::finalize_init()
{
  node::finalize_init();
}

#if SSTMAC_INTEGRATED_SST_CORE
simple_node::simple_node(
  SST::ComponentId_t id,
  SST::Params& params) : node(id, params)
{
  init_factory_params(params_);
  init_sst_params(params);
}

void
simple_node::init_sst_params(SST::Params &params)
{
  nic_->init_sst_params(params, this);
}
#else
void
simple_node::set_event_manager(event_manager* m)
{
  node::set_event_manager(m);
}
#endif

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
      sw::timed_compute_event* ev = safe_cast(timed_compute_event, data);
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

