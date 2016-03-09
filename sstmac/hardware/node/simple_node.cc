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
#include <sstmac/hardware/processor/processor.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/launch/machine_descriptor.h>
#include <sstmac/software/launch/launcher.h>

#include <sstmac/common/messages/timed_message.h>


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

#if !SSTMAC_INTEGRATED_SST_CORE
void
simple_node::set_event_manager(event_manager* m)
{
  node::set_event_manager(m);
}
#endif

void
simple_node::execute_kernel(ami::COMM_FUNC func,
                            sst_message* data)
{
  switch (func) {
    case sstmac::ami::COMM_SEND: {
      network_message* netmsg = safe_cast(network_message, data);
      netmsg->set_fromaddr(my_addr_);
      node_debug("sending to %d", int(netmsg->toaddr()));
      send_to_nic(netmsg);
      break;
    }
    default:
      spkt_throw_printf(sprockit::unimplemented_error,
         "simplenode: cannot process kernel %s", ami::tostr(func));
      break;
  }
}

bool
simple_node::try_comp_kernels(ami::COMP_FUNC func,
                              sst_message* data)
{
  bool handled = true;

  switch (func) {
    case sstmac::ami::COMP_INSTR:
    case sstmac::ami::COMP_TIME: {
      proc_->compute(data);
      break;
    }

    case sstmac::ami::COMP_MEM: {
      mem_model_->access(data);
      break;
    }

    case sstmac::ami::COMP_SLEEP: {
      timestamp delay = interface_cast(timed_interface, data)->time();
      send_delayed_self_message(delay, data);
      break;
    }
    default:
      handled = false;
  }

  return handled;
}

void
simple_node::execute_kernel(ami::COMP_FUNC func,
                            sst_message* data)
{
  bool hand = try_comp_kernels(func, data);
  if (!hand) {
    spkt_throw_printf(sprockit::spkt_error, "simplenode: cannot process kernel %s",
                     ami::tostr(func));
  }
}

bool
simple_node::kernel_supported(ami::COMP_FUNC func) const
{
  switch (func) {
    case sstmac::ami::COMP_TIME:
    case sstmac::ami::COMP_SLEEP:
    case sstmac::ami::COMP_INSTR:
    case sstmac::ami::COMP_MEM:
      return true;
    default:
      return false;
  }
  return false;
}

bool
simple_node::kernel_supported(ami::COMM_FUNC func) const
{
  switch (func) {
    case sstmac::ami::COMM_SEND:
      return true;
    default:
      return false;
  }
  return false;
}

}
} // end of namespace sstmac.

