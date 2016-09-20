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

#include <sstmac/hardware/switch/dist_dummyswitch.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/topology/topology.h>

#if !SSTMAC_INTEGRATED_SST_CORE

namespace sstmac {
namespace hw {

void
dist_dummy_switch::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler* comp)
{
}

void
dist_dummy_switch::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler* comp)
{
}

void
dist_dummy_switch::handle(event* ev)
{
  spkt_throw(sprockit::illformed_error,
    "dist_dummy_switch::handle: should never actually handle a message");
}

std::string
dist_dummy_switch::to_string() const
{
  return sprockit::printf("dummy switch %d", int(my_addr_));
}

  }
}

#endif
