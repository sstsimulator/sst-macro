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
dist_dummy_switch::connect_ejector(int src_outport, int dst_inport, event_handler* nic)
{
}

void
dist_dummy_switch::connect_injector(int src_outport, int dst_inport, event_handler* nic)
{
}

void
dist_dummy_switch::handle(event* ev)
{
  spkt_throw(sprockit::illformed_error,
    "dist_dummy_switch::handle: should never actually handle a message");
}

double
dist_dummy_switch::hop_bandwidth() const
{
  spkt_throw_printf(sprockit::illformed_error,
                   "dist_dummyswith::hop_bandwidth: should never be called");
  return 0;
}

timestamp
dist_dummy_switch::lookahead() const
{
  spkt_throw_printf(sprockit::illformed_error,
                   "dist_dummyswith::lookahead: should never be called");
  return timestamp(0);
}

timestamp
dist_dummy_switch::hop_latency() const
{
  spkt_throw_printf(sprockit::illformed_error,
                   "dist_dummyswith::hop_latency: should never be called");
  return timestamp(0);
}

std::string
dist_dummy_switch::to_string() const
{
  return sprockit::printf("dummy switch %d", int(my_addr_));
}

  }
}

#endif
