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

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/connectable_wrapper.h>
#include <sstmac/sst_core/integrated_component.h>
#endif

ImplementFactory(sstmac::hw::network_switch);
RegisterDebugSlot(network_switch)

RegisterKeywords("switch_name");
RegisterNamespaces("switch");

namespace sstmac {
namespace hw {

network_switch::~network_switch()
{
}


network_switch::network_switch(sprockit::sim_parameters *params, uint64_t id, event_manager *mgr,
                               device_id::type_t ty)
 : connectable_component(params, id,
                         device_id(params->get_int_param("id"), ty),
                         mgr) //no self messages for a switch
{
  my_addr_ = event_location().id();
  top_ = topology::static_topology(params);
}

void
network_switch::init(unsigned int phase)
{
#if SSTMAC_INTEGRATED_SST_CORE
  if (phase == 0){
    event_component::init(phase);
  }
#endif
}



}
}

