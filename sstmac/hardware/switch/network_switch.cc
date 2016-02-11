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
#include <sstmac/hardware/interconnect/switch_interconnect.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/message_event_wrapper.h>
#include <sstmac/sst_core/connectable_wrapper.h>
#include <sstmac/sst_core/integrated_component.h>
#endif

ImplementFactory(sstmac::hw::network_switch);
RegisterDebugSlot(network_switch);

namespace sstmac {

sprockit::StaticNamespaceRegister switch_ns("switch");

namespace hw {

int network_switch::packet_length_bytes_ = 0;

#if !SSTMAC_INTEGRATED_SST_CORE
network_switch::network_switch() :
  router_(0),
  top_(0)
{
}
#endif

#if SSTMAC_INTEGRATED_SST_CORE
network_switch::network_switch(
    SST::ComponentId_t id,
    SST::Params& params
) : connectable_component(id, params),
  router_(0),
  top_(0)
{

  // TODO configure self link @integrated_core @critical
}

void
network_switch::init(unsigned int phase)
{
  event_scheduler::init(phase);
  if (phase == 0){
    for(auto&& pair : link_map_->getLinkMap()) {
      const std::string& port_name = pair.first;
      SST::Link* link = pair.second;
      connection_details dets = parse_port_name(port_name);
      if (dets.src_type == connection_details::sw && dets.src_id == my_addr_){ 
        //create the link
        integrated_connectable_wrapper* next = new integrated_connectable_wrapper(link);
        connect_weighted(
            dets.src_port, 
            dets.dst_port,
            dets.type,
            next,
            dets.weight,
            dets.redundancy);
      } else { //somebody is injecting to me
        configureLink(port_name, new SST::Event::Handler<SSTIntegratedComponent>(this, &SSTIntegratedComponent::handle_event));
      }
    }
    initialize();
    set_event_manager(this);
  }
  configure_self_link();
}

void
network_switch::setup()
{
  event_scheduler::setup();
}
#endif

int
network_switch::eject_port(node_id addr)
{
  return top_->endpoint_to_ejection_port(addr);
}

int
network_switch::inject_port(node_id addr)
{
  return top_->endpoint_to_injection_port(addr);
}

void
network_switch::set_event_manager(event_manager* m)
{
#if !SSTMAC_INTEGRATED_SST_CORE
  router_->init_stats(m);
#endif
  event_scheduler::set_event_manager(m);
}

void
network_switch::init_factory_params(sprockit::sim_parameters* params)
{
  my_addr_ = switch_id(params->get_int_param("id"));
  init_loc_id(event_loc_id(my_addr_));

  router_ = router_factory::get_optional_param("router", "minimal", params);
  router_->set_switch(this);

  STATIC_INIT_TOPOLOGY(params)

  /**
   sstkeyword {
   docstring=The actual physical packet size.ENDL
   This is used primarily by adaptive routing schemesENDL
   in determining queue length.;
   }
   */
  packet_length_bytes_ = params->get_optional_byte_length_param(
                           "packet_length", 100);


}

network_switch::~network_switch()
{
  if (router_) delete router_;
}

void
network_switch::set_topology(topology *top)
{
  top_ = top;
  router_->set_topology(top);
  router_->init_vc();
}

void
network_switch::connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod)
{
 switch (ty) {
   case input: {
      if (top_->is_injection_port(dst_inport)){
        connect_injector(src_outport, dst_inport, safe_cast(event_handler, mod));
      } else {
        //printf("Connecting %d->%d not injection\n", src_outport, dst_inport);
        connect_weighted(src_outport, dst_inport, ty, mod, 1.0, 1);
      }
      break;
    }
    case output: {
      if (top_->is_injection_port(src_outport)){
        connect_ejector(src_outport, dst_inport, safe_cast(event_handler, mod));
      } else {
        //printf("Connecting %d->%d not ejection\n", src_outport, dst_inport);
        connect_weighted(src_outport, dst_inport, ty, mod, 1.0, 1);
      }
      //here my outport is zero

      break;
    }
    default:
      connectable_type_invalid(ty);
   }
}



}
}

