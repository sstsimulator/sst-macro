
#include <sst/core/component.h>
#include <sst/core/linkMap.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sstmac/sst_core/integrated_core.h>
#include <sstmac/sst_core/connectable_wrapper.h>
#include <sstmac/common/sst_event.h>
#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/output.h>

namespace sstmac {

SST::TimeConverter* SSTIntegratedComponent::time_converter_ = 0;

void
SSTIntegratedComponent::handle_event(SST::Event* ev)
{
  handle(static_cast<sstmac::event*>(ev));
}

SSTIntegratedComponent::SSTIntegratedComponent(
  sprockit::sim_parameters* params,
  uint64_t id) :
  SST::Component(SST::ComponentId_t(id)),
  self_link_(nullptr)
{
  sprockit::output::init_out0(&std::cout);
  sprockit::output::init_err0(&std::cerr);
  sprockit::output::init_outn(&std::cout);
  sprockit::output::init_errn(&std::cerr);

  link_map_ = SST::Simulation::getSimulation()->getComponentLinkMap(id);
  if (!time_converter_){
    time_converter_ = getTimeConverter(timestamp::tick_interval_string());
  }
}

void
SSTIntegratedComponent::init_links(sprockit::sim_parameters *params)
{
  for (auto& pair : link_map_->getLinkMap()){
    SST::Link* link = pair.second;
    std::istringstream istr(pair.first);
    std::string port_type;
    int src_outport, dst_inport;
    istr >> port_type;
    istr >> src_outport;
    istr >> dst_inport;
    sprockit::sim_parameters* port_params = hw::topology::get_port_params(params, src_outport);
    integrated_connectable_wrapper* wrapper = new integrated_connectable_wrapper(link);
    if (port_type == "input"){
      connect_input(port_params, src_outport, dst_inport, wrapper);
      configureLink(pair.first, handler(dst_inport));
    } else if (port_type == "output"){
      connect_output(port_params, src_outport, dst_inport, wrapper);
      configureLink(pair.first, handler(dst_inport));
    }
  }
}


void
SSTIntegratedComponent::configure_self_link()
{
  self_link_ = configureSelfLink("self", time_converter_, 
    new SST::Event::Handler<SSTIntegratedComponent>(this, &SSTIntegratedComponent::handle_self_link));
}

void
SSTIntegratedComponent::init(unsigned int phase)
{
  if (phase == 0){
    configure_self_link();
  }
}

void
SSTIntegratedComponent::handle_self_link(SST::Event* ev)
{
#if SSTMAC_SANITY_CHECK
  sstmac::event_queue_entry* entry = dynamic_cast<sstmac::event_queue_entry*>(ev);
  if (!entry){
    spkt_throw_printf(sprockit::value_error,
      "event on self link did not cast to an event entry");
#else
  sstmac::event_queue_entry* entry = static_cast<sstmac::event_queue_entry*>(ev);
#endif
  entry->execute();
  delete entry;
}

SST::SimTime_t
SSTIntegratedComponent::extra_delay(timestamp t) const
{
  SST::SimTime_t current = getCurrentSimTime(time_converter_);
  SST::SimTime_t timestamp_time = t.ticks_int64();
  return timestamp_time - current;
}

}

