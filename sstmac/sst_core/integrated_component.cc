
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

SSTIntegratedComponent::SSTIntegratedComponent(
  sprockit::sim_parameters* params,
  uint64_t id) :
  SST::Component(SST::ComponentId_t(id))
{
  sprockit::output::init_out0(&std::cout);
  sprockit::output::init_err0(&std::cerr);
  sprockit::output::init_outn(&std::cout);
  sprockit::output::init_errn(&std::cerr);

  link_map_ = SST::Simulation::getSimulation()->getComponentLinkMap(id);
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
      configureLink(pair.first, payload_handler(dst_inport));
    } else if (port_type == "output"){
      connect_output(port_params, src_outport, dst_inport, wrapper);
      configureLink(pair.first, credit_handler(src_outport));
    } else if (port_type == "in-out"){
      //no ack handlers - just setting up output handlers
      connect_output(port_params, src_outport, dst_inport, wrapper);
      configureLink(pair.first, payload_handler(src_outport));
    } else {
      //other special type of link I don't need to process
    }
  }
}

}

