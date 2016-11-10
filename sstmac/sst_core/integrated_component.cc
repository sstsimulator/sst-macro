
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
  //loop all the links in our map and determine what we need to do with them
  for (auto& pair : link_map_->getLinkMap()){
    SST::Link* link = pair.second;
    //extract link info from the port name
    std::istringstream istr(pair.first);
    std::string port_type;
    int src_outport, dst_inport;
    istr >> port_type;
    istr >> src_outport;
    istr >> dst_inport;
    sprockit::sim_parameters* port_params = hw::topology::get_port_params(params, src_outport);
    link_wrapper* wrapper = new link_wrapper(link);

    if (port_type == "input"){
      //I will receive incoming payloads on this link
      configureLink(pair.first, payload_handler(dst_inport));
      //setup up the link for sending credits back to source
      connect_input(port_params, src_outport, dst_inport, wrapper);
    } else if (port_type == "output"){
      //I will receive credits back after sending out payloads
      configureLink(pair.first, credit_handler(src_outport));
      //setup the link for sending output payloads to destination
      connect_output(port_params, src_outport, dst_inport, wrapper);
    } else if (port_type == "in-out"){
      //no credits involved here - just setting up output handlers
      connect_output(port_params, src_outport, dst_inport, wrapper);
      configureLink(pair.first, payload_handler(src_outport));
    } else {
      //other special type of link I don't need to process
    }
  }
}

}

