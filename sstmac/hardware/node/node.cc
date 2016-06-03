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

#include <sstmac/software/libraries/unblock_event.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/hardware/processor/processor.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/common/fail_event.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app_manager.h>
#include <sstmac/software/launch/launcher.h>
#include <sstmac/software/launch/launch_message.h>
#include <sstmac/common/runtime.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <sprockit/output.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/message_event_wrapper.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sstmac/sst_core/connectable_wrapper.h>
#endif

ImplementFactory(sstmac::hw::node);
RegisterDebugSlot(node)
RegisterNamespaces("os", "memory", "proc");

namespace sstmac {
namespace hw {

using namespace sstmac::sw;

#if SSTMAC_INTEGRATED_SST_CORE
node::node(
  SST::ComponentId_t id,
  SST::Params& params
) : connectable_component(id, params)
{
}

void
node::connect_nic()
{
  for(auto&& pair : link_map_->getLinkMap()) {
    const std::string& port_name = pair.first;
    SST::Link* link = pair.second;
    connection_details dets = parse_port_name(port_name);
    if (dets.src_type == connection_details::node){
      //outgoing from me, make the link
      nic_debug("connecting to port %s", port_name.c_str());
      integrated_connectable_wrapper* next = new integrated_connectable_wrapper(link);
      nic_->connect(dets.src_port, 
            dets.dst_port,
            dets.type, next);
    } else { //I'm the receiving end
      configureLink(port_name, new SST::Event::Handler<nic>(nic_, &nic::handle_event));
    }
  }
}

void
node::setup()
{
  event_scheduler::setup();
  launch();
}

void
node::init(unsigned int phase)
{
  event_scheduler::init(phase);
  if (phase == 0){ 
    set_event_manager(this);
    connect_nic();
    configure_self_link();
  }
}
#else
node::node() :
  os_(0),
  nic_(0),
  mem_model_(0),
  proc_(0)
{
}
#endif

node::~node()
{
  if (os_){
    os_->unregister_all_libs(this);
    delete os_;
  }
  if (mem_model_) delete mem_model_;
  if (proc_) delete proc_;
  //JJW 03/09/2015 - node does not own NIC
  //if (nic_) delete nic_;
}

void
node::connect(int src_outport, int dst_inport, connection_type_t ty, connectable *mod, config *cfg)
{
  spkt_throw(sprockit::unimplemented_error,
    "node::connect: should never be called");
}

void
node::init_factory_params(sprockit::sim_parameters *params)
{
  sprockit::sim_parameters* os_params = params->get_optional_namespace("os");
  os_ = sw::operating_system::construct(os_params);

  my_addr_ = node_id(params->get_int_param("id"));
  init_loc_id(event_loc_id(my_addr_));

  next_outgoing_id_.set_src_node(my_addr_);

#if SSTMAC_INTEGRATED_SST_CORE
  build_launchers(params);
  sprockit::sim_parameters* nic_params = params->get_namespace("nic");
  interconnect* null_ic = 0;
  nic_ = nic_factory::get_param("model", nic_params, null_ic);
  nic_->set_node(this);
#endif

  sprockit::sim_parameters* mem_params = params->get_optional_namespace("memory");
  mem_model_ = memory_model_factory::get_optional_param("model", "simple", mem_params, this);

  sprockit::sim_parameters* proc_params = params->get_optional_namespace("proc");
  proc_ = processor_factory::get_optional_param("processor", "instruction",
          proc_params,
          mem_model_, this);
  /**
  sstkeyword {
      gui=4;
      docstring=The total number of cores on the node.ENDL
      The number of cores per NUMA block is node_cores/node_sockets.;
  }
  */
  ncores_ = params->get_int_param("ncores");
  /**
  sstkeyword {
      gui=4;
      docstring=The total number of distinct sockets on the node.ENDL
      Taken to be synonymous with the number of distinct NUMA blocks.;
  }
  */
  nsocket_ = params->get_optional_int_param("nsockets", 1);

#if SSTMAC_INTEGRATED_SST_CORE
  finalize_init();
#endif
}

void
node::build_launchers(sprockit::sim_parameters* params)
{
  int aid = 1;
  app_manager* appman = app_manager::static_app_manager(aid, params);
  const std::list<int>& my_ranks = appman->rank_assignment(my_addr_);
  std::list<int>::const_iterator it, end = my_ranks.end();
  for (it=my_ranks.begin(); it != end; ++it){
    int rank = *it;
    sw::launch_message* lmsg = new launch_message(appman->launch_info(), sw::launch_message::ARRIVE, task_id(rank));
    runtime::register_node(sw::app_id(aid), task_id(rank), my_addr_);
    launchers_.push_back(lmsg);
  }
}

void
node::finalize_init()
{
  os_->set_node(this);
  os_->set_addr(my_addr_);
  os_->set_ncores(ncores_, nsocket_);
  os_->register_lib(this, new launcher);
}

std::string
node::to_string() const
{
  return sprockit::printf("node(%d)", int(my_addr_));
}

void
node::set_event_manager(event_manager* m)
{
#if !SSTMAC_INTEGRATED_SST_CORE
  //this only happens without integrated core
  event_scheduler::set_event_manager(m);
#endif
  os_->set_event_parent(this);
  mem_model_->set_event_parent(this);
  nic_->set_event_parent(this);
}

void
node::handle(event* ev)
{
  if (failed()){
    //do nothing - I failed
  }
  else if (ev->is_failure()){
    fail_stop();
  } else {
    os_->handle_event(ev);
  }
}

void
node::fail_stop()
{
  fail();
  nic_->fail();
  cancel_all_messages();
}

void
node::compute(timestamp t)
{
  sw::key* k = sw::key::construct();
  sw::unblock_event* ev = new sw::unblock_event(os_, k);
  schedule_delay(t, ev);
  os_->block(k);
  delete k;
}

void
node::send_to_nic(network_message* netmsg)
{
  netmsg->set_net_id(allocate_unique_id());
  netmsg->put_on_wire();
  if (netmsg->toaddr() == my_addr_){
    nic_->intranode_send(netmsg);
  } else {
    nic_->internode_send(netmsg);
  }
}

#if SSTMAC_INTEGRATED_SST_CORE
void
node::launch()
{
  std::list<sw::launch_message*>::iterator it, end = launchers_.end();
  for (it=launchers_.begin(); it != end; ++it){
    sw::launch_message* lmsg = *it;
    node_debug("launching task %d on node %d",
      int(lmsg->tid()), int(addr()));
    os_->handle_event(lmsg);
  }
}
#else
void
node::launch(timestamp start, launch_message* msg)
{
  schedule(start, new handler_event_queue_entry(msg, this, this->event_location()));
}
#endif

}
} // end of namespace sstmac


