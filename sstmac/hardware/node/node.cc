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
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/software/launch/launcher.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/runtime.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <sprockit/output.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/integrated_component.h>
#include <sstmac/sst_core/connectable_wrapper.h>
#endif

ImplementFactory(sstmac::hw::node);
RegisterDebugSlot(node)
RegisterNamespaces("os", "memory", "proc", "node");
RegisterKeywords(
"libname",
"ncores",
"nsockets",
"node_cores",
"node_name",
"node_memory_model",
"node_model",
"node_sockets",
"node_pipeline_speedup",
"node_frequency",
);

namespace sstmac {
namespace hw {

std::list<sw::app_launch*> node::app_launchers_;

using namespace sstmac::sw;

node::node(sprockit::sim_parameters* params,
  uint64_t id, event_manager* mgr)
  : connectable_component(params, id,
    device_id(params->get_int_param("id"), device_id::node),
    mgr),
  params_(params),
  app_refcount_(0)
{
#if SSTMAC_INTEGRATED_SST_CORE
  static bool init_debug = false;
  if (!init_debug){
    std::vector<std::string> debug_params;
    params->get_optional_vector_param("debug", debug_params);
    for (auto& str : debug_params){
      sprockit::debug::turn_on(str);
    }
    init_debug = true;
  }
#endif
  my_addr_ = event_location().id();
  next_outgoing_id_.set_src_node(my_addr_);

  sprockit::sim_parameters* nic_params = params->get_namespace("nic");
  nic_params->add_param_override("id", int(my_addr_));
  nic_ = nic_factory::get_param("model", nic_params, this);

  sprockit::sim_parameters* mem_params = params->get_optional_namespace("memory");
  mem_model_ = memory_model_factory::get_optional_param("model", "logP", mem_params, this);

  sprockit::sim_parameters* proc_params = params->get_optional_namespace("proc");
  proc_ = processor_factory::get_optional_param("processor", "instruction",
          proc_params,
          mem_model_, this);

  nsocket_ = params->get_optional_int_param("nsockets", 1);

  sprockit::sim_parameters* os_params = params->get_optional_namespace("os");
  os_ = new sw::operating_system(os_params, this);

  app_launcher_ = new app_launcher(os_);
  job_launcher_ = job_launcher::static_job_launcher(params, mgr);

  if (my_addr_ == job_launcher::launch_root()){
    increment_app_refcount();
  }
}

link_handler*
node::credit_handler(int port) const
{
  return nic_->credit_handler(port);
}

link_handler*
node::payload_handler(int port) const
{
  return nic_->payload_handler(port);
}

void
node::deadlock_check()
{
  nic_->deadlock_check();
}

void
node::setup()
{
  schedule_launches();
#if SSTMAC_INTEGRATED_SST_CORE
  event_component::setup();
#endif
}

void
node::init(unsigned int phase)
{
#if SSTMAC_INTEGRATED_SST_CORE
  event_component::init(phase);
#endif
  nic_->init(phase);
  if (phase == 0){
    build_launchers(params_);
  }
}

node::~node()
{
  if (app_launcher_) delete app_launcher_;
  if (mem_model_) delete mem_model_;
  if (proc_) delete proc_;
  /** JJW Delete this last since destructor may unregister libs from OS */
  if (os_) delete os_;
  if (nic_) delete nic_;
}

void
node::connect_output(sprockit::sim_parameters* params,
  int src_outport, int dst_inport,
  event_handler* mod)
{
  //forward connection to nic
  nic_->connect_output(params, src_outport, dst_inport, mod);
}

void
node::connect_input(sprockit::sim_parameters* params,
  int src_outport, int dst_inport,
  event_handler* mod)
{
  //forward connection to nic
  nic_->connect_input(params, src_outport, dst_inport, mod);
}

void
node::execute(ami::SERVICE_FUNC func, event* data)
{
  spkt_throw(sprockit::unimplemented_error,
             "node does not implement asynchronous services - choose new node model");
}

void
node::build_launchers(sprockit::sim_parameters* params)
{
  if (!app_launchers_.empty()) return;

  bool keep_going = true;
  int aid = 1;
  int last_used_aid = 0;
  while (keep_going || aid < 10){
    app_launch* appman = app_launch::static_app_launch(aid, params);
    if (appman){
      app_launchers_.push_back(appman);
      keep_going = true;
      last_used_aid = aid;
    } else {
      keep_going = false;
    }
    ++aid;
  }

  aid = last_used_aid+1;

  std::vector<std::string> services_to_launch;
  params->get_optional_vector_param("services", services_to_launch);
  for (std::string& str : services_to_launch){
    sprockit::sim_parameters* srv_params = params->get_namespace(str);
    //setup the name for app factory
    srv_params->add_param_override("name", "distributed_service");
    //setup the name for distributed service
    srv_params->add_param_override("libname", str);
    app_launch* appman = app_launch::static_app_launch(aid, str, srv_params);
    node_debug("adding distributed service %s", str.c_str());
    app_launchers_.push_back(appman);
    ++aid;
  }
}

std::string
node::to_string() const
{
  return sprockit::printf("node(%d)", int(my_addr_));
}

void
node::schedule_launches()
{
  for (app_launch* appman : app_launchers_){
    schedule(appman->time(), new_callback(job_launcher_,
                &job_launcher::handle_new_launch_request, appman, this));
  }
}

void
node::increment_app_refcount()
{
#if SSTMAC_INTEGRATED_SST_CORE
  if (app_refcount_ == 0){
    primaryComponentDoNotEndSim();
  }
#endif
  ++app_refcount_;
}

void
node::decrement_app_refcount()
{
  app_refcount_--;

#if SSTMAC_INTEGRATED_SST_CORE
  if (app_refcount_ == 0){
    primaryComponentOKToEndSim();
  }
#endif
}

void
node::handle(event* ev)
{
  if (failed()){
    //do nothing - I failed
  } else {
    node_debug("forwarding event %s to OS",
               sprockit::to_string(ev).c_str());
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
node::send_to_nic(network_message* netmsg)
{
  node_debug("sending to %d", int(netmsg->toaddr()));
  netmsg->set_flow_id(allocate_unique_id());
  netmsg->put_on_wire();

  if (netmsg->toaddr() == my_addr_){
    nic_->intranode_send(netmsg);
  } else {
    nic_->internode_send(netmsg);
  }
}

}
} // end of namespace sstmac


