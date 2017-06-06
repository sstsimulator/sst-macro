/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
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
#include <sstmac/software/launch/app_launcher.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/runtime.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <sprockit/output.h>

#include <sstmac/sst_core/integrated_component.h>
#include <sstmac/sst_core/connectable_wrapper.h>

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
"job_launcher",
);

namespace sstmac {
namespace hw {

using namespace sstmac::sw;

node::node(sprockit::sim_parameters* params,
  uint64_t id, event_manager* mgr)
  : connectable_component(params, id,
    device_id(params->get_int_param("id"), device_id::node),
    mgr),
  params_(params),
  app_refcount_(0),
  job_launcher_(nullptr)
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
  nic_params->add_param_override_recursive("id", int(my_addr_));
  nic_ = nic::factory::get_param("model", nic_params, this);

  sprockit::sim_parameters* mem_params = params->get_optional_namespace("memory");
  mem_model_ = memory_model::factory::get_optional_param("model", "logP", mem_params, this);

  sprockit::sim_parameters* proc_params = params->get_optional_namespace("proc");
  proc_ = processor::factory::get_optional_param("processor", "instruction",
          proc_params,
          mem_model_, this);

  nsocket_ = params->get_optional_int_param("nsockets", 1);

  sprockit::sim_parameters* os_params = params->get_optional_namespace("os");
  os_ = new sw::operating_system(os_params, this);

  app_launcher_ = new app_launcher(os_);

  launch_root_ = params->get_optional_int_param("launch_root", 0);
  if (my_addr_ == launch_root_){
    job_launcher_ =   job_launcher::factory::get_optional_param(
          "job_launcher", "default", params, os_);
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
#if SSTMAC_INTEGRATED_SST_CORE
  event_component::setup();
#endif
  if (job_launcher_)
    job_launcher_->schedule_launch_requests();
}

void
node::init(unsigned int phase)
{
#if SSTMAC_INTEGRATED_SST_CORE
  event_component::init(phase);
#endif
  nic_->init(phase);
}

node::~node()
{
  if (app_launcher_) delete app_launcher_;
  if (mem_model_) delete mem_model_;
  if (proc_) delete proc_;
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

std::string
node::to_string() const
{
  return sprockit::printf("node(%d)", int(my_addr_));
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
  nic_->inject_send(netmsg, os_);
}

}
} // end of namespace sstmac