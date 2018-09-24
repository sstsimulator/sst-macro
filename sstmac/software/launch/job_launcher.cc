/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launch_event.h>
#include <sstmac/software/launch/launch_request.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/logp/logp_switch.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
 { "services", "a list of services to launch on a subset or all of the nodes" },
 { "launch_app", "DEPRECATED: list of applications to launch", true },
 { "launch_start_app", "DEPRECATED: time to start application", true },
 { "launch_cmd_app", "DEPRECATED: cmd to launch application", true },
);

namespace sstmac {
namespace sw {

std::vector<task_mapping::ptr> task_mapping::app_ids_launched_(1024);
std::map<std::string, task_mapping::ptr> task_mapping::app_names_launched_;
std::vector<int> task_mapping::local_refcounts_(1024);

job_launcher::job_launcher(sprockit::sim_parameters* params,
                           operating_system* os) :
  service(std::string("job_launcher"), software_id(0,0,0), os)
{
  topology_ = sstmac::hw::topology::static_topology(params);
  int num_nodes = topology_->num_nodes();
  for (int i=0; i < num_nodes; ++i){
    available_.insert(i);
  }

  add_launch_requests(params);
}

void
job_launcher::incoming_event(event *ev)
{
  job_stop_event* stop_ev = safe_cast(job_stop_event, ev);
  cleanup_app(stop_ev);
  stop_event_received(stop_ev);
  delete stop_ev;
}

void
job_launcher::incoming_launch_request(app_launch_request *request)
{
  ordered_node_set allocation;
  bool startJob = handle_launch_request(request, allocation);
  if (startJob){
    satisfy_launch_request(request, allocation);
  }
}

void
job_launcher::schedule_launch_requests()
{
  for (app_launch_request* req : initial_requests_){
    os_->increment_app_refcount();
    auto ev = new_callback(os_->component_id(), this,
                           &job_launcher::incoming_launch_request, req);
    os_->send_self_event_queue(req->time(), ev);
  }
}

void
job_launcher::add_launch_requests(sprockit::sim_parameters* params)
{
  bool keep_going = true;
  int aid = 1;
  int last_used_aid = 0;
  sprockit::sim_parameters* all_app_params = params->get_optional_namespace("app");
  while (keep_going || aid < 10){
    std::string name = sprockit::printf("app%d",aid);
    if (params->has_namespace(name)){
      sprockit::sim_parameters* app_params = params->get_namespace(name);
      all_app_params->combine_into(app_params);
      app_launch_request* mgr = new app_launch_request(app_params, app_id(aid), name);
      initial_requests_.push_back(mgr);
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
    app_launch_request* mgr = new app_launch_request(srv_params, app_id(aid), str);
    node_debug("adding distributed service %s", str.c_str());
    initial_requests_.push_back(mgr);
    ++aid;
  }
}

uint32_t
job_launcher::component_id() const
{
  return os_->component_id();
}

void
job_launcher::cleanup_app(job_stop_event* ev)
{
  task_mapping::ptr themap = task_mapping::global_mapping(ev->aid());
  task_mapping::remove_global_mapping(ev->aid(), ev->unique_name());
  const std::vector<node_id>& rank_to_node = themap->rank_to_node();
  int num_ranks = rank_to_node.size();
  //put all the nodes back in the available map
  for (int i=0; i < num_ranks; ++i){
    node_id nid = rank_to_node[i];
    available_.insert(nid);
  }
}

void
job_launcher::satisfy_launch_request(app_launch_request* request, const ordered_node_set& allocation)
{
  task_mapping::ptr mapping = std::make_shared<task_mapping>(request->aid());
  request->index_allocation(
     topology_, allocation,
     mapping->rank_to_node(),
     mapping->node_to_rank());
  task_mapping::add_global_mapping(request->aid(), request->app_namespace(), mapping);
  os_->outcast_app_start(0, request->aid(), request->app_namespace(), mapping, request->app_params(),
                       true/*must include myself (the root) in bcast*/);
  delete request;
}

bool
default_job_launcher::handle_launch_request(app_launch_request* request,
                                            ordered_node_set& allocation)
{
  bool success = request->request_allocation(available_, allocation);
  if (!success){
    spkt_abort_printf("allocation of app %d failed - insufficient number of nodes available to meet allocation request", 
                      request->aid());
  }

  for (const node_id& nid : allocation){
    if (available_.find(nid) == available_.end()){
      spkt_throw_printf(sprockit::value_error,
                        "allocation requested node %d, but it's not available",
                        int(nid));
    }
    available_.erase(nid);
  }
  return true;
}

void
default_job_launcher::stop_event_received(job_stop_event *ev)
{
  os_->decrement_app_refcount();
}

bool
exclusive_job_launcher::handle_launch_request(app_launch_request *request, ordered_node_set& allocation)
{
  if (active_job_ == nullptr){
    active_job_ = request;
    return default_job_launcher::handle_launch_request(request, allocation);
  } else {
    pending_requests_.push_back(request);
    return false;
  }
}

void
exclusive_job_launcher::stop_event_received(job_stop_event *ev)
{
  active_job_ = nullptr;
  if (!pending_requests_.empty()){
    app_launch_request* next = pending_requests_.front();
    pending_requests_.pop_front(); //remove the running job
    job_launcher::incoming_launch_request(next);
  }
  os_->decrement_app_refcount();
}

static thread_lock lock;

task_mapping::ptr
task_mapping::serialize_order(app_id aid, serializer &ser)
{
  task_mapping::ptr mapping;
  if (ser.mode() == ser.UNPACK){
    int num_nodes;
    ser & num_nodes;
    mapping = std::make_shared<task_mapping>(aid);
    ser & mapping->rank_to_node_indexing_;
    mapping->node_to_rank_indexing_.resize(num_nodes);
    int num_ranks = mapping->rank_to_node_indexing_.size();
    for (int i=0; i < num_ranks; ++i){
      node_id nid = mapping->rank_to_node_indexing_[i];
      mapping->node_to_rank_indexing_[nid].push_back(i);
    }
    lock.lock();
    auto existing = app_ids_launched_[aid];
    if (!existing){
      app_ids_launched_[aid] = mapping;
    } else {
      mapping = existing;
    } 
    lock.unlock();
  } else {
    //packing or sizing
    mapping = app_ids_launched_[aid];
    if (!mapping) spkt_abort_printf("no task mapping exists for application %d", aid);
    int num_nodes = mapping->node_to_rank_indexing_.size();
    ser & num_nodes;
    ser & mapping->rank_to_node_indexing_;
  }
  return mapping;
}

task_mapping::ptr
task_mapping::global_mapping(const std::string& name)
{
  lock.lock();
  auto iter = app_names_launched_.find(name);
  if (iter == app_names_launched_.end()){
    spkt_abort_printf("cannot find global task mapping for %s", name.c_str());
  }
  auto ret = iter->second;
  lock.unlock();
  return ret;
}

const task_mapping::ptr&
task_mapping::global_mapping(app_id aid)
{
  auto& mapping = app_ids_launched_[aid];
  if (!mapping){
    spkt_abort_printf("No task mapping exists for app %d\n", aid);
  }
  return mapping;
}

void
task_mapping::add_global_mapping(app_id aid, const std::string &unique_name, const task_mapping::ptr &mapping)
{
  lock.lock();
  app_ids_launched_[aid] = mapping;
  app_names_launched_[unique_name] = mapping;
  local_refcounts_[aid]++;
  lock.unlock();
}

void
task_mapping::remove_global_mapping(app_id aid, const std::string& name)
{
  lock.lock();
  local_refcounts_[aid]--;
  if (local_refcounts_[aid] == 0){
    app_ids_launched_[aid] = 0;
    app_names_launched_.erase(name);
  }
  lock.unlock();
}

}
}
