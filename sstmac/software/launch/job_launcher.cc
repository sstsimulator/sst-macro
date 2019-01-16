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
#include <sstmac/software/process/app.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/logp/logp_switch.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/main/sstmac.h>

RegisterKeywords(
 { "services", "a list of services to launch on a subset or all of the nodes" },
 { "launch_app", "DEPRECATED: list of applications to launch", true },
 { "launch_start_app", "DEPRECATED: time to start application", true },
 { "launch_cmd_app", "DEPRECATED: cmd to launch application", true },
);

namespace sstmac {
namespace sw {

std::vector<TaskMapping::ptr> TaskMapping::app_ids_launched_(1024);
std::map<std::string, TaskMapping::ptr> TaskMapping::app_names_launched_;
std::vector<int> TaskMapping::local_refcounts_(1024);

JobLauncher::JobLauncher(SST::Params& params,
                           OperatingSystem* os) :
  Service(std::string("JobLauncher"), SoftwareId(0,0,0), os)
{
  topology_ = sstmac::hw::Topology::staticTopology(params);
  int num_nodes = topology_->numNodes();
  for (int i=0; i < num_nodes; ++i){
    available_.insert(i);
  }

  addLaunchRequests(params);
  os->rebuildMemoizations();
}

void
JobLauncher::incomingEvent(Event *ev)
{
  JobStopEvent* stop_ev = safe_cast(JobStopEvent, ev);
  cleanupApp(stop_ev);
  stopEventReceived(stop_ev);
  delete stop_ev;
}

void
JobLauncher::incomingLaunchRequest(AppLaunchRequest *request)
{
  ordered_node_set allocation;
  bool startJob = handleLaunchRequest(request, allocation);
  if (startJob){
    satisfyLaunchRequest(request, allocation);
  }
}

void
JobLauncher::scheduleLaunchRequests()
{
  for (AppLaunchRequest* req : initial_requests_){
    os_->incrementAppRefcount();
    auto ev = newCallback(os_->componentId(), this,
                           &JobLauncher::incomingLaunchRequest, req);
    os_->sendExecutionEvent(req->time(), ev);
  }
}

void
JobLauncher::addLaunchRequests(SST::Params& params)
{
  bool keep_going = true;
  int aid = 1;
  int last_used_aid = 0;
  SST::Params all_app_params = params->get_optional_namespace("app");
  while (keep_going || aid < 10){
    std::string name = sprockit::printf("app%d",aid);
    if (params->has_namespace(name)){
      SST::Params app_params = params.get_namespace(name);
      all_app_params.combine_into(app_params);
      AppLaunchRequest* mgr = new AppLaunchRequest(app_params, AppId(aid), name);
      initial_requests_.push_back(mgr);
      keep_going = true;
      last_used_aid = aid;

      App::dlopenCheck(aid, app_params);

      //if (app_params->has_param("exe")){
      //  //load and unload the library to bring statics in
      //  std::string libname = app_params->get_param("exe");
      //  void* handle = load_extern_library(libname, load_extern_path_str());
      //  unload_extern_library(handle);
      //}

    } else {
      keep_going = false;
    }
    ++aid;
  }

  aid = last_used_aid+1;

  std::vector<std::string> services_to_launch;
  params->get_optional_vector_param("services", services_to_launch);
  for (std::string& str : services_to_launch){
    SST::Params srv_params = params.get_namespace(str);
    //setup the name for app factory
    srv_params->add_param_override("name", "distributed_service");
    //setup the name for distributed service
    srv_params->add_param_override("libname", str);
    AppLaunchRequest* mgr = new AppLaunchRequest(srv_params, AppId(aid), str);
    node_debug("adding distributed service %s", str.c_str());
    initial_requests_.push_back(mgr);
    ++aid;
  }

  //just in case any memoizations were loaded
  os_->rebuildMemoizations();
}

uint32_t
JobLauncher::componentId() const
{
  return os_->componentId();
}

void
JobLauncher::cleanupApp(JobStopEvent* ev)
{
  App::dlcloseCheck(ev->aid());

  TaskMapping::ptr themap = TaskMapping::globalMapping(ev->aid());
  TaskMapping::removeGlobalMapping(ev->aid(), ev->uniqueName());
  const std::vector<NodeId>& rank_to_node = themap->rankToNode();
  int num_ranks = rank_to_node.size();
  //put all the nodes back in the available map
  for (int i=0; i < num_ranks; ++i){
    NodeId nid = rank_to_node[i];
    available_.insert(nid);
  }
}

void
JobLauncher::satisfyLaunchRequest(AppLaunchRequest* request, const ordered_node_set& allocation)
{
  TaskMapping::ptr mapping = std::make_shared<TaskMapping>(request->aid());
  request->indexAllocation(
     topology_, allocation,
     mapping->rankToNode(),
     mapping->nodeToRank());

  TaskMapping::addGlobalMapping(request->aid(), request->appNamespace(), mapping);
  os_->outcastAppStart(0, request->aid(), request->appNamespace(), mapping, request->appParams(),
                       true/*must include myself (the root) in bcast*/);
  delete request;
}

bool
DefaultJoblauncher::handleLaunchRequest(AppLaunchRequest* request,
                                        ordered_node_set& allocation)
{
  bool success = request->requestAllocation(available_, allocation);
  if (!success){
    spkt_abort_printf("allocation of app %d failed - insufficient number of nodes available to meet allocation request", 
                      request->aid());
  }

  for (const NodeId& nid : allocation){
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
DefaultJoblauncher::stopEventReceived(JobStopEvent *ev)
{
  os_->decrementAppRefcount();
}

bool
ExclusiveJoblauncher::handleLaunchRequest(AppLaunchRequest *request, ordered_node_set& allocation)
{
  if (active_job_ == nullptr){
    active_job_ = request;
    return DefaultJoblauncher::handleLaunchRequest(request, allocation);
  } else {
    pending_requests_.push_back(request);
    return false;
  }
}

void
ExclusiveJoblauncher::stopEventReceived(JobStopEvent *ev)
{
  active_job_ = nullptr;
  if (!pending_requests_.empty()){
    AppLaunchRequest* next = pending_requests_.front();
    pending_requests_.pop_front(); //remove the running job
    JobLauncher::incomingLaunchRequest(next);
  }
  os_->decrementAppRefcount();
}

static thread_lock lock;

TaskMapping::ptr
TaskMapping::serialize_order(AppId aid, serializer &ser)
{
  TaskMapping::ptr mapping;
  if (ser.mode() == ser.UNPACK){
    int num_nodes;
    ser & num_nodes;
    mapping = std::make_shared<TaskMapping>(aid);
    ser & mapping->rank_to_node_indexing_;
    mapping->node_to_rank_indexing_.resize(num_nodes);
    int num_ranks = mapping->rank_to_node_indexing_.size();
    for (int i=0; i < num_ranks; ++i){
      NodeId nid = mapping->rank_to_node_indexing_[i];
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

TaskMapping::ptr
TaskMapping::globalMapping(const std::string& name)
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

const TaskMapping::ptr&
TaskMapping::globalMapping(AppId aid)
{
  auto& mapping = app_ids_launched_[aid];
  if (!mapping){
    spkt_abort_printf("No task mapping exists for app %d\n", aid);
  }
  return mapping;
}

void
TaskMapping::addGlobalMapping(AppId aid, const std::string &unique_name, const TaskMapping::ptr &mapping)
{
  lock.lock();
  app_ids_launched_[aid] = mapping;
  app_names_launched_[unique_name] = mapping;
  local_refcounts_[aid]++;
  lock.unlock();
}

void
TaskMapping::removeGlobalMapping(AppId aid, const std::string& name)
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
