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
//#include <sstmac/main/sstmac.h>

RegisterKeywords(
 { "services", "a list of services to launch on a subset or all of the nodes" },
 { "launch_app", "DEPRECATED: list of applications to launch", true },
 { "launch_start_app", "DEPRECATED: time to start application", true },
 { "launch_cmd_app", "DEPRECATED: cmd to launch application", true },
);

namespace sstmac {
namespace sw {

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
JobLauncher::incomingRequest(Request *req)
{
  JobStopRequest* stop_req = safe_cast(JobStopRequest, req);
  cleanupApp(stop_req);
  stopEventReceived(stop_req);
  delete stop_req;
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
    auto ev = newCallback(this, &JobLauncher::incomingLaunchRequest, req);
    os_->sendExecutionEvent(req->time(), ev);
  }
}

void
JobLauncher::addLaunchRequests(SST::Params& params)
{
  bool keep_going = true;
  int aid = 1;
  int last_used_aid = 0;
  SST::Params all_app_params = params.find_scoped_params("app");
  while (keep_going || aid < 10){
    std::string name = sprockit::printf("app%d",aid);
    SST::Params app_params = params.find_scoped_params(name);
    if (!app_params.empty()){
      SST::Params app_params = params.find_scoped_params(name);
      app_params.insert(all_app_params);
      AppLaunchRequest* mgr = new AppLaunchRequest(app_params, AppId(aid), name);
      initial_requests_.push_back(mgr);
      node_debug("adding app launch request %d", aid);
      keep_going = true;
      last_used_aid = aid;

      App::dlopenCheck(aid, app_params, false/*no name check*/);
    } else {
      keep_going = false;
    }
    ++aid;
  }


  aid = last_used_aid+1;

  std::vector<std::string> services_to_launch;
  if (params.contains("services")){
    params.find_array("services", services_to_launch);
  }
  for (std::string& str : services_to_launch){
    SST::Params srv_params = params.find_scoped_params(str);
    //setup the name for app factory
    srv_params.insert("name", "distributed_service");
    //setup the name for distributed service
    srv_params.insert("libname", str);
    AppLaunchRequest* mgr = new AppLaunchRequest(srv_params, AppId(aid), str);
    node_debug("adding distributed service %s", str.c_str());
    initial_requests_.push_back(mgr);
    ++aid;
  }

  //just in case any memoizations were loaded
  os_->rebuildMemoizations();
}

void
JobLauncher::cleanupApp(JobStopRequest* ev)
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
      spkt_throw_printf(sprockit::ValueError,
                        "allocation requested node %d, but it's not available",
                        int(nid));
    }
    available_.erase(nid);
  }
  return true;
}

void
DefaultJoblauncher::stopEventReceived(JobStopRequest *ev)
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
ExclusiveJoblauncher::stopEventReceived(JobStopRequest *ev)
{
  active_job_ = nullptr;
  if (!pending_requests_.empty()){
    AppLaunchRequest* next = pending_requests_.front();
    pending_requests_.pop_front(); //remove the running job
    JobLauncher::incomingLaunchRequest(next);
  }
  os_->decrementAppRefcount();
}

}
}

