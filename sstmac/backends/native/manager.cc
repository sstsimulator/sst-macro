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

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/node/node.h>

#include <sstmac/common/sstmac_env.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/backends/native/manager.h>
#include <sstmac/backends/native/clock_cycle_event_container.h>

#include <sstmac/common/runtime.h>

#include <sstmac/dumpi_util/dumpi_meta.h>

#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/launch/job_launch_event.h>
#include <sstmac/software/launch/launch_request.h>

#include <sprockit/driver_util.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/output.h>
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

#include <iostream>
#include <iterator>
#include <cstdlib>


namespace sstmac {
namespace native {

static std::string get_first_line_from_cmd(const std::string& cmd)
{
  FILE* fp = popen(cmd.c_str(), "r");
  char buf[4096];
  char* ret = fgets(buf, 4096, fp);
  if (ret){
    std::string ret(buf);
    auto pos = ret.find_first_of('\n');
    if (pos == std::string::npos){
      return ret;
    } else {
      return ret.substr(0, pos);
    }
  } else {
    return std::string();
  }
}

static std::string get_file_from_suffix(const std::string& suffix)
{
  std::string cmd = "ls *." + suffix;
  return get_first_line_from_cmd(cmd);
}

static std::string find_file_from_suffix(const std::string& suffix)
{
  std::string cmd = "find . -name '*." + suffix + "'";
  return get_first_line_from_cmd(cmd);
}

static int recursive_count_files_from_suffix(const std::string& suffix)
{
  std::string cmd = "find . -name '*." + suffix + "' | wc";
  std::string ret = get_first_line_from_cmd(cmd);
  if (ret.empty()) return 0;

  std::istringstream istr(ret);
  int n; istr >> n;
  return n;
}

int
Manager::computeMaxNprocForApp(sprockit::SimParameters::ptr& app_params)
{
  int max_nproc = 0;
  /** Do a bunch of dumpi stuff */
  static const std::string dmeta = "dumpi_metaname";
  static const std::string ometa = "otf2_metafile";
  if (!app_params->hasParam("launch_cmd")){
    int nproc = 0;
    if (app_params->getParam("name") == "parsedumpi"){
      std::string dumpi_meta_filename;
      if (!app_params->hasParam(dmeta)){
        dumpi_meta_filename = get_file_from_suffix("meta");
        if (dumpi_meta_filename.empty()){
          sprockit::abort("no dumpi file found in folder or specified with dumpi_metaname");
        } else {
          app_params->addParamOverride(dmeta, dumpi_meta_filename);
        }
      } else {
        dumpi_meta_filename = app_params->getParam(dmeta);
      }
      sw::DumpiMeta* meta = new sw::DumpiMeta(dumpi_meta_filename);
      nproc = meta->numProcs();
      delete meta;
    } else if (app_params->getParam("name") == "parseotf2"){
      std::string otf2_meta_filename;
      if (!app_params->hasParam(ometa)){
        otf2_meta_filename = find_file_from_suffix("otf2");
        if (otf2_meta_filename.empty()){
          sprockit::abort("no OTF2 file found in folder or specified with otf2_metafile");
        } else {
          app_params->addParamOverride(ometa, otf2_meta_filename);
        }
      } else {
        otf2_meta_filename = ometa;
      }
      nproc = recursive_count_files_from_suffix("evt");
    } else {
    }
    max_nproc = std::max(max_nproc, nproc);
    std::string cmd = sprockit::sprintf("aprun -n %d -N 1", nproc);
    app_params->addParamOverride("launch_cmd", cmd);
  }

#if SSTMAC_INTEGRATED_SST_CORE
  SST::Params sst_params;
  if (app_params->hasParam("launch_cmd"))
    sst_params.insert("launch_cmd", app_params->getParam("launch_cmd"));
  else if (app_params->hasParam("size"))
    sst_params.insert("size", app_params->getParam("size"));
  else if (app_params->hasParam("concentration"))
    sst_params.insert("concentration", app_params->getParam("concentration"));
#else
  SST::Params sst_params(app_params);
#endif

  int nproc, procs_per_node;
  std::vector<int> ignore;
  sw::AppLaunchRequest::parseLaunchCmd(sst_params, nproc, procs_per_node, ignore);
  return std::max(nproc, max_nproc);
}

int
Manager::computeMaxNproc(sprockit::SimParameters::ptr& params)
{
  int appnum = 1;
  int max_nproc = 0;
  bool found_app = true;
  sprockit::SimParameters::ptr node_params = params->getNamespace("node");
  while (found_app || appnum < 10) {
    std::string app_namespace = sprockit::sprintf("app%d", appnum);
    if (node_params->hasNamespace(app_namespace)){
      auto app_params = node_params->getNamespace(app_namespace);
      int nproc = computeMaxNprocForApp(app_params);
      max_nproc = std::max(nproc, max_nproc);
    } else {
      found_app = false;
    }
    ++appnum;
  }
  return max_nproc;
}

#if SSTMAC_INTEGRATED_SST_CORE
Manager::Manager(SST::Params& /*params*/, ParallelRuntime* /*rt*/){}
#else
class TimestampPrefixFxn :
  public sprockit::DebugPrefixFxn
{
 public:
  TimestampPrefixFxn(SST::Params& params, EventManager* mgr) :
    mgr_(mgr)
  {
    units_ = params.find<std::string>("timestamp_print_units", "s");
    if (units_ == "ns"){
      mult_ = 1e9;
    } else if (units_ == "us"){
      mult_ = 1e6;
    } else if (units_ == "ms"){
      mult_ = 1e3;
    } else if (units_ == "s"){
      mult_ = 1;
    } else {
      spkt_abort_printf("invalid timestamp units for printing function: %s", units_.c_str());
    }
  }

  std::string str() {
    double t = mgr_->now().sec() * mult_;
    return sprockit::sprintf("T=%14.8f %s:", t, units_.c_str());
  }

 private:
  EventManager* mgr_;
  std::string units_;
  double mult_;

};

//
// Default constructor.
//
Manager::Manager(SST::Params& params, ParallelRuntime* rt) :
  interconnect_(nullptr),
  rt_(rt)
{
  std::string event_man = "map";
  if (rt_->nthread() > 1){
#if !SSTMAC_USE_MULTITHREAD
    spkt_abort_printf("did not compile with multithread support: cannot use nthread > 1");
    event_man = "multithread";
#endif
  } else if (rt_->nproc() > 1){
    event_man = "clock_cycle_parallel";
  }
  auto type = params.find<std::string>("event_manager", event_man);
  EventManager_ = sprockit::create<EventManager>("macro",type,params,rt_);
  EventManager::global = EventManager_;

  if (sprockit::Debug::slotActive(sprockit::dbg::timestamp)){
    sprockit::Debug::prefix_fxn = std::unique_ptr<sprockit::DebugPrefixFxn>(
          new TimestampPrefixFxn(params, EventManager_));
  }

  bool debug_startup = params.find<bool>("debug_startup", true);
  if (!debug_startup){
    sprockit::Debug::turnOff();
  }

  interconnect_ = hw::Interconnect::staticInterconnect(params, EventManager_);

  EventManager_->setInterconnect(interconnect_);
}

Manager::~Manager() throw ()
{
  sprockit::Debug::prefix_fxn = nullptr;
  if (this->running_){
    cerrn << "FATAL:  manager going out of scope while still running.\n";
    abort();
  }
  if (EventManager_) delete EventManager_;
}

void
Manager::start()
{
}

static void runManager(void* args){
  EventManager* mgr = (EventManager*) args;
  mgr->run();
}

Timestamp
Manager::run(Timestamp until)
{
  start();

  running_ = true;

  if (until.time.ticks() > 0) {
    EventManager_->scheduleStop(until);
  }

  //this is a little convoluted here, but necessary
  //to make multithreading easier
  EventManager_->spinUp(runManager, EventManager_);

  running_ = false;
  // Now call done routine to end simulation and print Stats.
  stop();


  return EventManager_->finalTime();
}

void
Manager::stop()
{
  EventManager::global = nullptr;
  Runtime::finish();
}


void
Manager::finish()
{
  EventManager_->finishStats();
  EventManager::global = nullptr;
}


#endif

}
} // end of namespace sstmac
