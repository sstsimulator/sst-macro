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

RegisterKeywords(
{ "event_manager", "the type of event manager for scheduling/managing events" },
{ "sst_rank", "my logical process within a parallel SST run" },
{ "sst_nproc", "the total number of logical processes within an SST run" },
{ "timestamp_print_units", "the units of time to print on debug statements" },
);


namespace sstmac {
namespace native {

using namespace sstmac::sw;
using namespace sstmac::hw;

class timestamp_prefix_fxn :
  public sprockit::debug_prefix_fxn
{
 public:
  timestamp_prefix_fxn(sprockit::sim_parameters* params, event_manager* mgr) :
    mgr_(mgr)
  {
    units_ = params->get_optional_param("timestamp_print_units", "s");
    if (units_ == "ns"){
      mult_ = 1e9;
    } else if (units_ == "us"){
      mult_ = 1e6;
    } else if (units_ == "s"){
      mult_ = 1e3;
    } else if (units_ == "s"){
      mult_ = 1;
    } else {
      spkt_abort_printf("invalid timestamp units for printing function: %s", units_.c_str());
    }
  }

  std::string str() {
    double t = mgr_->now().sec() * mult_;
    return sprockit::printf("T=%14.8f %s:", t, units_.c_str());
  }

 private:
  event_manager* mgr_;
  std::string units_;
  double mult_;

};

int
manager::compute_max_nproc_for_app(sprockit::sim_parameters* app_params)
{
  int max_nproc = 0;
  /** Do a bunch of dumpi stuff */
  static const char* dmeta = "dumpi_metaname";
  if (app_params->get_param("name") == "parsedumpi"
    && !app_params->has_param("launch_cmd"))
  {
    std::string dumpi_meta_filename;
    if (!app_params->has_param(dmeta)){
      FILE *fp = popen("ls *.meta", "r");
      char buf[1024];
      char* ret = fgets(buf, 1024, fp);
      int len = ::strlen(buf);
      if (ret){
        char& lastchar = buf[len-1];
        if (lastchar == '\n'){
          lastchar = '\0';
        }
        cout0 << "Using dumpi meta " << buf << std::endl;
        app_params->add_param(dmeta, buf);
      } else {
        sprockit::abort("no dumpi file found in folder or specified with dumpi_metaname");
      }
      dumpi_meta_filename = buf;
    } else {
      dumpi_meta_filename = app_params->get_param(dmeta);
    }
    sw::dumpi_meta* meta = new sw::dumpi_meta(dumpi_meta_filename);
    int nproc = meta->num_procs();
    std::string cmd = sprockit::printf("aprun -n %d -N 1", nproc);
    app_params->add_param("launch_cmd", cmd);
    max_nproc = std::max(max_nproc, nproc);
    delete meta;
  }
  int nproc, procs_per_node;
  std::vector<int> ignore;
  app_launch_request::parse_launch_cmd(app_params, nproc,
    procs_per_node, ignore);
  return std::max(nproc, max_nproc);
}

int
manager::compute_max_nproc(sprockit::sim_parameters* params)
{
  int appnum = 1;
  int max_nproc = 0;
  bool found_app = true;
  sprockit::sim_parameters* node_params = params->get_namespace("node");
  while (found_app || appnum < 10) {
    std::string app_namespace = sprockit::printf("app%d", appnum);
    found_app = node_params->has_namespace(app_namespace);
    if (found_app){
      sprockit::sim_parameters* app_params = node_params->get_namespace(app_namespace);
      int nproc = compute_max_nproc_for_app(app_params);
      max_nproc = std::max(nproc, max_nproc);
    }
    ++appnum;
  }
  return max_nproc;
}

#if SSTMAC_INTEGRATED_SST_CORE
manager::manager(sprockit::sim_parameters* params, parallel_runtime* rt){}
#else
//
// Default constructor.
//
manager::manager(sprockit::sim_parameters* params, parallel_runtime* rt) :
  next_ppid_(0),
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
  event_manager_ = event_manager::factory::get_optional_param("event_manager", event_man, params, rt_);
  event_manager::global = event_manager_;

  if (sprockit::debug::slot_active(sprockit::dbg::timestamp)){
    sprockit::debug_prefix_fxn* fxn = new timestamp_prefix_fxn(params, event_manager_);
    sprockit::debug::prefix_fxn = fxn;
  }

  bool debug_startup = params->get_optional_bool_param("debug_startup", true);
  if (!debug_startup){
    sprockit::debug::turn_off();
  }

  interconnect_ = hw::interconnect::static_interconnect(params, event_manager_);

  event_manager_->set_interconnect(interconnect_);
}

manager::~manager() throw ()
{
  if (sprockit::debug::prefix_fxn) 
    delete sprockit::debug::prefix_fxn;
  sprockit::debug::prefix_fxn = nullptr;
  if (this->running_){
    cerrn << "FATAL:  manager going out of scope while still running.\n";
    abort();
  }
  if (event_manager_) delete event_manager_;
}

void
manager::start()
{
}

static void run_manager(void* args){
  event_manager* mgr = (event_manager*) args;
  mgr->run();
}

timestamp
manager::run(timestamp until)
{
  start();

  running_ = true;

  if (until.ticks_int64() > 0) {
    event_manager_->schedule_stop(until);
  }

  //this is a little convoluted here, but necessary
  //to make multithreading easier
  event_manager_->spin_up(run_manager, event_manager_);

  running_ = false;
  // Now call done routine to end simulation and print Stats.
  stop();


  return event_manager_->final_time();
}

void
manager::stop()
{
  event_manager::global = nullptr;
  runtime::finish();
}


void
manager::finish()
{
  //interconnect_->deadlock_check();
  event_manager_->finish_stats();
  event_manager::global = nullptr;
}


#endif

}
} // end of namespace sstmac
