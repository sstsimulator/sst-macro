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

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/node/node.h>

#include <sstmac/common/sstmac_env.h>
#include <sstmac/backends/common/sim_partition.h>
#if !SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/backends/native/event_map.h>
#endif
#include <sstmac/backends/native/manager.h>
#include <sstmac/backends/native/clock_cycle_parallel/clock_cycle_event_container.h>

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
"event_manager",
"sst_rank",
"sst_nproc",
"nworkers",
);


namespace sstmac {
namespace native {

using namespace sstmac::sw;
using namespace sstmac::hw;

class timestamp_prefix_fxn :
  public sprockit::debug_prefix_fxn
{
 public:
  timestamp_prefix_fxn(event_manager* mgr) : mgr_(mgr){}

  std::string
  str() {
    double t_ms = mgr_->now().msec();
    return sprockit::printf("T=%12.8e ms:", t_ms);
  }

 private:
  event_manager* mgr_;
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
        spkt_throw(sprockit::input_error,
         "no dumpi file found in folder or specified with dumpi_metaname");
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
  while (found_app || appnum < 10) {
    std::string app_namespace = sprockit::printf("app%d", appnum);
    found_app = params->has_namespace(app_namespace);
    if (found_app){
      sprockit::sim_parameters* app_params = params->get_namespace(app_namespace);
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
  event_manager_ = event_manager_factory::get_optional_param(
                       "event_manager", SSTMAC_DEFAULT_EVENT_MANAGER_STRING, params, rt_);
  event_manager::global = event_manager_;

  if (sprockit::debug::slot_active(sprockit::dbg::timestamp)){
    sprockit::debug_prefix_fxn* fxn = new timestamp_prefix_fxn(event_manager_);
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
  sprockit::debug::prefix_fxn = 0;
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

timestamp
manager::run(timestamp until)
{
  start();

  running_ = true;

  if (until.sec() > 0) {
    event_manager_->schedule_stop(until);
  }

  event_manager_->run();

  running_ = false;
  // Now call done routine to end simulation and print Stats.
  stop();


  return event_manager_->now();
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


