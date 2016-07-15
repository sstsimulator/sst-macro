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
#include <sstmac/common/logger.h>

#include <sstmac/dumpi_util/dumpi_meta.h>

#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/launch/job_launch_event.h>
#include <sstmac/software/launch/app_launch.h>

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

using namespace sstmac::sw;
using namespace sstmac::hw;

const char* keywords[] = { "sst_rank", "sst_nproc" };
sprockit::StaticKeywordRegister reg_keywords(2, keywords);

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

//
// Default constructor.
//
manager::manager() :
  next_ppid_(0),
  interconnect_(0),
  rt_(0)
{
}

//
// Define a network.
//
void
manager::init_factory_params(sprockit::sim_parameters* params)
{
  build_apps(params);
}

int
manager::compute_max_nproc_for_app(sprockit::sim_parameters* app_params)
{
  int max_nproc = 0;
  /** Do a bunch of dumpi stuff */
  static const char* dmeta = "launch_dumpi_metaname";
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
         "no dumpi file found in folder or specified with launch_dumpi_metaname");
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
  app_launch::parse_launch_cmd(app_params, nproc,
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

void
manager::build_apps(sprockit::sim_parameters *params)
{
  int appnum = 1;
  bool found_app = true;
  while (found_app || appnum < 10) {
    std::string app_namespace = sprockit::printf("app%d", appnum);
    found_app = params->has_namespace(app_namespace);
    if (found_app){
      sprockit::sim_parameters* app_params
          = params->get_namespace(app_namespace);
      build_app(appnum, app_params);
    }
    ++appnum;
  }
}

void
manager::build_app(int appnum,
 sprockit::sim_parameters* params)
{
  sstmac::sw::app_id aid(appnum);
  app_launch* appman = app_launch_factory::get_optional_param(
        "launch_type", "default", params, aid, rt_);
  appman->set_topology(interconnect_->topol());

  app_managers_[appnum] = appman;
}

manager::~manager() throw ()
{
  if (sprockit::debug::prefix_fxn) 
    delete sprockit::debug::prefix_fxn;
  sprockit::debug::prefix_fxn = 0;

  if (interconnect_) delete interconnect_;

  std::map<int, app_launch*>::iterator it, end = app_managers_.end();
  for (it=app_managers_.begin(); it != end; ++it){
    delete it->second;
  }
}

#if SSTMAC_INTEGRATED_SST_CORE
void
sst_manager::init_factory_params(sprockit::sim_parameters* params)
{
  //these are not used
  parallel_runtime* rt = 0;
  partition* part = 0;
  const char* ic_param = params->has_param("network_name") ? "network_name" : "interconnect";
  interconnect_ = interconnect_factory::get_param(ic_param, params, part, rt);
}
#else
void
macro_manager::init_factory_params(sprockit::sim_parameters* params)
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

  /** sstkeyword {
        docstring = Specify the general type of network congestion model that will be used
                    for the interconnect;
        gui = train;
  } */
  const char* ic_param = params->has_param("network_name") ? "network_name" : "interconnect";
  interconnect_ = interconnect_factory::get_param(ic_param, params, event_manager_->topology_partition(), rt_);

  event_manager_->set_interconnect(interconnect_);
  interconnect_->set_event_manager(event_manager_);

  launcher_ = job_launcher_factory::get_optional_param("job_launcher", "default", params);
  launcher_->set_interconnect(interconnect_);

  sstmac::runtime::set_job_launcher(launcher_);

  logger::timer_ = event_manager_;

  //this should definitely be called last
  manager::init_factory_params(params);
}

void
macro_manager::start()
{
  launch_apps();
}

//
// Start the simulation.
//
timestamp
macro_manager::run(timestamp until)
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
macro_manager::stop()
{
  event_manager::global = 0;

  runtime::finish();
}

macro_manager::macro_manager(parallel_runtime* rt) :
  running_(false),
  event_manager_(0)
{
  rt_ = rt;
}

void
macro_manager::finish()
{
  //interconnect_->deadlock_check();
  event_manager_->finish_stats();
  event_manager::global = 0;
  logger::timer_ = 0;
}

void
macro_manager::launch_app(int appnum, timestamp start, sw::app_launch* appman)
{
  sw::job_launch_event* ev = new sw::job_launch_event(appman);
  event_manager_->schedule(start, appnum,
                new handler_event_queue_entry(ev, launcher_, event_loc_id::null));
}

void
macro_manager::launch_apps()
{
  std::map<int, app_launch*>::iterator it, end = app_managers_.end();
  for (it=app_managers_.begin(); it != end; ++it){
    int appnum = it->first;
    app_launch* appman = it->second;
    launch_app(appnum, appman->start(), appman);
  }
}

//
// Goodbye.
//
macro_manager::~macro_manager() throw ()
{
  if (this->running_) {
    cerrn << "FATAL:  manager going out of scope while still running.\n";
    abort();
  }
  if (event_manager_) delete event_manager_;
}
#endif

}
} // end of namespace sstmac


