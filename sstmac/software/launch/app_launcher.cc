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

#include <sstmac/software/launch/app_launcher.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <unistd.h>
#include <getopt.h>

namespace sstmac {
namespace sw {

app_launcher::app_launcher(operating_system* os) :
  is_completed_(false),
  service(std::string("launcher"), software_id(0,0), os)
{
}

app_launcher::~app_launcher() throw()
{
}

void
app_launcher::incoming_event(event* ev)
{
  start_app_event* lev = safe_cast(start_app_event, ev);
  if (lev->type() == launch_event::Start){
    task_mapping::add_global_mapping(lev->aid(), lev->unique_name(), lev->mapping());
    software_id sid(lev->aid(), lev->tid());
    sprockit::sim_parameters* app_params = new sprockit::sim_parameters(std::move(lev->app_params()));
    app* theapp = app::factory::get_param("name", app_params, sid, os_);
    theapp->set_unique_name(lev->unique_name());
    int intranode_rank = num_apps_launched_[lev->aid()]++;
    int core_affinity = lev->core_affinity(intranode_rank);
    theapp->set_affinity(core_affinity);
    os_->start_app(theapp, lev->unique_name());
  }
  delete lev;
}

void
app_launcher::start()
{
  service::start();
  if (!os_) {
    spkt_throw_printf(sprockit::value_error,
                     "app_launcher::start: OS hasn't been registered yet");
  }
}

hw::network_message*
launch_event::clone_injection_ack() const
{
  spkt_abort_printf("launch event should never be cloned for injection");
}

int
start_app_event::core_affinity(int intranode_rank) const
{
  return thread::no_core_affinity;
}

void
start_app_event::serialize_order(serializer &ser)
{
  launch_event::serialize_order(ser);
  ser & unique_name_;
  if (ser.mode() == ser.UNPACK){
    std::string paramStr;
    ser & paramStr;
    std::stringstream sstr(paramStr);
    app_params_.parse_stream(sstr, false, true);
  } else {
    std::stringstream sstr;
    app_params_.reproduce_params(sstr);
    std::string paramStr = sstr.str();
    ser & paramStr;
  }
  mapping_ = task_mapping::serialize_order(aid_, ser);
}

std::string
start_app_event::to_string() const
{
  return sprockit::printf("start_app_event: app=%d task=%d node=%d", aid_, tid(), toaddr());
}

std::string
job_stop_event::to_string() const
{
  return sprockit::printf("job_stop_event: app=%d task=%d node=%d", aid_, tid(), fromaddr());
}


}
}

