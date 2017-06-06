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
    sprockit::sim_parameters* app_params = new sprockit::sim_parameters(&lev->app_params());
    //lev->app_params().moved();
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
