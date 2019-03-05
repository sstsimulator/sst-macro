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

#include <sstmac/software/launch/app_launcher.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>
#include <sstmac/common/thread_lock.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <unistd.h>
#include <getopt.h>

namespace sstmac {
namespace sw {

AppLauncher::AppLauncher(OperatingSystem* os) :
  is_completed_(false),
  Service(std::string("launcher"), SoftwareId(0,0), os)
{
}

AppLauncher::~AppLauncher() throw()
{
}

void
AppLauncher::incomingRequest(Request* req)
{
  StartAppRequest* lreq = safe_cast(StartAppRequest, req);
  if (lreq->type() == LaunchRequest::Start){
    TaskMapping::addGlobalMapping(lreq->aid(), lreq->uniqueName(), lreq->mapping());

    //if necessary, bcast this to whomever else needs it
    os_->outcastAppStart(lreq->tid(), lreq->aid(), lreq->uniqueName(),
                         lreq->mapping(), lreq->appParams());

    SoftwareId sid(lreq->aid(), lreq->tid());
    SST::Params app_params = lreq->appParams();

    App::dlopenCheck(lreq->aid(), app_params);
    App* theapp = sprockit::create<App>(
          "macro", app_params.find<std::string>("name"), app_params, sid, os_);
    theapp->setUniqueName(lreq->uniqueName());
    int intranode_rank = num_apps_launched_[lreq->aid()]++;
    int core_affinity = lreq->coreAffinity(intranode_rank);
    if (core_affinity != Thread::no_core_affinity){
      theapp->setAffinity(core_affinity);
    }

    os_->startApp(theapp, lreq->uniqueName());
  }
  delete lreq;
}

void
AppLauncher::start()
{
  Service::start();
  if (!os_) {
    spkt_throw_printf(sprockit::ValueError,
                     "AppLauncher::start: OS hasn't been registered yet");
  }
}

hw::NetworkMessage*
LaunchRequest::cloneInjectionAck() const
{
  spkt_abort_printf("launch event should never be cloned for injection");
  return nullptr;
}

int
StartAppRequest::coreAffinity(int intranode_rank) const
{
  return Thread::no_core_affinity;
}

void
StartAppRequest::serialize_order(serializer &ser)
{
  LaunchRequest::serialize_order(ser);
  ser & unique_name_;
  ser & app_params_;
  mapping_ = TaskMapping::serialize_order(aid(), ser);
}

std::string
StartAppRequest::toString() const
{
  return sprockit::printf("start_app_event: app=%d task=%d node=%d", aid(), tid(), toaddr());
}

std::string
JobStopRequest::toString() const
{
  return sprockit::printf("job_stop_event: app=%d task=%d node=%d", aid(), tid(), fromaddr());
}


}
}
