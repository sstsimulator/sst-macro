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

#include <sstmac/software/launch/launcher.h>
#include <sstmac/software/launch/launch_message.h>
#include <sstmac/software/process/operating_system.h>
#include <sprockit/util.h>
#include <unistd.h>
#include <getopt.h>


ImplementFactory(sstmac::sw::launcher);

namespace sstmac {
namespace sw {

launcher::launcher() :
  is_completed_(false)
{
  libname_ = "launcher";
}

launcher::~launcher() throw()
{
}

void
launcher::incoming_message(message* msg)
{
  launch_message* lmsg = safe_cast(launch_message, msg);
  if (lmsg->is_nic_ack()){
    delete lmsg;
    return;
  }

  launch_info* linfo = lmsg->info();

  if (lmsg->launch_type() == launch_message::ARRIVE) {
    software_id sid(linfo->aid(), lmsg->tid());
    app* theapp = linfo->app_template()->clone(sid);
    theapp->consume_params(linfo->app_template()->params());
    int intranode_rank = num_apps_launched_[linfo->aid()]++;
    int core_affinity = linfo->core_affinity(intranode_rank);
    theapp->set_affinity(core_affinity);
    os_->start_app(theapp);

  }
  else if(lmsg->launch_type() == launch_message::COMPLETE) {
    //do nothing
  }

  delete lmsg;
}

void
launcher::start()
{
  service::start();
  if (!os_) {
    spkt_throw_printf(sprockit::value_error,
                     "instantlaunch::start: OS hasn't been registered yet");
  }
}


}
}

