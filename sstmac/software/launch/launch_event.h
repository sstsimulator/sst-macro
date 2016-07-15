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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_LAUNCH_MESSAGES_LAUNCH_MESSAGE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_LAUNCH_MESSAGES_LAUNCH_MESSAGE_H_INCLUDED

#include <sstmac/hardware/network/network_message.h>
#include <sstmac/common/messages/library_message.h>
#include <sstmac/common/messages/timed_event.h>
#include <sstmac/software/process/app_fwd.h>
#include <sstmac/software/process/app_id.h>

namespace sstmac {
namespace sw {

class launch_event :
  public event,
  public library_interface,
  public timed_interface
{
  NotSerializable(launch_event)

 public:
  launch_event(app* apptype,
               app_id aid,
               task_id tid,
               const std::vector<int>& core_affinities) :
    library_interface("launcher"),
    timed_interface(timestamp(0)),
    tid_(tid),
    aid_(aid),
    apptype_(apptype),
    core_affinities_(core_affinities)
  {
  }

  /**
   * Stringifier
   * @return String description
   */
  virtual std::string
  to_string() const {
    return "launch event";
  }

  task_id
  tid() const {
    return tid_;
  }

  app_id
  aid() const {
    return aid_;
  }

  app*
  app_template() const {
    return apptype_;
  }

  int
  core_affinity(int intranode_rank) const;

 protected:
  task_id tid_;
  app* apptype_;
  app_id aid_;
  std::vector<int> core_affinities_;
};

}
}

#endif

