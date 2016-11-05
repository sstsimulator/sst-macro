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
#include <sprockit/sim_parameters_fwd.h>

namespace sstmac {
namespace sw {

class launch_event :
  public hw::network_message,
  public library_interface,
  public timed_interface
{
  ImplementSerializable(launch_event)

 public:
  typedef enum {
    Start,
    Stop
  } type_t;

  std::string
  to_string() const override {
    return sprockit::printf("launch event app=%d task=%d node=%d",
                            aid_, tid_, toaddr_);
  }

  launch_event(type_t ty,
     app_id aid,
     task_id tid,
     node_id to,
     node_id from) :
    network_message(aid, to, from, 0),
    library_interface("launcher"),
    timed_interface(timestamp(0)),
    tid_(tid),
    aid_(aid),
    ty_(ty)
  {
    type_ = payload;
  }

  launch_event(){} //for serialization

  void
  serialize_order(serializer& ser) override {
    timed_interface::serialize_order(ser);
    library_interface::serialize_order(ser);
    hw::network_message::serialize_order(ser);
    ser & ty_;
    ser & aid_;
    ser & tid_;
  }

  task_id
  tid() const {
    return tid_;
  }

  type_t
  type() const {
    return ty_;
  }

  app_id
  aid() const {
    return aid_;
  }

  int
  core_affinity(int intranode_rank) const;

 protected:
  type_t ty_;
  app_id aid_;
  task_id tid_;
};

}
}

#endif

