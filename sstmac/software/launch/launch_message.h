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
#include <sstmac/common/messages/timed_message.h>
#include <sstmac/software/launch/launch_info.h>

namespace sstmac {
namespace sw {

class launch_message :
  public hw::network_message,
  public library_interface,
  public timed_interface
{

 public:
  enum LAUNCHTYPE {
    ARRIVE, START, COMPLETE, KILL, RESTART
  };

 public:
  launch_message(launch_info* i,
                 LAUNCHTYPE t,
                 task_id tid) :
    library_interface("launcher"),
    timed_interface(timestamp(0)),
    network_message(node_id(), node_id(), tid, tid, 0), //use zero bytes
    info_(i),
    launchtype_(t),
    tid_(tid) {
    needs_ack_ = false;
  }

  /**
   * Stringifier
   * @return String description
   */
  virtual std::string
  to_string() const {
    return "launch_message";
  }

  launch_info*
  get_info() {
    return info_;
  }

  virtual long
  byte_length() const {
    return 16;
  }

  LAUNCHTYPE
  get_launch_type() const {
    return launchtype_;
  }

  task_id
  tid() const {
    return tid_;
  }

  virtual void
  serialize_order(sprockit::serializer& ser){
    spkt_throw(sprockit::unimplemented_error,
        "launch_message::serializer_order");
  }

 protected:
  launch_info* info_;
  LAUNCHTYPE launchtype_;
  task_id tid_;

};

}
}

#endif

