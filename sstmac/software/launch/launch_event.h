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
#include <sstmac/software/launch/job_launcher.h>
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

  app_id
  aid() const {
    return aid_;
  }

  std::string
  unique_name() const {
    return unique_name_;
  }

  type_t
  type() const {
    return ty_;
  }

 protected:
  launch_event(type_t ty, app_id aid, task_id tid,
               const std::string& unique_name,
               node_id to, node_id from,
               const std::string& libname) :
    ty_(ty), aid_(aid), tid_(tid),
    unique_name_(unique_name),
    library_interface(libname),
    network_message(aid, to, from, 0)
  {
    network_message::type_ = payload;
    set_needs_ack(false);
  }

  launch_event(){} //for serialization

 private:
  app_id aid_;
  task_id tid_;
  std::string unique_name_;
  type_t ty_;

};

class start_app_event : public launch_event {

 public:
  start_app_event(app_id aid,
     const std::string& unique_name,
     task_mapping::ptr mapping,
     task_id tid,
     node_id to,
     node_id from,
     sprockit::sim_parameters* app_params) :
    launch_event(Start, aid, tid, unique_name, to, from, "launcher"),
    app_params_(app_params),
    mapping_(mapping)
  {
  }

  int
  core_affinity(int intranode_rank) const;

  start_app_event(){} //for serialization

  void
  serialize_order(serializer& ser) override {
    spkt_throw_printf(sprockit::unimplemented_error,
                      "start_app_event::serialize_order");
  }

  sprockit::sim_parameters*
  app_params() const {
    return app_params_;
  }

  task_mapping::ptr
  mapping() const {
    return mapping_;
  }

 private:
  app_id aid_;
  task_id tid_;
  std::string unique_name_;
  task_mapping::ptr mapping_;
  sprockit::sim_parameters* app_params_;

};

class job_stop_event : public launch_event
{
 public:
  job_stop_event(app_id aid,
     const std::string& unique_name,
     node_id to,
     node_id from) :
    launch_event(Stop, aid, 0, unique_name, to, from, "job_launcher")
  {
  }
};

}
}

#endif

