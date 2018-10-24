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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_LAUNCH_MESSAGES_LAUNCH_MESSAGE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_LAUNCH_MESSAGES_LAUNCH_MESSAGE_H_INCLUDED

#include <sstmac/hardware/network/network_message.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/process/app_fwd.h>
#include <sstmac/software/process/app_id.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sprockit/thread_safe_new.h>

namespace sstmac {
namespace sw {

class launch_event : public hw::network_message
{
 public:
  typedef enum {
    Start,
    Stop
  } type_t;

  std::string to_string() const override {
    return sprockit::printf("launch event app=%d task=%d node=%d",
                            aid(), tid_, toaddr());
  }

  void serialize_order(serializer& ser) override {
    hw::network_message::serialize_order(ser);
    ser & ty_;
    ser & tid_;
  }

  task_id tid() const {
    return tid_;
  }

  std::string unique_name() const {
    return unique_name_;
  }

  type_t type() const {
    return ty_;
  }

  network_message* clone_injection_ack() const override;

 protected:
  launch_event(uint64_t flow_id,
               type_t ty, app_id aid, task_id tid,
               const std::string& unique_name,
               node_id to, node_id from,
               const std::string& libname) :
    ty_(ty), tid_(tid),
    unique_name_(unique_name),
    network_message(flow_id, libname, aid, to, from, sizeof(launch_event),
                    false, nullptr, header{})
  {
  }

  launch_event(){} //for serialization

 private:
  task_id tid_;
  std::string unique_name_;
  type_t ty_;

};

class start_app_event :
  public launch_event,
  public sprockit::thread_safe_new<start_app_event>
{
  ImplementSerializable(start_app_event)
 public:
  start_app_event(uint64_t flow_id, app_id aid,
     const std::string& unique_name,
     task_mapping::ptr mapping,
     task_id tid,
     node_id to,
     node_id from,
     const sprockit::sim_parameters* app_params) :
    launch_event(flow_id, Start, aid, tid, unique_name, to, from, "launcher"),
    mapping_(mapping),
    app_params_(app_params)
  {
  }

  int core_affinity(int intranode_rank) const;

  std::string to_string() const override;

  start_app_event() {} //for serialization

  void serialize_order(serializer& ser) override;

  sprockit::sim_parameters& app_params() {
    return app_params_;
  }

  task_mapping::ptr mapping() const {
    return mapping_;
  }

 private:
  std::string unique_name_;
  task_mapping::ptr mapping_;
  sprockit::sim_parameters app_params_;

};

class job_stop_event : public launch_event
{
  ImplementSerializable(job_stop_event)
 public:
  job_stop_event(uint64_t flow_id, app_id aid,
     const std::string& unique_name,
     node_id to,
     node_id from) :
    launch_event(flow_id, Stop, aid, 0, unique_name, to, from, "job_launcher")
  {
  }

  job_stop_event(){} //for serialization

  std::string to_string() const override;
};

}
}

#endif
