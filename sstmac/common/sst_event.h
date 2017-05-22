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

#ifndef SSTMAC_BACKENDS_NATIVE_SSTEVENT_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_SSTEVENT_H_INCLUDED

#include <sstmac/common/serializable.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_scheduler_fwd.h>
#include <sstmac/common/event_location.h>
#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/event.h>
#include <sst/core/output.h>
#endif

namespace sstmac {


#if SSTMAC_INTEGRATED_SST_CORE
typedef SST::Event event;
#else
class event :
 public serializable
{
 public:
  void serialize_order(serializer& ser){}

  virtual bool is_payload() const {
    return true;
  }

  virtual bool is_ack() const {
    return false;
  }
};
#endif

class event_queue_entry : public event
{
  NotSerializable(event_queue_entry)
 public:
  virtual ~event_queue_entry() {}

#if SSTMAC_INTEGRATED_SST_CORE
  virtual void execute() override = 0;

  event_queue_entry(device_id dst, device_id src)
  {
    //simply ignore parameters - not needed
  }
#else
  event_queue_entry(device_id dst,
    device_id src) :
    dst_loc_(dst),
    src_loc_(src),
    seqnum_(0)
  {
  }

  virtual void execute() = 0;

  timestamp time() const {
    return time_;
  }

  void set_time(const timestamp& t) {
    time_ = t;
  }

  void set_seqnum(uint32_t seqnum) {
    seqnum_ = seqnum;
  }

  uint32_t seqnum() const {
    return seqnum_;
  }

  device_id event_location() const {
    return dst_loc_;
  }

  device_id src_location() const {
    return src_loc_;
  }

#if SSTMAC_HAVE_EVENT_CALENDAR
  event* next;
#endif

 protected:
  timestamp time_;
  device_id dst_loc_;
  device_id src_loc_;
  /** A unique sequence number from the source */
  uint32_t seqnum_;
#endif

};

class handler_event_queue_entry :
  public event_queue_entry
{

 public:
  virtual ~handler_event_queue_entry() {}

  handler_event_queue_entry(event* ev,
    event_handler* hand,
    device_id src_loc);

  void execute() override;

 protected:
  event* ev_to_deliver_;

  event_handler* handler_;

};

class callback :
  public event_queue_entry
{
 protected:
  callback(device_id local) :
    event_queue_entry(local, local)
  {
  }

  virtual ~callback(){}

};


} // end of namespace sstmac

#endif