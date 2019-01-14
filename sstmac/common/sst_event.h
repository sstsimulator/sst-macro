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

#ifndef SSTMAC_BACKENDS_NATIVE_SSTEVENT_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_SSTEVENT_H_INCLUDED

#include <sstmac/common/serializable.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_scheduler_fwd.h>
#include <sstmac/common/event_location.h>
#if ACTUAL_INTEGRATED_SST_CORE
#include <sst/core/event.h>
#endif

namespace sstmac {


#if ACTUAL_INTEGRATED_SST_CORE
using event = SST::Event;
#else
class Event : public serializable
{
 public:
  void serialize_order(serializer& ser){}
};
#endif

class ExecutionEvent : public Event
{
  NotSerializable(ExecutionEvent)
 public:
  virtual ~ExecutionEvent() {}

#if ACTUAL_INTEGRATED_SST_CORE
  virtual void execute() override = 0;

  ExecutionEvent(uint32_t dst, uint32_t src)
  {
    //simply ignore parameters - not needed
  }
#else
  ExecutionEvent() :
    linkId_(-1),
    seqnum_(-1)
  {
  }

  virtual void execute() = 0;

  Timestamp time() const {
    return time_;
  }

  void setTime(const Timestamp& t) {
    time_ = t;
  }

  void setSeqnum(uint32_t seqnum) {
    seqnum_ = seqnum;
  }

  void setLink(uint32_t id){
    linkId_ = id;
  }

  uint32_t seqnum() const {
    return seqnum_;
  }

  uint32_t linkId() const {
    return linkId_;
  }

 protected:
  Timestamp time_;
  uint32_t linkId_;
  /** A unique sequence number from the source */
  uint32_t seqnum_;
#endif

};

using Callback = ExecutionEvent;



} // end of namespace sstmac

#endif
