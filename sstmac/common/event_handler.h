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

#ifndef SSTMAC_COMMON_EVENTHANDLER_H_INCLUDED
#define SSTMAC_COMMON_EVENTHANDLER_H_INCLUDED

#include <sstmac/common/node_address.h>
#include <sstmac/common/event_location.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/event_handler_fwd.h>
#include <sprockit/printable.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/link.h>
#endif

namespace sstmac {

class locatable
{
 public:
  static const int null_threadid = -1;

  int
  thread_id() const {
    return thread_id_;
  }

  device_id
  event_location() const {
    return loc_id_;
  }

 protected:
#if SSTMAC_INTEGRATED_SST_CORE
  locatable(device_id id) :
    loc_id_(id),
    thread_id_(null_threadid)
  {
  }
#endif

  locatable(device_id id, int thread_id) :
    loc_id_(id),
    thread_id_(thread_id)
  {
  }

 private:
  device_id loc_id_;
  int thread_id_;
};

/**
 * The main interface for something that can respond to an event (sst_message).
 */
class event_handler :
  public locatable,
  public sprockit::printable
{
 public:
  static const int null_lpid = -1;


#if SSTMAC_INTEGRATED_SST_CORE
 public:
  virtual SST::Link*
  link() const {
    return nullptr;
  }

 protected:
  event_handler(device_id id) :
   locatable(id)
  {
  }
#else
 protected:
  event_handler(device_id id, int thread_id) :
    locatable(id, thread_id)
  {
  }
#endif

 public:
  virtual ~event_handler() {}

  virtual void
  handle(event* ev) = 0;


  /**
   * Whether an event handler is a "fake" handler that represents
   * logical process boundary.  Messages scheduled here are sent to another process.
   * @return If this is an IPC stub handler
   */
  virtual bool
  ipc_handler() const {
    return false;
  }

  virtual void
  deadlock_check(event* ev){}
  
  virtual void
  deadlock_check(){}



};


} // end of namespace sstmac
#endif