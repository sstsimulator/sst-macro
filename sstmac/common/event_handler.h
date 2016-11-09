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
  locatable(device_id id) :
    loc_id_(id),
    thread_id_(null_threadid)
  {
  }

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

protected:
 event_handler(device_id id) :
   locatable(id)
 {
 }

#if SSTMAC_INTEGRATED_SST_CORE
 public:
  virtual SST::Link*
  link() const {
    return nullptr;
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

