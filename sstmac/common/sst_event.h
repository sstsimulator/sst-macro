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

#ifndef SSTMAC_BACKENDS_NATIVE_SSTEVENT_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_SSTEVENT_H_INCLUDED

#include <sprockit/serializable.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_scheduler_fwd.h>

namespace sstmac {

class event  {

 public:
  virtual ~event() {}

  virtual void
  execute() = 0;

  virtual std::string
  to_string() const = 0;

#if SSTMAC_INTEGRATED_SST_CORE
  event(event_loc_id dst,
    event_loc_id src) 
  {
    //simply ignore parameters - not needed
  }
#else
  event(event_loc_id dst,
    event_loc_id src) :
    dst_loc_(dst),
    src_loc_(src),
    seqnum_(0)
  {
  }

  timestamp
  time() const {
    return time_;
  }

  void
  set_time(const timestamp& t) {
    time_ = t;
  }

  void
  set_seqnum(uint32_t seqnum) {
    seqnum_ = seqnum;
  }

  uint32_t
  seqnum() const {
    return seqnum_;
  }

  event_loc_id
  event_location() const {
    return dst_loc_;
  }

  event_loc_id
  src_location() const {
    return src_loc_;
  }

#if SSTMAC_HAVE_EVENT_CALENDAR
  event* next;
#endif

 protected:
  timestamp time_;
  event_loc_id dst_loc_;
  event_loc_id src_loc_;
  /** A unique sequence number from the source */
  uint32_t seqnum_;
#endif

};

class handler_event :
  public event
{

 public:
  virtual ~handler_event() {}

  handler_event(sst_message* msg,
    event_handler* hand,
    event_loc_id src_loc);

  virtual std::string
  to_string() const;

  sst_message*
  get_message() const {
    return msg_to_deliver_;
  }

  event_handler*
  get_handler() const {
    return handler_;
  }

  void
  execute();

 protected:
  sst_message* msg_to_deliver_;

  event_handler* handler_;

};

class null_msg_event :
  public event
{

 public:
  null_msg_event(event_handler* handler,
    event_loc_id src_loc)
    : handler_(handler),
      event(handler->event_location(), src_loc)
  {
  }

  virtual ~null_msg_event() {}

  virtual std::string
  to_string() const {
    return "null msg event";
  }

  event_handler*
  get_handler() const {
    return handler_;
  }

  event_loc_id
  event_location() const {
    return handler_->event_location();
  }

  void
  execute();

 protected:
  event_handler* handler_;

};

class generic_event :
  public event
{

 public:
  virtual std::string
  to_string() const {
    return "generic event";
  }

  virtual ~generic_event() { }

  virtual void
  execute() = 0;

 protected:
  generic_event(event_loc_id local) :
    event(local, local)
  {
  }


};

} // end of namespace sstmac

#endif

