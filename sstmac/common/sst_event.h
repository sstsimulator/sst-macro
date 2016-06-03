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

class event :
#if SSTMAC_INTEGRATED_SST_CORE
  public SST::Event
#else
  public sprockit::serializable
#endif
{
 public:
  virtual std::string
  to_string() const = 0;

  void
  serialize_order(serializer& ser){}

  /** convenience methods */
  virtual bool
  is_packet() const {
    return false;
  }

  virtual bool
  is_credit() const {
    return false;
  }

  virtual bool
  is_failure() const {
    return false;
  }

  template <class T>
  T*
  interface(){
    T* t = dynamic_cast<T*>(this);
    return t;
  }

};

class event_queue_entry
#if SSTMAC_INTEGRATED_SST_CORE
  : public SST::Event
#endif
{
  NotSerializable(event_queue_entry)
 public:
  virtual ~event_queue_entry() {}

  virtual void
  execute() = 0;

  virtual std::string
  to_string() const = 0;

#if SSTMAC_INTEGRATED_SST_CORE
  event_queue_entry(event_loc_id dst,
    event_loc_id src) 
  {
    //simply ignore parameters - not needed
  }
#else
  event_queue_entry(event_loc_id dst,
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

class handler_event_queue_entry :
  public event_queue_entry
{

 public:
  virtual ~handler_event_queue_entry() {}

  handler_event_queue_entry(event* ev,
    event_handler* hand,
    event_loc_id src_loc);

  virtual std::string
  to_string() const;

  void
  execute();

 protected:
  event* ev_to_deliver_;

  event_handler* handler_;

};

class generic_event :
  public event_queue_entry
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
    event_queue_entry(local, local)
  {
  }


};

} // end of namespace sstmac

#endif

