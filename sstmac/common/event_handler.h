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

namespace sstmac {

/**
 * The main interface for something that can respond to an event (sst_message).
 */
class event_handler
{
 public:
  static const int null_lpid = -1;
  static const int null_threadid = -1;

#if SSTMAC_INTEGRATED_SST_CORE
 public:
  typedef enum {
    self_handler,
    link_handler
  } type_t;

  type_t type() const {
    return type_;
  }

 protected:
  event_handler() : type_(self_handler), thread_id_(null_threadid) {}

  event_handler(type_t ty) : type_(ty), thread_id_(null_threadid) {}

 private:
  type_t type_;
#else
 protected:
  event_handler() : thread_id_(null_threadid) {}
#endif

 public:
  virtual std::string
  to_string() const = 0;

  virtual ~event_handler() {}

  virtual void
  handle(event* ev) = 0;

  event_loc_id
  event_location() const {
    return loc_id_;
  }

  /**
   * Whether an event handler is a "fake" handler that represents
   * logical process boundary.  Messages scheduled here are sent to another process.
   * @return If this is an IPC stub handler
   */
  virtual bool
  ipc_handler() const {
    return false;
  }

  int
  thread_id() const {
    return thread_id_;
  }

  virtual void
  deadlock_check(event* ev){}
  
  virtual void
  deadlock_check(){}

 protected:
  void
  init_loc_id(event_loc_id id){
    loc_id_ = id;
  }

  void
  init_thread_id(int id){
    thread_id_ = id;
  }


 private:
  event_loc_id loc_id_;
  int thread_id_;

};


} // end of namespace sstmac
#endif

