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

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/node_address.h>
#include <sstmac/common/event_location.h>

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
  handle(sst_message* msg) = 0;

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
  deadlock_check(sst_message* msg){}
  
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

class multi_event_handler :
  public event_handler
{
 public:
  typedef void (multi_event_handler::*handle_fxn)(sst_message* msg);

  class handle_functor {
   public:
    virtual void
    handle(multi_event_handler* handler, sst_message* msg) = 0;
  };

  template <class T, class Fxn>
  class handle_functor_impl :
    public handle_functor
  {
   public:
    handle_functor_impl(Fxn fxn) : fxn_(fxn) {}

    void
    handle(multi_event_handler* handler, sst_message* msg){
      T* me = static_cast<T*>(handler);
      (me->*fxn_)(msg);
    }

   protected:
    Fxn fxn_;
  };

  template <class T, class Fxn>
  void
  new_handler(sst_message::message_type_t ty, T* t, Fxn fxn){
    handle_functor* handler = new handle_functor_impl<T,Fxn>(fxn);
    handle_type(ty, handler);
  }

 public:
  virtual ~multi_event_handler(){}

  virtual void
  handle(sst_message* msg);

 protected:
  void
  handle_type(sst_message::message_type_t ty, handle_functor* handler);

 protected:
  std::map<sst_message::message_type_t, handle_functor*> fxns_;
};

class do_nothing_handler :
  public event_handler
{
 public:
  do_nothing_handler(event_loc_id id)
  {
    init_loc_id(id);
  }

  std::string
  to_string() const {
    return "do nothing handler";
  }

  void
  handle(sst_message* msg) {}

};

} // end of namespace sstmac
#endif

