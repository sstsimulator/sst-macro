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

#ifndef SSTMAC_COMMON_EVENTCALLBACK_H_INCLUDED
#define SSTMAC_COMMON_EVENTCALLBACK_H_INCLUDED

#include <sstmac/common/event_handler.h>
#include <sstmac/common/sst_event.h>

namespace sstmac {

/**
 * The main interface for something that can respond to an event (sst_message).
 */
class event_callback : public event_handler
{
 public:

 public:
  virtual ~event_callback();

  virtual void
  handle(event* ev);

  virtual void
  callback(event* ev) = 0;

  virtual std::string
  to_string() const;

 protected:
  event_callback(event_loc_id id)
  {
    init_loc_id(id);
  }

};

template<class Cls, typename Fxn>
class event_callback_instance : public event_callback
{
 private:
  Cls* obj_;

  Fxn fxn_;

 public:
  event_callback_instance(event_loc_id loc, Cls* obj, Fxn fxn) :
    obj_(obj), fxn_(fxn),
    event_callback(loc)
  {
  }

  void
  callback(event* ev) {
    (obj_->*fxn_)(ev);
  }

};

template<class Cls, typename Fxn, typename Arg1>
class event_callback_1_args : public event_callback
{
 private:
  Cls* obj_;

  Fxn fxn_;

  Arg1 arg1_;


 public:
  event_callback_1_args(event_loc_id loc, Cls* obj, Fxn fxn,
                           const Arg1& arg1) :
    obj_(obj), fxn_(fxn), arg1_(arg1),
    event_callback(loc)
  {
  }

  void
  callback(event* ev) {
    Cls& obj = *obj_;
    (obj_->*fxn_)(ev, arg1_);
  }

};

template<class Cls, typename Fxn, typename Arg1, typename Arg2>
class event_callback_2_args : public event_callback
{
 private:
  Cls* obj_;

  Fxn fxn_;

  Arg1 arg1_;

  Arg2 arg2_;

 public:
  event_callback_2_args(event_loc_id loc, Cls* obj, Fxn fxn,
                            const Arg1& arg1, const Arg2& arg2) :
    obj_(obj), fxn_(fxn), arg1_(arg1), arg2_(arg2),
    event_callback(loc)
  {
  }

  void
  callback(event* ev) {
    Cls& obj = *obj_;
    (obj_->*fxn_)(ev, arg1_, arg2_);
  }

};

template<class Cls, typename Fxn>
event_callback*
ev_callback(event_loc_id loc, Cls* cls, Fxn fxn)
{
  event_callback* callback = new event_callback_instance<Cls, Fxn> (loc, cls, fxn);
  return callback;
}

template<class Cls, typename Fxn, typename Arg1>
event_callback*
ev_callback(event_loc_id loc, Cls* cls, Fxn fxn,
             const Arg1& arg1)
{
  event_callback* callback = new event_callback_1_args<Cls, Fxn,
  Arg1> (loc, cls, fxn, arg1);
  return callback;
}

template<class Cls, typename Fxn, typename Arg1, typename Arg2>
event_callback*
ev_callback(event_loc_id loc, Cls* cls, Fxn fxn,
             const Arg1& arg1, const Arg2& arg2)
{
  event_callback* callback = new event_callback_2_args<Cls, Fxn,
  Arg1, Arg2> (loc, cls, fxn, arg1, arg2);
  return callback;
}


template <class Cls, class Fxn>
class generic_event_0_args :
  public generic_event
{
 private:
  Cls* obj_;

  Fxn fxn_;

 public:
  generic_event_0_args(event_loc_id local, Cls* obj, Fxn fxn) :
    obj_(obj), fxn_(fxn), generic_event(local)
  {
  }

  void
  execute() {
    if (obj_) {
      (obj_->*fxn_)();
    }
  }

  std::string
  to_string() const {
    return "generic event with 0 args";
  }

};

template <class Cls, class Fxn, class Arg1>
class generic_event_1_args :
  public generic_event
{
 private:
  Cls* obj_;

  Fxn fxn_;

  Arg1 arg1_;

 public:
  generic_event_1_args(event_loc_id local, Cls* obj, Fxn fxn,
                       const Arg1& arg1) :
    obj_(obj), fxn_(fxn), arg1_(arg1), generic_event(local)
  {
  }

  void
  execute() {
    if (obj_) {
      (obj_->*fxn_)(arg1_);
    }
  }

  std::string
  to_string() const {
    return "generic event with 1 arg";
  }

};

template <class Cls, class Fxn, class Arg1, class Arg2>
class generic_event_2_args :
  public generic_event
{
 private:
  Cls* obj_;

  Fxn fxn_;

  Arg1 arg1_;

  Arg2 arg2_;

 public:
  generic_event_2_args(event_loc_id local, Cls* obj, Fxn fxn,
                       const Arg1& arg1, const Arg2& arg2) :
    obj_(obj), fxn_(fxn), arg1_(arg1), arg2_(arg2), generic_event(local)
  {
  }

  std::string
  to_string() const {
    return "generic event with 2 args";
  }

  void
  execute() {
    if (obj_) {
      (obj_->*fxn_)(arg1_, arg2_);
    }
  }

};

template<class Cls, typename Fxn>
event_queue_entry*
new_event(event_loc_id local, Cls* cls, Fxn fxn)
{
  event_queue_entry* callback = new generic_event_0_args<Cls, Fxn> (local, cls, fxn);
  return callback;
}

template<class Cls, typename Fxn, class Arg1>
event_queue_entry*
new_event(event_loc_id local, Cls* cls, Fxn fxn, const Arg1& arg1)
{
  event_queue_entry* callback = new generic_event_1_args<Cls, Fxn, Arg1> (local, cls, fxn, arg1);
  return callback;
}

template<class Cls, typename Fxn, class Arg1, class Arg2>
event_queue_entry*
new_event(event_loc_id local, Cls* cls, Fxn fxn, const Arg1& arg1,
          const Arg2& arg2)
{
  event_queue_entry* callback = new generic_event_2_args<Cls, Fxn, Arg1, Arg2> (local, cls, fxn,
      arg1, arg2);
  return callback;
}

} // end of namespace sstmac
#endif

