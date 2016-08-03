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

template<int ...>
struct seq { };

template<int N, int ...S>
struct gens : gens<N-1, N-1, S...> { };

template<int ...S>
struct gens<0, S...> {
  typedef seq<S...> type;
};

class member_fxn_handler :
  public event_handler
{
 public:
  virtual ~member_fxn_handler(){}

  virtual std::string
  to_string() const {
    return "member fxn handler";
  }
 protected:
  member_fxn_handler(event_loc_id id)
  {
    init_loc_id(id);
  }
};

template <class Cls, typename Fxn, class ...Args>
class member_fxn_handler_impl :
  public member_fxn_handler
{

 public:
  virtual ~member_fxn_handler_impl(){}

  void
  handle(event* ev) {
    dispatch(ev, typename gens<sizeof...(Args)>::type());
  }

  member_fxn_handler_impl(event_loc_id id, Cls* obj, Fxn fxn, const Args&... args) :
    params_(args...),
    obj_(obj),
    fxn_(fxn),
    member_fxn_handler(id)
  {
  }

 private:
  template <int ...S>
  void
  dispatch(event* ev, seq<S...>){
    (obj_->*fxn_)(ev, std::get<S>(params_)...);
  }

  std::tuple<Args...> params_;
  Fxn fxn_;
  Cls* obj_;

};

#if 0
template <class Cls, typename Fxn>
class member_fxn_handler_impl<Cls,Fxn> :
  public member_fxn_handler
{
 public:
  virtual ~member_fxn_handler_impl(){}

  void
  handle(event* ev) {
    (obj_->*fxn_)(ev);
  }

  member_fxn_handler_impl(event_loc_id id, Cls* obj, Fxn fxn) :
    obj_(obj),
    fxn_(fxn),
    member_fxn_handler(id)
  {
  }

 private:
  Fxn fxn_;
  Cls* obj_;
};
#endif

template<class Cls, typename Fxn, class ...Args>
event_handler*
new_handler(Cls* cls, Fxn fxn, const Args&... args)
{
  return new member_fxn_handler_impl<Cls, Fxn, Args...>(
        cls->event_location(), cls, fxn, args...);
}


template <class Cls, typename Fxn, class ...Args>
class member_fxn_callback :
  public callback
{

 public:
  virtual ~member_fxn_callback(){}

  void
  execute() {
    dispatch(typename gens<sizeof...(Args)>::type());
  }

  virtual std::string
  to_string() const {
    return "member fxn callback";
  }

  member_fxn_callback(event_loc_id id, Cls* obj, Fxn fxn, const Args&... args) :
    callback(id),
    params_(args...),
    obj_(obj),
    fxn_(fxn)
  {
  }

 private:
  template <int ...S>
  void
  dispatch(seq<S...>){
    (obj_->*fxn_)(std::get<S>(params_)...);
  }

  std::tuple<Args...> params_;
  Fxn fxn_;
  Cls* obj_;

};

template<class Cls, typename Fxn, class ...Args>
callback*
new_callback(Cls* cls, Fxn fxn, const Args&... args)
{
  return new member_fxn_callback<Cls, Fxn, Args...>(
        cls->event_location(), cls, fxn, args...);
}

template<class Cls, typename Fxn, class ...Args>
callback*
new_callback(event_loc_id id, Cls* cls, Fxn fxn, const Args&... args)
{
  return new member_fxn_callback<Cls, Fxn, Args...>(
        id, cls, fxn, args...);
}

} // end of namespace sstmac
#endif

