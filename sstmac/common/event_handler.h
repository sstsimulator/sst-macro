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

#ifndef SSTMAC_COMMON_EVENTHANDLER_H_INCLUDED
#define SSTMAC_COMMON_EVENTHANDLER_H_INCLUDED

#include <sstmac/common/node_address.h>
#include <sstmac/common/event_location.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/event_handler_fwd.h>
#include <sprockit/printable.h>
#include <tuple>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/link.h>
#endif

namespace sstmac {

/**
 * The main interface for something that can respond to an event (sst_message).
 */
class event_handler :
  public sprockit::printable
{
 public:
  static const int null_lpid = -1;

 protected:
  event_handler(uint32_t comp_id) :
    comp_id_(comp_id)
  {
  }

 public:
  uint32_t component_id() const {
    return comp_id_;
  }

  virtual ~event_handler() {}

  virtual void handle(event* ev) = 0;

  virtual void deadlock_check(event* ev) = 0;
  
  virtual void deadlock_check() = 0;

 private:
  uint32_t comp_id_;
};

class member_fxn_handler :
  public event_handler
{
 public:
  virtual ~member_fxn_handler(){}

 protected:
    member_fxn_handler(uint32_t comp_id) :
      event_handler(comp_id)
  {
  }
};

template<int ...>
struct seq { };

template<int N, int ...S>
struct gens : gens<N-1, N-1, S...> { };

template<int ...S>
struct gens<0, S...> {
  typedef seq<S...> type;
};

template<typename C>
struct has_deadlock_check {
private:
    template<typename T>
    static constexpr auto check(T*)
    -> typename
        std::is_same<
            decltype( std::declval<T>().deadlock_check() ),
            void
        >::type;  // attempt to call it and see if the return type is correct

    template<typename>
    static constexpr std::false_type check(...);

    typedef decltype(check<C>(0)) type;

public:
    static constexpr bool value = type::value;
};

template <class Cls, typename Fxn, class ...Args>
class member_fxn_handler_impl :
  public member_fxn_handler
{

 public:
  virtual ~member_fxn_handler_impl(){}

  std::string to_string() const override {
    return sprockit::to_string(obj_);
  }

  void handle(event* ev) override {
    dispatch(ev, typename gens<sizeof...(Args)>::type());
  }

  member_fxn_handler_impl(uint32_t comp_id, Cls* obj, Fxn fxn, const Args&... args) :
    params_(args...),
    obj_(obj),
    fxn_(fxn),
    member_fxn_handler(comp_id)
  {
  }

  void deadlock_check() override {
    local_deadlock_check();
  }

  void deadlock_check(event* ev) override {
    local_deadlock_check(ev);
  }

 private:
  template <int ...S>
  void dispatch(event* ev, seq<S...>){
    (obj_->*fxn_)(ev, std::get<S>(params_)...);
  }

  template <class T = Cls>
  typename std::enable_if<has_deadlock_check<T>::value>::type
  local_deadlock_check() {
    obj_->deadlock_check();
  }

  template <class T = Cls>
  typename std::enable_if<has_deadlock_check<T>::value>::type
  local_deadlock_check(event* ev) {
    obj_->deadlock_check(ev);
  }

  template <class T = Cls>
  typename std::enable_if<!has_deadlock_check<T>::value>::type
  local_deadlock_check() {}

  template <class T = Cls>
  typename std::enable_if<!has_deadlock_check<T>::value>::type
  local_deadlock_check(event* ev) {}

  std::tuple<Args...> params_;
  Fxn fxn_;
  Cls* obj_;

};

template<class Cls, typename Fxn, class ...Args>
event_handler*
new_handler(Cls* cls, Fxn fxn, const Args&... args)
{
  return new member_fxn_handler_impl<Cls, Fxn, Args...>(
        cls->component_id(), cls, fxn, args...);
}


} // end of namespace sstmac
#endif
