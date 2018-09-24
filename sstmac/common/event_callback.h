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

#ifndef SSTMAC_COMMON_EVENTCALLBACK_H_INCLUDED
#define SSTMAC_COMMON_EVENTCALLBACK_H_INCLUDED

#include <sstmac/common/event_handler.h>
#include <sstmac/common/sst_event.h>
#include <sprockit/thread_safe_new.h>

namespace sstmac {

template <class Cls, typename Fxn, class ...Args>
class member_fxn_callback :
  public callback,
  public sprockit::thread_safe_new<member_fxn_callback<Cls,Fxn,Args...>>
{

 public:
  virtual ~member_fxn_callback(){}

  void execute() {
    dispatch(typename gens<sizeof...(Args)>::type());
  }

  member_fxn_callback(uint32_t comp_id, Cls* obj, Fxn fxn, const Args&... args) :
    callback(comp_id),
    params_(args...),
    obj_(obj),
    fxn_(fxn)
  {
  }

 private:
  template <int ...S>
  void dispatch(seq<S...>){
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
        cls->component_id(), cls, fxn, args...);
}

template<class Cls, typename Fxn, class ...Args>
callback*
new_callback(uint32_t comp_id, Cls* cls, Fxn fxn, const Args&... args)
{
  return new member_fxn_callback<Cls, Fxn, Args...>(
        comp_id, cls, fxn, args...);
}

/**
 * This bypasses any custom operators
 */
template<class Cls, typename Fxn, class ...Args>
callback*
placement_new_callback(uint32_t comp_id, Cls* cls, Fxn fxn, const Args&... args)
{
  size_t sz = sizeof(member_fxn_callback<Cls, Fxn, Args...>);
  char* space = new char[sz];
  return new (space) member_fxn_callback<Cls, Fxn, Args...>(
        comp_id, cls, fxn, args...);
}

} // end of namespace sstmac
#endif
