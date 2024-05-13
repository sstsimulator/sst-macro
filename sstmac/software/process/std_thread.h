/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

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
#ifndef _sstmac_software_process_std_thread_h_
#define _sstmac_software_process_std_thread_h_

#include <sstmac/software/process/thread_fwd.h>
#include <tuple>

namespace sstmac {
namespace sw {

namespace threads {

template <std::size_t... Is>
struct indices {};
template <std::size_t N, std::size_t... Is>
struct build_indices
  : build_indices<N-1, N-1, Is...> {};
template <std::size_t... Is>
struct build_indices<0, Is...> : indices<Is...> {};

void yield();

}

template <class T>
struct lock_guard_standin {
};

struct recursive_mutex_standin {
};

class std_thread_base {
 public:
  virtual void run() = 0;

  void setOwner(Thread* t){
    owner_ = t;
  }

  Thread* owner() const {
    return owner_;
  }

 private:
  Thread* owner_;
};

int start_std_thread(std_thread_base* thr);
void join_std_thread(std_thread_base* thr);

template <class Function, class... Args>
class std_thread : public std_thread_base {
  template <class T>
  typename std::decay<T>::type decay_copy(T&& v) { return std::forward<T>(v); }

 public:
  std_thread(Function&& f, Args&&...args) :
  f_(std::forward<Function>(f)), args_(decay_copy(std::forward<Args>(args))...)
  {
  }

  void run() override {
    gen(threads::build_indices<sizeof...(Args)>{});
  }

 private:
  template<std::size_t... Is>
  void gen(threads::indices<Is...>) {
    f_(std::get<Is>(args_)...);
  }

 private:
  Function f_;

  template <typename T>
  struct decay_remove_ref {
    typedef typename std::remove_reference<typename std::decay<T>::type>::type type;
  };

  std::tuple<typename decay_remove_ref<Args>::type...> args_;


};

class std_thread_standin {
 public:
  template <class Function, class... Args>
  std_thread_standin(Function&& f, Args&&...args)
  {
    thr_ = new std_thread<Function,Args...>(
          std::forward<Function>(f), std::forward<Args>(args)...);
    id_.id = start_std_thread(thr_);
  }

  struct id {
    int id;
  };

  id get_id() const noexcept {
    return id_;
  }


  void join(){
    join_std_thread(thr_);
  }

 private:
  id id_;
  std_thread_base* thr_;

};

}
}

#include <functional>

namespace std {

template <> struct hash<sstmac::sw::std_thread_standin::id> {
  std::size_t operator()(sstmac::sw::std_thread_standin::id const& s) const noexcept
  {
      return std::hash<int>()(s.id);
  }
};
}



#endif

