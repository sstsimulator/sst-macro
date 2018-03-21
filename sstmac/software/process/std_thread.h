#ifndef _sstmac_software_process_std_thread_h_
#define _sstmac_software_process_std_thread_h_

#include <sstmac/software/process/thread.h>
#include <tuple>

namespace sstmac {
namespace sw {

class std_thread_base : public thread {
 protected:
  std_thread_base(thread* current_thr);
};

class std_thread_ctor_wrapper : public std_thread_base {
 protected:
  std_thread_ctor_wrapper();
};

void start_std_thread(thread* thr);

namespace threads {

template <std::size_t... Is>
struct indices {};
template <std::size_t N, std::size_t... Is>
struct build_indices
  : build_indices<N-1, N-1, Is...> {};
template <std::size_t... Is>
struct build_indices<0, Is...> : indices<Is...> {};

}

template <class Function, class... Args>
class std_thread : public std_thread_ctor_wrapper {
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
    start_std_thread(thr_);
  }

  void join(){
    thr_->join();
  }

 private:
  thread* thr_;

};

}
}




namespace std {
using thread = sstmac::sw::std_thread_standin;
}

#endif

