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

