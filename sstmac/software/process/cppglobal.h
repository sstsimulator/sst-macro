#ifndef sstmac_software_process_CPPGLOBAL_H
#define sstmac_software_process_CPPGLOBAL_H

#include <tuple>
#include <iostream>

namespace sstmac {

class CppGlobal {
 public:
  virtual void allocate(void* globalPtr) = 0;
};

extern void registerCppGlobal(CppGlobal*);

namespace globals {

template <std::size_t... Is>
struct indices {};
template <std::size_t N, std::size_t... Is>
struct build_indices
  : build_indices<N-1, N-1, Is...> {};
template <std::size_t... Is>
struct build_indices<0, Is...> : indices<Is...> {};

}

template <class T, class... Args>
class CppGlobalImpl : public CppGlobal {
 public:
  CppGlobalImpl(int& offset, Args&... args) :
    offset_(offset),
    args_(std::forward<Args>(args)...)
  {
    registerCppGlobal(this);
  }

  void allocate(void* ptr) override {
    void* offsetPtr = (char*)ptr + offset_;
    gen(offsetPtr, globals::build_indices<sizeof...(Args)>{});
  }

 private:
  template<std::size_t... Is>
  void gen(void* ptr, globals::indices<Is...>) {
    new (ptr) T(std::get<Is>(args_)...);
  }

  std::tuple<Args&...> args_;
  int& offset_;
};

template <class T, class... Args>
CppGlobal* new_cpp_global(int& offset, Args&... args){
  return new CppGlobalImpl<T,Args...>(offset, args...);
}

}

#endif // CPPGLOBAL_H
