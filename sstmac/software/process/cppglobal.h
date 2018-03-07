#ifndef sstmac_software_process_CPPGLOBAL_H
#define sstmac_software_process_CPPGLOBAL_H

#include <tuple>
#include <sstmac/software/process/global.h>
#include <functional>

namespace sstmac {

class CppGlobal {
 public:
  virtual void allocate(void* globalPtr) = 0;
};

extern void registerCppGlobal(CppGlobal*, bool tls);

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
  CppGlobalImpl(int& offset, bool tls, Args&&... args) :
    offset_(offset),
    args_(std::forward<Args>(args)...)
  {
    registerCppGlobal(this, tls);
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

  std::tuple<Args...> args_;
  int& offset_;
};

template <class T, int N, class Init>
class CppGlobalImpl<T[N], Init> : public CppGlobal {
  typedef T arr[N];
 public:
  CppGlobalImpl(int& offset, bool tls, Init init) :
   offset_(offset)
  {
    registerCppGlobal(this, tls);
    ::memcpy(init_, init, sizeof(init));
  }

  void allocate(void* ptr) override {
    void* offsetPtr = (char*)ptr + offset_;
    T* space = new (offsetPtr) T[N];
    memcpy(space, init_, sizeof(arr));
  }

 private:
  T init_[N];
  int& offset_;
};

template <class T, size_t N, class... Args>
struct array_filler {
  static void fill(T* t, std::tuple<Args...>& args){
    t[N-1] = std::get<N-1>(args);
    array_filler<T,N-1,Args...>::fill(t, args);
  }
};

template <class T, class... Args>
struct array_filler<T,0,Args...> {
  //no-op, we are done
  static void fill(T* t, std::tuple<Args...>& args){}
};

template <class T, int N, class... Args>
class CppGlobalImpl<T[N], Args...> : public CppGlobal {
  typedef T arr[N];
 public:
  CppGlobalImpl(int& offset, bool tls, Args&&... args) :
   offset_(offset),
    args_(std::forward<Args>(args)...)
  {
    registerCppGlobal(this, tls);
  }

  void allocate(void* ptr) override {
    void* offsetPtr = (char*)ptr + offset_;
    T* space = new (offsetPtr) T[N];
    array_filler<T,N,Args...>::fill(space, args_);
  }

 private:
  std::tuple<Args...> args_;
  int& offset_;
};

template <class T>
class CppGlobalImpl<T,std::function<void(void*)>> : public CppGlobal
{
 public:
  CppGlobalImpl(int& offset, bool tls, std::function<void(void*)> fxn) :
   offset_(offset), fxn_(fxn)
  {
    registerCppGlobal(this, tls);
  }

  void allocate(void* ptr) override {
    void* offsetPtr = (char*)ptr + offset_;
    fxn_(offsetPtr);
  }

 private:
  std::function<void(void*)> fxn_;
  int& offset_;
};


template <class T>
struct CppInplaceGlobalInitializer {
  CppInplaceGlobalInitializer(int& offset) :
    gVar(offset, sizeof(T), "static", nullptr)
  {}
  void forceInitialization(){}
  GlobalVariable gVar;
};

template <class Tag, class T, class... Args>
struct CppInplaceGlobal {
  CppInplaceGlobal(bool tls, Args&&... args) :
    alloc(offset, tls, std::forward<Args>(args)...)
  {
    initer.forceInitialization();
  }
  CppGlobalImpl<T,Args...> alloc;
  static int offset;
  static CppInplaceGlobalInitializer<T> initer;
};


template <class Tag, class T, class... Args> int
  CppInplaceGlobal<Tag,T,Args...>::offset;
template <class Tag, class T, class... Args> CppInplaceGlobalInitializer<T>
  CppInplaceGlobal<Tag,T,Args...>::initer(CppInplaceGlobal<Tag,T,Args...>::offset);

template <class T, class... Args>
CppGlobal* new_cpp_global(int& offset, bool tls, Args&&... args){
  return new CppGlobalImpl<T,Args...>(offset, tls, std::forward<Args>(args)...);
}

template <class Tag, class T, class... Args>
int inplace_cpp_global(bool tls, Args&&... args){
  static CppInplaceGlobal<Tag,T,Args...> init(tls, std::forward<Args>(args)...);
  return init.offset;
}

/** a special wrapper for variable template members */
template <class Tag, class T, bool tls>
class CppVarTemplate {
 public:
  template <class... Args>
  CppVarTemplate(Args&&... args){
    offset = inplace_cpp_global<Tag,T,Args...>(tls, std::forward<Args>(args)...);
  }

  T& operator()(){
    return tls ? get_tls_ref_at_offset<T>(offset) : get_global_ref_at_offset<T>(offset);
  }

 private:
  static int offset;
};
template <class Tag, class T, bool tls> int CppVarTemplate<Tag,T,tls>::offset = 0;

}

#endif // CPPGLOBAL_H
