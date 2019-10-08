#ifndef sstmac_software_process_CPPGLOBAL_H
#define sstmac_software_process_CPPGLOBAL_H


#include <sstmac/software/process/global.h>
#include <new>

#if __cplusplus >= 201103L
#include <tuple>
#include <functional>

namespace sstmac {

/**
 * @brief The CppGlobalRegisterGuard struct
 * Constructor causes the global variable to be registered and requires init for each new thread
 * Destructor causes the global variable to be unregistestered
 */
struct CppGlobalRegisterGuard {
  CppGlobalRegisterGuard(int& offset, int size,
                         bool tls, const char* name, std::function<void(void*)>&& fxn);

  ~CppGlobalRegisterGuard();

 private:
  bool tls_;
  int offset_;
};

template <class T, bool tls>
struct CppInplaceGlobalInitializer {
  CppInplaceGlobalInitializer(const char* name)
  {
    offset = GlobalVariable::init(sizeof(T), name, tls);
  }
  static int offset;
};
template <class T, bool tls> int CppInplaceGlobalInitializer<T,tls>::offset;

template <class Tag, class T, bool tls>
int inplaceCppGlobal(const char* name, std::function<void(void*)>&& fxn){
  static CppInplaceGlobalInitializer<T,tls> init{name};
  static CppGlobalRegisterGuard holder(init.offset, sizeof(T), tls, name, std::move(fxn));
  return init.offset;
}

/** a special wrapper for variable template members */
template <class Tag, class T, bool tls>
class CppVarTemplate {
 public:
  template <class... CtorArgs>
  static void invoke(void* ptr, CtorArgs&&... args){
    new (ptr) T(std::forward<CtorArgs>(args)...);
  }

  template <class... Args>
  CppVarTemplate(Args&&... args){
    std::function<void(void*)> f = std::bind(&invoke<Args&...>, std::placeholders::_1, std::forward<Args>(args)...);
    offset_ = inplaceCppGlobal<Tag,T,tls>("no name", std::move(f));
  }

  int getOffset() const {
    return offset_;
  }

  T& operator()(){
    return tls ? get_tls_ref_at_offset<T>(offset_) : get_global_ref_at_offset<T>(offset_);
  }

  int offset_;
};


}

#endif //C++11

#endif // CPPGLOBAL_H
