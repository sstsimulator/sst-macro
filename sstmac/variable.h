
#ifndef fake_variable_h_
#define fake_variable_h_

#include <type_traits>

template <class T>
void
del(const T* t){
  if (t) delete[] t;
}

//inline void release(void* ptr){
//  if (ptr) free(ptr);
//}

template <class T> class VariablePtr;

template <class T>
class Variable 
{
 public:
  template <typename U,
    typename = std::enable_if<std::is_convertible<T,U>::value>>
  Variable(const U& u){
   //do nothing
  }

  template <typename = std::enable_if<std::is_default_constructible<T>::value>>
  Variable(){
   //do nothing
  }

  VariablePtr<T> operator&() {
    return 0;
  }

  void* operator new[](std::size_t count) throw() {
    return 0;
  }

  template <class U>
  Variable& operator=(const U& u){
    return *this;
  }

  template <class U>
  constexpr operator U() const {
    return 0;
  }

  static size_t nops;

};


template <class T> size_t Variable<T>::nops = 0;

#define COMPARE(op) \
  template <class T, class U> \
  bool \
  operator op(const Variable<T>& t, const U& u){ \
    return true; \
  } \
  template <class T, class U> \
  bool \
  operator op(const U& u, const Variable<T>& t){ \
    return true;\
  } \
  template <class T, class U> \
  bool \
  operator op(const Variable<T>& t, const Variable<U>& u){ \
    return true; \
  }

#define OPERATOR(op,CONST,REF) \
  template <class T, class U> \
  Variable<T>REF \
  operator op(CONST Variable<T>& t, const U& u){ \
    Variable<T>::nops++; \
    return t; \
  } \
  template <class T, class U> \
  U REF \
  operator op(CONST U& u, const Variable<T>& t){ \
    Variable<T>::nops++; \
    return u;\
  } \
  template <class T, class U> \
  Variable<T> REF \
  operator op(CONST Variable<T>& t, const Variable<U>& u){ \
    Variable<T>::nops++; \
    return t; \
  }

COMPARE(!=)
COMPARE(<)
COMPARE(>)
COMPARE(<=)
COMPARE(>=)
COMPARE(==)
OPERATOR(+,const,)
OPERATOR(-,const,)
OPERATOR(*,const,)
OPERATOR(/,const,)
OPERATOR(+=,,&)
OPERATOR(*=,,&)
OPERATOR(-=,,&)
OPERATOR(/=,,&)


template <class T>
class VariablePtr
{
 public:
  template <typename U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  VariablePtr(const U& u){}

  VariablePtr(){}

  Variable<T>& operator[](int idx){
    return nothing_;
  }

  const Variable<T>& operator[](int idx) const {
    return nothing_;
  }

  template <class U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  operator U() const {
    return 0;
  }

  template <class U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  VariablePtr<T>&
  operator=(const U& ptr){
    return *this;
  }

  template <class U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  const VariablePtr<T>&
  operator=(const U& ptr) const {
    return *this;
  }

  template <class U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  bool
  operator==(const U& ptr) const {
    return false;
  }

  template <class U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  bool
  operator!=(const U& ptr) const {
    return true;
  }

 private:
  Variable<T> nothing_;
};

template <class T>
void*
memset(const VariablePtr<T>& t, int value, size_t size){
  Variable<T>::nops += size / sizeof(T);
  return 0;
}

template <class T>
void*
memcpy(const VariablePtr<T>& dst, const VariablePtr<T>& src, size_t size){
  Variable<T>::nops += size / sizeof(T);
  return 0;
}

typedef Variable<double> Double;
typedef VariablePtr<double> DoublePtr;
typedef Variable<int> Int;
typedef VariablePtr<int> IntPtr;


#endif

