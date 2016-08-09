
#ifndef fake_variable_h_
#define fake_variable_h_

#include <type_traits>
#include <cstddef>
#include <sstmac/software/process/thread.h>

#define DECLARE_OPERATOR(op,CONST,REF) \
  template <class T, class U> \
  Variable<T>REF \
  operator op(CONST Variable<T>& t, const U& u); \
  template <class T, class U> \
  U REF \
  operator op(CONST U& u, const Variable<T>& t); \
  template <class T, class U> \
  Variable<T> REF \
  operator op(CONST Variable<T>& t, const Variable<U>& u);

#define FRIEND_OPERATOR(op,CONST,REF) \
  template <class T1, class U> \
  friend Variable<T1> REF \
  operator op(CONST Variable<T1>& t, const U& u); \
  template <class T1, class U> \
  friend U REF \
  operator op(CONST U& u, const Variable<T1>& t); \
  template <class T1, class U> \
  friend Variable<T1> REF \
  operator op(CONST Variable<T1>& t, const Variable<U>& u);

template <class T> class VariablePtr;
template <class T> class Variable;

DECLARE_OPERATOR(+,const,)
DECLARE_OPERATOR(-,const,)
DECLARE_OPERATOR(*,const,)
DECLARE_OPERATOR(/,const,)
DECLARE_OPERATOR(&,const,)
DECLARE_OPERATOR(|,const,)
DECLARE_OPERATOR(+=,,&)
DECLARE_OPERATOR(*=,,&)
DECLARE_OPERATOR(-=,,&)
DECLARE_OPERATOR(/=,,&)
DECLARE_OPERATOR(&=,,&)
DECLARE_OPERATOR(|=,,&)

template <class T>
Variable<T>
sqrt(const Variable<T> &t);

template <class T>
Variable<T>
cbrt(const Variable<T> &t);

template <class T>
Variable<T>
fabs(const Variable<T> &t);

template <class T>
void*
memset(const VariablePtr<T>& t, int value, size_t size);

template <class T>
void*
memcpy(const VariablePtr<T>& dst, const VariablePtr<T>& src, size_t size);

template <class T>
class Variable 
{
  FRIEND_OPERATOR(+,const,)
  FRIEND_OPERATOR(-,const,)
  FRIEND_OPERATOR(*,const,)
  FRIEND_OPERATOR(/,const,)
  FRIEND_OPERATOR(&,const,)
  FRIEND_OPERATOR(|,const,)
  FRIEND_OPERATOR(+=,,&)
  FRIEND_OPERATOR(*=,,&)
  FRIEND_OPERATOR(-=,,&)
  FRIEND_OPERATOR(/=,,&)
  FRIEND_OPERATOR(&=,,&)
  FRIEND_OPERATOR(|=,,&)
  friend Variable<T> sqrt<>(const Variable<T> &t);
  friend Variable<T> cbrt<>(const Variable<T> &t);
  friend Variable<T> fabs<>(const Variable<T> &t);

 public:
  template <typename = std::enable_if<std::is_default_constructible<T>::value>>
  Variable()
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      owns_nops(true)
  {
  }

  template <typename U,
    typename = std::enable_if<std::is_convertible<T,U>::value>>
  Variable(const U& u)
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      owns_nops(true)
  {
  }

  Variable(uint64_t &nops_array)
    : nops(nops_array),
      owns_nops(false)
  {
  }

  Variable(const Variable& v)
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      owns_nops(true)
  {
  }

  ~Variable(){
    if (owns_nops)
      sstmac::sw::operating_system::current_thread()
          ->remove_perf_ctr_variable(this);
  }

  VariablePtr<T> operator&() {
    return VariablePtr<T>(nops);
  }

  void* operator new[](std::size_t count) throw() {
    return 0;
  }

  Variable& operator=(const Variable& v) {
    return *this;
  }

  template <class U>
  Variable& operator=(const U& u){
    return *this;
  }

  constexpr operator T() const {
    return 0;
  }

  Variable operator-() const {
    nops++;
    return *this;
  }

  Variable& operator++(){
    nops++;
    return *this;
  }

  Variable operator++(int u){
    nops++;
    return *this;
  }

  Variable& operator--(){
    nops++;
    return *this;
  }

  Variable operator--(int u){
    nops++;
    return *this;
  }

 private:
  uint64_t &nops;
  bool owns_nops;
};

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
    t.nops++; \
    return t; \
  } \
  template <class T, class U> \
  U REF \
  operator op(CONST U& u, const Variable<T>& t){ \
    t.nops++; \
    return u;\
  } \
  template <class T, class U> \
  Variable<T> REF \
  operator op(CONST Variable<T>& t, const Variable<U>& u){ \
    t.nops++; \
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
OPERATOR(&,const,)
OPERATOR(|,const,)
OPERATOR(+=,,&)
OPERATOR(*=,,&)
OPERATOR(-=,,&)
OPERATOR(/=,,&)
OPERATOR(&=,,&)
OPERATOR(|=,,&)

template <class T>
Variable<T>
sqrt(const Variable<T> &t){
  t.nops++;
  return t;
}

template <class T>
Variable<T>
cbrt(const Variable<T> &t){
  t.nops++;
  return t;
}

template <class T>
Variable<T>
fabs(const Variable<T> &t){
  t.nops++;
  return t;
}


template <class T>
class VariablePtr
{
  friend void* memset<>(const VariablePtr<T>& t, int value, size_t size);
  friend void* memcpy<>(const VariablePtr<T>& dst, const VariablePtr<T>& src, size_t size);

 public:
  VariablePtr()
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      elem(nops)
  {
  }

  template <typename U,
    typename = std::enable_if<std::is_convertible<T*,U>::value>>
  VariablePtr(const U& u)
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      elem(nops)
  {
  }

  VariablePtr(uint64_t &nops_elem)
    : nops(nops_elem)
  {
  }

  VariablePtr(const VariablePtr &vp)
    : nops(sstmac::sw::operating_system::current_thread()
            ->register_perf_ctr_variable<uint64_t>(this)),
      elem(nops)
  {
  }

  ~VariablePtr(){
    sstmac::sw::operating_system::current_thread()
        ->remove_perf_ctr_variable(this);
  }

  void resize(size_t n){}

  void clear(){}

  Variable<T>& operator[](int idx){
    return elem;
  }

  const Variable<T>& operator[](int idx) const {
    return elem;
  }

  Variable<T>& operator[](Variable<int> idx){
    return elem;
  }

  const Variable<T>& operator[](Variable<int> idx) const {
    return elem;
  }

  Variable<T>& operator*() {
    return elem;
  }

  const Variable<T>& operator*() const {
    return elem;
  }

  VariablePtr<T>&
  operator=(const VariablePtr& ptr){
    return *this;
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

  constexpr operator void*() const {
    return 0;
  }

  VariablePtr<T>&
  operator+=(const ptrdiff_t &offset){
    return *this;
  }

  VariablePtr<T>&
  operator-=(const ptrdiff_t &offset){
    return *this;
  }

 private:
  uint64_t &nops;
  Variable<T> elem;
};

template <class T>
void*
memset(const VariablePtr<T>& t, int value, size_t size){
  t.nops += size / sizeof(T);
  return 0;
}

template <class T>
void*
memcpy(const VariablePtr<T>& dst, const VariablePtr<T>& src, size_t size){
  dst.nops += size / sizeof(T);
  return 0;
}

typedef Variable<double> Double;
typedef VariablePtr<double> DoublePtr;
typedef Variable<int> Int;
typedef VariablePtr<int> IntPtr;


#endif

