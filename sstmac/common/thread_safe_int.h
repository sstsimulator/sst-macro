#ifndef THREAD_SAFE_INT_H
#define THREAD_SAFE_INT_H

#include <sstmac/common/thread_lock.h>
#include <cstring>

namespace sstmac {

class thread_safe_int_base :
  public lockable
{
};

template <class Integer>
class thread_safe_int_t :
  public thread_safe_int_base
{

 public:
  template <class Y>
  thread_safe_int_t(const Y& y) :
   value_(y)
  {
  }

  template <class Y>
  Integer& operator=(const Y& y){
    value_ = y;
    return value_;
  }

  Integer
  operator++(){
    lock();
    ++value_;
    Integer tmp = value_;
    unlock();
    return tmp;
  }

  Integer
  operator++(int i){
    Integer tmp(value_);
    lock();
    value_++;
    unlock();
    return tmp;
  }

  operator Integer(){
    return value_;
  }

 protected:
  Integer value_;

};

typedef thread_safe_int_t<int> thread_safe_int;
typedef thread_safe_int_t<long> thread_safe_long;
typedef thread_safe_int_t<long long> thread_safe_long_long;
typedef thread_safe_int_t<size_t> thread_safe_size_t;

}

#endif // THREAD_SAFE_INT_H
