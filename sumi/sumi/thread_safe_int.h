#ifndef sumi_THREAD_SAFE_INT_H
#define sumi_THREAD_SAFE_INT_H

#include <sumi/lockable.h>

namespace sumi {

/** Can be used as a drop-in replacement for integral types where the
 *  provided API is sufficient.  Each member function locks on entry
 *  and unlocks before returning. */
template <class Integer>
class thread_safe_int :
  public lockable
{
 public:
  template <class Y>
  thread_safe_int(const Y& y) : value_(y) {}

  thread_safe_int(const thread_safe_int &y){
    lock();
    y.lock();
    value_ = y.value_;
    y.unlock();
    unlock();
  }

  template <class Y>
  Integer& operator=(const Y& y){
    lock();
    value_ = y;
    unlock();
    return value_;
  }

  thread_safe_int& operator=(const thread_safe_int &rhs){
    if (&rhs != this){  // avoid deadlock
      lock();
      rhs.lock();
      value_ = rhs.value_;
      rhs.unlock();
      unlock();
    }
    return *this;
  }

  Integer
  operator+=(Integer rhs){
    lock();
    value_ += rhs;
    Integer tmp = value_;
    unlock();
    return tmp;
  }

  Integer
  operator-=(Integer rhs){
    lock();
    value_ -= rhs;
    Integer tmp = value_;
    unlock();
    return tmp;
  }

  Integer
  operator|=(Integer rhs){
    lock();
    value_ |= rhs;
    Integer tmp = value_;
    unlock();
    return tmp;
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
    lock();
    Integer tmp(value_);
    value_++;
    unlock();
    return tmp;
  }

  Integer
  operator--(){
    lock();
    --value_;
    Integer tmp = value_;
    unlock();
    return tmp;
  }

  Integer
  operator--(int i){
    Integer tmp(value_);
    lock();
    value_--;
    unlock();
    return tmp;
  }

  bool
  operator==(const thread_safe_int &rhs) const {
    if (&rhs == this) return true;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ == r);
    unlock();
    return ret;
  }

  bool
  operator!=(const thread_safe_int &rhs) const {
    if (&rhs == this) return false;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ != r);
    unlock();
    return ret;
  }

  bool
  operator<(const thread_safe_int &rhs) const {
    if (&rhs == this) return false;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ < r);
    unlock();
    return ret;
  }

  bool
  operator>(const thread_safe_int &rhs) const {
    if (&rhs == this) return false;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ > r);
    unlock();
    return ret;
  }

  bool
  operator<=(const thread_safe_int &rhs) const {
    if (&rhs == this) return true;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ <= r);
    unlock();
    return ret;
  }

  bool
  operator>=(const thread_safe_int &rhs) const {
    if (&rhs == this) return true;  // avoid deadlock
    lock();
    Integer r = rhs;
    bool ret = (value_ >= r);
    unlock();
    return ret;
  }

  bool
  operator==(const Integer &rhs) const {
    lock();
    bool ret = (value_ == rhs);
    unlock();
    return ret;
  }

  bool
  operator!=(const Integer &rhs) const {
    lock();
    bool ret = (value_ != rhs);
    unlock();
    return ret;
  }

  bool
  operator<(const Integer &rhs) const {
    lock();
    bool ret = (value_ < rhs);
    unlock();
    return ret;
  }

  bool
  operator>(const Integer &rhs) const {
    lock();
    bool ret = (value_ > rhs);
    unlock();
    return ret;
  }

  bool
  operator<=(const Integer &rhs) const {
    lock();
    bool ret = (value_ <= rhs);
    unlock();
    return ret;
  }

  bool
  operator>=(const Integer &rhs) const {
    lock();
    bool ret = (value_ >= rhs);
    unlock();
    return ret;
  }

  operator Integer() const {
    lock();
    Integer tmp =  value_;
    unlock();
    return tmp;
  }

 protected:
  Integer value_;
};

}

#endif // THREAD_SAFE_INT_H
