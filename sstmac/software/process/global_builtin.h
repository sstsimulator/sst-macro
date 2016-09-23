/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_SOFTWARE_PROCESS_GLOBAL_BUILTIN_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_GLOBAL_BUILTIN_H_INCLUDED

#include <sstmac/software/process/global_base.h>
#include <sprockit/errors.h>
#include <iostream>

namespace sstmac {
namespace sw {

template<typename T>
class sstmac_global_builtin : public sstmac_global
{

 private:
  typedef spkt_unordered_map<long, T> val_map;
  val_map vals_;
  mutable T init_;
  mutable bool isinit_;

  bool
  check_init(process_context ptxt) const {

    val_map& vals = const_cast<val_map&> (vals_);
    if (ptxt != process_context::none) {
      typename val_map::iterator it = vals.find(ptxt);
      if (it == vals.end()) {
        vals[ptxt] = init_;
      }
      return true;
    }
    else {
      return false;
    }
  }

 public:
  explicit
  sstmac_global_builtin() {
    isinit_ = false;
  }

  explicit
  sstmac_global_builtin(T init) :
    init_(init), isinit_(true) {
  }

  explicit
  sstmac_global_builtin(const sstmac_global_builtin<T>& other) {
    spkt_throw_printf(sprockit::illformed_error,
                     "copy constructor should never be called for primitive global");
  }

  ~sstmac_global_builtin() {
  }

  void
  print_all() const {
    for (auto& pair : vals_) {
      std::cout << pair.second << std::endl;
    }
  }

  T&
  get_val() const {
    process_context ptxt = current_context();
    val_map& vals = const_cast<val_map&> (vals_);
    if (ptxt != process_context::none) {
      typename val_map::iterator it = vals.find(ptxt);
      if (it == vals.end()) {
        if(!isinit_) {
          init_ = 0;
          isinit_ = true;
        }
        T& ret = vals[ptxt];
        ret = init_;
        return ret;
      }
      else {
        return it->second;
      }
    }
    return const_cast<T&> (init_);
  }

  std::string
  to_string() const {
    std::stringstream ss;
    ss << get_val();
    return ss.str();
  }

  template<typename U>
  T&
  operator =(const U& b) {
    T& myval = get_val();
    myval = (T) b;
    return myval;
  }

  template<typename U>
  T&
  operator +=(const U& b) {
    T& myval = get_val();
    myval += (T) b;
    return myval;
  }

  template<typename U>
  T&
  operator -=(const U& b) {
    T& myval = get_val();
    myval -= (T) b;
    return myval;
  }

  template<typename U>
  T&
  operator *=(const U& b) {
    T& myval = get_val();
    myval *= (T) b;
    return myval;
  }

  template<typename U>
  T&
  operator /=(const U& b) {
    T& myval = get_val();
    myval /= (T) b;
    return myval;
  }

  template<typename U>
  bool
  operator ==(const U& b) const {
    T myval = get_val();
    T otherval = (T) b;
    return myval == otherval;
  }

  template<typename U>
  bool
  operator !=(const U& b) const {
    T myval = get_val();
    //     T otherval = (T) b;
    return myval != b;
  }

  //operator T() {
  //  T& myval = get_val();
  //  return myval;
  //}

  operator T&() {
    T& myval = get_val();
    return myval;
  }

  T
  operator ++(int dummy) {
    T& myval = get_val();
    return myval++;
  }

  T
  operator --(int dummy) {
    T& myval = get_val();
    return myval--;
  }

  T
  operator ++() {
    T& myval = get_val();
    return ++myval;
  }

  T
  operator --() {
    T& myval = get_val();
    return --myval;
  }

  operator double() const {
    return (double) get_val();
  }

  operator int() const {
    return (int) get_val();
  }

  operator bool() const {
    return (bool) get_val();
  }

  operator size_t() const {
    return (size_t) get_val();
  }

  operator long() const {
    return (long) get_val();
  }

  operator long long() const {
    return get_val();
  }

  template<typename U>
  T
  operator +(const U& b) const {
    return (get_val() + (T) b);
  }

  template<typename U>
  T
  operator -(const U& b) const {
    return (get_val() - (T) b);
  }

  template<typename U>
  T
  operator /(const U& b) const {
    return (get_val() / ((T) b));
  }

  template<typename U>
  T
  operator <<(const U& b) const {
    return (get_val() << ((T) b));
  }

  template<typename U>
  T
  operator >>(const U& b) const {
    return (get_val() >> ((T) b));
  }

  template<typename U>
  T
  operator %(const U& b) const {
    return get_val() % ((T) b);
  }

  template<typename U>
  T
  operator *(const U& b) const {
    return (get_val() * ((T) b));
  }

  T*
  operator&() const {
    return &get_val();
  }

  template<typename U>
  bool
  operator >(const U& b) {
    T myval = get_val();
    T otherval = (T) b;
    return myval > otherval;
  }

  template<typename U>
  bool
  operator <(const U& b) {
    T myval = get_val();
    T otherval = (T) b;
    return myval < otherval;
  }

  void
  delete_vals() {
    if (init_) {
      delete init_;
      init_ = 0;
    }
    typedef typename spkt_unordered_map<long, T>::iterator myiter;
    myiter it;
    for (it = vals_.begin(); it != vals_.end(); ++it)
      if (it->second) {
        delete it->second;
      }
    vals_.clear();
  }

  typename val_map::const_iterator
  begin() const {
    return vals_.begin();
  }

  typename val_map::const_iterator
  end() const {
    return vals_.end();
  }

};

template<typename T, typename U>
U
operator/(const sstmac_global_builtin<U>& b,
          const sstmac_global_builtin<T>& t)
{
  return ((U) b) / ((T) t);
}

template<typename T, typename U>
U
operator+(const sstmac_global_builtin<U>& b,
          const sstmac_global_builtin<T>& t)
{
  return (U) b + (T) t;
}

template<typename T, typename U>
U
operator-(const sstmac_global_builtin<U>& b,
          const sstmac_global_builtin<T>& t)
{
  return (U) b - (T) t;
}

#if 0
template<typename T, typename U>
U
operator*(const sstmac_global_builtin<U>& b,
          const sstmac_global_builtin<T>& t)
{
  return ((U) b) * ((T) t);
}

template<typename T, typename U>
U
operator*(const sstmac_global_builtin<U>& b,
          T& t)
{
  return ((U) b) * ((T) t);
}

template<typename T, typename U>
T
operator*(const U& b,
          const sstmac_global_builtin<T>& t)
{
  return ((U) b) * ((T) t);
}
#endif

template<typename T, typename U>
U
operator/(const U& b, const sstmac_global_builtin<T>& t)
{
  return b / ((T) t);
}

template<typename T, typename U>
U
operator+(const U& b, const sstmac_global_builtin<T>& t)
{
  return b + (T) t;
}

template<typename T, typename U>
U
operator-(const U& b, const sstmac_global_builtin<T>& t)
{
  return b - (T) t;
}

#if 0
template<typename T, typename U>
U
operator*(const U& b, const sstmac_global_builtin<T>& t)
{
  return b * ((T) t);
}
#endif

template<typename T, typename U>
U
operator%(const U& b, const sstmac_global_builtin<T>& t)
{
  return b % ((T) t);
}

template<typename T, typename U>
U
operator+=(U& b, const sstmac_global_builtin<T>& t)
{
  b += (T) t;
  return b;
}

template<typename T, typename U>
U
operator-=(const U& b, const sstmac_global_builtin<T>& t)
{
  b -= (T) t;
  return b;
}

template<typename T, typename U>
bool
operator<(const U& b, const sstmac_global_builtin<T>& t)
{
  return b < (T) t;
}

template<typename T, typename U>
bool
operator>(const U& b, const sstmac_global_builtin<T>& t)
{
  return b > (T) t;
}

template <typename T>
inline std::ostream&
operator<<(std::ostream& os, const sstmac_global_builtin<T>& t)
{
  os << (T) t;
  return os;
}

}
}
#endif

