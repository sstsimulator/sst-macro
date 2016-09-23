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

#ifndef SSTMAC_SOFTWARE_PROCESS_GLOBAL_PTR_ARRAY_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_GLOBAL_PTR_ARRAY_H_INCLUDED

#include <sstmac/software/process/global_base.h>

namespace sstmac {
namespace sw {

template<typename T, int N>
class sstmac_global_builtin_arr<T*, N> : public sstmac_global
{

 protected:
  typedef spkt_unordered_map<long, T*> val_map;
  typedef T* Tptr;
  val_map vals_[N];
  T* init_;

  bool
  check_init(process_context ptxt, int n = 0) {
    if (n >= N) {
      spkt_throw_printf(
        sprockit::value_error,
        "sstmac_global*::check_init: trying to access index %d outside of array size %d",
        n, N);
    }
    if (ptxt != process_context::none) {
      if (vals_[n].find(ptxt) == vals_[n].end()) {
        vals_[n][ptxt] = init_;
      }
      return true;
    }
    else {
      return false;
    }
  }

 public:
  explicit
  sstmac_global_builtin_arr() :
    init_(NULL) {
  }

  explicit
  sstmac_global_builtin_arr(T* init) :
    init_(init) {
  }

  virtual
  ~sstmac_global_builtin_arr() {
  }

  T*&
  get_val(int n = 0) const {
    if (n >= N) {
      spkt_throw_printf(
        sprockit::value_error,
        "sstmac_global*::get_val: trying to access index %d outside of array size %d",
        n, N);
    }
    process_context ptxt = current_context();
    val_map& vals = const_cast<val_map&> (vals_[n]);
    if (ptxt != process_context::none) {
      typename val_map::iterator it = vals.find(ptxt);
      if (it == vals.end()) {
        T*& ret = vals[ptxt] = init_;
        return ret;
      }
      else {
        return it->second;
      }
    }
    return const_cast<T*&> (init_);
  }

  std::string
  to_string() const {
    std::stringstream ss;
    ss << get_val();
    return ss.str();
  }

  template<typename U>
  T*&
  operator=(const sstmac_global_builtin<U>& b) {
    T*& myval = get_val();
    myval = (T*) b;
    return myval;
  }

  template<typename U>
  T*&
  operator=(U* b) {
    T*& myval = get_val();
    myval = (T*) b;
    return myval;
  }

  T*
  operator++() {
    Tptr& p = get_val();
    return ++p;
  }

  T*
  operator--() {
    Tptr& p = get_val();
    return --p;
  }

  T*
  operator++(int dummy) {
    Tptr& p = get_val();
    return p++;
  }

  T*
  operator--(int dummy) {
    Tptr& p = get_val();
    return p--;
  }

  template<typename U>
  T*
  operator+(const U& b) {
    Tptr p = get_val();
    return p + b;
  }

  template<typename U>
  T*
  operator-(const U& b) {
    Tptr p = get_val();
    return p - b;
  }

  operator char*() {
    return (char*) get_val();
  }

  operator int*() {
    return (int*) get_val();
  }

  operator long*() {
    return (long*) get_val();
  }

  operator void*() {
    return (void*) get_val();
  }

  operator const char*() const {
    return (const char*) get_val();
  }

  operator const int*() const {
    return (const int*) get_val();
  }

  operator const long*() const {
    return (const long*) get_val();
  }

  operator const void*() const {
    return (const void*) get_val();
  }

  T*&
  operator*() {
    return *(get_val());
  }

  template<typename U>
  bool
  operator ==(const U& b) const {
    T* myval = get_val();
    T* otherval = (T*) b;
    return myval == otherval;
  }

  T*
  operator->() {
    return get_val();
  }

  T*&
  operator[](int idx) {
    return get_val(idx);
  }

  T*&
  operator[](long idx) {
    return get_val(idx);
  }

};

}
}
#endif

