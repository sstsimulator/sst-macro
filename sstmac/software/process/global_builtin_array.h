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

#ifndef SSTMAC_SOFTWARE_PROCESS_GLOBAL_BUILTIN_ARRAY_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_GLOBAL_BUILTIN_ARRAY_H_INCLUDED

#include <sstmac/software/process/global_base.h>
#include <cstring>

namespace sstmac {
namespace sw {

template<typename T, int N>
class sstmac_global_builtin_arr : public sstmac_global
{

 private:
  typedef spkt_unordered_map<long, T*> val_map;
  val_map vals_;
  T* init_;

 public:
  typedef typename val_map::const_iterator const_iterator;
  typedef T static_arr[N];

  explicit
  sstmac_global_builtin_arr() :
    init_(nullptr) {
  }

  explicit
  sstmac_global_builtin_arr(static_arr init) {
    init_ = new T[N];
    ::memcpy(init_, init, N * sizeof(T));
  }

  sstmac_global_builtin_arr(const sstmac_global_builtin_arr<T, N>& other) {
    spkt_throw_printf(sprockit::illformed_error,
                     "copy constructor should never be called for primitive global");
  }

  virtual
  ~sstmac_global_builtin_arr() {
    for (auto& pair : vals_){
      T* t = pair.second;
      delete t;
    }
    if (init_) delete[] init_;
  }

  void
  print_all() const {
    typename val_map::const_iterator it = vals_.begin();
    for (; it != vals_.end(); ++it) {
      std::cout << it->second << std::endl;
    }
  }

  virtual T*
  get_val() const {
    process_context ptxt = current_context();
    val_map& vals = const_cast<val_map&> (vals_);
    if (ptxt != process_context::none) {
      typename val_map::iterator it = vals.find(ptxt);
      if (it == vals.end()) {
        T* ret = new T[N];
        vals[ptxt] = ret;
        if (init_) {
          ::memcpy(ret, init_, N * sizeof(T));
        }
        return ret;
      }
      else {
        T* r = it->second;
        return r;
      }
    }
    spkt_throw_printf(sprockit::illformed_error,
                     "getting static array value with no process context");
  }

  virtual std::string
  to_string() const {
    std::stringstream ss;
    ss << get_val();
    return ss.str();
  }

  template<typename U>
  T*
  operator =(const U& b) {
    T* myval = get_val();
    myval = (T*) b;
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
    T otherval = (T) b;
    return myval != otherval;
  }

  template <typename U>
  operator U() {
    U ret = (U) get_val();
    return ret;
  }

  T*
  operator ++(int dummy) {
    T* myval = get_val();
    return myval++;
  }

  T*
  operator --(int dummy) {
    T* myval = get_val();
    return myval--;
  }

  T*
  operator ++() {
    T* myval = get_val();
    return ++myval;
  }

  T*
  operator --() {
    T* myval = get_val();
    return --myval;
  }

  operator void*() const {
    return (void*) get_val();
  }

  operator double*() const {
    return (double*) get_val();
  }

  operator int*() const {
    return (int*) get_val();
  }

  operator bool*() const {
    return (bool*) get_val();
  }

  operator size_t*() const {
    return (size_t*) get_val();
  }

  operator long*() const {
    return (long*) get_val();
  }

  operator long long*() const {
    return (long long*) get_val();
  }

  template<typename U>
  T*
  operator +(const U& b) const {
    return get_val() + b;
  }

  template<typename U>
  T*
  operator -(const U& b) const {
    return get_val() - b;
  }



  T&
  operator*() {
    return *get_val();
  }

  T&
  operator[](int idx) {
    T* tarr = get_val();
    return tarr[idx];
  }

  T&
  operator[](long idx) {
    T* tarr = get_val();
    return tarr[idx];
  }

  T**
  operator&() const {
    return &get_val();
  }

  template<typename U>
  bool
  operator >(const U& b) {
    T* myval = get_val();
    T* otherval = (T*) b;
    return myval > otherval;
  }

  template<typename U>
  bool
  operator <(const U& b) {
    T* myval = get_val();
    T* otherval = (T) b;
    return myval < otherval;
  }

  const_iterator
  begin() const {
    return vals_[0].begin();
  }
  const_iterator
  end() const {
    return vals_[0].end();
  }

};


}
}
#endif

