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

#ifndef SSTMAC_SOFTWARE_PROCESS_GLOBAL_STRUCT_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_GLOBAL_STRUCT_H_INCLUDED

#include <sstmac/software/process/global_base.h>

namespace sstmac {
namespace sw {

template<typename T>
class sstmac_global_struct : public sstmac_global_builtin<T>
{
  typedef sstmac_global_builtin<T> parent;
 public:
  explicit
  sstmac_global_struct() {
  }

  explicit
  sstmac_global_struct(const T &init) :
    parent(init) {
  }

  operator T*() {
    return &parent::get_val();
  }

  operator const T*() const {
    return &parent::get_val();
  }

  template<typename U>
  T&
  operator=(const sstmac_global_struct<U>& b) {
    T*& myval = parent::get_val();
    myval = (T*) b;
    return myval;
  }

  T&
  operator=(T* b) {
    T*& myval = parent::get_val();
    myval = b;
    return myval;
  }

  template<typename U>
  T&
  operator=(const U &b) {
    T& myval = parent::get_val();
    myval = (T) b;
    return myval;
  }

  T*&
  operator ->() {
    T*& myval = &parent::get_val();
    return myval;
  }

  operator T() {
    T& myval = parent::get_val();
    return myval;
  }



  //this assumes you've overloaded the * operator in the struct
  T&
  operator *() {
    T myval = parent::get_val();
    return *(myval);
  }

};

template<typename T>
class sstmac_global_struct<T*> : public sstmac_global_builtin<T*>
{
  typedef sstmac_global_builtin<T*> parent;
 public:
  explicit
  sstmac_global_struct() {
  }

  explicit
  sstmac_global_struct(T* init) :
    parent(init) {
  }

  operator T*() {
    return parent::get_val();
  }

  operator const T*() const {
    return parent::get_val();
  }

  template<typename U>
  T*&
  operator=(const sstmac_global_struct<U>& b) {
    T*& myval = parent::get_val();
    myval = (T*) b;
    return myval;
  }

  T*&
  operator=(T* b) {
    T*& myval = parent::get_val();
    myval = b;
    return myval;
  }

  template<typename U>
  T*&
  operator=(U* b) {
    T*& myval = parent::get_val();
    myval = (T*) b;
    return myval;
  }

  T*&
  operator ->() {
    T*& myval = parent::get_val();
    return myval;
  }

};
}
}
#endif

