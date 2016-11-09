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

#ifndef SSTMAC_SOFTWARE_PROCESS_KEY_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_KEY_H_INCLUDED

#include <sstmac/software/threading/threading_interface.h>
#include <sstmac/software/process/thread_data.h>
#include <sprockit/unordered.h>
#include <stdint.h>

#include <cstring>
#include <set>

namespace sstmac {
namespace sw {

/**
 * A base type and default (empty) implementation of a handle
 * to block and unblock processes.
 */
class key  {

  /**
      Global identifier for all keys of a given type.
      This should be a STATIC object that gets passed
      into every key when selecting type.
  */
 public:
  typedef std::set<thread_data_t> blocking_t;

  class category
  {
   private:
    friend class library;

    int id_;

    //only callable by library
    category();

   public:
    category(const std::string& name);


    std::string
    name() const {
      return key::name(id_);
    }

    int id() const {
      return id_;
    }
  };


 public:
  static category general;

 public:
  static key*
  construct();

  static key*
  construct(const category& name);

  static std::string
  name(int keyname_id) {
    return (*category_id_to_name_)[keyname_id];
  }

  static int
  allocate_category_id(const std::string& name);

  static int
  event_typeid(const std::string& event_name);

  static int
  num_categories() {
    return category_name_to_id_->size();
  }

  std::string
  name() const {
    return name(keyname_id_);
  }

  int 
  event_typeid() const {
    return keyname_id_;
  }

  virtual ~key();

  void*
  operator new(size_t size);

  void
  operator delete(void* ptr);

  void
  block_thread(thread_data_t t){
    blocked_thread_ = t;
  }

  bool
  still_blocked() {
    return blocked_thread_.second;
  }

  bool
  timed_out() const {
    return timed_out_;
  }

  void
  set_timed_out() {
    timed_out_ = true;
  }

  void
  clear() {
    blocked_thread_.first = 0;
    blocked_thread_.second = 0;
  }

  static void
  delete_statics();

 private:
  friend class operating_system;

  key();

  key(const category& name);

 private:
  static spkt_unordered_map<std::string, int>* category_name_to_id_;
  static spkt_unordered_map<int, std::string>* category_id_to_name_;
  static uint64_t key_storage_size_;

  thread_data_t blocked_thread_;
  bool timed_out_;
  int keyname_id_;

};

}
} // end of namespace sstmac

#endif

