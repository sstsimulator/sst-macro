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

#ifndef sumi_THREAD_SAFE_PTR_TYPE_H
#define sumi_THREAD_SAFE_PTR_TYPE_H

namespace sumi {
class thread_safe_ptr_type;
}

#include <sprockit/errors.h>
#include <sumi/thread_safe_refcount_ptr.h>
#include <sumi/thread_safe_int.h>
#include <typeinfo>


namespace sumi {

class thread_safe_ptr_type
{
 private:
  template <class T> friend class thread_safe_refcount_ptr;
  mutable thread_safe_int<long> references;

 public:
  thread_safe_ptr_type() : references(0) {}

  long nreferences() const {
   return references;
  }

 protected:
  void
  incref() const {
    ++references;
  }

  long
  decref() const {
    return --references;
  }

 public:
  virtual ~thread_safe_ptr_type(){}

};

class printable_thread_safe_ptr_type : public thread_safe_ptr_type
{
 public:
  virtual std::string
  to_string() const override = 0;
};

template <class Out, class In>
sumi::thread_safe_refcount_ptr<Out>
__safe_ptr_cast__(const char* objname,
              const char* file,
              int line,
              const sumi::thread_safe_refcount_ptr<In>& in,
              const char* error_msg = "error")
{
  sumi::thread_safe_refcount_ptr<Out> out = dynamic_cast<Out*>(in.get());
  if (!out) {
    spkt_throw_printf(sprockit::value_error,
                     "%s: failed to cast to %s at %s:%d - %s",
                     error_msg, objname, file, line,
                     (in ? "wrong type" : "null object"));
  }
  return out;
}

/**
 * First entry in VA_ARGS is the obj
 * Second entry is optional being an error msg
*/
#define sumi_ptr_safe_cast(type,...) \
    ::sumi::__safe_ptr_cast__<type>(#type, __FILE__, __LINE__, __VA_ARGS__)

} // end of namespace sumi

#endif

