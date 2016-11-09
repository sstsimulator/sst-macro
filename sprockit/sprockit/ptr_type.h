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

#ifndef SPROCKIT_COMMON_PTR_TYPE_H_INCLUDED
#define SPROCKIT_COMMON_PTR_TYPE_H_INCLUDED

namespace sprockit {
class ptr_type;
class simple_ptr_type;
}

#include <sprockit/errors.h>
#include <sprockit/refcount_ptr.h>
#include <sprockit/spkt_config.h>
#include <sprockit/printable.h>
#include <typeinfo>

namespace sprockit {

class ptr_type
{
 private:
  template <class T> friend class refcount_ptr;
  mutable int references;

 public:
  ptr_type() : references(0) {}

  int nreferences() const {
   return references;
  }

 protected:
  void
  incref() {
    ref_increment(references);
  }

  int 
  decref() {
    return ref_decrement_return(references);
  }

 public:
  virtual ~ptr_type(){}

};

template <class Out, class In>
sprockit::refcount_ptr<Out>
__safe_ptr_cast__(const char* objname,
              const char* file,
              int line,
              const sprockit::refcount_ptr<In>& in,
              const char* error_msg = "error")
{
  sprockit::refcount_ptr<Out> out = dynamic_cast<Out*>(in.get());
  if (!out) {
    spkt_abort_printf("%s: failed to cast to %s at %s:%d - %s",
                     error_msg, objname, file, line,
                     sprockit::to_string(in.get()).c_str());
  }
  return out;
}

/**
 * First entry in VA_ARGS is the obj
 * Second entry is optional being an error msg
*/
#define ptr_safe_cast(type,...) \
    ::sprockit::__safe_ptr_cast__<type>(#type, __FILE__, __LINE__, __VA_ARGS__)

#define ptr_interface_cast(type,obj) \
    dynamic_cast<type*>(obj.get())

#define ptr_static_cast(type,obj) \
    static_cast<type*>(obj.get())

#define ptr_test_cast(type, obj) \
    dynamic_cast<type*>(obj.get())

#define ptr_known_cast(type,...) \
    ptr_safe_cast(type, __VA_ARGS__)


} // end of namespace sprockit

#endif

