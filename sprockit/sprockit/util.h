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

#ifndef sprockit_common_util_h
#define sprockit_common_util_h

#include <sprockit/spkt_config.h>
#include <sprockit/errors.h>
#include <sprockit/printable.h>

#if SPKT_HAVE_CPP11
#include <functional>
#include <tuple>
#endif

namespace sprockit {

template <class Out, class In>
Out*
__safe_cast__(const char* objname,
              const char* file,
              int line,
              In* in,
              const char* error_msg = "error")
{
  Out* out = dynamic_cast<Out*>(in);
  if (!out) {
    spkt_abort_printf("%s: failed to cast object at %s:%d\n%s",
                     error_msg, file, line,
                     in ? to_string(in).c_str() : "null");
  }
  return out;
}

/**
 * First entry in VA_ARGS is the obj
 * Second entry is optional being an error msg
*/
#define safe_cast(type,...) \
    ::sprockit::__safe_cast__<type>(#type, __FILE__, __LINE__, __VA_ARGS__)

#define test_cast(type, obj) \
    dynamic_cast<type*>(obj)

#define known_cast(type,...) \
    safe_cast(type, __VA_ARGS__)

#define interface_cast(type,obj) \
    dynamic_cast<type*>(obj)

} // end namespace sprockit


#endif
