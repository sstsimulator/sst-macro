/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
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