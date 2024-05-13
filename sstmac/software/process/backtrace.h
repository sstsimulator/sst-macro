/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#ifndef sstmac_software_process_backtrace_h
#define sstmac_software_process_backtrace_h

#include <sstmac/common/sstmac_config.h>
#include <map>
#include <memory>

#define CallGraphCreateTag(name) \
  struct graph_viz_##name : public sstmac::sw::CallGraphID<graph_viz_##name> {}; \
  static sstmac::sw::CallGraphRegistration graph_viz_reg_##name(#name, graph_viz_##name::id)

#define CallGraphTag(name) graph_viz_##name::id

namespace sstmac {
namespace sw {

struct CallGraphRegistration {
  CallGraphRegistration(const char* name, int id);

  static int numIds() {
    return id_count;
  }

  static const char* name(int id){
    auto iter = names->find(id);
    return iter->second;
  }

  static int id_count;

 private:
  static std::unique_ptr<std::map<int,const char*>> names;
};

template <class T>
struct CallGraphID {
 public:
  static int id;
};
template <class T> int CallGraphID<T>::id = CallGraphRegistration::id_count++;

class CallGraphIncrementStack
{
 public:
  /**
   * @brief graph_viz_increment_stack
   *        Should only ever be called from app threads, not the DES thread
   * @param The name of the function currently being invoked
   * @param Optional boolean to turn off collection.
   *        There are certain cass where this might get called from
   *        the DES thread, which is an error. This allows
   *        the backtrace to be turned off on the DES thread
   */
  CallGraphIncrementStack(int id);

  ~CallGraphIncrementStack();

};

}
}

#include <unusedvariablemacro.h>
#if SSTMAC_HAVE_CALL_GRAPH
#define CallGraphAppend(name) \
  struct graph_viz_##name : public sstmac::sw::CallGraphID<graph_viz_##name> {}; \
  static sstmac::sw::CallGraphRegistration __call_graph_register_variable__(#name, graph_viz_##name::id); \
  ::sstmac::sw::CallGraphIncrementStack __call_graph_append_variable__(graph_viz_##name::id)
#else
#define CallGraphAppend(...) SSTMAC_MAYBE_UNUSED int __call_graph_append_variable__
#endif

#endif
