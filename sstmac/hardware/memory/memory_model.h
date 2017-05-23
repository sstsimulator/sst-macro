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

#ifndef MEMORYMODEL_H_
#define MEMORYMODEL_H_

#include <sstmac/hardware/common/connection.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/memory/memory_id.h>

DeclareDebugSlot(memory)
#define mem_debug(...) \
    debug_printf(sprockit::dbg::memory, "Memory on Node %d: %s", int(nodeid_), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

class memory_model :
  public event_subcomponent
{
  DeclareFactory(memory_model,node*)
 public:
  memory_model(sprockit::sim_parameters* params,
               node* node);

  static void
  delete_statics();

  virtual ~memory_model();

  virtual void
  access(long bytes, double max_bw,
         callback* cb) = 0;

  virtual std::string
  to_string() const = 0;

  virtual double
  max_single_bw() const = 0;

  node_id addr() const;

 protected:
  memory_model();

 protected:
  node_id nodeid_;
  node* parent_node_;
  event_handler* done_;

};

}
} /* namespace sstmac */
#endif /* MEMORYMODEL_H_ */