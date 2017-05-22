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

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/software/launch/task_mapper.h>
#include <sstmac/backends/common/parallel_runtime.h>

RegisterDebugSlot(indexing);

namespace sstmac {
namespace sw {

task_mapper::~task_mapper() throw()
{
}

task_mapper::task_mapper(sprockit::sim_parameters* params) :
  rt_(nullptr), topology_(nullptr)
{
  rt_ = parallel_runtime::static_runtime(params);
  topology_ = sstmac::hw::topology::static_topology(params);
}

int
task_mapper::validate_nproc(int ppn, int num_nodes, int nproc,
                               const char *name) const
{
  if(nproc < 0) {
    nproc = num_nodes * ppn;
  }

  if(num_nodes == 0) {
    spkt_throw_printf(sprockit::value_error,
                     "%s::allocate:  empty node vector",
                     name);
  }

  if(ppn <= 0) {
    spkt_throw_printf(sprockit::value_error,
                     "%s::allocate:  ppn must be > 0. "
                     "If you're running DUMPI or another tracer, launch_indexing parameter "
                     "needs to be hostname or dumpi. Alternatively you can specify a app1.launch_cmd.",
                     name);
  }

  int max_nproc = ppn * num_nodes;
  if (max_nproc < nproc) {
    spkt_throw_printf(sprockit::value_error,
                     "%s::allocate: require ppn * num_nodes >= nproc: ppn=%d num_nodes=%d nproc=%d",
                     name, ppn, num_nodes, nproc);
  }

  return nproc;
}

}
}