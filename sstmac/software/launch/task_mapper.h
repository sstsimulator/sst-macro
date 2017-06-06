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

#ifndef SSTMAC_BACKENDS_NATIVE_LAUNCH_INDEXSTRATEGY_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_LAUNCH_INDEXSTRATEGY_H_INCLUDED

#include <sstmac/common/node_address.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>
#include <sprockit/printable.h>
#include <vector>
#include <sstmac/software/launch/node_set.h>

DeclareDebugSlot(indexing);

namespace sstmac {
namespace sw {

/**
 * Base class for strategies regarding how to sequentially number nodes
 * in a parallel simulation.
 */
class task_mapper : public sprockit::printable
{
  DeclareFactory(task_mapper)
 public:
  virtual ~task_mapper() throw ();

  /** Assign processes to nodes.
   @param aid The application ID for the application whose processes are being indexed
   @param nodes is the set of unique nodes to be used for the allocation
   @param ppn is the nominal number of processes allocated on each node.
   @param result is the resulting vector of length nodes (size nproc)
   @param nproc the total number of processes to allocate
   @throw value_error if nodes.empty()
   @throw value_error if ppn <= 0
   @throw value_error if nodes.size()*ppn < nproc
  */
  virtual void map_ranks(
    const ordered_node_set& allocation,
    int ppn,
    std::vector<node_id>& result,
    int nproc) = 0;

 protected:
  task_mapper(sprockit::sim_parameters* params);

  int validate_nproc(int ppn, int num_nodes, int nproc, const char* name) const;

 protected:
  hw::topology* topology_;
  parallel_runtime* rt_;

};

}
} // end of namespace sstmac

#endif