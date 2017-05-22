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

#ifndef SSTMAC_SOFTWARE_SERVICES_LAUNCH_INDEXING_FROMFILEINDEXER_H_INCLUDED
#define SSTMAC_SOFTWARE_SERVICES_LAUNCH_INDEXING_FROMFILEINDEXER_H_INCLUDED

#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sstmac/software/launch/task_mapper.h>

namespace sstmac {
namespace sw {


class dumpi_task_mapper : public task_mapper
{
  FactoryRegister("dumpi", task_mapper, dumpi_task_mapper,
              "indexes nodes based on hostname map file and hostname list in dumpi trace")
 public:
  dumpi_task_mapper(sprockit::sim_parameters *params);

  std::string to_string() const override {
    return "dumpi task mapper";
  }

  virtual ~dumpi_task_mapper() throw() {}

  void map_ranks(const ordered_node_set& nodes,
                int ppn,
                std::vector<node_id> &result,
                int nproc) override;
 protected:
  node_id node_id_from_hostname(const std::string& hostname);

  node_id node_id_from_coordinates(int ncoord, int* coordinates);

 protected:
  std::string metaname_;

  hw::cartesian_topology* regtop_;

};

}
}
#endif