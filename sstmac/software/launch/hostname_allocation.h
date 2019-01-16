/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

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

#ifndef SSTMAC_BACKENDS_NATIVE_LAUNCH_hostname_allocation_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_LAUNCH_hostname_allocation_H_INCLUDED

#include <sstmac/software/launch/node_allocator.h>
#include <sstmac/dumpi_util/dumpi_meta.h>

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace sw {

class HostnameAllocation : public NodeAllocator
{
  FactoryRegister("hostname", NodeAllocator, HostnameAllocation,
              "Given a file containing one hostname/coordinate pair per line, "
              "return a node allocation with all hosts in the file")
 public:
  HostnameAllocation(SST::Params& params);

  std::string toString() const override {
    return "hostname allocation";
  }

  /**
   * This function reads in a map file for mapping host names to network coordinates.
   * @param here the function making the call, used for more informative exceptions
   * @param mapfile name of the map file
   * @param hostmap map in which host mappings are placed
   */
  static void readHostFile(ParallelRuntime* rt,
                const char* here, const std::string &mapfile,
                std::vector<std::string>& hostmap);

  bool allocate(int nnode_requested,
    const ordered_node_set& available,
    ordered_node_set &allocation) const override;

  virtual ~HostnameAllocation() throw () {}


 protected:
  std::string hostfile_;

};

}
}

#endif /* hostname_allocation_H_ */
