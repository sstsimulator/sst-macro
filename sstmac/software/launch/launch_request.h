/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

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

#ifndef APPMANAGER_H
#define APPMANAGER_H

#include <sstmac/common/node_address.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/software/launch/node_set.h>

#include <sprockit/sim_parameters.h>
#include <sprockit/factory.h>
#include <unordered_map>

#include <sstmac/software/process/app_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>

#include <sstmac/software/launch/node_allocator.h>
#include <sstmac/software/launch/task_mapper.h>

#include <vector>

namespace sstmac {
namespace sw {

class SoftwareLaunchRequest
{

 public:
  SoftwareLaunchRequest(SST::Params& params);

  virtual ~SoftwareLaunchRequest();

  int nproc() const {
    return nproc_;
  }

  Timestamp time() const {
    return time_;
  }

  std::vector<int> coreAffinities() const {
    return core_affinities_;
  }

  /**
   * @brief request_allocation  Request a set of nodes to be used by the job
   * @param available   The set of nodes available
   * @param allocation  Reference return. Will contain all the nodes request for the allocation.
   *                    The allocation is NOT necessarily a subset of availabe. If the allocation fails
   *                    the function can still return an allocation request that might be satisfied
   *                    at a later time.
   */
  bool requestAllocation(const ordered_node_set& available,
                     ordered_node_set& allocation);

  /**
   * @brief index_allocation  Given an allocation of nodes, index or map the job. For MPI,
   *                          this means assigning MPI ranks to nodes (and possibly even cores).
   * @param allocation        The set of nodes returned by the allocation request
   */
  void indexAllocation(
    hw::Topology* top,
    const ordered_node_set& allocation,
    std::vector<NodeId>& rank_to_node,
    std::vector<std::list<int>>& node_to_rank);

  bool isIndexed() const {
    return indexed_;
  }

  static void parseAprun(const std::string& cmd, int& nproc, int& nproc_per_node,
              std::vector<int>& coreAffinities);

  static void parseLaunchCmd(
    SST::Params& params,
    int& nproc,
    int& procs_per_node,
    std::vector<int>& affinities);

  bool procDone() {
    ++num_finished_;
    return num_finished_ == nproc_;
  }

 private:
  sw::NodeAllocator* allocator_;
  sw::TaskMapper* indexer_;
  std::vector<int> core_affinities_;

  bool indexed_;

  Timestamp time_;

  int nproc_;

  int procs_per_node_;

  int num_finished_;

  void parseLaunchCmd(SST::Params& params);

 private:
  void initLaunchInfo();

};

class AppLaunchRequest : public SoftwareLaunchRequest
{
 public:
  AppLaunchRequest(SST::Params& params,
             AppId aid,
             const std::string& appNamespace);

  ~AppLaunchRequest() override;

  const std::string& appNamespace() const {
    return app_namespace_;
  }

  const SST::Params& appParams() const {
    return app_params_;
  }

  AppId aid() const {
    return aid_;
  }

 private:
  sw::AppId aid_;

  std::string app_namespace_;

  SST::Params app_params_;

};

}
}



#endif // APPMANAGER_H
