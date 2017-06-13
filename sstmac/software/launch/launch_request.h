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

#ifndef APPMANAGER_H
#define APPMANAGER_H

#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/common/node_address.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/software/launch/node_set.h>

#include <sprockit/factories/factory.h>
#include <sprockit/unordered.h>

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

class software_launch_request
{

 public:
  software_launch_request(sprockit::sim_parameters* params);

  virtual ~software_launch_request();

  int
  nproc() const {
    return nproc_;
  }

  timestamp
  time() const {
    return time_;
  }

  std::vector<int>
  core_affinities() const {
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
  void
  request_allocation(const ordered_node_set& available,
                     ordered_node_set& allocation);

  /**
   * @brief index_allocation  Given an allocation of nodes, index or map the job. For MPI,
   *                          this means assigning MPI ranks to nodes (and possibly even cores).
   * @param allocation        The set of nodes returned by the allocation request
   */
  void
  index_allocation(
    hw::topology* top,
    const ordered_node_set& allocation,
    std::vector<node_id>& rank_to_node,
    std::vector<std::list<int>>& node_to_rank);

  bool
  is_indexed() const {
    return indexed_;
  }

  static void
  parse_aprun(const std::string& cmd, int& nproc, int& nproc_per_node,
              std::vector<int>& core_affinities);

  static void
  parse_launch_cmd(
    sprockit::sim_parameters* params,
    int& nproc,
    int& procs_per_node,
    std::vector<int>& affinities);

  bool proc_done() {
    ++num_finished_;
    return num_finished_ == nproc_;
  }

 private:
  sw::node_allocator* allocator_;
  sw::task_mapper* indexer_;
  std::vector<int> core_affinities_;

  bool indexed_;

  timestamp time_;

  int nproc_;

  int procs_per_node_;

  int num_finished_;

  void parse_launch_cmd(sprockit::sim_parameters* params);

 private:
  void init_launch_info();

};

class app_launch_request : public software_launch_request
{
 public:
  app_launch_request(sprockit::sim_parameters* params,
             app_id aid,
             const std::string& app_namespace);

  virtual ~app_launch_request();

  const std::string&
  app_namespace() const {
    return app_namespace_;
  }

  sprockit::sim_parameters*
  app_params() const {
    return app_params_;
  }

  app_id
  aid() const {
    return aid_;
  }

 private:
  sw::app_id aid_;

  std::string app_namespace_;

  sprockit::sim_parameters* app_params_;

};

}
}



#endif // APPMANAGER_H