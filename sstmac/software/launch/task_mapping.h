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
#ifndef sstmac_sw_launch_task_mapping_h
#define sstmac_sw_launch_task_mapping_h

#include <list>
#include <vector>
#include <map>
#include <memory>
#include <sstmac/software/process/app_id.h>
#include <sstmac/common/node_address.h>
#include <sstmac/common/serializable.h>

namespace sstmac {
namespace sw {

class TaskMapping {
 public:
  TaskMapping(AppId aid) : aid_(aid) {}

  typedef std::shared_ptr<TaskMapping> ptr;

  NodeId rankToNode(int rank) const {
    return rank_to_node_indexing_[rank];
  }

  const std::list<int>& nodeToRanks(int node) const {
    return node_to_rank_indexing_[node];
  }

  AppId aid() const {
    return aid_;
  }

  static TaskMapping::ptr serialize_order(AppId aid, serializer& ser);

  int numRanks() const {
    return rank_to_node_indexing_.size();
  }

  int nproc() const {
    return rank_to_node_indexing_.size();
  }

  const std::vector<NodeId>& rankToNode() const {
    return rank_to_node_indexing_;
  }

  std::vector<NodeId>& rankToNode() {
    return rank_to_node_indexing_;
  }

  const std::vector<std::list<int>>& nodeToRank() const {
    return node_to_rank_indexing_;
  }

  std::vector<std::list<int>>& nodeToRank() {
    return node_to_rank_indexing_;
  }

  static const TaskMapping::ptr& globalMapping(AppId aid);

  static TaskMapping::ptr globalMapping(const std::string& unique_name);

  static void addGlobalMapping(AppId aid, const std::string& unique_name,
                     const TaskMapping::ptr& mapping);

  static void removeGlobalMapping(AppId aid, const std::string& name);

 private:
  AppId aid_;
  std::vector<NodeId> rank_to_node_indexing_;
  std::vector<std::list<int> > node_to_rank_indexing_;
  std::vector<int> core_affinities_;

  static std::vector<int>  local_refcounts_;
  static std::vector<TaskMapping::ptr> app_ids_launched_;
  static std::map<std::string, TaskMapping::ptr> app_names_launched_;

  static void deleteStatics();
};

}
}

#endif
