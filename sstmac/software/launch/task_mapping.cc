/**
Copyright 2009-2021 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2021, NTESS

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
#include <sstmac/software/launch/task_mapping.h>
#include <sstmac/common/thread_lock.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

std::vector<TaskMapping::ptr> TaskMapping::app_ids_launched_(1024);
std::map<std::string, TaskMapping::ptr> TaskMapping::app_names_launched_;
std::vector<int> TaskMapping::local_refcounts_(1024);

static thread_lock lock;

TaskMapping::ptr
TaskMapping::serialize_order(AppId aid, serializer &ser)
{
  TaskMapping::ptr mapping;
  if (ser.mode() == ser.UNPACK){
    int num_nodes;
    ser & num_nodes;
    mapping = std::make_shared<TaskMapping>(aid);
    ser & mapping->rank_to_node_indexing_;
    mapping->node_to_rank_indexing_.resize(num_nodes);
    int num_ranks = mapping->rank_to_node_indexing_.size();
    for (int i=0; i < num_ranks; ++i){
      NodeId nid = mapping->rank_to_node_indexing_[i];
      mapping->node_to_rank_indexing_[nid].push_back(i);
    }
    lock.lock();
    auto existing = app_ids_launched_[aid];
    if (!existing){
      app_ids_launched_[aid] = mapping;
    } else {
      mapping = existing;
    } 
    lock.unlock();
  } else {
    //packing or sizing
    mapping = app_ids_launched_[aid];
    if (!mapping) spkt_abort_printf("no task mapping exists for application %d", aid);
    int num_nodes = mapping->node_to_rank_indexing_.size();
    ser & num_nodes;
    ser & mapping->rank_to_node_indexing_;
  }
  return mapping;
}

TaskMapping::ptr
TaskMapping::globalMapping(const std::string& name)
{
  lock.lock();
  auto iter = app_names_launched_.find(name);
  if (iter == app_names_launched_.end()){
    spkt_abort_printf("cannot find global task mapping for %s", name.c_str());
  }
  auto ret = iter->second;
  lock.unlock();
  return ret;
}

const TaskMapping::ptr&
TaskMapping::globalMapping(AppId aid)
{
  auto& mapping = app_ids_launched_[aid];
  if (!mapping){
    spkt_abort_printf("No task mapping exists for app %d\n", aid);
  }
  return mapping;
}

void
TaskMapping::addGlobalMapping(AppId aid, const std::string &unique_name, const TaskMapping::ptr &mapping)
{
  lock.lock();
  app_ids_launched_[aid] = mapping;
  app_names_launched_[unique_name] = mapping;
  local_refcounts_[aid]++;
  lock.unlock();
}

void
TaskMapping::removeGlobalMapping(AppId aid, const std::string& name)
{
  lock.lock();
  local_refcounts_[aid]--;
  if (local_refcounts_[aid] == 0){
    app_ids_launched_[aid] = 0;
    app_names_launched_.erase(name);
  }
  lock.unlock();
}

}
}

