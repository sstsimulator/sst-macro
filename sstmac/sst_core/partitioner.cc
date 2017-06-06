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

#include <sst/core/part/sstpart.h>
#include <sst/core/elementinfo.h>
#include <sst_config.h>
#include <sst/core/configGraph.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sstmac/hardware/topology/topology.h>

using namespace SST;
using namespace SST::Partition;

namespace sstmac {

class dummy_runtime : public parallel_runtime
{
 public:
  dummy_runtime(sprockit::sim_parameters* params,
                   int me, int nproc, int nthread) :
    parallel_runtime(params, me, nproc)
  {
    nthread_ = nthread;
  }

  int64_t allreduce_min(int64_t mintime) override { return 0; }

  int64_t allreduce_max(int64_t maxtime) override { return 0; }

  void global_sum(long *data, int nelems, int root) override {}

  void global_sum(long long *data, int nelems, int root) override {}

  void global_max(int *data, int nelems, int root) override {}

  void global_max(long *data, int nelems, int root) override {}

  void gather(void *send_buffer, int num_bytes, void *recv_buffer, int root) override {}

  void allgather(void *send_buffer, int num_bytes, void *recv_buffer) override {}

  void send(int dst, void *buffer, int buffer_size) override {}

  void recv(int src, void *buffer, int buffer_size) override {}

  void bcast(void* buffer, int bytes, int root) override {}

  void finalize() override {}

  /**
   * @param The topology id to send a remote message to
   * @param buffer The buffer containing a serialized message
   * @param size The size of the buffer being sent
   */
  void send_event(timestamp t, switch_id sid, event* ev) {}

  void wait_merge_array(int tag) override {}

  void declare_merge_array(void* buffer, int size, int tag) override {}

  bool release_merge_array(int tag) override { return false; }

 protected:
  void do_send_message(int lp, void* buffer, int size) override {}

  void do_send_recv_messages(std::vector<void*>& buffers) override {}


};

class SSTMacroPartition : public SSTPartitioner
{
 private:
  RankInfo world_size;
  RankInfo me;

 public:
  SSTMacroPartition(RankInfo size, RankInfo my_rank, int verbosity) :
    world_size(size), me(my_rank)
  {
  }

  void performPartition(ConfigGraph* graph) override;

  bool requiresConfigGraph() override { return true; }

  bool spawnOnAllRanks() override { return false; }

  SST_ELI_REGISTER_PARTITIONER(SSTMacroPartition,"macro","block",
     SST_ELI_ELEMENT_VERSION(7,1,0),
     "Partitions using the SST/macro based partitioning schemes")

};

void
SSTMacroPartition::performPartition(SST::ConfigGraph *graph)
{
  SST::ConfigComponentMap_t& compMap = graph->getComponentMap();
  if (compMap.size() == 0)
    return;
  sprockit::sim_parameters part_params;
  sprockit::sim_parameters* top_subparams = part_params.get_optional_namespace("topology");
  //I need to figure out the topology
  ConfigComponent& front = *compMap.begin();

  sprockit::sim_parameters* params = make_spkt_params_from_sst_params(front.params);
  if (params->has_namespace("interconnect")){
    params->get_namespace("interconnect")
        ->get_namespace("topology")->combine_into(top_subparams);
  } else {
    params->get_namespace("topology")->combine_into(top_subparams);
  }

  //if we have no switches, logp network only
  bool is_logp = front.name.substr(0,4) == "Node";

  delete params;
  part_params.add_param_override("name", "block");
  int nthread = world_size.thread;
  int nproc = world_size.rank;
  dummy_runtime rt(&part_params, me.rank, nproc, nthread);
  block_partition part(&part_params, &rt);
  part.finalize_init();
  hw::topology* top = part.top();
  int num_switches = is_logp ? 0 : top->num_switches();
  int num_nodes = top->num_nodes();
  int node_cutoff = num_switches;
  int logp_cutoff = num_switches + num_nodes;

  for (ConfigComponent& comp : compMap){
    ComponentId_t id = comp.id;
    if (id >= logp_cutoff){
      int logp_offset = id - logp_cutoff;
      int rank = logp_offset / nthread;
      int thread = logp_offset % nthread;
      comp.rank.rank = rank;
      comp.rank.thread = thread;
    } else if (id >= node_cutoff){
      int node_offset = id - node_cutoff;
      int logp_id = top->node_to_logp_switch(node_offset);
      int rank = logp_id / nthread;
      int thread = logp_id % nthread;
      comp.rank.rank = rank;
      comp.rank.thread = thread;
    } else {
      int sw_id = id;
      int rank = part.lpid_for_switch(sw_id);
      int thread = part.thread_for_switch(sw_id);
      comp.rank.rank = rank;
      comp.rank.thread = thread;
    }
  }
}

}
