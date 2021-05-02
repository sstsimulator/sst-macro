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

#include <sstmac/main/sstmac.h>

#include <iostream>
#include <sstream>
#include <iterator>
#include <getopt.h>
#include <stdint.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/common/cartgrid.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/software/process/app.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/backends/native/manager.h>
#include <sprockit/fileio.h>
#include <sprockit/statics.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/basic_string_tokenizer.h>

namespace sstmac {

struct param_remap {
  const char* deprecated;
  const char* updated;
  bool del;
  param_remap(const char* d, const char* u, bool delet = true) :
    deprecated(d), updated(u), del(delet)
  {
  }

};
typedef param_remap pr;

param_remap remap_list[] = {
  pr("topology_name", "topology.name"),
  pr("topology_geometry", "topology.geometry"),
  pr("network_nodes_per_switch", "topology.concentration"),
  pr("topology_redundant", "topology.redundant"),
  pr("topology_output_graph", "topology.output_graph"),
  pr("topology_seed", "topology.seed"),
  pr("topology_redundant", "topology.redundant"),
  pr("topology_group_connections", "topology.group_connections"),
  pr("switch_geometry", "switch.geometry"),
  pr("switch_name", "switch.name"),
  pr("memory_latency", "node.memory.latency"),
  pr("memory_bandwidth", "node.memory.bandwidth"),
  pr("max_memory_bandwidth", "node.memory.max_single_bandwidth"),
  pr("node_name", "node.name"),
  pr("node_mem_latency", "node.memory.latency"),
  pr("node_mem_bandwidth", "node.memory.bandwidth"),
  pr("nic_negligible_size", "node.nic.negligible_size"),
  pr("nic_name", "node.nic.name"),
  pr("node_MemoryModel", "node.memory.name"),
  pr("node_frequency", "node.proc.frequency"),
  pr("router", "switch.router.name"),
  pr("router_seed", "switch.router.seed"),
  pr("network_bandwidth", "switch.link.bandwidth", false),
  pr("network_bandwidth", "switch.xbar.bandwidth"),
  pr("network_switch_bandwidth", "switch.xbar.bandwidth"),
  pr("network_latency", "switch.link.latency"),
  pr("network_hop_latency", "switch.link.latency"),
  pr("network_switch_type", "switch.name"),
  pr("pisces_injection_bandwidth", "switch.ejection_bandwidth", false),
  pr("pisces_injection_bandwidth", "node.nic.injection.bandwidth"),
  pr("pisces_injection_latency", "node.nic.injection.latency"),
  pr("PiscesSwitch_crossbar_latency", "switch.xbar.latency"),
  pr("node_cores", "node.proc.ncores"),
  pr("node_sockets", "node.nsockets"),
  pr("node_pipeline_speedup", "node.proc.parallelism"),
  pr("smp_single_copy_size", "node.app1.mpi.smp_single_copy_size"),
  pr("max_eager_msg_size", "node.app1.mpi.max_eager_msg_size"),
  pr("max_vshort_msg_size", "node.app1.mpi.max_vshort_msg_size"),
  pr("mpi_queue_post_rdma_delay", "node.mpi.queue.post_rdma_delay"),
  pr("mpi_queue_post_header_delay", "node.mpi.queue.post_header_delay"),
  pr("mpi_queue_poll_delay", "node.mpi.queue.poll_delay"),
  pr("mpi_spyplot", "node.mpi.queue.traffic_matrix.fileroot"),
  pr("network_spyplot", "node.nic.traffic_matrix.fileroot"),
  pr("ftq", "node.os.ftq.fileroot"),
  pr("ftq_epoch", "node.os.ftq.epoch"),
  pr("callGraph", "node.os.callGraph.fileroot"),
  pr("stack_size", "node.os.stack_size"),
  pr("stack_chunk_size", "node.os.stack_chunk_size"),
  pr("injection_redundant", "node.nic.injection.redundant", false),
  pr("injection_redundant", "switch.ejection.redundant", false),
  pr("injection_latency", "node.nic.injection.latency", false),
  pr("injection_bandwidth", "node.nic.injection.bandwidth", false),
  pr("injection_latency", "switch.ejection.latency"),
  pr("injection_bandwidth", "switch.ejection.bandwidth"),
  pr("intragroup_connection_file", "topology.intragroup_connection_file", false),
  pr("intergroup_connection_file", "topology.intergroup_connection_file"),
  pr("launch_app1", "node.app1.name"),
  pr("launch_start_app1", "node.app1.start"),
  pr("launch_allocation", "node.app1.allocation"),
  pr("launch_indexing", "node.app1.indexing"),
  pr("launch_cmd_app1", "node.app1.launch_cmd"),
  pr("launch_app1_type", "node.app1.launch_type"),
  pr("launch_app1_argv", "node.app1.argv"),
  pr("launch_app1_size", "node.app1.size"),
  pr("launch_dumpi_metaname", "node.app1.dumpi_metaname"),
  pr("launch_dumpi_mapname", "node.app1.dumpi_mapname"),
  pr("launch_node_id_file", "node.app1.node_id_file"),
  pr("launch_coordinate_file", "node.app1.coordinate_file"),
  pr("launch_dumpi_mapname", "node.app1.dumpi_mapname"),
  pr("launch_hostname_list", "node.app1.hostname_list"),
  pr("cart_launch_sizes", "node.app1.cart_sizes"),
  pr("cart_launch_offsets", "node.app1.cart_offsets"),
  pr("launch_node_id_file", "node.app1.node_id_file"),
  pr("launch_node_id_allocation_file", "node.app1.node_id_allocation_file"),
  pr("launch_node_id_mapper_file", "node.app1.node_id_indexing_file"),
  pr("launch_node_id_indexing_file", "node.app1.node_id_indexing_file"),
};


void
remapParams(sprockit::SimParameters::ptr params, bool verbose)
{
  TimeDelta::initStamps(100);

  sprockit::SimParameters::ptr top_params = params->getNamespace("topology");
  bool auto_top = top_params->getOptionalBoolParam("auto", false);
  if (auto_top){
    int max_nproc = native::Manager::computeMaxNproc(params);
    if (max_nproc == 0){
      params->printParams(std::cerr);
      spkt_abort_printf("computed max nproc=0 from parameters - need app1.launch_cmd or app1.size");
    }
    resizeTopology(max_nproc, params, verbose);
    //clear the auto keyword to keep params self-consistent
    //top_params->remove_param("auto");
  }

  //here is where we want to read debug params and active debug printing for stuff, maybe
  std::vector<std::string> debug_flags;
  if (params->hasParam("debug")){
    params->getVectorParam("debug", debug_flags);
  }

  for (int i=0; i < debug_flags.size(); ++i){
    sprockit::Debug::turnOn(debug_flags[i]);
  }

  /** If more than one thread, make sure event manager is multithreaded */
  if (params->hasParam("sst_nthread")){
    int nthr = params->getIntParam("sst_nthread");
    if (nthr > 1 && !params->hasParam("event_manager")){
      params->addParamOverride("event_manager", "multithread");
    }
  }
}

}

void
resizeTopology(int max_nproc, sprockit::SimParameters::ptr params, bool verbose)
{
  sprockit::SimParameters::ptr top_params = params->getNamespace("topology");
  if (top_params->hasParam("name")){
    spkt_abort_printf("cannot specify topology name with auto topology");
  }
  top_params->addParamOverride("name", "torus");

  //create a topology matching nproc
  int x, y, z;
  genCartGrid(max_nproc, x, y, z);
  std::string paramval = sprockit::sprintf("[%d,%d,%d]", x, y, z);
  top_params->addParamOverride("geometry", paramval);
  if (verbose)
    cerr0 << sprockit::sprintf("Using auto-generated geometry [%d %d %d] for nproc=%d\n", x, y, z, max_nproc);
}

#if 0
void
map_env_params(SST::Params& params)
{
  //read environmental variables as potential overrides
  char* param = getenv("MPICH_GNI_MAX_VSHORT_MSG_SIZE");
  if (param) {
    params.insert("max_vshort_msg_size", param);
  }

  param = getenv("MPICH_GNI_MAX_EAGER_MSG_SIZE");
  if (param) {
    params.insert("max_eager_msg_size", param);
  }

  param = getenv("MPICH_SMP_SINGLE_COPY_SIZE");
  if (param) {
    params.insert("smp_single_copy_size", param);
  }

  /** set to an absurdly high value - no single copies */
  param = getenv("MPICH_SMP_SINGLE_COPY_OFF");
  if (param) {
    params.insert("smp_single_copy_size", "99999999999");
  }
}
#endif
