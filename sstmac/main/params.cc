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
#include <sprockit/malloc.h>
#include <sprockit/spkt_new.h>
#include <sprockit/output.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sstmac/common/param_expander.h>

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
  pr("network_name", "interconnect"),
  pr("topology_name", "topology.name"),
  pr("topology_geometry", "topology.geometry"),
  pr("network_nodes_per_switch", "topology.concentration"),
  pr("topology_true_random_intermediate", "topology.true_random_intermediate"),
  pr("topology_redundant", "topology.redundant"),
  pr("topology_output_graph", "topology.output_graph"),
  pr("topology_seed", "topology.seed"),
  pr("topology_redundant", "topology.redundant"),
  pr("topology_group_connections", "topology.group_connections"),
  pr("switch_geometry", "switch.geometry"),
  pr("switch_name", "switch.model"),
  pr("memory_latency", "node.memory.latency"),
  pr("memory_bandwidth", "node.memory.bandwidth"),
  pr("max_memory_bandwidth", "node.memory.max_single_bandwidth"),
  pr("node_name", "node.model"),
  pr("node_mem_latency", "node.memory.latency"),
  pr("node_mem_bandwidth", "node.memory.bandwidth"),
  pr("nic_negligible_size", "node.nic.negligible_size"),
  pr("nic_name", "node.nic.model"),
  pr("node_memory_model", "node.memory.model"),
  pr("node_frequency", "node.proc.frequency"),
  pr("router", "switch.router.name"),
  pr("router_seed", "switch.router.seed"),
  pr("network_bandwidth_link", "switch.link.bandwidth"),
  pr("network_link_bandwidth", "switch.link.bandwidth"),
  pr("network_bandwidth", "switch.link.bandwidth", false),
  pr("network_bandwidth", "switch.xbar.bandwidth"),
  pr("network_switch_bandwidth", "switch.xbar.bandwidth"),
  pr("network_latency", "switch.link.latency"),
  pr("network_hop_latency", "switch.link.latency"),
  pr("network_switch_type", "switch.model"),
  pr("pisces_memory_bandwidth", "node.memory.total_bandwidth"),
  pr("pisces_memory_single_bandwidth", "node.memory.max_single_bandwidth"),
  pr("pisces_memory_latency", "node.memory.latency"),
  pr("pisces_memory_arbitrator", "node.memory.arbitrator"),
  pr("pisces_memory_mtu", "node.memory.mtu"),
  pr("pisces_injection_bandwidth", "switch.ejection_bandwidth", false),
  pr("pisces_injection_bandwidth", "node.nic.injection.bandwidth"),
  pr("pisces_injection_latency", "node.nic.injection.latency"),
  pr("pisces_eject_buffer_size", "node.nic.eject_buffer_size"),
  pr("pisces_network_link_bandwidth", "switch.link.bandwidth"),
  pr("pisces_network_hop_latency", "switch.link.latency"),
  pr("pisces_switch_output_buffer_size", "switch.output_buffer_size"),
  pr("pisces_switch_crossbar_bandwidth", "switch.xbar.bandwidth"),
  pr("pisces_switch_crossbar_latency", "switch.xbar.latency"),
  pr("pisces_switch_input_buffer_size", "switch.input_buffer_size", false),
  pr("pisces_switch_input_buffer_size", "node.nic.injection.credits"),
  pr("pisces_arbitrator", "node.nic.arbitrator", false),
  pr("pisces_arbitrator", "switch.arbitrator"),
  pr("node_cores", "node.ncores"),
  pr("node_sockets", "node.nsockets"),
  pr("node_model", "node.model"),
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
  pr("call_graph", "node.os.call_graph.fileroot"),
  pr("stack_size", "node.os.stack_size"),
  pr("stack_chunk_size", "node.os.stack_chunk_size"),
  pr("stack_protect", "node.os.stack_protect"),
  pr("injection_redundant", "node.nic.injection.redundant", false),
  pr("injection_redundant", "switch.ejection.redundant", false),
  pr("injection_latency", "node.nic.injection.latency", false),
  pr("injection_bandwidth", "node.nic.injection.bandwidth", false),
  pr("injection_latency", "switch.ejection.latency"),
  pr("injection_bandwidth", "switch.ejection.bandwidth"),
  pr("intragroup_connection_file", "topology.intragroup_connection_file", false),
  pr("intergroup_connection_file", "topology.intergroup_connection_file"),
  pr("launch_app1", "node.app1.name"),
  pr("launch_app1_start", "node.app1.start"),
  pr("launch_allocation", "node.app1.allocation"),
  pr("launch_indexing", "node.app1.indexing"),
  pr("launch_app1_cmd", "node.app1.launch_cmd"),
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
remap_deprecated_params(sprockit::sim_parameters* params)
{
  int num_remap = sizeof(remap_list) / sizeof(param_remap);
  for (int i=0; i < num_remap; ++i){
    param_remap& p = remap_list[i];
    if (params->has_param(p.deprecated)){
      params->parse_keyval(p.updated,
         params->get_param(p.deprecated),
         false/*fail on existing*/,
         false/*do not overwrite anything*/,
         false/*do not mark anything as read*/);
      if (p.del){
        params->remove_param(p.deprecated);
      }
    }
  }
}

void
remap_latency_params(sprockit::sim_parameters* params)
{

}

void
remap_params(sprockit::sim_parameters* params, bool verbose)
{
  remap_deprecated_params(params);
  remap_latency_params(params);

  int max_nproc = native::manager::compute_max_nproc(params);
  if (max_nproc == 0){
    params->print_scoped_params(std::cerr);
    spkt_abort_printf("computed max nproc=0 from parameters - need app1.launch_cmd or app1.size");
  }
  resize_topology(max_nproc, params, verbose);

  //here is where we might need to build supplemental params
  bool has_cong_model = params->has_param("congestion_model");
  bool has_amm_model = params->has_param("amm_model");
  if (has_cong_model && !has_amm_model){
    spkt_abort_printf("If specying congestion_model, must also specify amm_model");
  }
  if (has_amm_model && !has_cong_model){
    spkt_abort_printf("If specifiyng amm_model, must also specify congestion_model");

  }

  if (has_cong_model && has_amm_model){
    sstmac::param_expander* hw_expander = sstmac::param_expander::factory::get_param("congestion_model", params);
    hw_expander->expand(params);
    delete hw_expander;
  }

  //here is where we want to read debug params and active debug printing for stuff, maybe
  std::vector<std::string> debug_flags;
  if (params->has_param("debug")){
    params->get_vector_param("debug", debug_flags);
  }

  for (int i=0; i < debug_flags.size(); ++i){
    sprockit::debug::turn_on(debug_flags[i]);
  }

  /** If more than one thread, make sure event manager is multithreaded */
  if (params->has_param("sst_nthread")){
    int nthr = params->get_int_param("sst_nthread");
    if (nthr > 1 && !params->has_param("event_manager")){
      params->add_param_override("event_manager", "multithread");
    }
  }

  double timescale = params->get_optional_time_param("timestamp_resolution", 1e-12);
  int ps_per_tick = round(timescale/1e-12) + 0.02;
  timestamp::init_stamps(ps_per_tick);
}

}

void
resize_topology(int max_nproc, sprockit::sim_parameters *params, bool verbose)
{
  sprockit::sim_parameters* top_params = params->get_namespace("topology");
  if (top_params->has_param("geometry") || top_params->get_param("name") != "torus"){
    return; //don't need this
  }

  //create a topology matching nproc
  int x, y, z;
  gen_cart_grid(max_nproc, x, y, z);
  std::string paramval = sprockit::printf("%d %d %d", x, y, z);
  params->add_param("topology.geometry", paramval);
  if (verbose)
    cout0 << sprockit::printf("Using auto-generated geometry [%d %d %d] for nproc=%d\n", x, y, z, max_nproc);
}

void
map_env_params(sprockit::sim_parameters* params)
{
  //read environmental variables as potential overrides
  char* param = getenv("MPICH_GNI_MAX_VSHORT_MSG_SIZE");
  if (param) {
    params->add_param_override("max_vshort_msg_size", param);
  }

  param = getenv("MPICH_GNI_MAX_EAGER_MSG_SIZE");
  if (param) {
    params->add_param_override("max_eager_msg_size", param);
  }

  param = getenv("MPICH_SMP_SINGLE_COPY_SIZE");
  if (param) {
    params->add_param_override("smp_single_copy_size", param);
  }

  /** set to an absurdly high value - no single copies */
  param = getenv("MPICH_SMP_SINGLE_COPY_OFF");
  if (param) {
    params->add_param_override("smp_single_copy_size", "99999999999");
  }
}