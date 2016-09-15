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
#include <sstmac/software/launch/app_launch.h>
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
  pr("router", "switch.router"),
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
  pr("node_name", "node.model"),
  pr("node_mem_latency", "node.memory.latency"),
  pr("node_mem_bandwidth", "node.memory.bandwidth"),
  pr("nic_immediate_nack", "nic.immediate_nack"),
  pr("nic_negligible_size", "nic.negligible_size"),
  pr("nic_name", "nic.model"),
  pr("node_memory_model", "node.memory.model"),
  pr("node_frequency", "node.proc.frequency"),
  pr("network_bandwidth_link", "switch.link.bandwidth"),
  pr("network_link_bandwidth", "switch.link.bandwidth"),
  pr("network_bandwidth", "switch.link.bandwidth", false),
  pr("network_bandwidth", "switch.xbar.bandwidth"),
  pr("network_switch_bandwidth", "switch.xbar.bandwidth"),
  pr("network_latency", "switch.link.latency"),
  pr("network_hop_latency", "switch.link.latency"),
  pr("network_switch_type", "switch.model"),
  pr("packet_flow_memory_bandwidth", "node.memory.total_bandwidth"),
  pr("packet_flow_memory_single_bandwidth", "node.memory.max_single_bandwidth"),
  pr("packet_flow_memory_latency", "node.memory.latency"),
  pr("packet_flow_memory_arbitrator", "node.memory.arbitrator"),
  pr("packet_flow_memory_mtu", "node.memory.mtu"),
  pr("packet_flow_injection_bandwidth", "switch.ejection_bandwidth", false),
  pr("packet_flow_injection_bandwidth", "nic.injection.bandwidth"),
  pr("packet_flow_injection_latency", "nic.injection.latency"),
  pr("packet_flow_eject_buffer_size", "nic.eject_buffer_size"),
  pr("packet_flow_network_link_bandwidth", "switch.link.bandwidth"),
  pr("packet_flow_network_hop_latency", "switch.link.latency"),
  pr("packet_flow_switch_output_buffer_size", "switch.output_buffer_size"),
  pr("packet_flow_switch_crossbar_bandwidth", "switch.xbar.bandwidth"),
  pr("packet_flow_switch_crossbar_latency", "switch.xbar.latency"),
  pr("packet_flow_switch_input_buffer_size", "switch.input_buffer_size", false),
  pr("packet_flow_switch_input_buffer_size", "nic.injection.credits"),
  pr("packet_flow_arbitrator", "nic.arbitrator", false),
  pr("packet_flow_arbitrator", "switch.arbitrator"),
  pr("packet_flow_mtu", "switch.mtu", false),
  pr("packet_flow_mtu", "nic.mtu"),
  pr("packet_flow_negligible_size", "switch.negligible_size"),
  pr("router", "switch.router"),
  pr("sanity_check_queue_depth_reporting", "switch.sanity_check_queue_depth_reporting"),
  pr("sanity_check_queue_depth_delta", "switch.sanity_check_queue_depth_delta"),
  pr("node_preemption", "node.preemption"),
  pr("node_cores", "node.ncores"),
  pr("node_sockets", "node.nsockets"),
  pr("node_model", "node.model"),
  pr("negligible_compute_time", "node.negligible_compute_time"),
  pr("node_pipeline_speedup", "node.proc.parallelism"),
  pr("smp_single_copy_size", "mpi.smp_single_copy_size"),
  pr("max_eager_msg_size", "mpi.max_eager_msg_size"),
  pr("max_vshort_msg_size", "mpi.max_vshort_msg_size"),
  pr("mpi_implementation", "mpi.queue.implementation"),
  pr("mpi_queue_post_rdma_delay", "mpi.queue.post_rdma_delay"),
  pr("mpi_queue_post_header_delay", "mpi.queue.post_header_delay"),
  pr("mpi_queue_poll_delay", "mpi.queue.poll_delay"),
  pr("mpi_spyplot", "mpi.queue.traffic_matrix.fileroot"),
  pr("network_spyplot", "nic.traffic_matrix.fileroot"),
  pr("ftq", "node.os.ftq.fileroot"),
  pr("ftq_epoch", "node.os.ftq.epoch"),
  pr("call_graph", "node.os.call_graph.fileroot"),
  pr("stack_size", "node.os.stack_size"),
  pr("stack_chunk_size", "node.os.stack_chunk_size"),
  pr("stack_protect", "node.os.stack_protect"),
  pr("injection_redundant", "nic.injection.redundant", false),
  pr("injection_redundant", "switch.ejection.redundant", false),
  pr("injection_latency", "nic.injection.latency", false),
  pr("injection_bandwidth", "nic.injection.bandwidth", false),
  pr("injection_latency", "switch.ejection.latency"),
  pr("injection_bandwidth", "switch.ejection.bandwidth"),
  pr("intragroup_connection_file", "topology.intragroup_connection_file", false),
  pr("intergroup_connection_file", "topology.intergroup_connection_file"),
  pr("launch_app1", "app1.name"),
  pr("launch_app1_start", "app1.start"),
  pr("launch_allocation", "app1.launch_allocation"),
  pr("launch_indexing", "app1.launch_indexing"),
  pr("launch_app1_cmd", "app1.launch_cmd"),
  pr("launch_app1_type", "app1.launch_type"),
  pr("launch_app1_argv", "app1.argv"),
  pr("launch_app1_size", "app1.size"),
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
    params->print_params(std::cerr);
    spkt_throw(sprockit::value_error,
               "computed max nproc=0 from parameters - need app1.launch_cmd or app1.size");
  }
  resize_topology(max_nproc, params, verbose);

  //here is where we might need to build supplemental params
  if (params->has_param("congestion_model")){
    if (!params->has_param("amm_model")){
      spkt_throw(sprockit::input_error, "require an abstract machine model via amm_model parameter");
    }
    sstmac::param_expander* hw_expander = sstmac::param_expander_factory::get_param("congestion_model", params);
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

  int timescale = params->get_optional_int_param("timestamp_resolution", 1);
  timestamp::init_stamps(timescale);
}

}

void
resize_topology(int max_nproc, sprockit::sim_parameters *params, bool verbose)
{
  sprockit::sim_parameters* top_params = params->get_namespace("topology");
  if (top_params->has_param("geometry") || top_params->get_param("name") != "hdtorus"){
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

