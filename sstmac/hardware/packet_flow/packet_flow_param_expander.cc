#include <sstmac/hardware/packet_flow/packet_flow_param_expander.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("packet_flow", sstmac::param_expander, packet_flow_param_expander);

void
packet_flow_param_expander::expand(sprockit::sim_parameters* params)
{
  std::string amm_type = params->get_param("amm_model");
  if (amm_type == "amm4"){
    tiled_switch_ = true;
  } else {
    tiled_switch_ = false;
  } 

  sprockit::sim_parameters* nic_params = params->get_optional_namespace("nic");
  sprockit::sim_parameters* node_params = params->get_optional_namespace("node");
  sprockit::sim_parameters* mem_params = node_params->get_optional_namespace("memory");
  sprockit::sim_parameters* switch_params = params->get_optional_namespace("switch");
  sprockit::sim_parameters* top_params = params->get_optional_namespace("topology");
  sprockit::sim_parameters* proc_params = node_params->get_optional_namespace("proc");

  node_params->add_param_override("model", params->get_param("node_name"));
  nic_params->add_param_override("model", "packet_flow");
  params->add_param_override("interconnect", "switch");
  switch_params->add_param_override("model", "packet_flow");
  mem_params->add_param_override("model", "packet_flow");

  if (!top_params->has_param("concentration")){
    int conc = params->get_optional_int_param("network_nodes_per_switch", 1);
    top_params->add_param_override("concentration", conc);
  }

  proc_params->add_param_override("frequency", params->get_param("node_frequency"));
  node_params->add_param_override("ncores", params->get_param("node_cores"));
  node_params->add_param_override("nsockets", params->get_optional_param("node_sockets", "1"));

  if (!top_params->has_param("name")){
    top_params->add_param_override("name", params->get_param("topology_name"));
  }
  if (!top_params->has_param("geometry")){
    top_params->add_param_override("geometry", params->get_param("topology_geometry"));
  }

  if (!top_params->has_param("topology.redundant")
    && params->has_param("topology_redundant")){
    top_params->add_param_override("redundant", params->get_param("topology_redundant"));
  }

  if (!top_params->has_param("group_connections") 
    && params->has_param("topology_group_connections")){
    top_params->add_param_override("group_connections",
                     params->get_param("topology_group_connections"));
  }

  int red = params->get_optional_int_param("injection_redundant", 1);
  top_params->add_param_override("injection_redundant", red);

  buffer_depth_ = params->get_optional_int_param("network_buffer_depth", 8);

  //by default, quite coarse-grained
  int packet_size = params->get_optional_int_param("accuracy_parameter", 4096);
  int net_packet_size = params->get_optional_int_param("network_accuracy_parameter", packet_size);
  int mem_packet_size = params->get_optional_int_param("memory_accuracy_parameter", packet_size);

  mem_params->add_param_override("mtu", mem_packet_size);
  switch_params->add_param_override("mtu", net_packet_size);

  if (amm_type == "amm1"){
    expand_amm1_memory(params, mem_params);
    expand_amm1_network(params, switch_params, net_packet_size);
    expand_amm1_nic(params, nic_params);
    top_params->add_param_override("netlink_endpoints", "false");
  }
  else if (amm_type == "amm2"){
    expand_amm2_memory(params, mem_params);
    expand_amm1_network(params, switch_params, net_packet_size);
    expand_amm1_nic(params, nic_params);
    top_params->add_param_override("netlink_endpoints", "false");
  }
  else if (amm_type == "amm3"){
    expand_amm2_memory(params, mem_params);
    expand_amm3_network(params, switch_params, net_packet_size);
    expand_amm1_nic(params, nic_params);
    top_params->add_param_override("netlink_endpoints", "false");
  }
  else if (amm_type == "amm4"){
    expand_amm2_memory(params, mem_params);
    expand_amm4_network(params, top_params, switch_params, net_packet_size);
    expand_amm4_nic(params, top_params, nic_params);
    top_params->add_param_override("netlink_endpoints", "true");
  }
  else {
    spkt_throw_printf(sprockit::input_error, "invalid hardware model %s given",
        amm_type.c_str());
  }

  nic_params->add_param_override("injection_credits",
                    switch_params->get_param("input_buffer_size"));

  //erase all the top-level params so we don't have replica parameters
  //params->remove_param("network_bandwidth");
  //params->remove_param("network_switch_bandwidth");
  //params->remove_param("network_hop_latency");
  //params->remove_param("memory_bandwidth");
  //params->remove_param("memory_latency");
  //params->remove_param("max_memory_banwidth");
  //params->remove_param("accuracy_parameter");
}

void
packet_flow_param_expander::expand_amm1_memory(sprockit::sim_parameters* params, sprockit::sim_parameters* mem_params) 
{
  //verify we have valid timstamp and bandwidth params
  mem_params->add_param_override("latency", params->get_param("memory_latency"));
  mem_params->add_param_override("total_bandwidth", params->get_param("memory_bandwidth"));
  mem_params->add_param_override("max_single_bandwidth", params->get_param("memory_bandwidth"));
}

void
packet_flow_param_expander::expand_amm1_network(sprockit::sim_parameters* params, sprockit::sim_parameters* switch_params, int packet_size)
{
  //here is where the enormous complexity lies - how do we pick the number of virtual channels

  //verify correct formatting and then add params

  //if redundant links, appropriately multiply the bandwidth
  double bw_multiplier = network_bandwidth_multiplier(params);
  if (bw_multiplier > 1.0001){
    double link_bw = params->get_bandwidth_param("network_bandwidth");
    double xbar_bw = link_bw * bw_multiplier;
    switch_params->add_param_override("link_bandwidth", link_bw);
    switch_params->add_param_override("crossbar_bandwidth", xbar_bw);
  } else {
    std::string bw = params->get_param("network_bandwidth");
    switch_params->add_param_override("link_bandwidth", bw);
    switch_params->add_param_override("crossbar_bandwidth", bw);
  }

  switch_params->add_param_override("hop_latency", params->get_param("network_hop_latency"));

  int size_multiplier = switch_buffer_multiplier(params);
  int buffer_size = buffer_depth_ * packet_size * size_multiplier;
  switch_params->add_param_override("input_buffer_size", buffer_size);
  switch_params->add_param_override("output_buffer_size", buffer_size);

  if (params->has_param("ejection_bandwidth")){
    switch_params->add_param_override("ejection_bandwidth", params->get_bandwidth_param("ejection_bandwidth"));
  } else {
    switch_params->add_param_override("ejection_bandwidth", params->get_param("injection_bandwidth"));
  }

}

void
packet_flow_param_expander::expand_amm1_nic(sprockit::sim_parameters* params, sprockit::sim_parameters* nic_params)
{
  //verify formatting
  nic_params->add_param_override("injection_bandwidth", params->get_param("injection_bandwidth"));
  nic_params->add_param_override("injection_latency", params->get_param("injection_latency"));

  if (params->has_param("ejection_bandwidth")){
    nic_params->add_param_override("ejection_bandwidth", params->get_param("ejection_bandwidth"));
  } 
}

void
packet_flow_param_expander::expand_amm2_memory(sprockit::sim_parameters* params, sprockit::sim_parameters* mem_params)
{
  expand_amm1_memory(params, mem_params);
  mem_params->add_param_override("max_single_bandwidth", params->get_param("max_memory_bandwidth"));
}

void
packet_flow_param_expander::expand_amm3_network(sprockit::sim_parameters* params, sprockit::sim_parameters* switch_params, int packet_size)
{
  expand_amm1_network(params, switch_params, packet_size);
  //verify

  double sw_bw = params->get_bandwidth_param("network_switch_bandwidth");
  double bw_multiplier = switch_bandwidth_multiplier(params);
  double xbar_bw = sw_bw * bw_multiplier;
  switch_params->add_param_override("crossbar_bandwidth", xbar_bw);
}

void
packet_flow_param_expander::expand_amm4_network(sprockit::sim_parameters* params,
  sprockit::sim_parameters* top_params,
  sprockit::sim_parameters* switch_params,
  int packet_size)
{
  tiled_switch_ = true;
  std::string top = params->get_param("topology_name");
  std::string newtop = std::string("tiled_") + top;
  std::vector<int> switch_geom; params->get_vector_param("switch_geometry", switch_geom);
  if (switch_geom.size() != 2){
    spkt_throw(sprockit::input_error,
      "AMM4: need switch geometry vector with 2 params:\n"
      "tiles-per-row, tiles-per-col");
  }
  int nrows = switch_geom[0];
  int ncols = switch_geom[1];
  top_params->add_param_override("tiles_per_row", nrows);
  top_params->add_param_override("tiles_per_col", ncols);
  top_params->add_param_override("name", newtop);

  switch_params->add_param_override("model", "packet_flow_tiled");
  switch_params->add_param_override("geometry", params->get_param("switch_geometry"));

  if (params->has_param("router")){
    std::string router = params->get_param("router");
    std::string new_router = router + "_multipath";
    switch_params->add_param_override("router", new_router);
  } else {
    spkt_throw_printf(sprockit::value_error,
      "if using amm4, must specify router = X\n"
      "valid options are minimal, ugal, valiant, min_ad)");
  }


  int buffer_size = buffer_depth_ * packet_size;
  switch_params->add_param_override("row_buffer_size", buffer_size);
  switch_params->add_param_override("nrows", nrows);
  switch_params->add_param_override("ncols", ncols);

  expand_amm3_network(params, switch_params, packet_size);
}

void
packet_flow_param_expander::expand_amm4_nic(sprockit::sim_parameters* params, sprockit::sim_parameters* top_params, sprockit::sim_parameters* nic_params)
{
  expand_amm1_nic(params, nic_params);
  sprockit::sim_parameters* netlink_params = params->get_optional_namespace("netlink");
  int red = params->get_optional_int_param("injection_redundant", 1);
  int radix = params->get_optional_int_param("netlink_radix", 1);
  //the netlink block combines all the paths together
  netlink_params->add_param_override("ninject", red);
  netlink_params->add_param_override("neject", radix);
  netlink_params->add_param_override("model", "packet_flow");
  top_params->add_param_override("netlink_radix", radix);
}

}
}
