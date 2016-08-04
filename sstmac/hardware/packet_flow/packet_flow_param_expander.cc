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

  nic_params->add_param_override("model", "packet_flow");
  params->add_param_override("interconnect", "switch");
  switch_params->add_param_override("model", "packet_flow");
  if (!mem_params->has_scoped_param("model")){
      mem_params->add_param_override("model", "packet_flow");
  }

  buffer_depth_ = params->get_optional_int_param("network_buffer_depth", 8);

  //by default, quite coarse-grained
  int packet_size = params->get_optional_int_param("accuracy_parameter", 4096);
  int net_packet_size = params->get_optional_int_param("network_accuracy_parameter", packet_size);
  int mem_packet_size = params->get_optional_int_param("memory_accuracy_parameter", packet_size);

  mem_params->add_param_override("mtu", mem_packet_size);
  switch_params->add_param_override("mtu", net_packet_size);
  nic_params->add_param_override("mtu", net_packet_size);

  if (amm_type == "amm1"){
    expand_amm1_memory(params, mem_params);
    expand_amm1_network(params, switch_params, net_packet_size, true/*set xbar*/);
    expand_amm1_nic(params, nic_params);
    top_params->add_param_override("netlink_endpoints", "false");
  }
  else if (amm_type == "amm2"){
    expand_amm2_memory(params, mem_params);
    expand_amm1_network(params, switch_params, net_packet_size, true/*set xbar*/);
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

}

void
packet_flow_param_expander::expand_amm1_memory(sprockit::sim_parameters* params,
                                               sprockit::sim_parameters* mem_params)
{
  if (mem_params->get_scoped_param("model") != "null"){
    mem_params->add_param_override("total_bandwidth", mem_params->get_param("bandwidth"));
    mem_params->add_param_override("max_single_bandwidth", mem_params->get_param("bandwidth"));
  }
}

void
packet_flow_param_expander::expand_amm1_network(sprockit::sim_parameters* params,
                                                sprockit::sim_parameters* switch_params,
                                                int packet_size, bool set_xbar)
{


  //JJW - no, don't do this
  //The link banwidthds will get multiplied during the connect
  //if redundant links, appropriately multiply the bandwidth
  //double bw_multiplier = network_bandwidth_multiplier(params);
  //double link_bw = switch_params->get_bandwidth_param("link_bandwidth");
  //if (bw_multiplier > 1.0001){
  //  link_bw *= bw_multiplier;
  //  switch_params->add_param_override("link_bandwidth", link_bw);
  //}

  //make the xbar much faster than links
  if (set_xbar){
    double link_bw = switch_params->get_bandwidth_param("link_bandwidth");
    double xbar_bw = link_bw * buffer_depth_;
    switch_params->add_param_override("crossbar_bandwidth", xbar_bw);
  }


  int size_multiplier = switch_buffer_multiplier(params);
  int buffer_size = buffer_depth_ * packet_size * size_multiplier;
  if (!switch_params->has_param("input_buffer_size")){
    switch_params->add_param_override("input_buffer_size", buffer_size);
  }
  if (!switch_params->has_param("output_buffer_size")){
    switch_params->add_param_override("output_buffer_size", buffer_size);
  }



  if (!switch_params->has_param("ejection_bandwidth")){
    sprockit::sim_parameters* nic_params = params->get_namespace("nic");
    if (nic_params->has_param("ejection_bandwidth")){
      switch_params->add_param_override("ejection_bandwidth",
                                nic_params->get_param("ejection_bandwidth"));
    } else {
      switch_params->add_param_override("ejection_bandwidth",
                                nic_params->get_param("injection_bandwidth"));
    }
  }

}

void
packet_flow_param_expander::expand_amm1_nic(sprockit::sim_parameters* params,
                                            sprockit::sim_parameters* nic_params)
{
  //nothing doing here
}

void
packet_flow_param_expander::expand_amm2_memory(sprockit::sim_parameters* params,
                                               sprockit::sim_parameters* mem_params)
{
  expand_amm1_memory(params, mem_params);
  if (mem_params->get_scoped_param("model") != "null"){
    mem_params->add_param_override("max_single_bandwidth", params->get_param("max_memory_bandwidth"));
  }
}

void
packet_flow_param_expander::expand_amm3_network(sprockit::sim_parameters* params,
                                                sprockit::sim_parameters* switch_params,
                                                int packet_size)
{
  expand_amm1_network(params, switch_params, packet_size, false);

  double sw_bw = switch_params->get_bandwidth_param("crossbar_bandwidth");
  double bw_multiplier = switch_bandwidth_multiplier(params);
  if (bw_multiplier > 1.0001){
    double xbar_bw = sw_bw * bw_multiplier;
    switch_params->add_param_override("crossbar_bandwidth", xbar_bw);
  }
}

void
packet_flow_param_expander::expand_amm4_network(sprockit::sim_parameters* params,
  sprockit::sim_parameters* top_params,
  sprockit::sim_parameters* switch_params,
  int packet_size)
{
  tiled_switch_ = true;
  std::string top = top_params->get_param("name");
  std::string newtop = std::string("tiled_") + top;
  std::vector<int> switch_geom; switch_params->get_vector_param("geometry", switch_geom);
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

  if (switch_params->has_param("router")){
    std::string router = switch_params->get_param("router");
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
packet_flow_param_expander::expand_amm4_nic(sprockit::sim_parameters* params,
                                            sprockit::sim_parameters* top_params,
                                            sprockit::sim_parameters* nic_params)
{
  expand_amm1_nic(params, nic_params);
  sprockit::sim_parameters* netlink_params = params->get_optional_namespace("netlink");
  int red = top_params->get_optional_int_param("injection_redundant", 1);
  int radix = params->get_optional_int_param("netlink_radix", 1);
  //the netlink block combines all the paths together
  netlink_params->add_param_override("ninject", red);
  netlink_params->add_param_override("neject", radix);
  netlink_params->add_param_override("model", "packet_flow");
  top_params->add_param_override("netlink_radix", radix);
}

}
}
