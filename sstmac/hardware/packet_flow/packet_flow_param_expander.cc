#include <sstmac/hardware/packet_flow/packet_flow_param_expander.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("packet_flow", sprockit::param_expander, packet_flow_param_expander);

void
packet_flow_param_expander::expand(sprockit::sim_parameters* params)
{
  //this is a switch network
  params->add_param("network_name", "switch");
  params->add_param("nic_name", "packet_flow");
  params->add_param("node_memory_model", "packet_flow");
  params->add_param("network_switch_type", "packet_flow");

  int red = params->get_optional_int_param("injection_redundant", 1);
  params->add_param_override("topology.injection_redundant", red);

  //scale up the injection bandwidth by the redundancy
  double inj_bw = params->get_bandwidth_param("injection_bandwidth");
  double new_inj_bw = nic_bandwidth_multiplier(params) * inj_bw;
  double kiviat_scale = params->get_optional_double_param("scale_injection_bandwidth", 1.0);
  std::string bw_str = sprockit::printf("%25.10fB/s", new_inj_bw*kiviat_scale);
  params->add_param_override("injection_bandwidth", bw_str);

  if (params->has_param("scale_injection_latency")){
    timestamp inj_lat = params->get_time_param("injection_latency");
    double kiviat_scale = params->get_optional_double_param("scale_injection_latency", 1.0);
    double new_lat_us = inj_lat.usec() * kiviat_scale;
    std::string bw_str = sprockit::printf("%12.8fus", new_lat_us);
    params->add_param_override("injection_latency", new_lat_us);
  }

  buffer_depth_ = params->get_optional_int_param("network_buffer_depth", 8);

  std::string amm_type = params->get_param("amm_model");
  //by default, quite coarse-grained
  int packet_size = params->get_optional_int_param("accuracy_parameter", 4096);
  int net_packet_size = params->get_optional_int_param("network_accuracy_parameter", packet_size);
  int mem_packet_size = params->get_optional_int_param("memory_accuracy_parameter", packet_size);

  params->add_param_override("packet_flow_mtu", net_packet_size);
  params->add_param_override("packet_flow_memory_mtu", mem_packet_size);

  if (params->has_param("arbitrator")){
    params->add_param_override("packet_flow_arbitrator", params->get_param("arbitrator"));
  }

  tiled_switch_ = false; //default

  if (amm_type == "amm1"){
    expand_amm1(params, net_packet_size, mem_packet_size);
  }
  else if (amm_type == "amm2"){
    expand_amm2(params, net_packet_size, mem_packet_size);
  }
  else if (amm_type == "amm3"){
    expand_amm3(params, net_packet_size, mem_packet_size);
  }
  else if (amm_type == "amm4"){
    expand_amm4(params, net_packet_size, mem_packet_size);
  }
  else {
    spkt_throw_printf(sprockit::input_error, "invalid hardware model %s given",
        amm_type.c_str());
  }

  //erase all the top-level params so we don't have replica parameters
  params->remove_param("network_bandwidth");
  params->remove_param("network_switch_bandwidth");
  params->remove_param("network_hop_latency");
  params->remove_param("memory_bandwidth");
  params->remove_param("memory_latency");
  params->remove_param("max_memory_banwidth");
  params->remove_param("accuracy_parameter");
  params->remove_param("amm_model");

}

void
packet_flow_param_expander::expand_amm1(sprockit::sim_parameters* params, int net_packet_size, int mem_packet_size)
{
  expand_amm1_memory(params, mem_packet_size);
  expand_amm1_network(params, net_packet_size);
  expand_amm1_nic(params, net_packet_size);
}

void
packet_flow_param_expander::expand_amm2(sprockit::sim_parameters* params, int net_packet_size, int mem_packet_size)
{
  expand_amm2_memory(params, mem_packet_size);
  expand_amm1_network(params, net_packet_size);
  expand_amm1_nic(params, net_packet_size);
}

void
packet_flow_param_expander::expand_amm3(sprockit::sim_parameters* params, int net_packet_size, int mem_packet_size)
{
  expand_amm2_memory(params, mem_packet_size);
  expand_amm3_network(params, net_packet_size);
  expand_amm1_nic(params, net_packet_size);
}

void
packet_flow_param_expander::expand_amm4(sprockit::sim_parameters* params, int net_packet_size, int mem_packet_size)
{
  expand_amm2_memory(params, mem_packet_size);
  expand_amm4_network(params, net_packet_size);
  expand_amm4_nic(params, net_packet_size);
}

void
packet_flow_param_expander::expand_amm1_memory(sprockit::sim_parameters* params, int packet_size)
{
  //verify we have valid timstamp and bandwidth params
  double ignore_mem_bw = params->get_bandwidth_param("memory_bandwidth");
  timestamp ignore_mem_lat = params->get_time_param("memory_latency");
  //now just get the strings
  std::string mem_lat_str = params->get_param("memory_latency");
  std::string mem_bw_str = params->get_param("memory_bandwidth");

  params->add_param("packet_flow_memory_latency", mem_lat_str);
  params->add_param("packet_flow_memory_single_bandwidth", mem_bw_str);
  params->add_param("packet_flow_memory_bandwidth", mem_bw_str);
}

void
packet_flow_param_expander::expand_amm1_network(sprockit::sim_parameters* params, int packet_size)
{
  //here is where the enormous complexity lies - how do we pick the number of virtual channels

  //verify correct formatting and then add params
  params->get_bandwidth_param("network_bandwidth");
  if (params->has_param("scale_network_bandwidth")){
    double kiviat_scale = params->get_double_param("scale_network_bandwidth");
    double new_bw = params->get_bandwidth_param("network_bandwidth") * kiviat_scale;
    params->add_param_override("network_bandwidth", sprockit::printf("%25.10fB/s", new_bw));
  } 
  params->add_param("packet_flow_network_link_bandwidth", params->get_param("network_bandwidth"));

  //if redundant links, appropriately multiply the bandwidth
  double bw_multiplier = network_bandwidth_multiplier(params);
  double xbar_bw = params->get_bandwidth_param("network_bandwidth") * bw_multiplier;
  params->add_param("packet_flow_switch_crossbar_bandwidth", sprockit::printf("%25.10fB/s", xbar_bw));

  //figure out buffer sizes - make them 8 packets deep for now if not specified
  int buffer_size = buffer_depth_ * packet_size;
  const char* inparam = "packet_flow_switch_input_buffer_size";
  sprockit::sim_parameters* switch_params = params->get_optional_namespace("switch");
  if (!params->has_param(inparam) && !switch_params->has_param("input_buffer_size")){
    int size_multiplier = switch_buffer_multiplier(params);
    std::string buffer_size_str = sprockit::printf("%dB", buffer_size*size_multiplier);
    params->add_param(inparam, buffer_size_str);
  }
  const char* outparam = "packet_flow_switch_output_buffer_size";
  if (!params->has_param(outparam) && !switch_params->has_param("output_buffer_size")){
    std::string buffer_size_str = sprockit::printf("%dB", buffer_size);
    params->add_param(outparam, buffer_size_str);
  }

  timestamp hop_lat = params->get_time_param("network_hop_latency");
  double kiviat_scale = params->get_optional_double_param("scale_hop_latency", 1.0);
  std::string hop_lat_str = sprockit::printf("%8.4fns", hop_lat.nsec()*kiviat_scale);
  params->add_param("packet_flow_network_hop_latency", hop_lat_str);
}

void
packet_flow_param_expander::expand_amm1_nic(sprockit::sim_parameters* params, int packet_size)
{
  //verify formatting
  params->get_bandwidth_param("injection_bandwidth");
  params->get_time_param("injection_latency");
  //redo the strings
  std::string inj_bw_str = params->get_param("injection_bandwidth");
  std::string inj_lat_str = params->get_param("injection_latency");
  params->add_param("packet_flow_injection_bandwidth", inj_bw_str);
  params->add_param("packet_flow_injection_latency", inj_lat_str);

  params->remove_param("injection_latency");
  params->remove_param("injection_bandwidth");

  //JJW - for now use really, really large buffer size
  //either hold 4KB or 8 packets - whichever is larger
  //int buffer_size = std::max(int(4096), packet_size * 8);
  //std::string buffer_size_str = sprockit::printf("%dB", buffer_size);
  //params->add_param("packet_flow_eject_buffer_size", buffer_size_str);
}

void
packet_flow_param_expander::expand_amm2_memory(sprockit::sim_parameters* params, int packet_size)
{
  expand_amm1_memory(params, packet_size);
  //verify
  params->get_bandwidth_param("max_memory_bandwidth");
  std::string max_bw_str = params->get_param("max_memory_bandwidth");
  params->add_param_override("packet_flow_memory_single_bandwidth", max_bw_str);
}

void
packet_flow_param_expander::expand_amm3_network(sprockit::sim_parameters* params, int packet_size)
{
  expand_amm1_network(params, packet_size);
  //verify

  double sw_bw = params->get_bandwidth_param("network_switch_bandwidth");
  double bw_multiplier = switch_bandwidth_multiplier(params);
  double xbar_bw = sw_bw * bw_multiplier;
  std::string sw_bw_str = sprockit::printf("%25.10fB/s", xbar_bw);
  params->add_param_override("packet_flow_switch_crossbar_bandwidth", sw_bw_str);
}

void
packet_flow_param_expander::expand_amm4_network(sprockit::sim_parameters* params, int packet_size)
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
  params->add_param_override("topology.tiles_per_row", nrows);
  params->add_param_override("topology.tiles_per_col", ncols);

  params->add_param_override("topology_name", newtop);
  params->add_param_override("network_switch_type", "packet_flow_tiled");

  if (params->has_param("router")){
    std::string router = params->get_param("router");
    std::string new_router = router + "_multipath";
    params->add_param_override("router", new_router);
  } else {
    spkt_throw_printf(sprockit::value_error,
      "if using amm4, must specify router = X\n"
      "valid options are minimal, ugal, valiant, min_ad)");
  }


  int buffer_size = buffer_depth_ * packet_size;
  std::string buffer_size_str = sprockit::printf("%dB", buffer_size);
  params->add_param_override("switch.row_buffer_size", buffer_size_str);
  params->add_param_override("switch.nrows", nrows);
  params->add_param_override("switch.ncols", ncols);
  expand_amm3_network(params, packet_size);
}

void
packet_flow_param_expander::expand_amm4_nic(sprockit::sim_parameters* params, int packet_size)
{
  expand_amm1_nic(params, packet_size);
  int red = params->get_optional_int_param("injection_redundant", 1);
  int radix = params->get_optional_int_param("netlink_radix", 1);
  //the netlink block combines all the paths together
  params->add_param_override("netlink.ninject", red);
  params->add_param_override("netlink.neject", radix);
  params->add_param_override("topology.netlink_radix", radix);
  params->add_param_override("netlink.model", "packet_flow");
}

}
}
