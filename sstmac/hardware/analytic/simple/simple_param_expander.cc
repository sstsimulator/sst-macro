#include <sstmac/hardware/analytic/simple/simple_param_expander.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("simple", sprockit::param_expander, simple_param_expander);

void
simple_param_expander::expand(sprockit::sim_parameters* params)
{
  //this is a switch network
  params->add_param("network_name", "simple");
  params->add_param("nic_name", "simple");
  //this is basically a simple model - but really is needed for higher accuracy
  params->add_param("node_memory_model", "packet_flow");

  if (!params->has_param("nic_negligible_size")){
    params->add_param_override("nic_negligible_size", "512");
  }

  double inj_bw = params->get_bandwidth_param("injection_bandwidth");
  double new_inj_bw = nic_bandwidth_multiplier(params) * inj_bw;
  std::string bw_str = sprockit::printf("%25.10fB/s", new_inj_bw);
  params->add_param_override("injection_bandwidth", bw_str);

  std::string amm_type = params->get_param("amm_model");
  if (amm_type == "amm1"){
    expand_amm1(params);
  }
  else if (amm_type == "amm2"){
    expand_amm2(params);
  }
  else if (amm_type == "amm3"){
    expand_amm3(params);
  }
  else if (amm_type == "amm4"){
    expand_amm4(params);
  }
  else {
    spkt_throw_printf(sprockit::input_error, "invalid hardware model %s given",
        amm_type.c_str());
  }
}

void
simple_param_expander::expand_amm1(sprockit::sim_parameters* params)
{
  expand_amm1_memory(params);
  expand_amm1_network(params);
  expand_amm1_nic(params);
}

void
simple_param_expander::expand_amm2(sprockit::sim_parameters* params)
{
  expand_amm2_memory(params);
  expand_amm1_network(params);
  expand_amm1_nic(params);
}

void
simple_param_expander::expand_amm3(sprockit::sim_parameters* params)
{
  expand_amm2_memory(params);
  expand_amm3_network(params);
  expand_amm1_nic(params);
}

void
simple_param_expander::expand_amm4(sprockit::sim_parameters* params)
{
  expand_amm2_memory(params);
  expand_amm3_network(params);
  expand_amm4_nic(params);
}

void
simple_param_expander::expand_amm1_memory(sprockit::sim_parameters* params)
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
simple_param_expander::expand_amm1_network(sprockit::sim_parameters* params)
{
  double link_bw = params->get_bandwidth_param("network_bandwidth") * param_expander::network_bandwidth_multiplier(params);
  double gbs = link_bw / 1e9;
  double scale = params->get_optional_double_param("scale_network_bandwidth", 1.0);
  std::string net_bw_str = sprockit::printf("%12.8fGB/s", gbs*scale);
  params->add_param_override("network_bandwidth", net_bw_str);
}

void
simple_param_expander::expand_amm1_nic(sprockit::sim_parameters* params)
{
  //nothing to do here
}

void
simple_param_expander::expand_amm2_memory(sprockit::sim_parameters* params)
{
  expand_amm1_memory(params);
  //verify
  params->get_bandwidth_param("max_memory_bandwidth");
  std::string max_bw_str = params->get_param("max_memory_bandwidth");
  params->add_param_override("packet_flow_memory_single_bandwidth", max_bw_str);
}

void
simple_param_expander::expand_amm3_network(sprockit::sim_parameters* params)
{
  expand_amm1_network(params);
  double link_bw = params->get_bandwidth_param("network_bandwidth");
  double sw_multiplier = param_expander::switch_bandwidth_multiplier(params);
  double sw_bw = params->get_bandwidth_param("network_switch_bandwidth") * sw_multiplier;
  //the network bandwidth is the min of link/sw bandwidth
  double net_bw = std::min(link_bw, sw_bw);
  double gbs = net_bw / 1e9;
  std::string net_bw_str = sprockit::printf("%12.8fGB/s", gbs);
  params->add_param_override("network_bandwidth", net_bw_str);
}

void
simple_param_expander::expand_amm4_nic(sprockit::sim_parameters* params)
{
  spkt_throw(sprockit::unimplemented_error,
    "simple is not currently compatible with NIC model in abstract machine model amm4 -"
    "only a single injection pathway is used on the NIC, not distinct paths for RDMA and UDP sends");
}

}
}

