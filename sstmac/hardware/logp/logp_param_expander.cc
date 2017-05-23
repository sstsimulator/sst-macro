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

#include <sstmac/hardware/logp/logp_param_expander.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

void
logp_param_expander::expand(sprockit::sim_parameters* params)
{
  sprockit::sim_parameters* node_params = params->get_optional_namespace("node");
  sprockit::sim_parameters* nic_params = node_params->get_optional_namespace("nic");
  sprockit::sim_parameters* mem_params = node_params->get_optional_namespace("memory");
  sprockit::sim_parameters* switch_params = params->get_optional_namespace("switch");
  sprockit::sim_parameters* top_params = params->get_optional_namespace("topology");
  sprockit::sim_parameters* proc_params = node_params->get_optional_namespace("proc");

  nic_params->add_param_override("model", "logP");
  params->add_param_override("interconnect", "simple");
  switch_params->add_param_override("model", "logP");
  mem_params->add_param_override("model", "pisces");

  int packet_size = params->get_optional_int_param("accurary_parameter", 4096);
  int mem_packet_size = params->get_optional_int_param("memory_accuracy_parameter", packet_size);
  if (!mem_params->has_param("mtu")){
    mem_params->add_param_override("mtu", mem_packet_size);
  }

  std::string amm_type = params->get_param("amm_model");
  if (amm_type == "amm1"){
    expand_amm1_memory(params, mem_params);
    expand_amm1_network(params, switch_params);
    expand_amm1_nic(params, nic_params, switch_params);
  }
  else if (amm_type == "amm2"){
    expand_amm2_memory(params, mem_params);
    expand_amm1_network(params, switch_params);
    expand_amm1_nic(params, nic_params, switch_params);
  }
  else if (amm_type == "amm3"){
    expand_amm2_memory(params, mem_params);
    expand_amm3_network(params, switch_params);
    expand_amm1_nic(params, nic_params, switch_params);
  }
  else if (amm_type == "amm4"){
    expand_amm4_nic(params, nic_params, switch_params);
  }
  else {
    spkt_throw_printf(sprockit::input_error, "invalid hardware model %s given",
        amm_type.c_str());
  }
}

void
logp_param_expander::expand_amm1_memory(
  sprockit::sim_parameters* params,
  sprockit::sim_parameters* mem_params)
{
  //now just get the strings
  std::string mem_bw_str = mem_params->get_param("bandwidth");
  mem_params->add_param_override("max_single_bandwidth", mem_bw_str);
  mem_params->add_param_override("total_bandwidth", mem_bw_str);
}

void
logp_param_expander::expand_amm1_network(
  sprockit::sim_parameters* params,
  sprockit::sim_parameters* switch_params)
{
  sprockit::sim_parameters* link_params = switch_params->get_namespace("link");
  double link_bw = link_params->get_bandwidth_param("bandwidth");
  double gbs = link_bw *param_expander::network_bandwidth_multiplier(params) / 1e9;
  std::string net_bw_str = sprockit::printf("%12.8fGB/s", gbs);
  link_params->add_param_override("bandwidth", net_bw_str);
}

void
logp_param_expander::expand_amm1_nic(
 sprockit::sim_parameters* params,
 sprockit::sim_parameters* nic_params,
 sprockit::sim_parameters* switch_params)
{
  sprockit::sim_parameters* ej_params = switch_params->get_optional_namespace("ejection");
  sprockit::sim_parameters* inj_params = nic_params->get_optional_namespace("injection");
  if (!ej_params->has_param("bandwidth")){
    ej_params->add_param_override("bandwidth", inj_params->get_param("bandwidth"));
  }
  if (!ej_params->has_param("latency")){
    ej_params->add_param_override("latency", inj_params->get_param("latency"));
  }
}

void
logp_param_expander::expand_amm2_memory(
 sprockit::sim_parameters* params,
 sprockit::sim_parameters* mem_params)
{
  expand_amm1_memory(params, mem_params);
}

void
logp_param_expander::expand_amm3_network(
  sprockit::sim_parameters* params,
  sprockit::sim_parameters* switch_params)
{
  expand_amm1_network(params, switch_params);

  sprockit::sim_parameters* link_params = switch_params->get_namespace("link");
  sprockit::sim_parameters* xbar_params = switch_params->get_namespace("xbar");
  double link_bw = link_params->get_bandwidth_param("bandwidth");
  double sw_multiplier = param_expander::switch_bandwidth_multiplier(params);
  double sw_bw = xbar_params->get_bandwidth_param("bandwidth") * sw_multiplier;
  //the network bandwidth is the min of link/sw bandwidth
  double net_bw = std::min(link_bw, sw_bw);
  double gbs = net_bw / 1e9;
  std::string net_bw_str = sprockit::printf("%12.8fGB/s", gbs);
  link_params->add_param_override("bandwidth", net_bw_str);
}

void
logp_param_expander::expand_amm4_nic(
  sprockit::sim_parameters* params,
  sprockit::sim_parameters* nic_params,
  sprockit::sim_parameters* switch_params)
{
  spkt_throw(sprockit::unimplemented_error,
    "simple is not currently compatible with NIC model in abstract machine model amm4 -"
    "only a single injection pathway is used on the NIC, not distinct paths for RDMA and UDP sends");
}

}
}