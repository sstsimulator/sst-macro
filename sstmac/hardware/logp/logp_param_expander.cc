/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

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

#include <sstmac/hardware/logp/logp_param_expander.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

void
LogPParamExpander::expand(sprockit::sim_parameters::ptr& params)
{
  sprockit::sim_parameters::ptr node_params = params->get_optional_namespace("node");
  sprockit::sim_parameters::ptr  nic_params = node_params->get_optional_namespace("nic");
  sprockit::sim_parameters::ptr  mem_params = node_params->get_optional_namespace("memory");
  sprockit::sim_parameters::ptr  switch_params = params->get_optional_namespace("switch");
  sprockit::sim_parameters::ptr  top_params = params->get_optional_namespace("topology");
  sprockit::sim_parameters::ptr  proc_params = node_params->get_optional_namespace("proc");

  nic_params->add_param_override("name", "logp");
  switch_params->add_param_override("name", "logp");
  mem_params->add_param_override("name", "pisces");

  int packet_size = params->get_optional_int_param("accurary_parameter", 4096000);
  int mem_packet_size = params->get_optional_int_param("memory_accuracy_parameter", packet_size);
  if (!mem_params->has_param("mtu")){
    mem_params->add_param_override("mtu", mem_packet_size);
  }

  std::string amm_type = params->get_param("amm_model");
  if (amm_type == "amm1"){
    expandAmm1Memory(params, mem_params);
    expandAmm1Network(params, switch_params);
    expandAmm1Nic(params, nic_params, switch_params);
  } else if (amm_type == "amm2"){
    expandAmm2Memory(params, mem_params);
    expandAmm1Network(params, switch_params);
    expandAmm1Nic(params, nic_params, switch_params);
  } else if (amm_type == "amm3"){
    expandAmm2Memory(params, mem_params);
    expandAmm3Network(params, switch_params);
    expandAmm1Nic(params, nic_params, switch_params);
  } else if (amm_type == "amm4"){
    expandAmm4Nic(params, nic_params, switch_params);
  } else {
    spkt_throw_printf(sprockit::input_error, "invalid hardware model %s given",
        amm_type.c_str());
  }
}

void
LogPParamExpander::expandAmm1Memory(
  sprockit::sim_parameters::ptr& params,
  sprockit::sim_parameters::ptr& mem_params)
{
  //now just get the strings
  std::string mem_bw_str = mem_params->get_param("bandwidth");
  if (!mem_params->has_param("max_single_bandwidth")){
    mem_params->add_param_override("max_single_bandwidth", mem_bw_str);
  } 
  if (!mem_params->has_param("total_bandwidth")){
    mem_params->add_param_override("total_bandwidth", mem_bw_str);
  }
}

void
LogPParamExpander::expandAmm1Network(
  sprockit::sim_parameters::ptr& params,
  sprockit::sim_parameters::ptr& switch_params)
{
  expandInto(switch_params, params, switch_params);
}

void
LogPParamExpander::expandInto(
  sprockit::sim_parameters::ptr& dst_params,
  sprockit::sim_parameters::ptr& params,
  sprockit::sim_parameters::ptr& switch_params)
{
  if (!switch_params->has_param("bandwidth")){
    sprockit::sim_parameters::ptr  link_params = switch_params->get_namespace("link");
    double link_bw = link_params->get_bandwidth_param("bandwidth");
    double gbs = link_bw *ParamExpander::networkBandwidthMultiplier(params) / 1e9;
    std::string net_bw_str = sprockit::printf("%12.8fGB/s", gbs);

    dst_params->add_param_override("bandwidth", net_bw_str);
  } else {
    dst_params->add_param_override("bandwidth", switch_params->get_param("bandwidth"));
  }

  if (!switch_params->has_param("hop_latency")){
    sprockit::sim_parameters::ptr  link_params = switch_params->get_optional_namespace("link");
    if (link_params->has_param("sendLatency")){
      dst_params->add_param_override("hop_latency", link_params->get_time_param("sendLatency"));
    } else {
      dst_params->add_param_override("hop_latency", link_params->get_time_param("latency"));
    }
  } else {
    dst_params->add_param_override("hop_latency", switch_params->get_param("hop_latency"));
  }

  if (!switch_params->has_param("out_in_latency")){
    sprockit::sim_parameters::ptr  inj_params = params->get_namespace("node")->get_namespace("nic")
                                                 ->get_namespace("injection");

    sprockit::sim_parameters::ptr  ej_params = switch_params->get_optional_namespace("ejection");

    Timestamp inj_lat = inj_params->get_time_param("latency");
    Timestamp ej_lat = inj_lat;

    if (ej_params->has_param("sendLatency")){
      ej_lat = ej_params->get_time_param("sendLatency");
    } else if (ej_params->has_param("latency")){
      ej_lat = ej_params->get_time_param("latency");
    }

    Timestamp total_lat = inj_lat + ej_lat;
    dst_params->add_param_override("out_in_latency", sprockit::printf("%12.8fus", total_lat.usec()));
  } else {
    dst_params->add_param_override("out_in_latency", switch_params->get_param("out_in_latency"));
  }

}

void
LogPParamExpander::expandAmm1Nic(
 sprockit::sim_parameters::ptr& params,
 sprockit::sim_parameters::ptr& nic_params,
 sprockit::sim_parameters::ptr& switch_params)
{
}

void
LogPParamExpander::expandAmm2Memory(
 sprockit::sim_parameters::ptr& params,
 sprockit::sim_parameters::ptr& mem_params)
{
  expandAmm1Memory(params, mem_params);
}

void
LogPParamExpander::expandAmm3Network(
  sprockit::sim_parameters::ptr& params,
  sprockit::sim_parameters::ptr& switch_params)
{
  expandAmm1Network(params, switch_params);

  sprockit::sim_parameters::ptr link_params = switch_params->get_namespace("link");
  sprockit::sim_parameters::ptr xbar_params = switch_params->get_namespace("xbar");
  double link_bw = link_params->get_bandwidth_param("bandwidth");
  double sw_multiplier = ParamExpander::switchBandwidthMultiplier(params);
  double sw_bw = xbar_params->get_bandwidth_param("bandwidth") * sw_multiplier;
  //the network bandwidth is the min of link/sw bandwidth
  double net_bw = std::min(link_bw, sw_bw);
  double gbs = net_bw / 1e9;
  std::string net_bw_str = sprockit::printf("%12.8fGB/s", gbs);
  link_params->add_param_override("bandwidth", net_bw_str);
}

void
LogPParamExpander::expandAmm4Nic(
  sprockit::sim_parameters::ptr& params,
  sprockit::sim_parameters::ptr& nic_params,
  sprockit::sim_parameters::ptr& switch_params)
{
  sprockit::abort("simple is not currently compatible with NIC model in abstract machine model amm4 -"
    "only a single injection pathway is used on the NIC, not distinct paths for RDMA and UDP sends");
}

}
}
