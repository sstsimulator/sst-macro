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

#include <sstmac/hardware/pisces/pisces_param_expander.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"buffer_size",
);

namespace sstmac {
namespace hw {

void
pisces_param_expander::expand(sprockit::sim_parameters* params)
{
  std::string amm_type = params->get_param("amm_model");
  if (amm_type == "amm4"){
    tiled_switch_ = true;
  } else {
    tiled_switch_ = false;
  } 

  sprockit::sim_parameters* node_params = params->get_optional_namespace("node");
  sprockit::sim_parameters* nic_params = node_params->get_optional_namespace("nic");
  sprockit::sim_parameters* inj_params = nic_params->get_optional_namespace("injection");
  sprockit::sim_parameters* mem_params = node_params->get_optional_namespace("memory");
  sprockit::sim_parameters* switch_params = params->get_optional_namespace("switch");
  sprockit::sim_parameters* top_params = params->get_optional_namespace("topology");
  sprockit::sim_parameters* proc_params = node_params->get_optional_namespace("proc");
  sprockit::sim_parameters* netlink_params = params->get_optional_namespace("netlink");


  nic_params->add_param_override("model", "pisces");
  params->add_param_override("interconnect", "switch");
  switch_params->add_param_override("model", "pisces");
  if (!mem_params->has_scoped_param("model")){
      mem_params->add_param_override("model", "pisces");
  }

  buffer_depth_ = params->get_optional_int_param("network_buffer_depth", 8);
  //by default, quite coarse-grained


  std::string arb = params->get_optional_param("arbitrator", "cut_through");
  if (!switch_params->has_param("arbitrator")){
    switch_params->add_param("arbitrator", arb);
  }
  if (!nic_params->has_param("arbitrator")){
    nic_params->add_param("arbitrator",arb);
  }
  if (!netlink_params->has_param("arbitrator")){
    netlink_params->add_param("arbitrator",arb);
  }


  int packet_size = params->get_optional_int_param("accuracy_parameter", 4096);
  int net_packet_size = params->get_optional_int_param("network_accuracy_parameter", packet_size);
  int mem_packet_size = params->get_optional_int_param("memory_accuracy_parameter", packet_size);


  if (!mem_params->has_param("mtu")){
    mem_params->add_param_override("mtu", mem_packet_size);
  }
  if (!switch_params->has_param("mtu")){
    switch_params->add_param_override("mtu", net_packet_size);
  }
  if (!nic_params->has_param("mtu")){
    nic_params->add_param_override("mtu", net_packet_size);
  }

  if (amm_type == "amm1"){
    expand_amm1_memory(params, mem_params);
    expand_amm1_network(params, switch_params, true/*set xbar*/);
    expand_amm1_nic(params, nic_params);
    netlink_params->add_param_override("radix", "1");
  }
  else if (amm_type == "amm2"){
    expand_amm2_memory(params, mem_params);
    expand_amm1_network(params, switch_params, true/*set xbar*/);
    expand_amm1_nic(params, nic_params);
    netlink_params->add_param_override("radix", "1");
  }
  else if (amm_type == "amm3"){
    expand_amm2_memory(params, mem_params);
    expand_amm3_network(params, switch_params);
    expand_amm1_nic(params, nic_params);
    netlink_params->add_param_override("radix", "1");
  }
  else if (amm_type == "amm4"){
    expand_amm2_memory(params, mem_params);
    expand_amm4_network(params, top_params, switch_params);
    expand_amm4_nic(params, top_params, nic_params);
  }
  else {
    spkt_throw_printf(sprockit::input_error, "invalid hardware model %s given",
        amm_type.c_str());
  }

}

void
pisces_param_expander::expand_amm1_memory(sprockit::sim_parameters* params,
                                          sprockit::sim_parameters* mem_params)
{
  if (mem_params->get_scoped_param("model") != "null"){
    mem_params->add_param_override("total_bandwidth", mem_params->get_param("bandwidth"));
  }
}

void
pisces_param_expander::expand_amm1_network(sprockit::sim_parameters* params,
                                           sprockit::sim_parameters* switch_params,
                                           bool set_xbar)
{

  //JJW - no, don't do this
  //The link bandwidths will get multiplied during the connect
  //if redundant links, appropriately multiply the bandwidth
  //double bw_multiplier = network_bandwidth_multiplier(params);
  //double link_bw = switch_params->get_bandwidth_param("link_bandwidth");
  //if (bw_multiplier > 1.0001){
  //  link_bw *= bw_multiplier;
  //  switch_params->add_param_override("link_bandwidth", link_bw);
  //}

  sprockit::sim_parameters* link_params = switch_params->get_namespace("link");
  sprockit::sim_parameters* xbar_params = switch_params->get_namespace("xbar");
  sprockit::sim_parameters* ej_params = switch_params->get_optional_namespace("ejection");
  sprockit::sim_parameters* node_params = params->get_namespace("node");
  sprockit::sim_parameters* nic_params = node_params->get_namespace("nic");
  sprockit::sim_parameters* inj_params = nic_params->get_namespace("injection");
  sprockit::sim_parameters* netlink_params = params->get_optional_namespace("netlink");

  std::string link_lat = link_params->get_either_or_param("send_latency","latency");
  std::string xbar_lat = xbar_params->get_optional_param("latency", "0ns");
  if (!link_params->has_param("send_latency")){
    if (link_lat.size() == 0){
      spkt_abort_printf("must specify latency or send_latency for link");
    }
    link_params->add_param_override("send_latency", link_lat);
  }
  if (!link_params->has_param("credit_latency")){
    link_params->add_param_override("credit_latency", "0ns");
  }

  if (!xbar_params->has_param("send_latency")){
    xbar_params->add_param_override("send_latency", xbar_lat);
  }

  if (!xbar_params->has_param("credit_latency")){
    xbar_params->add_param_override("credit_latency", link_lat);
  }

  //make the xbar much faster than links
  if (set_xbar){
    double link_bw = link_params->get_bandwidth_param("bandwidth");
    double xbar_bw = link_bw * buffer_depth_;
    xbar_params->add_param_override("bandwidth", xbar_bw);
    xbar_params->add_param_override("arbitrator", "null");
  }

  int buffer_size;
  if (switch_params->has_param("buffer_size")){
    buffer_size = switch_params->get_byte_length_param("buffer_size");
  } else {
    int size_multiplier = switch_buffer_multiplier(params);
    int packet_size = params->get_optional_int_param("accuracy_parameter", 4096);
    buffer_size = buffer_depth_ * packet_size * size_multiplier;
    switch_params->add_param_override("buffer_size", buffer_size);
  }


  link_params->add_param_override("credits", buffer_size);

  if (!ej_params->has_param("send_latency")){
    if (!ej_params->has_param("latency")){
      ej_params->add_param_override("send_latency", inj_params->get_param("latency"));
    } else {
      ej_params->add_param_override("send_latency",
                                    ej_params->get_param("latency"));
    }
  }
  if (!ej_params->has_param("bandwidth")){
    ej_params->add_param_override("bandwidth",
                 inj_params->get_param("bandwidth"));
  }

  (*ej_params)["credits"].setByteLength(100, "GB");
  if (!ej_params->has_param("arbitrator")){
    ej_params->add_param("arbitrator", "cut_through");
  }
  if (!ej_params->has_param("credit_latency"))
    ej_params->add_param_override("credit_latency", "0ns");



  std::string net_model = netlink_params->get_optional_param("model", "null");
  if (net_model != "null"){
    sprockit::sim_parameters* inj_params = netlink_params->get_optional_namespace("injection");
    sprockit::sim_parameters* ej_params = netlink_params->get_optional_namespace("ejection");
    if (!inj_params->has_param("send_latency")){
      inj_params->add_param_override("send_latency", inj_params->get_param("latency"));
    }
    if (!inj_params->has_param("credit_latency")){
      inj_params->add_param_override("credit_latency", inj_params->get_param("latency"));
    }
    if (!ej_params->has_param("send_latency")){
      ej_params->add_param_override("send_latency", ej_params->get_param("latency"));
    }
    if (!ej_params->has_param("credit_latency")){
      ej_params->add_param_override("credit_latency", ej_params->get_param("latency"));
    }
    //expand netlink params
    (*inj_params)["credits"].setByteLength(buffer_size, "B");
    (*inj_params)["num_vc"] = 1;
    (*ej_params)["credits"].setByteLength(100, "GB");
    (*ej_params)["num_vc"] = 1;
  }
}

void
pisces_param_expander::expand_amm1_nic(sprockit::sim_parameters* params,
                                       sprockit::sim_parameters* nic_params)
{
  sprockit::sim_parameters* xbar_params = params->get_namespace("switch")->get_namespace("xbar");
  sprockit::sim_parameters* inj_params = nic_params->get_namespace("injection");
  if (!inj_params->has_param("arbitrator")){
    inj_params->add_param("arbitrator", "cut_through");
  }

  int inj_red = inj_params->get_optional_int_param("redundant",1);
  int buf_size = xbar_params->get_byte_length_param("buffer_size");
  int inj_credits = buf_size * inj_red;
  (*inj_params)["credits"].setByteLength(inj_credits, "B");
}

void
pisces_param_expander::expand_amm2_memory(sprockit::sim_parameters* params,
                                          sprockit::sim_parameters* mem_params)
{
  expand_amm1_memory(params, mem_params);
  if (mem_params->get_scoped_param("model") != "null"){
    //mem_params->add_param_override("max_single_bandwidth",
    //                               params->get_param("max_memory_bandwidth"));
  }
}

void
pisces_param_expander::expand_amm3_network(sprockit::sim_parameters* params,
                                                sprockit::sim_parameters* switch_params)
{
  expand_amm1_network(params, switch_params, false);
  sprockit::sim_parameters* xbar_params = switch_params->get_namespace("xbar");
  double sw_bw = xbar_params->get_bandwidth_param("bandwidth");
  double bw_multiplier = switch_bandwidth_multiplier(params);
  if (bw_multiplier > 1.0001){
    double xbar_bw = sw_bw * bw_multiplier;
    xbar_params->add_param_override("bandwidth", xbar_bw);
  }
}

void
pisces_param_expander::expand_amm4_network(sprockit::sim_parameters* params,
  sprockit::sim_parameters* top_params,
  sprockit::sim_parameters* switch_params)
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

  switch_params->add_param_override("model", "pisces_tiled");

  if (switch_params->has_param("router")){
    std::string router = switch_params->get_param("router");
    std::string new_router = router + "_multipath";
    switch_params->add_param_override("router", new_router);
  } else {
    spkt_throw_printf(sprockit::value_error,
      "if using amm4, must specify router = X\n"
      "valid options are minimal, ugal, valiant, min_ad)");
  }


  int buffer_size = switch_params->get_int_param("buffer_size");
  switch_params->add_param_override("row_buffer_size", buffer_size);
  switch_params->add_param_override("nrows", nrows);
  switch_params->add_param_override("ncols", ncols);

  expand_amm3_network(params, switch_params);
}

void
pisces_param_expander::expand_amm4_nic(sprockit::sim_parameters* params,
                                            sprockit::sim_parameters* top_params,
                                            sprockit::sim_parameters* nic_params)
{
  expand_amm1_nic(params, nic_params);
  sprockit::sim_parameters* netlink_params = params->get_optional_namespace("netlink");
  int conc = netlink_params->get_int_param("concentration");
  int red = top_params->get_optional_int_param("injection_redundant", 1);
  //the netlink block combines all the paths together
  netlink_params->add_param_override("ninject", red);
  netlink_params->add_param_override("neject", conc);
  netlink_params->add_param_override("model", "pisces");
}

}
}