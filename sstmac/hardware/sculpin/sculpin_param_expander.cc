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

#include <sstmac/hardware/sculpin/sculpin_param_expander.h>
#include <sstmac/common/timestamp.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

namespace sstmac {
namespace hw {

void
SculpinParamExpander::expand(sprockit::sim_parameters::ptr& params)
{
  std::string amm_type = params->get_param("amm_model");

  sprockit::sim_parameters::ptr node_params = params->get_optional_namespace("node");
  sprockit::sim_parameters::ptr nic_params = node_params->get_optional_namespace("nic");
  sprockit::sim_parameters::ptr inj_params = nic_params->get_optional_namespace("injection");
  sprockit::sim_parameters::ptr mem_params = node_params->get_optional_namespace("memory");
  sprockit::sim_parameters::ptr switch_params = params->get_optional_namespace("switch");
  sprockit::sim_parameters::ptr top_params = params->get_optional_namespace("topology");
  sprockit::sim_parameters::ptr proc_params = node_params->get_optional_namespace("proc");


  nic_params->add_param_override("name", "sculpin");
  switch_params->add_param_override("name", "sculpin");
  if (!mem_params->has_scoped_param("name")){
    mem_params->add_param_override("name", "pisces");
  }

  if (!mem_params->has_param("mtu")){
    int mem_packet_size = params->get_optional_int_param("memory_accuracy_parameter", 4096000);
    mem_params->add_param_override("mtu", mem_packet_size);
  }

  int packet_size = params->get_optional_int_param("accuracy_parameter", 4096);
  int net_packet_size = params->get_optional_int_param("network_accuracy_parameter", packet_size);
  if (!switch_params->has_param("mtu")){
    switch_params->add_param_override("mtu", net_packet_size);
  }
  if (!nic_params->has_param("mtu")){
    nic_params->add_param_override("mtu", net_packet_size);
  }

  if (amm_type == "amm1"){
    expandAmm1Memory(params, mem_params);
    expandAmm1Network(params, switch_params);
    expandAmm1Nic(params, top_params, nic_params);
  } else if (amm_type == "amm2" || amm_type == "amm3") {
    expandAmm1Memory(params, mem_params);
    expandAmm1Network(params, switch_params);
    expandAmm1Nic(params, top_params, nic_params);
  } else if (amm_type == "amm4") {
    expandAmm4Network(params, top_params, switch_params);
    expandAmm4Nic(params, top_params, nic_params);
  } else {
    spkt_throw_printf(sprockit::input_error, "invalid hardware model %s given",
        amm_type.c_str());
  }
}

void
SculpinParamExpander::expandAmm1Memory(sprockit::sim_parameters::ptr& params,
                                       sprockit::sim_parameters::ptr& mem_params)
{
  if (mem_params->get_scoped_param("name") != "null"){
    mem_params->add_param_override("total_bandwidth", mem_params->get_param("bandwidth"));
  }
}

void
SculpinParamExpander::checkBandwidth(sprockit::sim_parameters::ptr& params,
                                     sprockit::sim_parameters::ptr& deflt_params)
{
  if (!params->has_param("bandwidth")){
    if (deflt_params){
      params->add_param_override("bandwidth",
                                 deflt_params->get_param("bandwidth"));
    } else {
      params->print_scoped_params(std::cerr);
      spkt_abort_printf("do not have bandwidth parameter");
    }
  }
}

void
SculpinParamExpander::checkLatency(sprockit::sim_parameters::ptr& params,
                                   sprockit::sim_parameters::ptr& deflt_params)
{
  if (!params->has_param("sendLatency")){
    if (params->has_param("latency")){
      params->add_param_override("sendLatency", params->get_param("latency"));
    } else if (deflt_params){
      params->add_param_override("sendLatency",
                                 deflt_params->get_either_or_param("latency", "sendLatency"));
    } else {
      params->print_scoped_params(std::cerr);
      spkt_abort_printf("do not have sendLatency parameter");
    }
  }

  if (!params->has_param("creditLatency")){
    if (params->has_param("latency")){
      params->add_param_override("creditLatency", params->get_param("latency"));
    } else if (deflt_params){
      params->add_param_override("creditLatency",
                       deflt_params->get_either_or_param("latency", "creditLatency"));
    } else {
      params->print_scoped_params(std::cerr);
      spkt_abort_printf("do not have creditLatency parameter");
    }
  }
}

void
SculpinParamExpander::expandAmm1Network(sprockit::sim_parameters::ptr& params,
                                        sprockit::sim_parameters::ptr& switch_params)
{
  sprockit::sim_parameters::ptr link_params = switch_params->get_namespace("link");
  sprockit::sim_parameters::ptr ej_params = switch_params->get_optional_namespace("ejection");
  sprockit::sim_parameters::ptr node_params = params->get_namespace("node");
  sprockit::sim_parameters::ptr nic_params = node_params->get_namespace("nic");
  sprockit::sim_parameters::ptr inj_params = nic_params->get_namespace("injection");

  sprockit::sim_parameters::ptr empty{};

  checkLatency(link_params, empty);
  checkLatency(ej_params, inj_params);
  checkBandwidth(link_params, empty);
  checkBandwidth(ej_params, inj_params);

}

void
SculpinParamExpander::expandAmm1Nic(sprockit::sim_parameters::ptr& params,
                                    sprockit::sim_parameters::ptr& top_params,
                                    sprockit::sim_parameters::ptr& nic_params)
{
  sprockit::sim_parameters::ptr inj_params = nic_params->get_namespace("injection");
  sprockit::sim_parameters::ptr empty{};
  checkLatency(inj_params, empty);
  if (!inj_params->has_param("arbitrator")){
    inj_params->add_param("arbitrator", "cut_through");
  }
}



void
SculpinParamExpander::expandAmm4Network(sprockit::sim_parameters::ptr& params,
  sprockit::sim_parameters::ptr& top_params,
  sprockit::sim_parameters::ptr& switch_params)
{
  spkt_abort_printf("sculpin::does not support amm4");
}

void
SculpinParamExpander::expandAmm4Nic(sprockit::sim_parameters::ptr& params,
                                    sprockit::sim_parameters::ptr& top_params,
                                    sprockit::sim_parameters::ptr& nic_params)
{
  spkt_abort_printf("sculpin::does not support amm4");
}

}
}
