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

#include <string>

#include <sstmac/hardware/pisces/pisces_branched_switch.h>
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sstmac/hardware/pisces/pisces_nic.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/switch/dist_dummyswitch.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"switch_geometry",
"row_buffer_size",
"nrows",
"ncols",
"ninject",
"neject",
"netlink_radix",
);

namespace sstmac {
namespace hw {

pisces_branched_switch::pisces_branched_switch(
    sprockit::sim_parameters* params,
    uint64_t id, event_manager* mgr)
  : pisces_abstract_switch(params, id, mgr)
{
  n_local_xbars_ = params->get_int_param("n_local_xbars");
  n_local_ports_ = params->get_int_param("n_local_ports");

#if !SSTMAC_INTEGRATED_SST_CORE
  payload_handler_ = new_handler(this, &pisces_branched_switch::handle_payload);
  ack_handler_ = new_handler(this, &pisces_branched_switch::handle_credit);
#endif

  init_components(params);
}

pisces_branched_switch::~pisces_branched_switch()
{
  delete xbar_;
  for (pisces_muxer* mux : input_muxers_)
    if (mux) delete mux;
  for (pisces_demuxer* demux : output_demuxers_)
    if (demux) delete demux;
  for (pisces_network_buffer* buf : output_buffers_)
    if (buf) delete buf;
}

void
pisces_branched_switch::init_components(sprockit::sim_parameters* params)
{
  // [muxer -> xbar -> demuxer -> output_buffer] -> [muxer...]

  if (!input_muxers_.empty())
    return;

  sprockit::sim_parameters* input_params =
      params->get_namespace("input");
  input_params->add_param_override("num_vc", router_->max_num_vc());

  sprockit::sim_parameters* xbar_params =
      params->get_namespace("xbar");
  xbar_params->add_param_override("num_vc", router_->max_num_vc());

  sprockit::sim_parameters* output_params =
      params->get_namespace("output");
  output_params->add_param_override("num_vc", router_->max_num_vc());

  sprockit::sim_parameters* link_params =
      params->get_namespace("link");
  link_params->add_param_override("num_vc", router_->max_num_vc());

  // construct the elements
  xbar_ = new pisces_crossbar(xbar_params, this);
  xbar_->configure_outports(n_local_xbars_,divide_port_mapper(n_local_xbars_));
  input_muxers_.resize(n_local_xbars_);
  output_demuxers_.resize(n_local_xbars_);
  output_buffers_.resize(n_local_xbars_ * n_local_ports_);
  for (int i=0; i < n_local_xbars_; ++i) {
    std::string location(std::to_string(i));

    pisces_muxer* mux = new pisces_muxer(input_params, this);
    mux->configure_outports(1,constant_port_mapper(0));
    mux->set_tile_id(location);
    mux->set_update_vc(false);
    input_muxers_[i] = mux;

    pisces_demuxer* demux = new pisces_demuxer(output_params, this);
    demux->configure_outports(n_local_ports_,mod_port_mapper(n_local_ports_));
    demux->set_tile_id(location);
    demux->set_update_vc(false);
    output_demuxers_[i] = demux;

    for (int j=0; j < n_local_ports_; ++j) {
      pisces_network_buffer* buf =
          new pisces_network_buffer(link_params, this);
      output_buffers_[i*n_local_ports_ + j] = buf;
    }
  }

  // connect input muxers to central xbar
  for (int i=0; i < n_local_xbars_; ++i) {
    pisces_muxer* mux = input_muxers_[i];
    mux->set_output(input_params,0,i,xbar_->payload_handler());
    xbar_->set_input(xbar_params,i,0,mux->credit_handler());
  }

  // connect output demuxers to central xbar and output buffers
  for (int i=0; i < n_local_xbars_; ++i) {
    pisces_demuxer* demux = output_demuxers_[i];
    xbar_->set_output(xbar_params,i,0,demux->payload_handler());
    demux->set_input(output_params,0,i,xbar_->credit_handler());
    for(int j=0; j < n_local_ports_; ++j) {
      pisces_network_buffer* buf = output_buffers_[i*n_local_ports_ + j];
      demux->set_output(output_params,j,0,buf->payload_handler());
      buf->set_input(link_params,0,j,demux->credit_handler());
    }
  }
}

void
pisces_branched_switch::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler *mod)
{
  params->add_param_override("num_vc", router_->max_num_vc());
  pisces_network_buffer* buf = output_buffers_[src_outport];
  buf->set_output(params, 0, dst_inport, mod);
}

void
pisces_branched_switch::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler *mod)
{
  pisces_muxer* demuxer = input_muxers_[dst_inport/n_local_ports_];
  demuxer->set_input(params, dst_inport % n_local_ports_, src_outport, mod);
}

int
pisces_branched_switch::queue_length(int port) const
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "pisces_tiled_switch::queue_length");
}

void
pisces_branched_switch::handle_credit(event *ev)
{
  pisces_credit* credit = static_cast<pisces_credit*>(ev);
  pisces_network_buffer* buf = output_buffers_[credit->port()];
  buf->handle_credit(credit);
}

void
pisces_branched_switch::handle_payload(event *ev)
{
  pisces_payload* payload = static_cast<pisces_payload*>(ev);
  debug_printf(sprockit::dbg::pisces,
               "tiled switch %d: incoming payload %s",
               int(my_addr_), payload->to_string().c_str());
  pisces_muxer* muxer = input_muxers_[payload->inport()/n_local_ports_];
  //now figure out the new port I am routing to
  router_->route(payload);
  debug_printf(sprockit::dbg::pisces,
               "tiled switch %d: routed payload %s to port %d",
               int(my_addr_), payload->to_string().c_str(),
               payload->next_port());
  muxer->handle_payload(payload);
}

std::string
pisces_branched_switch::to_string() const
{
  return sprockit::printf("pisces tiled switch %d", int(my_addr_));
}

link_handler*
pisces_branched_switch::credit_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new_link_handler(this, &pisces_branched_switch::handle_credit);
#else
  return ack_handler_;
#endif
}

link_handler*
pisces_branched_switch::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new_link_handler(this, &pisces_branched_switch::handle_payload);
#else
  return payload_handler_;
#endif
}

void
pisces_branched_switch::deadlock_check()
{
  for (int i=0; i < n_local_xbars_; ++i){
    input_muxers_[i]->deadlock_check();
    output_demuxers_[i]->deadlock_check();
  }
  for (int i=0; i < n_local_xbars_ * n_local_ports_; ++i) {
    output_buffers_[i]->deadlock_check();
  }
}

void
pisces_branched_switch::deadlock_check(event *ev)
{
  for (int i=0; i < n_local_xbars_; ++i){
    input_muxers_[i]->deadlock_check(ev);
    output_demuxers_[i]->deadlock_check(ev);
  }
  for (int i=0; i < n_local_xbars_ * n_local_ports_; ++i) {
    output_buffers_[i]->deadlock_check(ev);
  }
}

}

}
