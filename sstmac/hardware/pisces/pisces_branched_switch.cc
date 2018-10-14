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

#include <string>

#include <sstmac/hardware/pisces/pisces_branched_switch.h>
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sstmac/hardware/pisces/pisces_nic.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
{ "switch_geometry", "DEPRECATED: array specifying geometry of the switch" },
{ "geometry", "array specifying geometry of the switch" },
{ "row_buffer_size", "the size of the input buffer in each row" },
{ "nrows", "the number of row tiles in a switch" },
{ "ncols", "the number of col tiles in a switch" },
);

namespace sstmac {
namespace hw {

pisces_branched_switch::pisces_branched_switch(
    sprockit::sim_parameters* params,
    uint64_t id, event_manager* mgr)
  : pisces_abstract_switch(params, id, mgr)
{
  std::vector<int> args;
  params->get_vector_param("geometry", args);
  n_local_ports_ = args[0];
  n_local_xbars_ = args[1];

  init_components(params);
}

timestamp
pisces_branched_switch::send_latency(sprockit::sim_parameters *params) const
{
  return params->get_namespace("output")->get_time_param("send_latency");
}

timestamp
pisces_branched_switch::credit_latency(sprockit::sim_parameters *params) const
{
  return params->get_namespace("input")->get_time_param("credit_latency");
}

pisces_branched_switch::~pisces_branched_switch()
{
  delete xbar_;
  for (auto& mux : input_muxers_)
    if (mux.mux) delete mux.mux;
  for (pisces_demuxer* demux : output_demuxers_)
    if (demux) delete demux;
}

void
pisces_branched_switch::init_components(sprockit::sim_parameters* params)
{
  // [muxer -> xbar -> demuxer -> output_buffer] -> [muxer...]

  if (!input_muxers_.empty())
    return;

  sprockit::sim_parameters* input_params = params->get_namespace("input");

  sprockit::sim_parameters* xbar_params = params->get_namespace("xbar");

  sprockit::sim_parameters* output_params = params->get_namespace("output");


  // construct the elements
  xbar_ = new pisces_crossbar(xbar_params, this, n_local_xbars_, n_local_xbars_,
                              router_->num_vc(), true/*yes, update vc*/);
  input_muxers_.resize(n_local_xbars_);
  output_demuxers_.resize(n_local_xbars_);
  for (int i=0; i < n_local_xbars_; ++i) {
    pisces_muxer* mux = new pisces_muxer(input_params, this, n_local_ports_, router_->num_vc(),
                                         false/*no vc update here*/);
    input_port& input = input_muxers_[i];
    input.mux = mux;
    input.parent = this;

    pisces_demuxer* demux = new pisces_demuxer(output_params, this, n_local_ports_, router_->num_vc(),
                                               false/*no vc update here*/);
    output_demuxers_[i] = demux;
  }

  // connect input muxers to central xbar
  for (int i=0; i < n_local_xbars_; ++i) {
    pisces_muxer* mux = input_muxers_[i].mux;
    auto out_lnk = allocate_local_link(mux->send_latency(), this, xbar_->payload_handler());
    mux->set_output(input_params,0,i,out_lnk);
    auto in_lnk = allocate_local_link(xbar_->credit_latency(), this, mux->credit_handler());
    xbar_->set_input(xbar_params,i,0,in_lnk);
  }

  // connect output demuxers to central xbar
  for (int i=0; i < n_local_xbars_; ++i) {
    pisces_demuxer* demux = output_demuxers_[i];
    auto out_link = allocate_local_link(xbar_->send_latency(), this, demux->payload_handler());
    xbar_->set_output(xbar_params,i,0,out_link);
    auto in_link = allocate_local_link(demux->credit_latency(), this, xbar_->credit_handler());
    demux->set_input(output_params,0,i,in_link);
  }
}

void
pisces_branched_switch::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_link* link)
{
  params->add_param_override("num_vc", router_->num_vc());
  pisces_demuxer* demux = output_demuxers_[src_outport/n_local_ports_];
  demux->set_output(params, src_outport % n_local_ports_, dst_inport, link);
}

void
pisces_branched_switch::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_link* link)
{
  pisces_muxer* muxer = input_muxers_[dst_inport/n_local_ports_].mux;
  muxer->set_input(params, dst_inport % n_local_ports_, src_outport, link);
}

int
pisces_branched_switch::queue_length(int port) const
{
  spkt_abort_printf("unimplemented: pisces_tiled_switch::queue_length");
  return 0;
}

std::string
pisces_branched_switch::to_string() const
{
  return sprockit::printf("pisces tiled switch %d", int(my_addr_));
}

link_handler*
pisces_branched_switch::credit_handler(int port)
{
  pisces_demuxer* demux = output_demuxers_[port];
  return new_link_handler(demux, &pisces_demuxer::handle_payload);
}

void
pisces_branched_switch::input_port::handle(event *ev)
{
  pisces_packet* pkt = static_cast<pisces_packet*>(ev);
  debug_printf(sprockit::dbg::pisces,
               "tiled switch %d: incoming payload %s",
               int(parent->addr()), pkt->to_string().c_str());
  //now figure out the new port I am routing to
  parent->rter()->route(pkt);

  int edge_port = pkt->edge_outport();
  int xbar_exit_port = edge_port / parent->n_local_ports_;
  int demuxer_exit_port = edge_port % parent->n_local_ports_;;

  pkt->reset_stages(0, xbar_exit_port, demuxer_exit_port);

  debug_printf(sprockit::dbg::pisces,
               "tiled switch %d: routed payload %s to port %d",
               parent->addr(), pkt->to_string().c_str(),
               pkt->edge_outport());
  mux->handle_payload(pkt);
}

link_handler*
pisces_branched_switch::payload_handler(int port)
{
  input_port* mux = const_cast<input_port*>(&input_muxers_[port]);
  return new_link_handler(mux, &input_port::handle);
}

void
pisces_branched_switch::deadlock_check()
{
  for (int i=0; i < n_local_xbars_; ++i){
    input_muxers_[i].mux->deadlock_check();
    output_demuxers_[i]->deadlock_check();
  }
}

void
pisces_branched_switch::deadlock_check(event *ev)
{
  for (int i=0; i < n_local_xbars_; ++i){
    input_muxers_[i].mux->deadlock_check(ev);
    output_demuxers_[i]->deadlock_check(ev);
  }
}

}

}
