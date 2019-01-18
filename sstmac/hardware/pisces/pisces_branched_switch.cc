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

PiscesBranchedSwitch::PiscesBranchedSwitch(SST::Params& params, uint32_t id)
  : PiscesAbstractSwitch(params, id)
{
  std::vector<int> args;
  params->get_vector_param("geometry", args);
  n_local_ports_ = args[0];
  n_local_xbars_ = args[1];

  initComponents(params);
}

Timestamp
PiscesBranchedSwitch::sendLatency(SST::Params& params) const
{
  return Timestamp(params.get_namespace("output")->get_time_param("sendLatency"));
}

Timestamp
PiscesBranchedSwitch::creditLatency(SST::Params& params) const
{
  return Timestamp(params.get_namespace("input")->get_time_param("creditLatency"));
}

PiscesBranchedSwitch::~PiscesBranchedSwitch()
{
  delete xbar_;
  for (auto& mux : input_muxers_)
    if (mux.mux) delete mux.mux;
  for (PiscesDemuxer* demux : output_demuxers_)
    if (demux) delete demux;
}

void
PiscesBranchedSwitch::initComponents(SST::Params& params)
{
  // [muxer -> xbar -> demuxer -> output_buffer] -> [muxer...]

  if (!input_muxers_.empty())
    return;

  SST::Params input_params = params.get_namespace("input");

  SST::Params xbar_params = params.get_namespace("xbar");

  SST::Params output_params = params.get_namespace("output");


  // construct the elements
  xbar_ = new PiscesCrossbar(xbar_params, this, n_local_xbars_, n_local_xbars_,
                              router_->numVC(), true/*yes, update vc*/);
  input_muxers_.resize(n_local_xbars_);
  output_demuxers_.resize(n_local_xbars_);
  for (int i=0; i < n_local_xbars_; ++i) {
    PiscesMuxer* mux = new PiscesMuxer(input_params, this, n_local_ports_, router_->numVC(),
                                         false/*no vc update here*/);
    input_port& input = input_muxers_[i];
    input.mux = mux;
    input.parent = this;

    PiscesDemuxer* demux = new PiscesDemuxer(output_params, this, n_local_ports_, router_->numVC(),
                                               false/*no vc update here*/);
    output_demuxers_[i] = demux;
  }

  // connect input muxers to central xbar
  for (int i=0; i < n_local_xbars_; ++i) {
    PiscesMuxer* mux = input_muxers_[i].mux;
    auto out_lnk = allocateSubLink(mux->sendLatency(), this, xbar_->payloadHandler());
    mux->setOutput(input_params,0,i,out_lnk);
    auto in_lnk = allocateSubLink(xbar_->creditLatency(), this, mux->creditHandler());
    xbar_->setInput(xbar_params,i,0,in_lnk);
  }

  // connect output demuxers to central xbar
  for (int i=0; i < n_local_xbars_; ++i) {
    PiscesDemuxer* demux = output_demuxers_[i];
    auto out_link = allocateSubLink(xbar_->sendLatency(), this, demux->payloadHandler());
    xbar_->setOutput(xbar_params,i,0,out_link);
    auto in_link = allocateSubLink(demux->creditLatency(), this, xbar_->creditHandler());
    demux->setInput(output_params,0,i,in_link);
  }
}

void
PiscesBranchedSwitch::connectOutput(
  SST::Params& params,
  int src_outport,
  int dst_inport,
  EventLink* link)
{
  params->add_param_override("num_vc", router_->numVC());
  PiscesDemuxer* demux = output_demuxers_[src_outport/n_local_ports_];
  demux->setOutput(params, src_outport % n_local_ports_, dst_inport, link);
}

void
PiscesBranchedSwitch::connectInput(
  SST::Params& params,
  int src_outport,
  int dst_inport,
  EventLink* link)
{
  PiscesMuxer* muxer = input_muxers_[dst_inport/n_local_ports_].mux;
  muxer->setInput(params, dst_inport % n_local_ports_, src_outport, link);
}

int
PiscesBranchedSwitch::queueLength(int port) const
{
  spkt_abort_printf("unimplemented: PiscesTiledSwitch::queue_length");
  return 0;
}

std::string
PiscesBranchedSwitch::toString() const
{
  return sprockit::printf("pisces tiled switch %d", int(my_addr_));
}

LinkHandler*
PiscesBranchedSwitch::creditHandler(int port)
{
  PiscesDemuxer* demux = output_demuxers_[port];
  return newLinkHandler(demux, &PiscesDemuxer::handlePayload);
}

void
PiscesBranchedSwitch::input_port::handle(Event *ev)
{
  PiscesPacket* pkt = static_cast<PiscesPacket*>(ev);
  debug_printf(sprockit::dbg::pisces,
               "tiled switch %d: incoming payload %s",
               int(parent->addr()), pkt->toString().c_str());
  //now figure out the new port I am routing to
  parent->router()->route(pkt);

  int edge_port = pkt->edgeOutport();
  int xbar_exit_port = edge_port / parent->n_local_ports_;
  int demuxer_exit_port = edge_port % parent->n_local_ports_;;

  pkt->resetStages(0, xbar_exit_port, demuxer_exit_port);

  debug_printf(sprockit::dbg::pisces,
               "tiled switch %d: routed payload %s to port %d",
               parent->addr(), pkt->toString().c_str(),
               pkt->edgeOutport());
  mux->handlePayload(pkt);
}

LinkHandler*
PiscesBranchedSwitch::payloadHandler(int port)
{
  input_port* mux = const_cast<input_port*>(&input_muxers_[port]);
  return newLinkHandler(mux, &input_port::handle);
}

}

}
