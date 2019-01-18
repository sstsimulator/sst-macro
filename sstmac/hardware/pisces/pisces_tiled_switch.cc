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

#include <sstmac/hardware/pisces/pisces_tiled_switch.h>
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

PiscesTiledSwitch::PiscesTiledSwitch(SST::Params& params, uint32_t id)
  : PiscesAbstractSwitch(params, id)
{
  nrows_ = params->get_int_param("nrows");
  ncols_ = params->get_int_param("ncols");

  initComponents(params);
}

PiscesTiledSwitch::~PiscesTiledSwitch()
{
  for (PiscesDemuxer* dm : row_input_demuxers_){
    if (dm) delete dm;
  }
  for (PiscesCrossbar* xbar : xbar_tiles_){
    if (xbar) delete xbar;
  }
  for (PiscesMuxer* mux : col_output_muxers_){
    if (mux) delete mux;
  }
}

int
PiscesTiledSwitch::rowColToTile(int row, int col){
  return row*ncols_ + col;
}

void
PiscesTiledSwitch::tileToRowCol(int tile, int& row, int& col){
  row = tile / ncols_;
  col = tile % ncols_;
}

void
PiscesTiledSwitch::initComponents(SST::Params& params)
{
  if (!xbar_tiles_.empty())
    return;

  SST::Params demuxer_params = params.get_namespace("input");

  SST::Params xbar_params = params.get_namespace("xbar");

  SST::Params muxer_params = params.get_namespace("link");

  int ntiles = nrows_ * ncols_;
  dst_inports_.resize(ntiles);
  row_input_demuxers_.resize(ntiles);
  xbar_tiles_.resize(ntiles);
  col_output_muxers_.resize(ntiles);

  for (int r=0; r < nrows_; ++r){
    for (int c=0; c < ncols_; ++c){
      int tile = rowColToTile(r, c);
      PiscesDemuxer* dm = new PiscesDemuxer(demuxer_params, this, ncols_,
                                              router_->numVC(), false/*no vc update*/);
      row_input_demuxers_[tile] = dm;

      // divide by num columns to get row for output muxer
      PiscesCrossbar* xbar = new PiscesCrossbar(xbar_params, this, ncols_, ncols_,
                                                  router_->numVC(), true/*yes vc update*/);
      xbar->setStatCollector(xbar_stats_);

      // packet leaves the switch from the muxer
      // credits will arrive back at muxer with global port ids,
      // need to map these to local ports as well as payloads
      PiscesMuxer* muxer = new PiscesMuxer(muxer_params, this, ncols_,
                                             router_->numVC(), false/*no vc update*/);

      col_output_muxers_[tile] = muxer;
      xbar_tiles_[tile] = xbar;
    }
  }

  for (int row_dm=0; row_dm < nrows_; ++row_dm){
    PiscesDemuxer* demuxer = row_input_demuxers_[row_dm];
    for (int col_dm=0; col_dm < ncols_; ++col_dm){
      for (int col_out=0; col_out < ncols_; ++col_out){
        /**
        int tile_xbar = rowColToTile(row_dm, col_dm);
        PiscesCrossbar* xbar = xbar_tiles_[tile_xbar];
        auto out_link = allocateLocalLink(demuxer->sendLatency(), this, xbar->payloadHandler());
        demuxer->setOutput(demuxer_params,col_dm,0,out_link);

        auto in_link = allocateLocalLink(xbar->creditLatency(), this, demuxer->creditHandler());
        xbar->setInput(xbar_params,0,col_dm,in_link);
        */
      }
    }
  }

  //now loop all the xbars and link them up to ouptut muxers
  for (int rx=0; rx < nrows_; ++rx){
    //wire up the crossbars to inputs and outputs
    for (int cx=0; cx < ncols_; ++cx){
      int tile_xbar = rowColToTile(rx, cx);
      PiscesCrossbar* xbar = xbar_tiles_[tile_xbar];
      //connect current xbar to all the output muxers in the same col
      for (int rm=0; rm < nrows_; ++rm){
        int tile_muxer = rowColToTile(rm, cx);
        PiscesMuxer* muxer = col_output_muxers_[tile_muxer];
        //use zero-based input ports corresponding to row number for the muxer
        pisces_debug(
         "Connecting %s:%p local port %d to %s:%p local port %d",
          xbar->toString().c_str(), xbar, rm,
          muxer->toString().c_str(), muxer, rx);
        auto out_link = allocateSubLink(xbar->sendLatency(), this, muxer->payloadHandler());
        xbar->setOutput(xbar_params, rm, rx, out_link);
        auto in_link = allocateSubLink(muxer->creditLatency(), this, xbar->creditHandler());
        muxer->setInput(muxer_params, rx, rm, in_link);
      }
    }
  }
}

void
PiscesTiledSwitch::connectOutput(
  SST::Params& params,
  int src_outport,
  int dst_inport,
  EventLink* link)
{
  params->add_param_override("num_vc", router_->numVC());
  PiscesSender* muxer = col_output_muxers_[src_outport];
  muxer->setOutput(params, 0, dst_inport, link);
  dst_inports_[src_outport] = dst_inport;
}

void
PiscesTiledSwitch::connectInput(
  SST::Params& params,
  int src_outport,
  int dst_inport,
  EventLink* link)
{
  int row = dst_inport % nrows_;
  PiscesSender* demuxer = row_input_demuxers_[row];
  demuxer->setInput(params, row, src_outport, link);
}

Timestamp
PiscesTiledSwitch::sendLatency(SST::Params& params) const
{
  return Timestamp(params.get_namespace("link")->get_time_param("sendLatency"));
}

Timestamp
PiscesTiledSwitch::creditLatency(SST::Params& params) const
{
  return Timestamp(params.get_namespace("input")->get_time_param("creditLatency"));
}

int
PiscesTiledSwitch::queueLength(int port) const
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "PiscesTiledSwitch::queue_length");
}

void
PiscesTiledSwitch::handleCredit(Event *ev)
{
  PiscesCredit* credit = static_cast<PiscesCredit*>(ev);
  PiscesMuxer* recver = col_output_muxers_[credit->port()];
  recver->handleCredit(credit);
}

void
PiscesTiledSwitch::handlePayload(Event *ev)
{
  PiscesPacket* payload = static_cast<PiscesPacket*>(ev);

  debug_printf(sprockit::dbg::pisces,
               "tiled switch %d: incoming payload %s",
               int(my_addr_), payload->toString().c_str());

  int row;// = get_row(hdr->arrival_port);
  PiscesDemuxer* demuxer = row_input_demuxers_[row];
  //now figure out the new port I am routing to
  router_->route(payload);

  int edge_port = payload->edgeOutport();
  int dst_inport = dst_inports_[edge_port];

  payload->resetStages(getCol(edge_port), getRow(edge_port));

  debug_printf(sprockit::dbg::pisces,
               "tiled switch %d: routed payload %s to port %d, vc %d = %d,%d",
               int(my_addr_), payload->toString().c_str(),
               payload->edgeOutport(), payload->nextVC(),
               getRow(edge_port), getCol(edge_port));
  demuxer->handlePayload(payload);
}

std::string
PiscesTiledSwitch::toString() const
{
  return sprockit::printf("pisces tiled switch %d", int(my_addr_));
}

LinkHandler*
PiscesTiledSwitch::creditHandler(int port)
{
  return newLinkHandler(this, &PiscesTiledSwitch::handleCredit);
}

LinkHandler*
PiscesTiledSwitch::payloadHandler(int port)
{
  return newLinkHandler(this, &PiscesTiledSwitch::handlePayload);
}

}

}
