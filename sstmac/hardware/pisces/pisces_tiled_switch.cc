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

#include <sstmac/hardware/pisces/pisces_tiled_switch.h>
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sstmac/hardware/pisces/pisces_nic.h>
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

#if 0

namespace sstmac {
namespace hw {

#if !SSTMAC_INTEGRATED_SST_CORE
SpktRegister("pisces_tiled | pisces_tiled", network_switch, pisces_tiled_switch);
#endif

pisces_tiled_switch::pisces_tiled_switch(sprockit::sim_parameters* params,
                                                   uint64_t id, event_manager* mgr)
  : pisces_abstract_switch(params, id, mgr)
{
  //row_buffer_num_bytes = params->get_byte_length_param("row_buffer_size");
  nrows_ = params->get_int_param("nrows");
  ncols_ = params->get_int_param("ncols");
  init_components(params);
}

pisces_tiled_switch::~pisces_tiled_switch()
{
  for (pisces_demuxer* dm : row_input_demuxers_){
    if (dm) delete dm;
  }
  for (pisces_crossbar* xbar : xbar_tiles_){
    if (xbar) delete xbar;
  }
  for (pisces_muxer* mux : col_output_muxers_){
    if (mux) delete mux;
  }
}

int
pisces_tiled_switch::row_col_to_tile(int row, int col){
  return row*ncols_ + col;
}

void
pisces_tiled_switch::tile_to_row_col(int tile, int& row, int& col){
  row = tile / ncols_;
  col = tile % ncols_;
}

void
pisces_tiled_switch::deadlock_check()
{
  for (int i=0; i < col_output_muxers_.size(); ++i){
    pisces_muxer* muxer = col_output_muxers_[i];
    muxer->deadlock_check();
  }
}

void
pisces_tiled_switch::init_components(sprockit::sim_parameters* params)
{
  spkt_throw(sprockit::unimplemented_error, "init_components");
#if 0
  if (!xbar_tiles_.empty())
    return;

  int min_buffer_size = xbar_input_buffer_num_bytes / router_->max_num_vc();
  if (min_buffer_size < packet_size_){
    spkt_throw(sprockit::value_error,
               "chosen packet size of %d is bigger than chosen buffer size %d = %d over %d vcs",
               packet_size_, min_buffer_size, xbar_input_buffer_num_bytes, router_->max_num_vc());
  }

  int ntiles = nrows_ * ncols_;
  row_input_demuxers_.resize(ntiles);
  xbar_tiles_.resize(ntiles);
  col_output_muxers_.resize(ntiles);
  for (int r=0; r < nrows_; ++r){
    for (int c=0; c < ncols_; ++c){
      int tile = row_col_to_tile(r, c);
      pisces_crossbar* xbar = new pisces_crossbar(this,
        timestamp(0), //just assume no latency in crossbar
        timestamp(0), //just assume no latency in crossbar
        xbar_bw,
        router_->max_num_vc(),
        xbar_input_buffer_num_bytes,
        link_arbitrator_template->clone(-1));
      //we can route to a destination port that differs from the local port
      int xbar_mapper = ncols_; //map port by dividing by ncols
      xbar->set_event_location(my_addr_);
      xbar->configure_div_ports(xbar_mapper, ntiles-1);
      xbar->set_update_vc(false);

      pisces_muxer* muxer = new pisces_muxer(this,
        hop_lat, //put all the latency in the send
        timestamp(0), //assume zero latency credits
        link_bw,
        router_->max_num_vc(),
        xbar_output_buffer_num_bytes,
        link_arbitrator_template->clone(-1));
      int muxer_offset = tile;
      int muxer_max_port = tile;
      muxer->set_event_location(my_addr_);
      muxer->configure_offset_ports(muxer_offset, muxer_max_port);

      pisces_demuxer* dm = new pisces_demuxer(this,
        timestamp(0), //assume zero latency send
        hop_lat, //credit latency
        router_->max_num_vc(),
        row_buffer_num_bytes);
      //routed port numbers are 0-48, e.g. for a 6x8
      //we route locally to a given column number
      int dm_mod = ncols_;
      dm->set_event_location(my_addr_);
      dm->configure_mod_ports(dm_mod);
      dm->set_update_vc(false);

      row_input_demuxers_[tile] = dm;
      col_output_muxers_[tile] = muxer;
      xbar_tiles_[tile] = xbar;
    }
  }

  //this is a bit confusing - the demuxer could actually send to ANY output tile
  //rather than require the demuxer to internally do routing logic,
  //we just configure it with mappings for all NxM outputs
  for (int row_dm=0; row_dm < nrows_; ++row_dm){
    for (int col_dm=0; col_dm < ncols_; ++col_dm){
      int tile_dm = row_col_to_tile(row_dm, col_dm);
      pisces_sender* demuxer = row_input_demuxers_[tile_dm];
      //demuxer is connected to all xbars in the row
      for (int col_out=0; col_out < ncols_; ++col_out){
        //we must traverse the xbar at (row_dm, col_out)
        int tile_xbar = row_col_to_tile(row_dm, col_out);
        pisces_sender* xbar = xbar_tiles_[tile_xbar];
        //label unique input ports on the xbar by column
        demuxer->set_output(col_out, col_dm, xbar);
        demuxer->init_credits(col_out, xbar->num_initial_credits());
        xbar->set_input(col_dm, col_out, demuxer);
      }
    }
  }

  //now loop all the xbars and link them up to ouptut muxers
  for (int rx=0; rx < nrows_; ++rx){
    //wire up the crossbars to inputs and outputs
    for (int cx=0; cx < ncols_; ++cx){
      int tile_xbar = row_col_to_tile(rx, cx);
      pisces_crossbar* xbar = xbar_tiles_[tile_xbar];
      //connect current xbar to all the output muxers in the same col
      for (int rm=0; rm < nrows_; ++rm){
        int tile_muxer = row_col_to_tile(rm, cx);
        pisces_muxer* muxer = col_output_muxers_[tile_muxer];
        //use zero-based input ports corresponding to row number for the muxer
        xbar->set_output(tile_muxer, rx, muxer);
        xbar->init_credits(tile_muxer, muxer->num_initial_credits());
        muxer->set_input(rx, tile_muxer, xbar);
      }
    }
  }
#endif
}

void
pisces_tiled_switch::connect_output(sprockit::sim_parameters* params,
                                         int src_outport, int dst_inport,
                                         connectable *mod)
{
  connect_output(params, src_outport, dst_inport,
                 safe_cast(event_handler, mod));
}

void
pisces_tiled_switch::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler *mod)
{
  pisces_sender* muxer = col_output_muxers_[src_outport];
  muxer->set_output(params, src_outport, dst_inport, mod);
}

void
pisces_tiled_switch::connect_input(sprockit::sim_parameters* params,
                                        int src_outport, int dst_inport,
                                        connectable *mod)
{
  connect_input(params, src_outport, dst_inport, safe_cast(event_handler, mod));
}

void
pisces_tiled_switch::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler *mod)
{
  pisces_sender* demuxer = row_input_demuxers_[dst_inport];
  demuxer->set_input(params, dst_inport, src_outport, mod);
}

void
pisces_tiled_switch::connect(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  connection_type_t ty,
  connectable* mod)
{
  event_handler* ev = safe_cast(event_handler, mod);
  switch(ty) {
    case output:
      connect_output(params, src_outport, dst_inport, ev);
      break;
    case input:
      connect_input(params, src_outport, dst_inport, ev);
      break;
  }
}

void
pisces_tiled_switch::connect_injector(sprockit::sim_parameters* params,
                      int src_outport, int dst_inport, event_handler* handler)
{
  pisces_sender* demuxer = row_input_demuxers_[dst_inport];
  demuxer->set_input(params, dst_inport, src_outport, handler);
}

void
pisces_tiled_switch::connect_ejector(sprockit::sim_parameters* params,
                                          int src_outport, int dst_inport,
                                          event_handler* handler)
{
  debug_printf(sprockit::dbg::pisces_config,
    "Switch %d: connecting to endpoint on outport %d, inport %d",
    int(my_addr_), src_outport, dst_inport);
  pisces_sender* muxer = col_output_muxers_[src_outport];
  muxer->set_output(params, src_outport, dst_inport, handler);
}

int
pisces_tiled_switch::queue_length(int port) const
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "pisces_tiled_switch::queue_length");
}

void
pisces_tiled_switch::handle(event* ev)
{
  //this should only happen in parallel mode...
  //this means we are getting a message that has crossed the parallel boundary
  pisces_interface* fmsg = interface_cast(pisces_interface, ev);
  switch (fmsg->type()) {
    case pisces_interface::credit: {
      pisces_credit* credit = static_cast<pisces_credit*>(ev);
      pisces_muxer* recver = col_output_muxers_[credit->port()];
      recver->handle_credit(credit);
      break;
    }
    case pisces_interface::payload: {
      pisces_payload* payload = static_cast<pisces_payload*>(ev);
      //routable* rtbl = payload->interface<routable>();
      debug_printf(sprockit::dbg::pisces,
         "tiled switch %d: incoming payload %s",
          int(my_addr_), payload->to_string().c_str());
      pisces_demuxer* demuxer = row_input_demuxers_[payload->inport()];
      //now figure out the new port I am routing to
      router_->route(payload);
      debug_printf(sprockit::dbg::pisces,
         "tiled switch %d: routed payload %s to outport %d",
          int(my_addr_), payload->to_string().c_str(),
          payload->next_port());
      demuxer->handle_payload(payload);
      break;
    }
  }
}

std::string
pisces_tiled_switch::to_string() const override
{
  return sprockit::printf("pisces switch %d", int(my_addr_));
}

}
}

#endif