/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/hardware/packet_flow/packet_flow_tiled_switch.h>
#include <sstmac/hardware/packet_flow/packet_flow_stats.h>
#include <sstmac/hardware/packet_flow/packet_flow_nic.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/switch/dist_dummyswitch.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

namespace sstmac {
namespace hw {

#if !SSTMAC_INTEGRATED_SST_CORE
SpktRegister("packet_flow_tiled", network_switch, packet_flow_tiled_switch);
#endif

void
packet_flow_tiled_switch::init_factory_params(sprockit::sim_parameters *params)
{
  packet_flow_abstract_switch::init_factory_params(params);
  row_buffer_num_bytes = params->get_byte_length_param("row_buffer_size");
  nrows_ = params->get_int_param("nrows");
  ncols_ = params->get_int_param("ncols");
}

packet_flow_tiled_switch::~packet_flow_tiled_switch()
{
}

int
packet_flow_tiled_switch::row_col_to_tile(int row, int col){
  return row*ncols_ + col;
}

void
packet_flow_tiled_switch::tile_to_row_col(int tile, int& row, int& col){
  row = tile / ncols_;
  col = tile % ncols_;
}

void
packet_flow_tiled_switch::deadlock_check()
{
  for (int i=0; i < col_output_muxers_.size(); ++i){
    packet_flow_muxer* muxer = col_output_muxers_[i];
    muxer->deadlock_check();
  }
}

void
packet_flow_tiled_switch::init_components()
{
  if (!xbar_tiles_.empty())
    return;

  int ntiles = nrows_ * ncols_;
  row_input_demuxers_.resize(ntiles);
  xbar_tiles_.resize(ntiles);
  col_output_muxers_.resize(ntiles);
  for (int r=0; r < nrows_; ++r){
    for (int c=0; c < ncols_; ++c){
      int tile = row_col_to_tile(r, c);
      packet_flow_crossbar* xbar = new packet_flow_crossbar(
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

      packet_flow_muxer* muxer = new packet_flow_muxer(
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

      packet_flow_demuxer* dm = new packet_flow_demuxer(
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
}

void
packet_flow_tiled_switch::initialize()
{
  //this is a bit confusing - the demuxer could actually send to ANY output tile
  //rather than require the demuxer to internally do routing logic,
  //we just configure it with mappings for all NxM outputs
  for (int row_dm=0; row_dm < nrows_; ++row_dm){
    for (int col_dm=0; col_dm < ncols_; ++col_dm){
      int tile_dm = row_col_to_tile(row_dm, col_dm);
      packet_flow_sender* demuxer = row_input_demuxers_[tile_dm];
      //demuxer is connected to all xbars in the row
      for (int col_out=0; col_out < ncols_; ++col_out){
        //we must traverse the xbar at (row_dm, col_out)
        int tile_xbar = row_col_to_tile(row_dm, col_out);
        packet_flow_sender* xbar = xbar_tiles_[tile_xbar];
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
      packet_flow_crossbar* xbar = xbar_tiles_[tile_xbar];
      //connect current xbar to all the output muxers in the same col
      for (int rm=0; rm < nrows_; ++rm){
        int tile_muxer = row_col_to_tile(rm, cx);
        packet_flow_muxer* muxer = col_output_muxers_[tile_muxer];
        //use zero-based input ports corresponding to row number for the muxer
        xbar->set_output(tile_muxer, rx, muxer);
        xbar->init_credits(tile_muxer, muxer->num_initial_credits());
        muxer->set_input(rx, tile_muxer, xbar);
      }
    }
  }
}

void
packet_flow_tiled_switch::connect_output(int src_outport, int dst_inport, connectable *mod, config *cfg)
{
  connect_output(src_outport, dst_inport, safe_cast(event_handler, mod), cfg);
}

void
packet_flow_tiled_switch::connect_output(
  int src_outport,
  int dst_inport,
  event_handler *mod,
  config* cfg)
{
  packet_flow_sender* muxer = col_output_muxers_[src_outport];
  muxer->set_output(src_outport, dst_inport, mod);
  muxer->init_credits(src_outport, row_buffer_num_bytes);
}

void
packet_flow_tiled_switch::connect_input(int src_outport, int dst_inport, connectable *mod, config *cfg)
{
  connect_input(src_outport, dst_inport, safe_cast(event_handler, mod), cfg);
}

void
packet_flow_tiled_switch::connect_input(
  int src_outport,
  int dst_inport,
  event_handler *mod,
  config* cfg)
{
  packet_flow_sender* demuxer = row_input_demuxers_[dst_inport];
  demuxer->set_input(dst_inport, src_outport, mod);
}

void
packet_flow_tiled_switch::connect(
  int src_outport,
  int dst_inport,
  connection_type_t ty,
  connectable* mod,
  config* cfg)
{
  init_components();
  event_handler* ev = safe_cast(event_handler, mod);
  switch(ty) {
    case output:
      connect_output(src_outport, dst_inport, ev, cfg);
      break;
    case input:
      connect_input(src_outport, dst_inport, ev, cfg);
      break;
  }
}

void
packet_flow_tiled_switch::connect_injector(int src_outport, int dst_inport, event_handler* handler)
{
  init_components();
  packet_flow_sender* demuxer = row_input_demuxers_[dst_inport];
  demuxer->set_input(dst_inport, src_outport, handler);
}

void
packet_flow_tiled_switch::connect_ejector(int src_outport, int dst_inport, event_handler* handler)
{
  debug_printf(sprockit::dbg::packet_flow_config,
    "Switch %d: connecting to endpoint on outport %d, inport %d",
    int(my_addr_), src_outport, dst_inport);
  init_components();
  packet_flow_component* comp = safe_cast(packet_flow_component, handler);
  packet_flow_sender* muxer = col_output_muxers_[src_outport];
  muxer->set_output(src_outport, dst_inport, handler);
  muxer->init_credits(src_outport, comp->initial_credits());
}

std::vector<switch_id>
packet_flow_tiled_switch::connected_switches() const
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "packet_flow_tiled_switch::connected_switches");
}

void
packet_flow_tiled_switch::set_event_manager(event_manager* m)
{
  network_switch::set_event_manager(m);
  int ntiles = nrows_ * ncols_;
  for (int i=0; i < ntiles; ++i){
    xbar_tiles_[i]->set_event_parent(this);
    row_input_demuxers_[i]->set_event_parent(this);
    col_output_muxers_[i]->set_event_parent(this);
  }
}

int
packet_flow_tiled_switch::queue_length(int port) const
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "packet_flow_tiled_switch::queue_length");
}

void
packet_flow_tiled_switch::handle(event* ev)
{
  //this should only happen in parallel mode...
  //this means we are getting a message that has crossed the parallel boundary
  packet_flow_interface* fmsg = interface_cast(packet_flow_interface, ev);
  switch (fmsg->type()) {
    case packet_flow_interface::credit: {
      packet_flow_credit* credit = static_cast<packet_flow_credit*>(ev);
      packet_flow_muxer* recver = col_output_muxers_[credit->port()];
      recver->handle_credit(credit);
      break;
    }
    case packet_flow_interface::payload: {
      packet_flow_payload* payload = static_cast<packet_flow_payload*>(ev);
      //routable* rtbl = payload->interface<routable>();
      debug_printf(sprockit::dbg::packet_flow,
         "tiled switch %d: incoming payload %s",
          int(my_addr_), payload->to_string().c_str());
      packet_flow_demuxer* demuxer = row_input_demuxers_[payload->inport()];
      //now figure out the new port I am routing to
      router_->route(payload);
      debug_printf(sprockit::dbg::packet_flow,
         "tiled switch %d: routed payload %s to outport %d",
          int(my_addr_), payload->to_string().c_str(),
          payload->next_port());
      demuxer->handle_payload(payload);
      break;
    }
  }
}

std::string
packet_flow_tiled_switch::to_string() const
{
  return sprockit::printf("packet_flow switch %d", int(my_addr_));
}

}
}



