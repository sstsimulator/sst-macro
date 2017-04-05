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

#include <string>

#include <sstmac/hardware/pisces/pisces_tiled_switch.h>
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

#if !SSTMAC_INTEGRATED_SST_CORE
SpktRegister("pisces_tiled | pisces_tiled", network_switch, pisces_tiled_switch);
#endif

pisces_tiled_switch::pisces_tiled_switch(sprockit::sim_parameters* params,
                                                   uint64_t id, event_manager* mgr)
  : pisces_abstract_switch(params, id, mgr)
{
  nrows_ = params->get_int_param("nrows");
  ncols_ = params->get_int_param("ncols");

#if !SSTMAC_INTEGRATED_SST_CORE
  payload_handler_ = new_handler(this, &pisces_tiled_switch::handle_payload);
  ack_handler_ = new_handler(this, &pisces_tiled_switch::handle_credit);
#endif

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
pisces_tiled_switch::init_components(sprockit::sim_parameters* params)
{
  if (!xbar_tiles_.empty())
    return;

  sprockit::sim_parameters* demuxer_params = params->get_namespace("input");
  demuxer_params->add_param_override("num_vc", router_->max_num_vc());

  sprockit::sim_parameters* xbar_params = params->get_namespace("xbar");
  xbar_params->add_param_override("num_vc", router_->max_num_vc());

  sprockit::sim_parameters* muxer_params = params->get_namespace("link");
  muxer_params->add_param_override("num_vc", router_->max_num_vc());

  //int min_buffer_size = row_buffer_num_bytes_ / router_->max_num_vc();
  //if (min_buffer_size < packet_size_){
  //  spkt_throw(sprockit::value_error,
  //             "chosen packet size of %d is bigger than chosen buffer size %d = %d over %d vcs",
  //             packet_size_, min_buffer_size, row_buffer_num_bytes_, router_->max_num_vc());
  //}

  int ntiles = nrows_ * ncols_;
  row_input_demuxers_.resize(ntiles);
  xbar_tiles_.resize(ntiles);
  col_output_muxers_.resize(ntiles);

  for (int r=0; r < nrows_; ++r){
    for (int c=0; c < ncols_; ++c){
      int tile = row_col_to_tile(r, c);
      std::string location(std::to_string(tile) + "(" + std::to_string(r) + ":" + std::to_string(c) + ")");

      pisces_crossbar* xbar = new pisces_crossbar(xbar_params, this);
      xbar->set_stat_collector(xbar_stats_);
      //hw::structured_topology* s_top = safe_cast(hw::structured_topology, top_);
      //xbar->configure_basic_ports(s_top->max_num_ports());

      //we can route to a destination port that differs from the local port
      int xbar_mapper = ncols_;
      //xbar->configure_div_ports(xbar_mapper, ntiles-1);
      xbar->configure_div_ports(xbar_mapper, nrows_);
      xbar->set_update_vc(false);
      xbar->set_tile_id(location);

      pisces_muxer* muxer = new pisces_muxer(muxer_params, this);
      int muxer_offset = tile;
      //int muxer_max_port = tile;
      //muxer->set_event_location(my_addr_);
      muxer->configure_div_ports(ntiles, 1);
      muxer->set_tile_id(location);

      pisces_demuxer* dm = new pisces_demuxer(demuxer_params, this);
//      std::cerr << "created dm: " << dm << "\n";
      //routed port numbers are 0-48, e.g. for a 6x8
      //we route locally to a given column number
      int dm_mod = ncols_;
      dm->configure_mod_ports(dm_mod);
      dm->set_update_vc(false);
      dm->set_tile_id(location);

      row_input_demuxers_[tile] = dm;
      col_output_muxers_[tile] = muxer;
      xbar_tiles_[tile] = xbar;
    }
  }

  for (int row_dm=0; row_dm < nrows_; ++row_dm){
    for (int col_dm=0; col_dm < ncols_; ++col_dm){
      int tile_dm = row_col_to_tile(row_dm, col_dm);
      pisces_demuxer* demuxer = row_input_demuxers_[tile_dm];
      //demuxer is connected to all xbars in the row
      for (int col_out=0; col_out < ncols_; ++col_out){
        //we must traverse the xbar at (row_dm, col_out)
        int tile_xbar = row_col_to_tile(row_dm, col_out);
        pisces_crossbar* xbar = xbar_tiles_[tile_xbar];
        //label unique input ports on the xbar by column
        demuxer->set_output(demuxer_params,col_out,col_dm,xbar->payload_handler());
        xbar->set_input(xbar_params, col_dm, col_out, demuxer->credit_handler());
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
        pisces_debug(
         "Connecting %s:%p local port %d to %s:%p local port %d",
          xbar->to_string().c_str(), this, rm,
          muxer->to_string().c_str(), this, rx);
        xbar->set_output(xbar_params, rm, rx, muxer->payload_handler());
        muxer->set_input(muxer_params, rx, rm, xbar->credit_handler());
      }
    }
  }
}

void
pisces_tiled_switch::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler *mod)
{
  params->add_param_override("num_vc", router_->max_num_vc());
  pisces_sender* muxer = col_output_muxers_[src_outport];
  //std::cerr << "setting muxer output on outport " << src_outport << "\n";
  muxer->set_output(params, 0, dst_inport, mod);
}

void
pisces_tiled_switch::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler *mod)
{
  pisces_sender* demuxer = row_input_demuxers_[dst_inport];
//  std::cerr << "demuxer: " << demuxer << std::endl;
  demuxer->set_input(params, dst_inport, src_outport, mod);
}

#if 0
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
#endif

int
pisces_tiled_switch::queue_length(int port) const
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "pisces_tiled_switch::queue_length");
}

#if 0
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
#endif

void
pisces_tiled_switch::handle_credit(event *ev)
{
  pisces_credit* credit = static_cast<pisces_credit*>(ev);
  pisces_muxer* recver = col_output_muxers_[credit->port()];
  recver->handle_credit(credit);
}

void
pisces_tiled_switch::handle_payload(event *ev)
{
  pisces_payload* payload = static_cast<pisces_payload*>(ev);
  //routable* rtbl = payload->interface<routable>();
  debug_printf(sprockit::dbg::pisces,
               "tiled switch %d: incoming payload %s",
               int(my_addr_), payload->to_string().c_str());
  pisces_demuxer* demuxer = row_input_demuxers_[payload->inport()];
  //now figure out the new port I am routing to
  router_->route(payload);
  debug_printf(sprockit::dbg::pisces,
               "tiled switch %d: routed payload %s to port %d",
               int(my_addr_), payload->to_string().c_str(),
               payload->next_port());
  demuxer->handle_payload(payload);
}

std::string
pisces_tiled_switch::to_string() const
{
  return sprockit::printf("pisces tiled switch %d", int(my_addr_));
}

link_handler*
pisces_tiled_switch::credit_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<pisces_switch>(const_cast<pisces_switch*>(this),
                          &pisces_tiled_switch::handle_credit);
#else
  return ack_handler_;
#endif
}

link_handler*
pisces_tiled_switch::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<pisces_switch>(const_cast<pisces_switch*>(this),
                          &pisces_tiled_switch::handle_payload);
#else
  return payload_handler_;
#endif
}

void
pisces_tiled_switch::deadlock_check()
{
  for (int r=0; r < nrows_; ++r){
    for (int c=0; c < ncols_; ++c){
      int tile = row_col_to_tile(r, c);
      row_input_demuxers_[tile]->deadlock_check();
      col_output_muxers_[tile]->deadlock_check();
      xbar_tiles_[tile]->deadlock_check();
    }
  }
}

void
pisces_tiled_switch::deadlock_check(event *ev)
{
  for (int r=0; r < nrows_; ++r){
    for (int c=0; c < ncols_; ++c){
      int tile = row_col_to_tile(r, c);
      row_input_demuxers_[tile]->deadlock_check(ev);
      col_output_muxers_[tile]->deadlock_check(ev);
      xbar_tiles_[tile]->deadlock_check(ev);
    }
  }
}

}

}


