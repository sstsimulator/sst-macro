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

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/router/routable.h>
#include <sstmac/hardware/pisces/pisces_nic.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#include <stddef.h>

RegisterNamespaces("congestion_delays", "congestion_matrix");

RegisterKeywords(
"pisces_injection_latency",
"pisces_injection_bandwidth",
);

namespace sstmac {
namespace hw {

const int pisces_netlink::really_big_buffer = 1<<30;

pisces_nic::pisces_nic(sprockit::sim_parameters* params, node* parent) :
  nic(params, parent),
  packetizer_(nullptr)
{
  sprockit::sim_parameters* inj_params = params->get_namespace("injection");


  packetizer_ = packetizer::factory::get_optional_param("packetizer", "cut_through",
                                              inj_params, parent);
  packetizer_->setArrivalNotify(this);
  packetizer_->setInjectionAcker(mtl_handler());

  //make port 0 a copy of the injection params
  sprockit::sim_parameters* port0_params = params->get_optional_namespace("port0");
  inj_params->combine_into(port0_params);

#if !SSTMAC_INTEGRATED_SST_CORE
  ack_handler_ = packetizer_->new_credit_handler();
  payload_handler_ = packetizer_->new_payload_handler();
#endif
}

void
pisces_nic::init(unsigned int phase)
{
  packetizer_->init(phase);
}

void
pisces_nic::setup()
{
  packetizer_->setup();
}

//
// Goodbye.
//
pisces_nic::~pisces_nic() throw ()
{
  if (packetizer_) delete packetizer_;
#if !SSTMAC_INTEGRATED_SST_CORE
  delete ack_handler_;
  delete payload_handler_;
#endif
}

link_handler*
pisces_nic::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  if (port == nic::LogP){
    return new SST::Event::Handler<pisces_nic>(
          const_cast<pisces_nic*>(this), &nic::mtl_handle);
  } else {
    return packetizer_->new_payload_handler();
  }
#else
  if (port == nic::LogP){
    return link_mtl_handler_;
  } else {
    return payload_handler_;
  }
#endif
}

link_handler*
pisces_nic::credit_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return packetizer_->new_credit_handler();
#else
  return ack_handler_;
#endif
}

void
pisces_nic::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler* mod)
{
  if (src_outport == Injection){
    pisces_packetizer* packer = safe_cast(pisces_packetizer, packetizer_);
    packer->set_output(params, dst_inport, mod);
  } else if (src_outport == LogP){
    logp_switch_ = mod;
  } else {
    spkt_abort_printf("Invalid switch port %d in pisces_nic::connect_output", src_outport);
  }
}

void
pisces_nic::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler* mod)
{
  if (!mod) abort();
  pisces_packetizer* packer = safe_cast(pisces_packetizer, packetizer_);
  packer->set_input(params, src_outport, mod);
}

void
pisces_nic::do_send(network_message* payload)
{
  nic_debug("packet flow: sending %s", payload->to_string().c_str());
  int vn = 0; //we only ever use one virtual network
  packetizer_->start(vn, payload);
  //schedule_delay(inj_lat_, new_callback(packetizer_, &packetizer::start, vn, payload));
}

void
pisces_nic::deadlock_check()
{
  packetizer_->deadlock_check();
}

void
pisces_netlink::connect_output(
  sprockit::sim_parameters* params,
  int src_outport, int dst_inport,
  event_handler *mod)
{
  if (is_node_port(src_outport)){
    ej_block_->set_output(params, src_outport, dst_inport, mod);
  } else {
    inj_block_->set_output(params, src_outport, dst_inport, mod);
  }
}

void
pisces_netlink::connect_input(
  sprockit::sim_parameters* params,
  int src_outport, int dst_inport,
  event_handler* mod)
{
  if (is_node_port(dst_inport)){
    inj_block_->set_input(params, dst_inport, src_outport, mod);
    ej_block_->set_input(params, dst_inport, src_outport, mod);
  } else {
    ej_block_->set_input(params, dst_inport, src_outport, mod);
  }
}

pisces_netlink::pisces_netlink(sprockit::sim_parameters *params, node *parent)
  : netlink(params, parent),
  inj_block_(nullptr),
  ej_block_(nullptr),
  tile_rotater_(0)
{
  sprockit::sim_parameters* inj_params = params->get_optional_namespace("injection");
  inj_block_ = new pisces_crossbar(inj_params, parent);
  inj_block_->configure_basic_ports(num_tiles_ + conc_);
  sprockit::sim_parameters* ej_params = params->get_optional_namespace("ejection");
  ej_block_ = new pisces_crossbar(ej_params, parent);
  ej_block_->configure_offset_ports(num_tiles_, num_tiles_ + conc_);

#if !SSTMAC_INTEGRATED_SST_CORE
  ack_handler_ = new_handler(this,
                             &pisces_netlink::handle_credit);

  payload_handler_ = new_handler(this,
                             &pisces_netlink::handle_payload);
#endif
}

void
pisces_netlink::deadlock_check()
{
  ej_block_->deadlock_check();
  inj_block_->deadlock_check();
}

link_handler*
pisces_netlink::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<pisces_netlink>(
       const_cast<pisces_netlink*>(this), &pisces_netlink::handle_payload);
#else
  return payload_handler_;
#endif
}

link_handler*
pisces_netlink::credit_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<pisces_netlink>(
     const_cast<pisces_netlink*>(this), &pisces_netlink::handle_credit);
#else
  return ack_handler_;
#endif
}

void
pisces_netlink::handle_credit(event* ev)
{
  pisces_credit* credit = static_cast<pisces_credit*>(ev);
  debug_printf(sprockit::dbg::pisces,
     "netlink %s:%p handling credit %s",
     topology::global()->label(event_location()).c_str(),
     this,
     credit->to_string().c_str());
  if (is_node_port(credit->port())){
    ej_block_->handle_credit(credit);
  } else {
    inj_block_->handle_credit(credit);
  }
}

void
pisces_netlink::handle_payload(event* ev)
{
  pisces_payload* payload = static_cast<pisces_payload*>(ev);
  routable* rtbl = payload->interface<routable>();
  debug_printf(sprockit::dbg::pisces,
       "netlink %d:%p handling payload %s",
        //topology::global()->label(event_location()).c_str(),
        int(id_), this, payload->to_string().c_str());
  node_id toaddr = payload->toaddr();
  netlink_id dst_netid(toaddr / conc_);
  routable::path& p = rtbl->current_path();
  if (dst_netid == id_){
    //stays local - goes to a node
    int node_offset = toaddr % conc_;
    p.outport = netlink::node_port(node_offset);
    p.vc = 0;
    p.geometric_id = 0;
    debug_printf(sprockit::dbg::pisces,
     "netlink %d ejecting %s to node %d at offset %d to port %d",
        int(id_), sprockit::to_string(ev).c_str(), int(toaddr), node_offset, p.outport);
    ej_block_->handle_payload(payload);
  } else {
    //goes to switch
    p.outport = netlink::switch_port(tile_rotater_);
    p.vc = 0;
    debug_printf(sprockit::dbg::pisces,
     "netlink %d injecting msg %s to switch %d on redundant path %d of %d to port %d",
        int(id_), sprockit::to_string(ev).c_str(),
        int(toaddr), tile_rotater_, num_tiles_, p.outport);
    tile_rotater_ = (tile_rotater_ + 1) % num_tiles_;
    inj_block_->handle_payload(payload);
  }
}

pisces_netlink::~pisces_netlink()
{
  if (ej_block_) delete ej_block_;
  if (inj_block_) delete inj_block_;
#if !SSTMAC_INTEGRATED_SST_CORE
  delete ack_handler_;
  delete payload_handler_;
#endif
}

}
} // end of namespace sstmac.