#include <sstmac/hardware/packet_flow/packet_flow_packetizer.h>

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

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/packet_flow/packet_flow_packetizer.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/hardware/packet_flow/packet_allocator.h>
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#include <stddef.h>

RegisterNamespaces("congestion_delays", "congestion_matrix");

namespace sstmac {
namespace hw {

SpktRegister("cut_through | null", packetizer, packet_flow_cut_through_packetizer);
SpktRegister("simple", packetizer, packet_flow_simple_packetizer);

packet_flow_nic_packetizer::packet_flow_nic_packetizer(sprockit::sim_parameters* params,
                                                       event_scheduler* parent,
                                                       packetizer_callback* cb) :
 inj_buffer_(nullptr),
 ej_buffer_(nullptr),
 stat_collector_(nullptr),
 buf_stats_(nullptr),
 pkt_allocator_(nullptr),
 payload_handler_(nullptr),
 packet_flow_packetizer(params, parent, cb)
{
  stat_collector_ = packet_stats_callback_factory::
                        get_optional_param("stats", "null", params, parent);

  sprockit::sim_parameters* buf_params = params->get_optional_namespace("buffer");
  buf_stats_ = packet_stats_callback_factory::
                get_optional_param("stats", "null", buf_params, parent);

  sprockit::sim_parameters* ej_params = params->get_optional_namespace("ejection");
  sprockit::sim_parameters* inj_params = params->get_namespace("injection");
  //do not put any latency on eject buffer
  ej_params->add_param_override("send_latency", "0ns");
  ej_params->add_param_override("credit_latency", "0ns");
  ej_params->add_param_override("credits", 1<<30);

  inj_params->add_param_override("send_latency", inj_params->get_time_param("latency"));
  inj_params->add_param_override("credit_latency", "0ns");

  inj_buffer_ = new packet_flow_injection_buffer(inj_params, parent);
  ej_buffer_ = new packet_flow_eject_buffer(ej_params, parent);

  pkt_allocator_ = packet_allocator_factory
      ::get_optional_param("packet_allocator", "structured_routable", params);

  inj_buffer_->set_stat_collector(buf_stats_);

  payload_handler_ = new_handler(this, &packet_flow_nic_packetizer::recv_packet);
}

packet_flow_nic_packetizer::~packet_flow_nic_packetizer()
{
  if (inj_buffer_) delete inj_buffer_;
  if (ej_buffer_) delete ej_buffer_;
  if (stat_collector_) delete stat_collector_;
  if (buf_stats_) delete buf_stats_;
  if (pkt_allocator_) delete pkt_allocator_;
  if (payload_handler_) delete payload_handler_;
}

void
packet_flow_nic_packetizer::set_acker(event_handler *handler)
{
  inj_buffer_->set_acker(handler);
}

void
packet_flow_nic_packetizer::recv_credit(event* ev)
{
  inj_buffer_->handle_credit(ev);
  int vn = 0;
  sendWhatYouCan(vn);
}

bool
packet_flow_nic_packetizer::spaceToSend(int vn, int num_bits) const
{
  //convert back to bytes
  return inj_buffer_->space_to_send(num_bits/8);
}

void
packet_flow_nic_packetizer::inject(int vn, long bytes, long byte_offset, message* msg)
{
  packet_flow_payload* payload = pkt_allocator_->new_packet(bytes, byte_offset, msg);
  inj_buffer_->handle_payload(payload);
}

void
packet_flow_nic_packetizer::recv_packet_common(packet_flow_payload* pkt)
{
  ej_buffer_->return_credit(pkt);
  stat_collector_->collect_final_event(pkt);
}

void
packet_flow_nic_packetizer::set_output(sprockit::sim_parameters* params,
                                       int inj_port, event_handler* handler)
{
  inj_buffer_->set_output(params, 0, inj_port, handler);
}

void
packet_flow_nic_packetizer::set_input(sprockit::sim_parameters* params,
                                      int ej_port, event_handler* handler)
{
  int only_port = 0;
  ej_buffer_->set_output(params, only_port, only_port, payload_handler_);
  ej_buffer_->set_input(params, only_port, ej_port, handler);
}

void
packet_flow_simple_packetizer::recv_packet(event* ev)
{
  packet_flow_payload* pkt = static_cast<packet_flow_payload*>(ev);
  recv_packet_common(pkt);
  int vn = 0;
  packetArrived(vn, pkt);
}

void
packet_flow_cut_through_packetizer::recv_packet(event* ev)
{
  packet_flow_payload* pkt = static_cast<packet_flow_payload*>(ev);
  int vn = 0;
  recv_packet_common(pkt);
  timestamp delay(pkt->num_bytes() / pkt->bw());
  debug_printf(sprockit::dbg::packet_flow,
    "packet %s scheduled to arrive at packetizer after delay of t=%12.6es",
     pkt->to_string().c_str(), delay.sec());
  send_delayed_self_event_queue(delay,
    new_callback(this, &packetizer::packetArrived, vn, (packet*)pkt));
}

}
} // end of namespace sstmac.



