#include <sstmac/hardware/pisces/pisces_packetizer.h>

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
#include <sstmac/hardware/pisces/pisces_packetizer.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/hardware/pisces/packet_allocator.h>
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

#include <stddef.h>

RegisterNamespaces("congestion_delays", "congestion_matrix");

namespace sstmac {
namespace hw {

SpktRegister("cut_through | null", packetizer, pisces_cut_through_packetizer);
SpktRegister("simple", packetizer, pisces_simple_packetizer);

pisces_packetizer::pisces_packetizer(sprockit::sim_parameters* params,
                                     event_scheduler* parent) :
 inj_buffer_(nullptr),
 ej_buffer_(nullptr),
 stat_collector_(nullptr),
 buf_stats_(nullptr),
 pkt_allocator_(nullptr),
 payload_handler_(nullptr),
 packetizer(params, parent)
#if SSTMAC_INTEGRATED_SST_CORE
 ,SimpleNetwork(dynamic_cast<SST::Component*>(parent))
 ,initialized_(false),
 wrapper_(nullptr)
#endif
{
  init(params, parent);
}

void
pisces_packetizer::init(sprockit::sim_parameters* params, event_scheduler* parent)
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

  inj_params->add_param_override("send_latency", inj_params->get_param("latency"));
  inj_params->add_param_override("credit_latency", "0ns");

  inj_buffer_ = new pisces_injection_buffer(inj_params, parent);
  ej_buffer_ = new pisces_eject_buffer(ej_params, parent);

  pkt_allocator_ = packet_allocator_factory
      ::get_optional_param("packet_allocator", "pisces", params);

  inj_buffer_->set_stat_collector(buf_stats_);

  payload_handler_ = new_handler(this, &pisces_packetizer::recv_packet);
}

pisces_packetizer::~pisces_packetizer()
{
  if (inj_buffer_) delete inj_buffer_;
  if (ej_buffer_) delete ej_buffer_;
  if (stat_collector_) delete stat_collector_;
  if (buf_stats_) delete buf_stats_;
  if (pkt_allocator_) delete pkt_allocator_;
  if (payload_handler_) delete payload_handler_;
}

void
pisces_packetizer::init(unsigned int phase)
{
}

void
pisces_packetizer::setup()
{
}

link_handler*
pisces_packetizer::new_ack_handler() const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<pisces_packetizer>(
           const_cast<pisces_packetizer*>(this),
           &pisces_packetizer::recv_credit);
#else
  return new_link_handler(const_cast<pisces_packetizer*>(this),
                         &pisces_packetizer::recv_credit);
#endif
}

link_handler*
pisces_packetizer::new_payload_handler() const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<pisces_packetizer>(
        const_cast<pisces_packetizer*>(this),
        &pisces_packetizer::recv_packet);
#else
  return new_link_handler(const_cast<pisces_packetizer*>(this),
                           &pisces_packetizer::recv_packet);
#endif
}

void
pisces_packetizer::recv_credit(event* ev)
{
  inj_buffer_->handle_credit(ev);
  int vn = 0;
  sendWhatYouCan(vn);
}

bool
pisces_packetizer::spaceToSend(int vn, int num_bits)
{
  //convert back to bytes
  return inj_buffer_->space_to_send(num_bits/8);
}

void
pisces_packetizer::inject(int vn, long bytes, long byte_offset, message* msg)
{
  bool is_tail = (byte_offset + bytes) == msg->byte_length();
  //only carry the payload if you're the tail packet
  pisces_payload* payload = pkt_allocator_->new_packet(bytes, byte_offset,
                                                       msg->toaddr(), msg->fromaddr(),
                                                       msg->flow_id(), is_tail ? msg : nullptr);
  inj_buffer_->handle_payload(payload);
}

void
pisces_packetizer::recv_packet_common(pisces_payload* pkt)
{
  ej_buffer_->return_credit(pkt);
  stat_collector_->collect_final_event(pkt);
}

void
pisces_packetizer::set_output(sprockit::sim_parameters* params,
                                       int inj_port, event_handler* handler)
{
  inj_buffer_->set_output(params, 0, inj_port, handler);
}

void
pisces_packetizer::set_input(sprockit::sim_parameters* params,
                                      int ej_port, event_handler* handler)
{
  int only_port = 0;
  ej_buffer_->set_output(params, only_port, only_port, payload_handler_);
  ej_buffer_->set_input(params, only_port, ej_port, handler);
}

void
pisces_simple_packetizer::recv_packet(event* ev)
{
  pisces_payload* pkt = static_cast<pisces_payload*>(ev);
  recv_packet_common(pkt);
  int vn = 0;
  packetArrived(vn, pkt);
}

void
pisces_cut_through_packetizer::recv_packet(event* ev)
{
  pisces_payload* pkt = static_cast<pisces_payload*>(ev);
  int vn = 0;
  recv_packet_common(pkt);
  timestamp delay(pkt->num_bytes() / pkt->bw());
  debug_printf(sprockit::dbg::pisces,
    "packet %s scheduled to arrive at packetizer after delay of t=%12.6es",
     pkt->to_string().c_str(), delay.sec());
  send_delayed_self_event_queue(delay,
    new_callback(this, &packetizer::packetArrived, vn, (packet*)pkt));
}

#if SSTMAC_INTEGRATED_SST_CORE
void
pisces_packetizer::sst_component_wrapper::connect_output(
    sprockit::sim_parameters *params, int src_outport, int dst_inport, event_handler *mod)
{
  spkt_abort_printf("component wrapper should never connect to event handler");
}

void
pisces_packetizer::sst_component_wrapper::connect_input(
    sprockit::sim_parameters *params, int src_outport, int dst_inport, event_handler *mod)
{
  spkt_abort_printf("component wrapper should never connect to event handler");
}


pisces_packetizer::pisces_packetizer(sprockit::sim_parameters *params, SST::Component *comp) :
  wrapper_(new sst_component_wrapper(params, event_loc_id::null, comp)),
  packetizer(params, wrapper_),
  SST::Interfaces::SimpleNetwork(comp)
{
  init(params, wrapper_);
}

void
pisces_packetizer::init_links(sprockit::sim_parameters *params)
{

}

bool
pisces_packetizer::send(Request *req, int vn)
{
  //pisces_payload* payload = pkt_allocator_
  return true;
}

void
pisces_packetizer::sendInitData(SST::Interfaces::SimpleNetwork::Request* req)
{
}

SST::Interfaces::SimpleNetwork::Request*
pisces_packetizer::recvInitData()
{
}

SST::Interfaces::SimpleNetwork::Request*
pisces_packetizer::recv(int vn)
{
  return nullptr;
}

bool
pisces_packetizer::requestToReceive(int vn)
{
  return false;
}

bool
pisces_packetizer::initialize(const std::string &portName,
                              const SST::UnitAlgebra &link_bw, int vns,
                              const SST::UnitAlgebra &in_buf_size,
                              const SST::UnitAlgebra &out_buf_size)
{
  sst_link_bw_ = link_bw;
  initialized_ = true;
  return true;
}

#endif

}
} // end of namespace sstmac.



