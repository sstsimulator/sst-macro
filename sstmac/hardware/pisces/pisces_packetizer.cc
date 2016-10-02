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

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/connectable_wrapper.h>
#endif

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
  sprockit::sim_parameters* inj_params = params->get_optional_namespace("injection");
  //do not put any latency on eject buffer
  ej_params->add_param_override("send_latency", "0ns");
  ej_params->add_param_override("credit_latency", "0ns");
  ej_params->add_param_override("credits", 1<<30);

  if (!inj_params->has_param("send_latency)")){
    inj_params->add_param_override("send_latency", inj_params->get_param("latency"));
  }
  if (!inj_params->has_param("credit_latency")){
    inj_params->add_param_override("credit_latency", "0ns");
  }


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
pisces_packetizer::new_credit_handler() const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<pisces_packetizer>(
           const_cast<pisces_packetizer*>(this),
           &pisces_packetizer::recv_credit);
#else
  return new_handler(const_cast<pisces_packetizer*>(this),
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
  return new_handler(const_cast<pisces_packetizer*>(this),
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
  pisces_payload* payload = pkt_allocator_->new_packet(bytes, msg->flow_id(), is_tail,
                                                       msg->toaddr(), msg->fromaddr(),
                                                       is_tail ? msg : nullptr);
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
class simple_network_packet : public pisces_routable_packet
{
  NotSerializable(simple_network_packet)

 public:
  simple_network_packet(
    serializable* msg,
    int num_bytes,
    bool is_tail,
    node_id toaddr,
    node_id fromaddr,
    int vn) :
   pisces_routable_packet(msg, num_bytes, is_tail, toaddr, fromaddr),
   vn_(vn)
  {
  }

  uint64_t
  flow_id() const override {
    spkt_abort_printf("simple network does not use flow IDs");
    return 0;
  }

  SST::Interfaces::SimpleNetwork::Request*
  request() const {
    return dynamic_cast<SST::Interfaces::SimpleNetwork::Request*>(orig_);
  }

  int vn() const {
    return vn_;
  }

 private:
  int vn_;

};

class simple_network_message : public network_message
{
 public:

};

pisces_simple_network::pisces_simple_network(sprockit::sim_parameters *params, SST::Component *comp) :
  SST::Interfaces::SimpleNetwork(comp),
  event_scheduler(comp, init_loc(params))
{
  //we need a self link
  event_scheduler::init_self_link(comp);
  sprockit::sim_parameters* inj_params = params->get_optional_namespace("injection");
  inj_buffer_ = new pisces_injection_buffer(inj_params, this);
  sprockit::sim_parameters* ej_params = params->get_optional_namespace("ejection");
  ej_buffer_ = new pisces_eject_buffer(ej_params, this);

  SST::LinkMap* link_map = SST::Simulation::getSimulation()->getComponentLinkMap(comp->getId());
  for (auto& pair : link_map->getLinkMap()){
    SST::Link* link = pair.second;
    std::istringstream istr(pair.first);
    std::string port_type;
    int src_outport, dst_inport;
    istr >> port_type;
    istr >> src_outport;
    istr >> dst_inport;
    if (port_type == "input"){
      integrated_connectable_wrapper* wrapper = new integrated_connectable_wrapper(link);
      ej_buffer_->set_input(ej_params, dst_inport, src_outport, wrapper);
      configureLink(pair.first,
       new SST::Event::Handler<pisces_eject_buffer>(ej_buffer_, &pisces_eject_buffer::handle_payload));
    } else if (port_type == "output"){
      integrated_connectable_wrapper* wrapper = new integrated_connectable_wrapper(link);
      inj_buffer_->set_output(inj_params, src_outport, dst_inport, wrapper);
      configureLink(pair.first,
       new SST::Event::Handler<pisces_injection_buffer>(inj_buffer_, &pisces_injection_buffer::handle_credit));
    } else if (port_type == "in-out"){
      logp_link_ = link;
    }
  }
}

void
pisces_simple_network::init(unsigned int phase)
{
  //TODO
}

void
pisces_simple_network::sendInitData(SST::Interfaces::SimpleNetwork::Request* req)
{
  spkt_abort_printf("pisces simple network cannot send init data");
}

SST::Interfaces::SimpleNetwork::Request*
pisces_simple_network::recvInitData()
{
  spkt_abort_printf("pisces simple network cannot recv init data");
  return nullptr;
}

event_loc_id
pisces_simple_network::init_loc(sprockit::sim_parameters* params)
{
  nid_ = params->get_int_param("id");
  return event_loc_id(node_id(nid_));
}

bool
pisces_simple_network::send(Request *req, int vn)
{
  int bytes = req->size_in_bits / 8;
  if (!inj_buffer_->space_to_send(bytes))
    return false;

  uint64_t ignore_flow_id = 0;

  simple_network_packet* pkt = new simple_network_packet(req, bytes, req->tail,
                                                    req->dest, nid_, vn);

  if (vn == 0){
    //primary payload network
    inj_buffer_->handle_payload(pkt);
  } else if (vn == 1){
    //control network
    logp_link_->send(pkt);
  } else {
    spkt_abort_printf("PISCES cannot handle vn's other than 0 and 1");
  }
  return true;
}

void
pisces_simple_network::packet_arrived(event* ev)
{
  simple_network_packet* pkt = safe_cast(simple_network_packet, ev);
  vn_reqs_[pkt->vn()].push_back(pkt->request());
  if (recv_functor_) {
    bool keep = (*recv_functor_)(pkt->vn());
    if (!keep) recv_functor_ = nullptr;
  }
}

SST::Interfaces::SimpleNetwork::Request*
pisces_simple_network::recv(int vn)
{
  auto req = vn_reqs_[vn].front();
  vn_reqs_[vn].pop_front();
  return req;
}

bool
pisces_simple_network::requestToReceive(int vn)
{
  return !vn_reqs_[vn].empty();
}

bool
pisces_simple_network::spaceToSend(int vn, int num_bits)
{
  int bytes = num_bits / 8;
  return bytes;
}

bool
pisces_simple_network::initialize(const std::string &portName,
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



