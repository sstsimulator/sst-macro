#include <sstmac/hardware/packet_flow/packet_flow_sender.h>
#include <sstmac/hardware/router/valiant_routing.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/output.h>

namespace sstmac {
namespace hw {

packet_flow_payload::ptr
packet_flow_MTL::next_chunk(long byte_offset, const sst_message::ptr &parent)
{
  long bytes_left = parent->byte_length() - byte_offset;
  long bytes_to_send = bytes_left > mtu_ ? mtu_ : bytes_left;

  packet_flow_payload::ptr payload = new packet_flow_payload(
                                         parent,
                                         bytes_to_send, //only a single message
                                         byte_offset);

  return payload;
}

packet_flow_sender::packet_flow_sender(
  const timestamp& send_lat,
  const timestamp& credit_lat)
  : send_lat_(send_lat),
    credit_lat_(credit_lat),
    congestion_spyplot_(0),
    acc_delay_(false),
    acker_(0),
    update_vc_(true)
{
}

packet_flow_sender::packet_flow_sender() :
    congestion_spyplot_(0),
    acc_delay_(false),
    acker_(0),
    update_vc_(true)
{
}

void
packet_flow_sender::send_credit(
  const packet_flow_input& src,
  const packet_flow_payload::ptr& payload,
  timestamp credit_leaves)
{
  int src_vc = payload->vc(); //we have not updated to the new virtual channel
  packet_flow_credit::ptr credit = new packet_flow_credit(src.src_outport,
                                   src_vc, payload->num_bytes());
  //there is a certain minimum latency on credits
  timestamp min_arrival = now() + credit_lat_;
  //assume for now the packet flow sender is smart enough to pipeline credits efficiently
  timestamp credit_arrival = min_arrival > credit_leaves ? min_arrival : credit_leaves;
  packet_flow_debug(
      "On %s:%p on inport %d, crediting %s:%p port:%d vc:%d {%s} at [%9.5e] after latency %9.5e with %p",
      to_string().c_str(), this, payload->inport(),
      src.handler->to_string().c_str(), src.handler,
      src.src_outport, src_vc,
      payload->to_string().c_str(),
      credit_arrival.sec(), credit_lat_.sec(),
      credit.get());
  schedule(credit_arrival, src.handler, credit);
}

void
packet_flow_sender::send(
  packet_flow_bandwidth_arbitrator* arb,
  const packet_flow_payload::ptr& msg,
  const packet_flow_input& src,
  const packet_flow_output& dest)
{
  double incoming_bw = msg->bw();
  const timestamp& now_ = now();
  timestamp packet_head_leaves;
  timestamp packet_tail_leaves;
  timestamp credit_leaves;
  if (arb) {
    arb->arbitrate(now_, msg,
      packet_head_leaves, packet_tail_leaves, credit_leaves);
  } else {
    packet_head_leaves = packet_tail_leaves = credit_leaves = now_;
  }

  if (acc_delay_ || congestion_spyplot_){
    double delta_t = (packet_tail_leaves - now_).sec();
    double min_delta_t = msg->byte_length() / incoming_bw;
    double congestion_delay = delta_t - min_delta_t;
#if SSTMAC_SANITY_CHECK
    if (congestion_delay < -1e-9){
      spkt_throw_printf(sprockit::value_error,
        "got a negative congestion delay arbitrating packet");
    }
#endif
    congestion_delay = std::max(0., congestion_delay);
    double congestion_delay_us = congestion_delay * 1e6;
    if (acc_delay_){
      msg->add_delay_us(congestion_delay);
    }
    if (congestion_spyplot_){
      long my_id = event_location().location;
      congestion_spyplot_->add(msg->fromaddr(), my_id, congestion_delay_us);
    }
  }

#if SSTMAC_SANITY_CHECK
  if (msg->bw() <= 0 && msg->bw() != packet_flow_payload::uninitialized_bw) {
    spkt_throw_printf(sprockit::value_error,
                     "On %s, got negative bandwidth for msg %s",
                     to_string().c_str(),
                     msg->to_string().c_str());
  }
#endif

  if (src.handler) {
    send_credit(src, msg, credit_leaves);
  }

  if (acker_ && msg->is_tail()) {
    network_message::ptr netmsg = ptr_test_cast(network_message, msg->orig());
    if (netmsg && netmsg->needs_ack()) {
      START_VALID_SCHEDULE(this)
      network_message::ptr ack = netmsg->clone_injection_ack();
      schedule(packet_tail_leaves, acker_, ack);
      STOP_VALID_SCHEDULE(this)
    }
  }

  packet_flow_debug(
    "On %s:%p, sending to port:%d vc:%d {%s} to handler %s:%p on inport %d at head_leaves=%9.5e tail_leaves=%9.5e",
    to_string().c_str(), this,
    msg->port(), msg->rinfo().current_path().vc,
    msg->to_string().c_str(),
    dest.handler->to_string().c_str(), dest.handler,
    dest.dst_inport,
    packet_head_leaves.sec(),
    packet_tail_leaves.sec());

  // leaving me, on arrival at dest message will occupy a specific inport at dest
  msg->set_inport(dest.dst_inport);
  //weird hack to update vc from routing
  if (update_vc_) msg->update_vc();

  timestamp arrival = packet_head_leaves + send_lat_;
  START_VALID_SCHEDULE(this)
  schedule(arrival, dest.handler, msg);
  STOP_VALID_SCHEDULE(this)
}

std::string
packet_flow_sender::to_string() const
{
  if (event_location() == event_loc_id::uninitialized){
    cerrn << "to_string failed for sender of type " << packet_flow_name() << std::endl;
    abort();
  }
  return packet_flow_name() + topology::global()->label(event_location());
}

}
}

