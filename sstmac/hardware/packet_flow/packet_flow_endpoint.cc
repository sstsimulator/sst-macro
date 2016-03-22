#include <sstmac/hardware/packet_flow/packet_flow_endpoint.h>
#include <sstmac/common/event_callback.h>

ImplementFactory(sstmac::hw::packet_flow_endpoint);

namespace sstmac {
namespace hw {

SpktRegister("simple", packet_flow_endpoint, packet_flow_simple_endpoint,
            "Simple packet_flow endpoint that assumes whole message arrives as single unit");

SpktRegister("null", packet_flow_endpoint, packet_flow_null_endpoint,
            "Null packet_flow endpoint that assumes whole message arrives with no congestion");

SpktRegister("cut_through", packet_flow_endpoint,
            packet_flow_cut_through_endpoint,
            "Packetflow endpoint that assumes only header arrives. This then adds bandwidth delay before delivering message");

packet_flow_endpoint::packet_flow_endpoint()
{
}

void
packet_flow_endpoint::payload_arrived(packet_flow_payload* pkt)
{
  debug_printf(sprockit::dbg::packet_flow, "payload arrived: %s",
    pkt->to_string().c_str());
  message* parent_msg = completion_queue_.recv(pkt);
  if (parent_msg){
    exit_->handle(parent_msg);
  }
  delete pkt;
}

void
packet_flow_simple_endpoint::handle_payload(packet_flow_payload* pkt)
{
  payload_arrived(pkt);
}

void
packet_flow_null_endpoint::handle_payload(packet_flow_payload* pkt)
{
  timestamp delay(pkt->num_bytes() / pkt->bw());
  debug_printf(sprockit::dbg::packet_flow,
    "scheduling payload %s to finish at endpoint after delay t=%12.8e",
    pkt->to_string().c_str(), delay.sec());
  send_delayed_self_event_queue(delay,
    new_event(event_location(), this, &packet_flow_endpoint::payload_arrived, pkt));
}

void
packet_flow_cut_through_endpoint::handle_payload(packet_flow_payload* pkt)
{
  timestamp delay(pkt->num_bytes() / pkt->bw());
  debug_printf(sprockit::dbg::packet_flow,
    "scheduling payload %s to finish at endpoint after delay t=%12.8e",
    pkt->to_string().c_str(), delay.sec());
  send_delayed_self_event_queue(delay,
    new_event(event_location(), this, &packet_flow_endpoint::payload_arrived, pkt));
}

}
}

