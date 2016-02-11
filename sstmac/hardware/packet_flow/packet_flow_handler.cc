#include <sstmac/hardware/packet_flow/packet_flow_handler.h>

namespace sstmac {
namespace hw {

packet_flow_handler::packet_flow_handler()
{
}

void
packet_flow_handler::handle(const sst_message::ptr& msg)
{
  packet_flow_interface* fmsg = ptr_interface_cast(packet_flow_interface, msg);
  switch (fmsg->type()) {
    case packet_flow_interface::credit:
      handle_credit(ptr_static_cast(packet_flow_credit, msg));
      break;
    case packet_flow_interface::payload:
      handle_payload(ptr_static_cast(packet_flow_payload, msg));
      break;
  }
}

void
packet_flow_handler::handle_credit(const packet_flow_credit::ptr& credit)
{
  spkt_throw_printf(sprockit::illformed_error, "%s should not handle credits",
                   to_string().c_str());
}

packet_flow_payload::ptr
payload_queue::front()
{
  if (queue.empty()){
    return packet_flow_payload::ptr();
  }

  return queue.front();
}

packet_flow_payload::ptr
payload_queue::pop(int num_credits)
{
  if (queue.empty()) {
    return packet_flow_payload::ptr();
  }

  packet_flow_payload::ptr head = queue.front();
  if (head->num_bytes() <= num_credits) {
    queue.pop_front();
    return head;
  }

  return packet_flow_payload::ptr();
}



}
}

