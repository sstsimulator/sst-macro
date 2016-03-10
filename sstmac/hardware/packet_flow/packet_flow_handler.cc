#include <sstmac/hardware/packet_flow/packet_flow_handler.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

packet_flow_handler::packet_flow_handler()
{
}

void
packet_flow_handler::handle(sst_message* msg)
{
  packet_flow_interface* fmsg = interface_cast(packet_flow_interface, msg);
  switch (fmsg->type()) {
    case packet_flow_interface::credit:
      handle_credit(static_cast<packet_flow_credit*>(msg));
      break;
    case packet_flow_interface::payload:
      handle_payload(static_cast<packet_flow_payload*>(msg));
      break;
  }
}

void
packet_flow_handler::handle_credit(packet_flow_credit* credit)
{
  spkt_throw_printf(sprockit::illformed_error, "%s should not handle credits",
                   to_string().c_str());
}

packet_flow_payload*
payload_queue::front()
{
  if (queue.empty()){
    return NULL;
  }

  return queue.front();
}

packet_flow_payload*
payload_queue::pop(int num_credits)
{
  if (queue.empty()) {
    return NULL;
  }

  packet_flow_payload* head = queue.front();
  if (head->num_bytes() <= num_credits) {
    queue.pop_front();
    return head;
  }

  return NULL;
}



}
}

