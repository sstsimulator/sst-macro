#include <sstmac/hardware/nic/null_nic.h>
#include <sstmac/common/messages/message_chunk.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

SpktRegister("nullnic | null", nic, null_nic,
            "Implements a nic which immediately injects messages with no memory reads or latency");

void
null_nic::recv_packet(event* pkt)
{
  message* parent_msg = completion_queue_.recv(safe_cast(packet, pkt));
  if (parent_msg){
    parent_->handle(parent_msg);
  }
}

void
null_nic::do_send(network_message* msg)
{
  nic_debug("null model: sending message %s", msg->to_string().c_str());
  injector_->handle(msg);
  if (msg->needs_ack()) {
    network_message* acker = msg->clone_injection_ack();
    send_to_node(acker);
  }
}

timestamp
null_nic::injection_latency() const
{
  return timestamp(0);
}

void
null_nic::connect(
  int src_outport,
  int dst_inport,
  connection_type_t ty,
  connectable* mod)
{
  switch(ty)
  {
    case output:
      injector_ = safe_cast(event_handler, mod);
      break;
    case input:
      //just ignore
      break;
    default:
      nic::connect(src_outport, dst_inport, ty, mod);
  }
}

}
}

