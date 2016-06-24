#include <sstmac/hardware/packet_flow/packet_flow_handler.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

packet_flow_handler::packet_flow_handler()
{
}

void
packet_flow_handler::handle(event* ev)
{
  packet_flow_interface* fmsg = interface_cast(packet_flow_interface, ev);
  switch (fmsg->type()) {
    case packet_flow_interface::credit:
      handle_credit(static_cast<packet_flow_credit*>(ev));
      break;
    case packet_flow_interface::payload:
      handle_payload(static_cast<packet_flow_payload*>(ev));
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

void
payload_queue::push_back(packet_flow_payload *payload)
{
  for (int i=0; i < MAX_PAYLOAD_QUEUE_INDEX; ++i){
    if (slots_[i].push_back(payload)){
      return;
    }
  }
  //it is extra
  extras_.push_back(payload);
}

packet_flow_payload*
payload_queue::find_pending(int num_credits, std::list<packet_flow_payload*>& queue)
{
  std::list<packet_flow_payload*>::iterator it, end = queue.end();
  for (it=queue.begin(); it != end; ++it){
    packet_flow_payload* flow = *it;
    if (flow->num_bytes() <= num_credits){
      queue.erase(it);
      return flow;
    }
  }
  return 0;
}

packet_flow_payload*
payload_queue::pop(int num_credits)
{
  for (int i=0; i < MAX_PAYLOAD_QUEUE_INDEX; ++i){
    packet_flow_payload* flow = find_pending(num_credits, slots_[i].queue);
    if (flow){
      slots_[i].clear_port(flow->inport());
      return flow;
    }
  }
  return find_pending(num_credits, extras_);
}



}
}

