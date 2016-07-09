#ifndef PACKETFLOW_COMPONENT_H
#define PACKETFLOW_COMPONENT_H

#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/topology/topology.h>
#include <list>

namespace sstmac {
namespace hw {

struct payload_arbitration_slot
{
  payload_arbitration_slot() : port_bitmap(0){}

  std::list<packet_flow_payload*> queue;
  uint64_t port_bitmap;

  void
  clear_port(int port) {
    port_bitmap = port_bitmap ^ (1<<port);
  }

  bool
  push_back(packet_flow_payload* flow){
    uint64_t flow_bitmask = (1 << flow->inport());
    if (port_bitmap & flow_bitmask){
      port_bitmap = port_bitmap | flow_bitmask;
      queue.push_back(flow);
      return false;
    } else {
      return false;
    }
  }

};

struct payload_queue {

  std::list<packet_flow_payload*> queue;

  typedef std::list<packet_flow_payload*>::iterator iterator;

  packet_flow_payload*
  pop(int num_credits);

  packet_flow_payload*
  front();

  void
  push_back(packet_flow_payload* payload);

  packet_flow_payload*
  find_pending(int num_credits, std::list<packet_flow_payload*>& queue);

  #define MAX_PAYLOAD_QUEUE_INDEX 4
  payload_arbitration_slot slots_[MAX_PAYLOAD_QUEUE_INDEX];
  std::list<packet_flow_payload*> extras_;
};

class packet_flow_handler :
  public event_subscheduler
{

 public:
  packet_flow_handler();

  virtual std::string
  to_string() const {
    return "packet_flow_handler";
  }

  virtual ~packet_flow_handler() {}

  virtual void
  handle(event* ev);

  virtual void
  handle_credit(packet_flow_credit* msg) = 0;

  virtual void
  handle_payload(packet_flow_payload* pkt) = 0;

  int
  thread_id() const {
    return event_subscheduler::thread_id();
  }

};

struct packet_flow_input {
  int src_outport;
  event_handler* handler;
  packet_flow_input() :
    src_outport(-1),
    handler(0)
  {
  }
};

struct packet_flow_output {
  int dst_inport;
  event_handler* handler;
  packet_flow_output() :
    dst_inport(-1),
    handler(0)
  {
  }
};








}
}

#endif // PACKETFLOW_COMPONENT_H

