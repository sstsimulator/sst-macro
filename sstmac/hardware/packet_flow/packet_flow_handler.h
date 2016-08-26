#ifndef PACKETFLOW_COMPONENT_H
#define PACKETFLOW_COMPONENT_H

#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/topology/topology.h>
#include <list>

namespace sstmac {
namespace hw {

struct payload_queue {

  std::list<packet_flow_payload*> queue;

  typedef std::list<packet_flow_payload*>::iterator iterator;

  packet_flow_payload*
  pop(int num_credits);

  packet_flow_payload*
  front();

  void
  push_back(packet_flow_payload* payload);

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

