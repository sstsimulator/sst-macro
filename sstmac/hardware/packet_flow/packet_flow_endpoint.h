#ifndef PACKETFLOW_ENDPOINT_H
#define PACKETFLOW_ENDPOINT_H

#include <sstmac/common/event_scheduler.h>
#include <sstmac/hardware/common/recv_cq.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/packet_flow/packet_flow_handler.h>

namespace sstmac {
namespace hw {

class packet_flow_endpoint :
  public packet_flow_handler,
  public sprockit::factory_type
{

 public:
  virtual std::string
  to_string() const {
    return "packet_flow_endpoint";
  }

  virtual ~packet_flow_endpoint() {}

  void
  handle_credit(const packet_flow_credit::ptr& msg) {
    packet_flow_handler::handle_credit(msg);
  }

  virtual void
  handle_payload(const packet_flow_payload::ptr& msg) = 0;

  void
  set_exit(event_handler* handler) {
    exit_ = handler;
  }

  void
  init_param1(node_id nid) {
    my_id_ = nid;
    init_loc_id(event_loc_id(my_id_));
  }

  void
  payload_arrived(const packet_flow_payload::ptr& msg);

 protected:
  packet_flow_endpoint();

  recv_cq completion_queue_;

  event_handler* exit_;

  node_id my_id_;

};

class packet_flow_simple_endpoint :
  public packet_flow_endpoint
{
 public:
  void
  handle_payload(const packet_flow_payload::ptr& msg);

  packet_flow_endpoint*
  clone() const {
    return new packet_flow_simple_endpoint;
  }
};

class packet_flow_null_endpoint :
  public packet_flow_endpoint
{
 public:
  void
  handle_payload(const packet_flow_payload::ptr& msg);

  packet_flow_endpoint*
  clone() const {
    return new packet_flow_null_endpoint;
  }
};

class packet_flow_cut_through_endpoint :
  public packet_flow_endpoint
{
 public:
  void
  handle_payload(const packet_flow_payload::ptr& msg);

  packet_flow_endpoint*
  clone() const {
    return new packet_flow_cut_through_endpoint;
  }
};

DeclareFactory(packet_flow_endpoint);


}
}

#endif // PACKETFLOW_ENDPOINT_H

