#ifndef PACKETFLOW_H
#define PACKETFLOW_H

#include <sstmac/hardware/common/packet.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/hardware/router/routing_enum.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

DeclareDebugSlot(packet_flow)
DeclareDebugSlot(packet_flow_queue)
DeclareDebugSlot(packet_flow_config)

namespace sstmac {
namespace hw {

class packet_flow_component
{
 public:
  virtual int initial_credits() const = 0;
};

class packet_flow_interface
{
 public:
  typedef enum {
    credit,
    payload
  } type_t;

 public:
  type_t
  type() const {
    return type_;
  }

  int
  vc() const {
    return vc_;
  }

  void
  set_vc(int vc) {
    vc_ = vc;
  }

  void
  serialize_order(serializer& ser);

  static const long infinity = -1;

  static const int unassigned_vc = -1;

 protected:
  packet_flow_interface(int vc, type_t ty)
    : vc_(vc), type_(ty)
  {
  }

  packet_flow_interface(type_t ty) :
    type_(ty), vc_(unassigned_vc)
  {
  }

  packet_flow_interface()
    : vc_(unassigned_vc) {
  }

 protected:
  int vc_;
  type_t type_;

};

/**
 @class packet_flow
 Encapsulates a group of machine packets traveling together on the
 same path between endpoints.  This is usually one fraction of
 a larger message.
 */
class packet_flow_payload :
  public packet_flow_interface,
  public packet
  //public serializable_type<packet_flow_payload>
{
 public:
  static const double uninitialized_bw;

  //ImplementSerializable(packet_flow_payload)

 public:
  packet_flow_payload(
    message* parent,
    int num_bytes,
    long offset);

  packet_flow_payload(){} //for serialization

  virtual ~packet_flow_payload() {}

  type_t
  type() const {
    return payload;
  }

  /**
    Needed because of routable_message ambiguity.
  */
  int
  vc() const {
    return vc_;
  }

  virtual int
  next_vc() const = 0;

  virtual int
  next_port() const = 0;

  void
  update_vc() {
    int new_vc = next_vc();
    if (new_vc == routing::uninitialized){
      vc_ = 0;
    } else {
      vc_ = new_vc;
    }
  }

  /**
   @return The total number of bytes in the complete message
   summed over all trains
   */
  long
  num_bytes_total() const {
    return orig_->byte_length();
  }

  /**
   @return The number of bytes in this packet_flow, NOT
   the total number of bytes in the parent message.
   See #num_bytes_total
   */
  int
  num_bytes() const {
    return num_bytes_;
  }

  timestamp
  arrival() const {
    return arrival_;
  }

  void
  set_arrival(double time) {
    arrival_ = time;
  }

  void
  init_bw(double bw) {
    bw_ = bw_ == uninitialized_bw ? bw : bw_;
  }

  void
  set_max_bw(double bw){
    init_bw(bw);
    bw_ = std::min(bw_, bw);
  }

  /**
   @return The bandwidth in number of bytes per second
   */
  double
  bw() const {
    return bw_;
  }

  /**
   @param The bandwidth in number of bytes per second
   */
  void
  set_bw(double bw) {
    bw_ = bw;
  }

  double
  max_incoming_bw() const {
    return max_in_bw_;
  }

  void
  set_max_incoming_bw(double bw) {
    max_in_bw_ = bw;
  }

  double
  ser_delay() const {
    return num_bytes_ / bw_;
  }

  void
  set_inport(int port) {
    inport_ = port;
  }

  int
  inport() const {
    return inport_;
  }

  std::string
  to_string() const;

  void
  serialize_order(serializer& ser);

 protected:
  int inport_;

  double bw_;

  double max_in_bw_;

  timestamp arrival_;

};

class packet_flow_credit :
  public event,
  public packet_flow_interface,
  public serializable_type<packet_flow_credit>
{

 public:
  ImplementSerializable(packet_flow_credit)

 public:
  packet_flow_credit(){} //for serialization

  packet_flow_credit(
    int port,
    int vc,
    long num_credits)
    : port_(port),
      num_credits_(num_credits),
      packet_flow_interface(vc, credit) {
  }

  type_t
  type() const {
    return credit;
  }

  int
  port() const {
    return port_;
  }

  virtual bool
  is_credit() const {
    return true;
  }

  virtual bool
  is_chunk() const {
    return true;
  }

  long
  num_credits() const {
    return num_credits_;
  }

  std::string
  to_string() const;

  void
  serialize_order(serializer& ser);

 protected:
  int num_credits_;
  int port_;


};

}
}


#endif // PACKETFLOW_H

