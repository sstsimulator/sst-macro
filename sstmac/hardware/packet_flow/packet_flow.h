#ifndef PACKETFLOW_H
#define PACKETFLOW_H

#ifndef PACKET_TRAIN_H
#define PACKET_TRAIN_H

#include <sstmac/hardware/router/routable_message.h>
#include <sstmac/common/messages/message_chunk.h>
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
  serialize_order(sprockit::serializer& ser);

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
  public routable,
  public packet_flow_interface,
  public message_chunk,
  public sprockit::serializable_type<packet_flow_payload>
{
 public:
  typedef sprockit::refcount_ptr<packet_flow_payload> ptr;
  typedef sprockit::refcount_ptr<const packet_flow_payload> const_ptr;

  static const double uninitialized_bw;

  ImplementSerializable(packet_flow_payload)

 public:
  packet_flow_payload(
    const sst_message::ptr& parent,
    int num_bytes,
    long offset);

  virtual ~packet_flow_payload() {}

  type_t
  type() const {
    return payload;
  }

  node_id
  toaddr() const {
    return routable::toaddr();
  }

  node_id
  fromaddr() const {
    return routable::fromaddr();
  }

  /**
    Needed because of routable_message ambiguity.
  */
  int
  vc() const {
    return vc_;
  }

  void
  update_vc() {
    if (routable::vc() == routing_info::uninitialized){
      vc_ = 0;
    } else {
      vc_ = routable::vc();
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
   @return The minimum number of bytes needed in a train. An 8K
   messages with min_bytes=4K will be split into two
   messages of size 4K.
   */
  static int
  min_num_bytes_per_packet() {
    return min_num_bytes_per_packet_;
  }

  /**
   @return Whether the parent message is being sent as a single
   packet_flow, i.e. the parent message is smaller
   than the min_bytes per train.
   */
  bool
  is_single_packet() const {
    return num_bytes_total() <= min_num_bytes_per_packet_;
  }

  /**
   @param num_bytes The number of bytes sent in a message
   @return Whether num_bytes would be sent as a single packet train
   */
  static bool
  is_single_packet(uint64_t num_bytes) {
    return num_bytes <= min_num_bytes_per_packet_;
  }

  /**
   @return The number of bytes in this packet_flow, NOT
   the total number of bytes in the parent message.
   See #num_bytes_total
   */
  long
  num_bytes() const {
    return num_bytes_;
  }

  double
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

  static void
  init_statics(int min_bytes);

  void
  serialize_order(sprockit::serializer& ser);

 protected:
  int inport_;

  double bw_;

  double max_in_bw_;

  double arrival_;

  static int min_num_bytes_per_packet_;

};

class packet_flow_credit :
  public sst_message,
  public packet_flow_interface,
  public sprockit::serializable_type<packet_flow_credit>
{

 public:
  typedef sprockit::refcount_ptr<packet_flow_credit> ptr;

  ImplementSerializable(packet_flow_credit)

 public:
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
  serialize_order(sprockit::serializer& ser);

 protected:
  int num_credits_;
  int port_;


};

}
}

#endif // PACKET_TRAIN_H


#endif // PACKETFLOW_H

