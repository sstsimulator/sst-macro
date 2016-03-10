#ifndef sstmac_hardware_network_NETWORK_MESSAGE_H
#define sstmac_hardware_network_NETWORK_MESSAGE_H

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/hardware/router/routing_enum.h>
#include <sstmac/hardware/network/network_id.h>
#include <sstmac/software/process/task_id.h>

namespace sstmac {
namespace hw {

class network_message :
  public sst_message,
  public sprockit::serializable_type<network_message>
{
  ImplementSerializableDefaultConstructor(network_message)

 public:
  typedef enum {
    RDMA_GET_FAILED,
    RDMA_GET_REQ_TO_RSP,
    NVRAM_GET_REQ_TO_RSP
  } nic_event_t;

  typedef enum {
    null_netmsg_type=0,
    rdma_get_request=1,
    rdma_get_sent_ack=2,
    rdma_get_nack=3,
    rdma_put_sent_ack=4,
    rdma_put_nack=5,
    payload_sent_ack=6,
    payload=7,
    rdma_get_payload=8,
    rdma_put_payload=9,
    nvram_get_request=10,
    nvram_get_payload=11,
    failure_notification=12
  } type_t;

  static message_type_t NETWORK;

 public:
  network_message(node_id toaddr,
                  node_id fromaddr,
                  sw::task_id src,
                  sw::task_id dst,
                  long payload_bytes);

  network_message(long payload_bytes);

  network_message();

  virtual ~network_message() {}

  static const char*
  tostr(nic_event_t mut);

  static const char*
  tostr(type_t ty);

  bool
  is_metadata() const;

  virtual network_message*
  clone_injection_ack() const {
    network_message* cln = new network_message;
    clone_into(cln);
    return cln;
  }

  routing::algorithm_t
  route_algo() const {
    return route_algo_;
  }

  virtual void
  nic_reverse(type_t newtype);

  void
  set_route_algo(routing::algorithm_t algo) {
    route_algo_ = algo;
  }

  bool
  is_nic_ack() const;

  node_id
  toaddr() const {
    return toaddr_;
  }

  virtual void
  put_on_wire();

  node_id
  fromaddr() const {
    return fromaddr_;
  }

  void
  set_toaddr(node_id addr) {
    toaddr_ = addr;
  }

  void
  set_fromaddr(node_id addr) {
    fromaddr_ = addr;
  }

  void
  set_needs_ack(bool n) {
    needs_ack_ = n;
  }

  virtual bool
  needs_ack() const {
    //only paylods get acked
    return needs_ack_ && type_ >= payload;
  }

  virtual void
  serialize_order(sprockit::serializer& ser);

  void
  convert_to_ack();

  void
  set_net_id(const network_id& id) {
    net_id_ = id;
  }

  network_id
  net_id() const {
    return net_id_;
  }

  uint64_t
  unique_id() const {
    return uint64_t(net_id_);
  }

  sw::task_id
  source_task() const {
    return src_task_;
  }

  sw::task_id
  dest_task() const {
    return dest_task_;
  }

  type_t
  type() const {
    return type_;
  }

  void
  set_type(type_t ty){
    type_ = ty;
  }

  long
  source_thread(sw::operating_system* os) const;

  long
  dest_thread(sw::operating_system* os) const;

  virtual void
  reverse();

  long
  byte_length() const;

  node_id toaddr_;

  node_id fromaddr_;

 protected:
  void
  clone_into(network_message* cln) const;

 protected:
  network_id net_id_;

  bool needs_ack_;

  routing::algorithm_t route_algo_;

  sw::task_id src_task_;

  sw::task_id dest_task_;

  long bytes_;

  type_t type_;

};

}
}
#endif // NETWORK_MESSAGE_H

