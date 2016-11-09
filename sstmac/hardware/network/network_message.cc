#include <sstmac/hardware/network/network_message.h>

#define enumcase(x) case x: return #x;

namespace sstmac {
namespace hw {

bool
network_message::is_nic_ack() const
{
  switch(type_)
  {
  case payload_sent_ack:
  case rdma_put_sent_ack:
  case rdma_get_sent_ack:
    return true;
  default:
    return false;
  }
}

void
network_message::convert_to_ack()
{
  reverse();
  switch(type_)
  {
    case rdma_get_payload:
      type_ = rdma_get_sent_ack;
      break;
    case rdma_put_payload:
      type_ = rdma_put_sent_ack;
      break;
    case payload:
      type_ = payload_sent_ack;
      break;
    default:
      spkt_throw_printf(sprockit::value_error,
        "network_message::clone_injection_ack: cannot ack msg type %s",
        tostr(type_));
      break;
  }
}

void
network_message::reverse()
{
  //also flip the addresses
  node_id dst = fromaddr_;
  node_id src = toaddr_;
  toaddr_ = dst;
  fromaddr_ = src;
}

const char*
network_message::tostr(nic_event_t mut)
{
  switch (mut) {
      enumcase(RDMA_GET_REQ_TO_RSP);
      enumcase(RDMA_GET_FAILED);
      enumcase(NVRAM_GET_REQ_TO_RSP);
  }
  spkt_throw_printf(sprockit::value_error,
       "network_message: invalid nic event %d", mut);
}

const char*
network_message::tostr(type_t ty)
{
  switch(ty)
  {
      enumcase(payload);
      enumcase(payload_sent_ack);
      enumcase(rdma_get_request);
      enumcase(rdma_get_payload);
      enumcase(rdma_get_sent_ack);
      enumcase(rdma_get_nack);
      enumcase(rdma_put_payload);
      enumcase(rdma_put_sent_ack);
      enumcase(rdma_put_nack);
      enumcase(null_netmsg_type);
      enumcase(nvram_get_request);
      enumcase(nvram_get_payload);
      enumcase(failure_notification);
  }
  spkt_throw_printf(sprockit::value_error,
    "network_message::tostr: unknown type_t %d",
    ty);
}

void
network_message::serialize_order(serializer& ser)
{
  ser & needs_ack_;
  ser & toaddr_;
  ser & fromaddr_;
  ser & flow_id_;
  ser & bytes_;
  ser & type_;
  message::serialize_order(ser);
}

bool
network_message::is_metadata() const
{
  switch(type_)
  {
    case nvram_get_request:
    case rdma_get_request:
    case rdma_get_sent_ack:
    case rdma_put_sent_ack:
    case payload_sent_ack:
    case rdma_get_nack:
    case rdma_put_nack:
    case failure_notification:
    case null_netmsg_type:
      return true;
    case payload:
    case rdma_get_payload:
    case nvram_get_payload:
    case rdma_put_payload:
      return false;
  }
}

void
network_message::nic_reverse(type_t newtype)
{
  reverse();
  type_ = newtype;
}

long
network_message::byte_length() const
{
  switch (type_)
  {
    case rdma_get_request:
    case rdma_get_sent_ack:
    case rdma_put_sent_ack:
    case payload_sent_ack:
    case rdma_get_nack:
    case rdma_put_nack:
      return 32; //hack for now CHANGE
    default:
      //never return less than 8 bytes - every message has to carry somethng
      return std::max(8L, bytes_);
  }
}

void
network_message::clone_into(network_message* cln) const
{
  cln->needs_ack_ = needs_ack_;
  cln->toaddr_ = toaddr_;
  cln->fromaddr_ = fromaddr_;
  cln->flow_id_ = flow_id_;
  cln->bytes_ = bytes_;
  cln->type_ = type_;
}

void
network_message::put_on_wire()
{
 //do nothing by default
}

}
}



