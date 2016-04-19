#include <sumi/message.h>
#include <sprockit/serializer.h>

DeclareSerializable(sumi::message)
DeclareSerializable(sumi::rdma_message)
DeclareSerializable(sumi::payload_message)

namespace sumi {

const int message::ack_size = 16;
const int message::header_size = 64;

bool
message::is_nic_ack() const
{
  switch(payload_type_)
  {
  case rdma_put_ack:
  case rdma_get_ack:
  case eager_payload_ack:
    return true;
  default:
    return false;
  }
}

message*
message::clone_ack() const
{
  message* ack = clone_msg();
  switch (payload_type()){
    case message::rdma_get:
      ack->set_payload_type(message::rdma_get_ack);
      break;
    case message::rdma_put:
      ack->set_payload_type(message::rdma_put_ack);
      break;
    default:
      spkt_throw_printf(sprockit::value_error,
        "message::clone_ack: invalid payload type %s",
        message::tostr(payload_type()));
  }
  return ack;
}

void*&
message::eager_buffer()
{
  //by default, messages have no extra eager buffer
  static void* no_buffer = 0;
  return no_buffer;
}

void
message::reverse()
{
  int tmp = sender_;
  sender_ = recver_;
  recver_ = tmp;
}

public_buffer&
message::local_buffer()
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "rdma_interface unimplemented for %s",
    to_string().c_str());
}

public_buffer&
message::remote_buffer()
{
  spkt_throw_printf(sprockit::unimplemented_error,
    "rdma_interface unimplemented for %s",
    to_string().c_str());
}

message*
message::clone() const
{
  message* cln = new message;
  clone_into(cln);
  return cln;
}

std::string
message::to_string() const
{
  return sprockit::printf("message %s %s %d->%d",
            tostr(payload_type_), tostr(class_),
            sender_, recver_);
}

#define enumcase(x) case x: return #x
const char*
message::tostr(payload_type_t ty)
{
  switch(ty) {
    enumcase(header);
    enumcase(eager_payload);
    enumcase(eager_payload_ack);
    enumcase(rdma_put);
    enumcase(rdma_get);
    enumcase(rdma_put_ack);
    enumcase(rdma_get_ack);
    enumcase(rdma_get_nack);
    enumcase(nvram_get);
    enumcase(failure);
    enumcase(none);
    enumcase(software_ack);
  }
  spkt_throw_printf(sprockit::value_error,
    "message::tostr: invalid payload type %d", ty);
}

const char*
message::tostr(class_t ty)
{
  switch(ty)
  {
    enumcase(pt2pt);
    enumcase(unexpected);
    enumcase(collective);
    enumcase(collective_done);
    enumcase(ping);
    enumcase(terminate);
    enumcase(no_class);
    enumcase(fake);
  }
  spkt_throw_printf(sprockit::value_error,
    "message::tostr: invalid message type %d", ty);
}

void
message::clone_into(message* cln) const
{
  cln->payload_type_ = payload_type_;
  cln->class_ = class_;
  cln->sender_ = sender_;
  cln->recver_ = recver_;
}

void
message::serialize_order(sprockit::serializer &ser)
{
  ser & sender_;
  ser & recver_;
  ser & class_;
  ser & payload_type_;
  ser & num_bytes_;
  ser & transaction_id_;
  ser & needs_send_ack_;
  ser & needs_recv_ack_;
}

void
payload_message::serialize_order(sprockit::serializer &ser)
{
  ser & sprockit::buffer(buffer_, num_bytes_);
  message::serialize_order(ser);
}

void
rdma_message::clone_into(rdma_message *cln) const
{
  cln->local_buffer_ = local_buffer_;
  cln->remote_buffer_ = remote_buffer_;
  message::clone_into(cln);
}

void
rdma_message::serialize_order(sprockit::serializer &ser)
{
  message::serialize_order(ser);
  ser & local_buffer_;
  ser & remote_buffer_;
}

}

