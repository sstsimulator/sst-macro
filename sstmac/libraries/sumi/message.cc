#include <sstmac/libraries/sumi/message.h>
#include <sprockit/serializer.h>
#include <sprockit/util.h>
#include <iostream>

DeclareSerializable(sstmac::transport_message)

namespace sstmac {
  
transport_message::transport_message(const payload_ptr& msg, long byte_length)
  : library_interface("sumi"),
    network_message(byte_length),
    payload_(msg),
    buffer_(0)
{
  sst_message::msgtype_ = NETWORK;
}

void
transport_message::serialize_order(sprockit::serializer& ser)
{
  network_message::serialize_order(ser);
  library_interface::serialize_order(ser);
  ser & payload_;
  if (network_message::is_metadata()){
    ser & buffer_; //just dump the void*, rdma request or similar
  } else { //I am a data thing, need to send the actual payload
    ser & sprockit::buffer(buffer_, bytes_);
  }
}

std::string
transport_message::to_string() const
{
  sst_message* msg = ptr_test_cast(sst_message, payload_);
  return sprockit::printf("sumi transport message %lu carrying %s",
    unique_id(), (msg ? msg->to_string().c_str() : "null"));
}

void
transport_message::put_on_wire()
{
  if (buffer_ && !is_metadata()){
    void* new_buf = new char[bytes_];
    ::memcpy(new_buf, buffer_, bytes_);
    buffer_ = new_buf;
  }
}

void
transport_message::complete_transfer(void *buf)
{
  if (buffer_){
    ::memcpy(buf, buffer_, bytes_);
  }
}

sstmac::hw::network_message*
transport_message::clone_injection_ack() const
{
#if SSTMAC_SANITY_CHECK
  if (network_message::type_ == network_message::null_netmsg_type){
    spkt_throw(sprockit::value_error,
        "message::clone_injection_ack: null network message type");
  }
#endif
  transport_message* cln = new transport_message;
  clone_into(cln);
#if SSTMAC_SANITY_CHECK
  if (cln->network_message::type() == network_message::null_netmsg_type){
    spkt_throw(sprockit::value_error,
        "message::clone_injection_ack: did not clone correctly");
  }
#endif
  cln->convert_to_ack();
  return cln;
}

void
transport_message::clone_into(transport_message* cln) const
{
  //the payload is actually immutable now - so this is safe
  cln->payload_ = payload_->clone();
  network_message::clone_into(cln);
  library_interface::clone_into(cln);
  cln->buffer_ = buffer_;
}

void
transport_message::reverse()
{
  //payload_->reverse();
  network_message::reverse();
}  
  
}
