#include <sstmac/libraries/sumi/message.h>
#include <sstmac/common/serializable.h>
#include <sumi/message.h>
#include <sprockit/util.h>
#include <sprockit/printable.h>
#include <iostream>

namespace sstmac {
  
void
transport_message::serialize_order(serializer& ser)
{
  network_message::serialize_order(ser);
  library_interface::serialize_order(ser);
  sumi::message* msg = payload_.get();
  ser & msg;
  payload_ = msg;
  ser & src_;
  ser & dest_;
  ser & src_app_;
  ser & dest_app_;
}

std::string
transport_message::to_string() const
{
  return sprockit::printf("sumi transport message %lu to node %d from %d:%d to %d:%d carrying %s",
    flow_id(), toaddr_, src_, src_app_, dest_, dest_app_, sprockit::to_string(payload_.get()).c_str());
}

void
transport_message::put_on_wire()
{
  if (!is_metadata()){
    payload_->buffer_send();
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
  cln->src_app_ = src_app_;
  cln->dest_app_ = dest_app_;
  cln->src_ = src_;
  cln->dest_ = dest_;
  network_message::clone_into(cln);
  library_interface::clone_into(cln);
}

void
transport_message::reverse()
{
  //payload_->reverse();
  network_message::reverse();
  int src = src_;
  int dst = dest_;
  src_ = dst;
  dest_ = src;

  src = src_app_;
  dst = dest_app_;
  src_app_ = dst;
  dest_app_ = src;
}  
  
}
