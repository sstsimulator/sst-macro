#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sprockit/serializer.h>

DeclareSerializable(sstmac::hw::packet_flow_payload);
DeclareSerializable(sstmac::hw::packet_flow_credit);


RegisterDebugSlot(packet_flow,
    "print all the details of the packet_flow model including crossbar arbitration"
    ", buffer occupancies, and queue depths. This can be a LOT of information. User beware")

RegisterDebugSlot(packet_flow_queue,
    "print all the details of queue lengths in the packet flow model");

RegisterDebugSlot(packet_flow_config,
    "print all the details of the initial configuration of packet flow connections/credits/buffers/vcs");

namespace sstmac {
namespace hw {

std::string packet_flow_payload::default_arb_ = "simple";
int packet_flow_payload::min_num_bytes_per_packet_ = 0;
const double packet_flow_payload::uninitialized_bw = -1;

void
packet_flow_interface::serialize_order(sprockit::serializer& ser)
{
  ser & type_;
  ser & vc_;
}

packet_flow_payload::packet_flow_payload(
  const sst_message::ptr& parent,
  int num_bytes,
  long offset) :
  message_chunk(parent, num_bytes, offset),
  routable(parent->toaddr(), parent->fromaddr()),
  packet_flow_interface(payload),
  bw_(uninitialized_bw)
{
  sst_message::msgtype_ = parent->type();
}

void
packet_flow_payload::init_statics(int min_bytes)
{
  min_num_bytes_per_packet_ = min_bytes;
}

void
packet_flow_payload::set_min_bw(double bw)
{
  init_bw(bw);
  bw_ = std::min(bw_, bw);
}

std::string
packet_flow_payload::to_string() const
{
  return sprockit::printf("flow %16lu, %5lu:%5lu bw=%8.4e %s",
                   uint64_t(unique_id()),
                   byte_offset_,
                   byte_offset_ + num_bytes_,
                   bw_, "");
                   //(orig() ? orig()->to_string().c_str() : "no parent"));
}

void
packet_flow_payload::serialize_order(sprockit::serializer& ser)
{
  packet_flow_interface::serialize_order(ser);
  routable::serialize_order(ser);
  message_chunk::serialize_order(ser);
  ser & inport_;
  ser & bw_;
  ser & arrival_;
}

std::string
packet_flow_credit::to_string() const
{
  return sprockit::printf("credits, num=%d", num_credits_);
}

void
packet_flow_credit::serialize_order(sprockit::serializer& ser)
{
  sst_message::serialize_order(ser);
  packet_flow_interface::serialize_order(ser);
  ser & num_credits_;
  ser & port_;
}


}
}

