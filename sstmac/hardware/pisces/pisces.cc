#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/router/routable.h>

RegisterDebugSlot(pisces,
    "print all the details of the pisces model including crossbar arbitration"
    ", buffer occupancies, and queue depths. This can be a LOT of information. User beware")

RegisterDebugSlot(pisces_queue,
    "print all the details of queue lengths in the packet flow model");

RegisterDebugSlot(pisces_config,
    "print all the details of the initial configuration of packet flow connections/credits/buffers/vcs");

namespace sstmac {
namespace hw {

const double pisces_payload::uninitialized_bw = -1;

pisces_payload::pisces_payload(
  serializable* msg,
  uint64_t flow_id,
  int num_bytes,
  long offset) :
  packet(msg, flow_id, num_bytes, offset),
  //routable(parent->toaddr(), parent->fromaddr()),
  bw_(uninitialized_bw),
  max_in_bw_(1.0)
{
}

std::string
pisces_payload::to_string() const
{
  return sprockit::printf("flow %16lu, %5lu:%5lu bw=%8.4e %d->%d",
                   uint64_t(flow_id()),
                   byte_offset_,
                   byte_offset_ + num_bytes_, bw_,
                   int(fromaddr()), int(toaddr()), bw_);
}

void
pisces_payload::serialize_order(serializer& ser)
{
  //routable::serialize_order(ser);
  packet::serialize_order(ser);
  ser & inport_;
  ser & bw_;
  ser & max_in_bw_;
  ser & arrival_;
}

std::string
pisces_credit::to_string() const
{
  return sprockit::printf("credits, num=%d", num_credits_);
}

void
pisces_credit::serialize_order(serializer& ser)
{
  event::serialize_order(ser);
  ser & num_credits_;
  ser & port_;
}

}
}

