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
  int num_bytes,
  bool is_tail) :
  packet(msg, num_bytes, is_tail),
  bw_(uninitialized_bw),
  max_in_bw_(1.0)
{
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
  ser & vc_;
}

void
pisces_routable_packet::serialize_order(serializer& ser)
{
  pisces_payload::serialize_order(ser);
  routable::serialize_order(ser);
}

void
pisces_default_packet::serialize_order(serializer& ser)
{
  pisces_routable_packet::serialize_order(ser);
  ser & flow_id_;
}

void
pisces_delay_stats_packet::serialize_order(serializer& ser)
{
  pisces_default_packet::serialize_order(ser);
  ser & congestion_delay_;
}

std::string
pisces_default_packet::to_string() const
{
  return sprockit::printf("flow %16lu%s, %d bytes bw=%8.4e %d->%d",
                   uint64_t(flow_id()),
                   is_tail_ ? " tail" : "",
                   num_bytes_, bw_,
                   int(fromaddr()), int(toaddr()), bw_);
}

std::string
pisces_credit::to_string() const
{
  return sprockit::printf("credits n=%d port=%d vc=%d",
                          num_credits_, port_, vc_);
}

void
pisces_credit::serialize_order(serializer& ser)
{
  event::serialize_order(ser);
  ser & num_credits_;
  ser & port_;
  ser & vc_;
}

}
}

