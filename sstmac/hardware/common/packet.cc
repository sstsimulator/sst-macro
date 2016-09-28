#include <sstmac/hardware/common/packet.h>
#include <sstmac/hardware/common/unique_id.h>
#include <sstmac/common/messages/sst_message.h>
#include <sprockit/serializer.h>

namespace sstmac {
namespace hw {

packet::packet(
  serializable* orig,
  uint64_t flow_id,
  long num_bytes,
  long byte_offset) :
 num_bytes_(num_bytes),
 byte_offset_(byte_offset),
 flow_id_(flow_id),
 orig_(orig)
{
}

void
packet::serialize_order(serializer& ser)
{
  event::serialize_order(ser);
  ser & orig_;
  ser & num_bytes_;
  ser & byte_offset_;
  ser & flow_id_;
}

}
}
