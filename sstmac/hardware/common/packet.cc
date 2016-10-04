#include <sstmac/hardware/common/packet.h>
#include <sstmac/hardware/common/unique_id.h>
#include <sstmac/common/messages/sst_message.h>
#include <sprockit/serializer.h>

namespace sstmac {
namespace hw {

packet::packet(
  serializable* orig,
  int num_bytes,
  bool is_tail) :
 num_bytes_(num_bytes),
 is_tail_(is_tail),
 orig_(orig)
{
}

void
packet::serialize_order(serializer& ser)
{
  event::serialize_order(ser);
  ser & orig_;
  ser & num_bytes_;
  ser & is_tail_;
}

}
}
