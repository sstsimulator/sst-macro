#include <sstmac/hardware/common/packet.h>
#include <sstmac/hardware/common/unique_id.h>
#include <sstmac/common/messages/sst_message.h>
#include <sprockit/serializer.h>

namespace sstmac {
namespace hw {

packet::packet(
  message* orig,
  long num_bytes,
  long byte_offset) :
 num_bytes_(num_bytes),
 byte_offset_(byte_offset),
 flow_id_(orig->flow_id()),
 orig_(nullptr)
{
#if SSTMAC_SANITY_CHECK
  hw::unique_msg_id null_msg_id;
  if (unique_id_ == uint64_t(null_msg_id)){
    spkt_throw(sprockit::value_error,
        "message_chunk: chunk created for parent without unique id");
  }
#endif

  bool is_tail = (byte_offset + num_bytes) == orig->byte_length();
  //only carry the payload if you're the tail packet
  orig_ = is_tail ? orig : 0;

#if SSTMAC_SANITY_CHECK
  if (orig->byte_length() < 256 && !orig_){
    spkt_throw_printf(sprockit::illformed_error,
        "message_chunk: chunk of size %ld, offset %ld for parent of size %ld should have link to orig_",
        num_bytes, byte_offset, orig->byte_length());
  }
#endif
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
