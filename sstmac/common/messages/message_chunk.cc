#include <sstmac/common/messages/message_chunk.h>
#include <sprockit/serializer.h>
#include <sstmac/hardware/common/unique_id.h>

namespace sstmac {

message_chunk::message_chunk(
  const sst_message::ptr& orig,
  long num_bytes,
  long byte_offset) :
 num_bytes_(num_bytes),
 byte_offset_(byte_offset),
 cumulative_delay_us_(0),
 unique_id_(orig->unique_id())
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
  //orig_ = is_tail ? orig : 0;
  orig_ = orig;
  sst_message::msgtype_ = orig->type();

#if SSTMAC_SANITY_CHECK
  if (orig->byte_length() < 256 && !orig_){
    spkt_throw_printf(sprockit::illformed_error,
        "message_chunk: chunk of size %ld, offset %ld for parent of size %ld should have link to orig_",
        num_bytes, byte_offset, orig->byte_length());
  }
#endif
}

void
message_chunk::serialize_order(sprockit::serializer& ser)
{
  sst_message::serialize_order(ser);
  ser & orig_;
  ser & num_bytes_;
  ser & byte_offset_;
  ser & unique_id_;
  ser & cumulative_delay_us_;
}

bool
message_chunk::is_tail() const
{
  //explicit cast needed for newer, crappy boost versions
  //return bool(orig_);
  return (byte_offset_ + num_bytes_) == orig_->byte_length();
}

}

