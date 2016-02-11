#ifndef MESSAGE_CHUNK_H
#define MESSAGE_CHUNK_H

#include <sstmac/common/messages/sst_message.h>
#include <sprockit/metadata_bits.h>

namespace sstmac {

class message_chunk :
  public sst_message
{

 public:
  typedef sprockit::refcount_ptr<message_chunk> ptr;

  sst_message::ptr
  orig() const {
    return orig_;
  }

  virtual std::string
  to_string() const = 0;

  void
  set_orig(const sst_message::ptr& orig){
    orig_ = orig;
  }

  sst_message::ptr
  parent() const {
    return orig_;
  }

  bool
  is_tail() const;

  virtual long
  byte_length() const {
    return num_bytes_;
  }

  virtual uint64_t
  unique_id() const {
    return unique_id_;
  }

  void
  add_delay_us(double delay_us){
    cumulative_delay_us_ += delay_us;
  }

  double
  delay_us() const {
    return cumulative_delay_us_;
  }

  virtual void
  serialize_order(sprockit::serializer& ser);

  virtual bool
  is_chunk() const {
    return true;
  }

 protected:
  message_chunk() {}

  message_chunk(
    const sst_message::ptr& orig,
    long num_bytes,
    long byte_offset);

 protected:
  sst_message::ptr orig_;

  long num_bytes_;

  long byte_offset_;

  uint64_t unique_id_;

  double cumulative_delay_us_;

};


}

#endif // MESSAGE_CHUNK_H

