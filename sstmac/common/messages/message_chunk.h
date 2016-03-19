#ifndef MESSAGE_CHUNK_H
#define MESSAGE_CHUNK_H

#include <sstmac/common/messages/sst_message.h>
#include <sprockit/metadata_bits.h>

namespace sstmac {

class packet :
  public event
{

 public:
  message*
  orig() const {
    return orig_;
  }

  virtual std::string
  to_string() const {
    return "packet";
  }

  bool
  is_tail() const {
    return orig_;
  }

  long
  byte_length() const {
    return num_bytes_;
  }

  uint64_t
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
  is_packet() const {
    return true;
  }

  virtual node_id
  toaddr() const = 0;

  virtual node_id
  fromaddr() const = 0;

 protected:
  packet() : orig_(0) {}

  packet(
    message* orig,
    long num_bytes,
    long byte_offset);

 protected:
  message* orig_;

  long num_bytes_;

  long byte_offset_;

  uint64_t unique_id_;

  double cumulative_delay_us_;

};


}

#endif // MESSAGE_CHUNK_H

