#ifndef sstmac_hardware_packet_h
#define sstmac_hardware_packet_h

#include <sstmac/common/sst_event.h>
#include <sstmac/common/messages/sst_message_fwd.h>
#include <sprockit/metadata_bits.h>

namespace sstmac {
namespace hw {

class packet :
  public event,
  public sprockit::printable
{

 public:
  serializable*
  orig() const {
    return orig_;
  }

  virtual std::string
  to_string() const override {
    return "packet";
  }

  bool
  is_tail() const {
    return orig_;
  }

  int
  byte_length() const {
    return num_bytes_;
  }

  uint64_t
  flow_id() const {
    return flow_id_;
  }

  virtual void
  serialize_order(serializer& ser) override;

  virtual bool
  is_packet() const {
    return true;
  }

  virtual node_id
  toaddr() const = 0;

  virtual node_id
  fromaddr() const = 0;

  long
  byte_offset() const {
    return byte_offset_;
  }

  template <class T>
  T*
  interface(){
    T* t = dynamic_cast<T*>(this);
    return t;
  }

 protected:
  packet() : orig_(nullptr) {}

  packet(serializable* orig,
    uint64_t flow_id,
    long num_bytes,
    long byte_offset);

 protected:
  serializable* orig_;

  int num_bytes_;

  long byte_offset_;

  uint64_t flow_id_;

};


}
}

#endif

