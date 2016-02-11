#ifndef sumi_message_h
#define sumi_message_h

#include <sstmac/hardware/network/network_message.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/process/key_fwd.h>
#include <sstmac/common/messages/library_message.h>
#include <sprockit/serializer_fwd.h>
#include <sprockit/ser_ptr_type.h>
#include <sprockit/clonable.h>

namespace sstmac {
namespace sumi {

class transport_message :
  public sstmac::hw::network_message,
  public sstmac::library_interface,
  public sprockit::serializable_type<transport_message>
{
   ImplementSerializable(transport_message)
   typedef sprockit::clonable<sprockit::serializable_ptr_type> payload_t;
   typedef sprockit::refcount_ptr<payload_t> payload_ptr;

 public:
  typedef sprockit::refcount_ptr<transport_message> ptr;

  transport_message(const payload_ptr& msg, long byte_length);

  virtual void
  serialize_order(sprockit::serializer& ser);

  serializable_ptr_type::ptr
  payload() const {
    return payload_;
  }

  std::string
  to_string() const;

  int
  dest() const {
    return dest_task_;
  }

  void
  set_dest(int dest) {
    dest_task_ = sstmac::sw::task_id(dest);
  }

  int
  src() const {
    return src_task_;
  }

  void
  set_src(int src) {
    src_task_ = sstmac::sw::task_id(src);
  }

  void
  set_buffer(void* buf){
    buffer_ = buf;
  }

  void*
  buffer() const {
    return buffer_;
  }

  virtual void
  put_on_wire();

  void
  complete_transfer(void* buf);

  sstmac::hw::network_message::ptr
  clone_injection_ack() const;

 protected:
  void
  clone_into(const ptr &cln) const;

  void
  reverse();

 private:
  payload_ptr payload_;
  void* buffer_;

};


}}

#endif // MESSAGE_H
