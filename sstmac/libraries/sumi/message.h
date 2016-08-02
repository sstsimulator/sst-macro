#ifndef sumi_message_h
#define sumi_message_h

#include <sstmac/hardware/network/network_message.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/process/key_fwd.h>
#include <sstmac/common/messages/library_message.h>
#include <sstmac/libraries/sumi/message_fwd.h>
#include <sumi/message_fwd.h>

namespace sstmac {

class transport_message :
  public ::sstmac::hw::network_message,
  public ::sstmac::library_interface,
  public serializable_type<transport_message>
{
   ImplementSerializable(transport_message)

 public:
  transport_message(){} //needed for serialization

  transport_message(sw::app_id aid,
     const sumi::message_ptr& msg,
     long byte_length);

  virtual void
  serialize_order(serializer& ser);

  sumi::message_ptr
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
    dest_task_ = ::sstmac::sw::task_id(dest);
  }

  int
  src() const {
    return src_task_;
  }

  void
  set_src(int src) {
    src_task_ = ::sstmac::sw::task_id(src);
  }

  virtual void
  put_on_wire();

  ::sstmac::hw::network_message*
  clone_injection_ack() const;

 protected:
  void
  clone_into(transport_message* cln) const;

  void
  reverse();

 private:
  sumi::message_ptr payload_;

};


}

#endif // MESSAGE_H
