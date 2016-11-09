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
  public ::sstmac::library_interface
{
   ImplementSerializable(transport_message)

 public:
  transport_message(){} //needed for serialization

  transport_message(
     const std::string& libname,
     sw::app_id aid,
     const sumi::message_ptr& msg,
     long byte_length)
   : library_interface(libname),
      network_message(aid, byte_length),
      payload_(msg)
  {
  }

  virtual void
  serialize_order(serializer& ser) override;

  sumi::message_ptr
  payload() const {
    return payload_;
  }

  std::string
  to_string() const override;

  int
  dest_rank() const {
    return dest_;
  }

  void
  set_dest_rank(int dest) {
    dest_ = dest;
  }

  int
  src_rank() const {
    return src_;
  }

  void
  set_src_rank(int src) {
    src_ = src;
  }

  void
  set_apps(int src, int dst){
    src_app_ = src;
    dest_app_ = dst;
  }

  int
  src_app() const {
    return src_app_;
  }

  int
  dest_app() const {
    return dest_app_;
  }

  virtual void
  put_on_wire() override;

  ::sstmac::hw::network_message*
  clone_injection_ack() const override;

 protected:
  void
  clone_into(transport_message* cln) const;

  void
  reverse() override;

 private:
  sumi::message_ptr payload_;
  int src_;
  int dest_;
  int src_app_;
  int dest_app_;


};


}

#endif // MESSAGE_H
