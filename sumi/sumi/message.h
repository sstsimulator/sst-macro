#ifndef sumi_api_MESSAGE_H
#define sumi_api_MESSAGE_H

#include <sprockit/util.h>
#include <sprockit/ptr_type.h>
#include <sumi/rdma_interface.h>
#include <sumi/serialization.h>

START_SERIALIZATION_NAMESPACE
template <>
class serialize<sumi::public_buffer>
{
 public:
  void
  operator()(sumi::public_buffer& buf, serializer& ser){
    ser.primitive(buf);
  }
};
END_SERIALIZATION_NAMESPACE

namespace sumi {



class message :
  public sprockit::ptr_type,
  public sumi::serializable,
  public sumi::serializable_type<message>
{
 ImplementSerializableDefaultConstructor(message)

 public:
  virtual std::string
  to_string() const;

  typedef enum {
    header,
    eager_payload,
    eager_payload_ack,
    software_ack,
    nvram_get,
    rdma_put,
    rdma_put_ack,
    rdma_get,
    rdma_get_ack,
    rdma_get_nack,
    failure,
    none
  } payload_type_t;

 typedef enum {
    terminate,
    pt2pt,
    unexpected,
    collective,
    collective_done,
    ping,
    no_class,
    fake
 } class_t;

 public:
  static const int ack_size;
  static const int header_size;

  typedef sprockit::refcount_ptr<message> ptr;

  message() :
   payload_type_(none),
   class_(pt2pt),
   transaction_id_(-1),
   needs_send_ack_(false),
   needs_recv_ack_(false)
  {
    num_bytes_ = sizeof(message);
  }

  message(long num_bytes) :
   payload_type_(none),
   class_(pt2pt),
   transaction_id_(-1),
   num_bytes_(num_bytes),
   needs_send_ack_(false),
   needs_recv_ack_(false)
  {
  }

  virtual public_buffer&
  local_buffer();

  virtual public_buffer&
  remote_buffer();

  virtual void*&
  eager_buffer();

  static const char*
  tostr(payload_type_t ty);

  static const char*
  tostr(class_t ty);

  payload_type_t
  payload_type() const {
    return payload_type_;
  }

  bool
  is_nic_ack() const;

  virtual void
  serialize_order(sumi::serializer &ser);

  void
  set_payload_type(payload_type_t ty) {
    payload_type_ = ty;
  }

  virtual message*
  clone() const;

  message*
  clone_ack() const;

  message*
  clone_msg() const {
    return clone();
  }

  class_t
  class_type() const {
    return class_;
  }

  void
  set_class_type(class_t cls) {
    class_ = cls;
  }

  int
  recver() const {
    return recver_;
  }

  void
  set_recver(int dst) {
    recver_ = dst;
  }

  int
  sender() const {
    return sender_;
  }

  void
  set_sender(int src) {
    sender_ = src;
  }

  long
  byte_length() const {
    return num_bytes_;
  }

  void
  set_byte_length(long bytes) {
    num_bytes_ = bytes;
  }

  int
  transaction_id() const {
    return transaction_id_;
  }

  void
  set_transaction_id(int tid) {
    transaction_id_ = tid;
  }

  bool
  has_transaction_id() const {
    return transaction_id_ >= 0;
  }

  virtual void
  reverse();

  bool
  needs_send_ack() const {
    return needs_send_ack_;
  }

  void
  set_needs_send_ack(bool need) {
    needs_send_ack_ = need;
  }

  bool
  needs_recv_ack() const {
    return needs_recv_ack_;
  }

  void
  set_needs_recv_ack(bool need) {
    needs_recv_ack_ = need;
  }

 protected:
  void
  clone_into(message* cln) const;

 protected:
  payload_type_t payload_type_;

  class_t class_;

  int sender_;

  int recver_;

  int transaction_id_;

  long num_bytes_;

  bool needs_send_ack_;

  bool needs_recv_ack_;

};

class payload_message :
  public message,
  public sumi::serializable_type<payload_message>
{
  ImplementSerializable(payload_message)

 public:
  payload_message(){} //for serialization

  payload_message(void* buffer, int num_bytes)
  {
    class_ = pt2pt;
    num_bytes_ = num_bytes;
  }

  void
  serialize_order(sumi::serializer &ser);

 private:
  void* buffer_;
};

class rdma_message :
  public message,
  public sumi::serializable_type<rdma_message>
{
  ImplementSerializableDefaultConstructor(rdma_message)
  ImplementRdmaAPI

 public:
  typedef sprockit::refcount_ptr<rdma_message> ptr;

 public:
  rdma_message()
  {
    class_ = pt2pt;
  }

  rdma_message(long num_bytes) :
    message(num_bytes)
  {
  }

  virtual message*
  clone() const {
    rdma_message* ptr = new rdma_message;
    clone_into(ptr);
    return ptr;
  }

  virtual void
  serialize_order(sumi::serializer& ser);

 protected:
  void
  clone_into(rdma_message* cln) const;

};


}

#endif // SIMPLE_MESSAGE_H

