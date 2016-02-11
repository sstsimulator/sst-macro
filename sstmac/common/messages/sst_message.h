/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_COMMON_MESSAGES_SST_MESSAGE_H_INCLUDED
#define SSTMAC_COMMON_MESSAGES_SST_MESSAGE_H_INCLUDED


#include <sprockit/ser_ptr_type.h>
#include <sprockit/serializable.h>
#include <sstmac/common/node_address.h>

#include <sprockit/expandable_enum.h>
#include <sprockit/metadata_bits.h>

#include <sstmac/common/event_callback_fwd.h>
#include <sstmac/common/event_handler_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/process/key_fwd.h>

namespace sstmac {

/**
 * A class describing an event.
 */
class sst_message :
  public sprockit::serializable_ptr_type,
  public sprockit::serializable_type<sst_message>
{
  ImplementSerializableDefaultConstructor(sst_message)

 public:
  typedef sprockit::refcount_ptr<sst_message> ptr;
  typedef sprockit::refcount_ptr<const sst_message> const_ptr;

  declare_expandable_enum(message_type_t);
  declare_expandable_enum(field);

  static message_type_t SST;
  static message_type_t NONE;

 public:
  virtual ~sst_message() {}

  virtual std::string
  to_string() const {
    return "sst_message";
  }

  // --------------------------------------//

 public:
  sst_message();

  /**
   * Virtual function to return size. Child classes should impement this
   * if they want any size tracked / modeled.
   * @return Zero size, meant to be implemented by children.
   */
  virtual long
  byte_length() const;

  /**
   * Serialize this message during parallel simulation.
   * @param ser The serializer to use
   */
  virtual void
  serialize_order(sprockit::serializer& ser);

  /**
   * Message type getter
   * @return message type (child class)
   */
  message_type_t
  type() const {
    return msgtype_;
  }

  void
  set_type(message_type_t t) {
    msgtype_ = t;
  }

  virtual node_id
  toaddr() const {
    return node_id();
  }

  virtual node_id
  fromaddr() const {
    return node_id();
  }

  virtual bool
  is_chunk() const {
    return false;
  }

  virtual bool
  is_credit() const {
    return false;
  }

  /**
    Many message types carry a "payload" - a reference to a parent message.
    This happens often enough to make it a top-level virtual function.
    If the message has no parent, it is its own parent.
  */
  virtual sst_message::ptr
  parent() const;

  virtual uint64_t
  unique_id() const;
  
  bool
  has_key() const {
   return key_;
  }
  
  sw::key*
  key() const {
    return key_;
  }
  
  void
  set_key(sw::key* k){
    key_ = k;
  }

  template <class T>
  T&
  get_field(field name){
    uint64_t* ptr = &fields_[name];
    return *reinterpret_cast<T*>(ptr);
  }

  template <class T>
  T*
  interface(){
    T* t = dynamic_cast<T*>(this);
    return t;
  }

 protected:
  message_type_t msgtype_;
  sw::key* key_;
  std::map<field, uint64_t> fields_;

};

implement_enum_functions(sst_message::message_type_t)

implement_enum_functions(sst_message::field)




} // end of namespace sstmac
#endif

