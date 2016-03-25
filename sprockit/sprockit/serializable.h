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

#ifndef SPROCKIT_COMMON_MESSAGES_SERIALIZABLE_H_INCLUDED
#define SPROCKIT_COMMON_MESSAGES_SERIALIZABLE_H_INCLUDED

#include <sprockit/serializable_type.h>
#include <sprockit/unordered.h>
#include <typeinfo>
#include <stdint.h>

namespace sprockit {

#define ImplementVirtualSerializable(obj) \
    protected: \
        obj(cxn_flag_t flag){}

#define NotSerializable(obj) \
 public: \
  static void \
  throw_exc(){ \
     spkt_throw_printf(sprockit::illformed_error, \
      "type %s should not be serialized", \
      #obj); \
  } \
  virtual void \
  serialize_order(sprockit::serializer& sst){ \
    throw_exc(); \
  } \
  virtual uint32_t \
  cls_id() const { \
    throw_exc(); \
    return -1; \
  } \
  static obj* \
  construct_deserialize_stub() { \
    throw_exc(); \
    return 0; \
  } \
  virtual std::string \
  serialization_name() const { \
    throw_exc(); \
    return ""; \
  } \
  virtual const char* \
  cls_name() const { \
    throw_exc(); \
    return ""; \
  } \
  virtual obj* \
  you_forgot_to_add_ImplementSerializable_to_this_class() { \
    return 0; \
  } \

#define ImplementSerializableDefaultConstructor(obj) \
 public: \
  virtual const char* \
  cls_name() const { \
    return #obj; \
  } \
  virtual uint32_t \
  cls_id() const { \
    return ::sprockit::serializable_type< obj >::cls_id(); \
  } \
  static obj* \
  construct_deserialize_stub() { \
    return new obj; \
  } \
  virtual std::string \
  serialization_name() const { \
    return #obj; \
  } \
  virtual obj* \
  you_forgot_to_add_ImplementSerializable_to_this_class() { \
    return 0; \
  } \

#define ImplementSerializable(obj) \
 public: \
  obj(){} \
 ImplementSerializableDefaultConstructor(obj)


class serializable_builder
{
 public:
  virtual serializable*
  build() const = 0;

  virtual ~serializable_builder(){}

  virtual const char*
  name() const = 0;

  virtual bool
  sanity(serializable* ser) = 0;
};

template<class T>
class serializable_builder_impl : public serializable_builder
{
 protected:
  static const char* name_;

 public:
  serializable*
  build() const {
    return T::construct_deserialize_stub();
  }

  const char*
  name() const {
    return name_;
  }

  bool
  sanity(serializable* ser) {
    return (typeid(T) == typeid(*ser));
  }
};


class serializable_factory
{
 protected:
  typedef spkt_unordered_map<long, serializable_builder*> builder_map;
  static builder_map* builders_;

 public:
  static serializable*
  get_serializable(uint32_t cls_id);

  /**
      @return The cls id for the given builder
  */
  static uint32_t
  add_builder(serializable_builder* builder);

  static bool
  sanity(serializable* ser, uint32_t cls_id) {
    return (*builders_)[cls_id]->sanity(ser);
  }

  static void
  delete_statics();

};

}

#include <sprockit/serialize_serializable.h>

#define SerializableName(obj) #obj

#define DeclareSerializable(...) \
namespace sprockit { \
template<> const char* serializable_builder_impl<__VA_ARGS__ >::name_ = SerializableName((__VA_ARGS__)); \
template<> uint32_t serializable_type<__VA_ARGS__ >::cls_id_ = serializable_factory::add_builder(new serializable_builder_impl<__VA_ARGS__ >); \
}

#endif

