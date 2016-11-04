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

#define NotSerializable(obj) \
 public: \
  static void \
  throw_exc(){ \
     spkt_throw_printf(sprockit::illformed_error, \
      "type %s should not be serialized", \
      #obj); \
  } \
  virtual void \
  serialize_order(sprockit::serializer& sst) override { \
    throw_exc(); \
  } \
  virtual uint32_t \
  cls_id() const override { \
    throw_exc(); \
    return -1; \
  } \
  static obj* \
  construct_deserialize_stub() { \
    throw_exc(); \
    return 0; \
  } \
  virtual const char* \
  cls_name() const override { \
    throw_exc(); \
    return ""; \
  }

#define ImplementSerializableDefaultConstructor(obj) \
 public: \
  virtual const char* \
  cls_name() const override { \
    return #obj; \
  } \
  virtual uint32_t \
  cls_id() const override { \
    return ::sprockit::serializable_builder_impl< obj >::static_cls_id(); \
  } \
  static obj* \
  construct_deserialize_stub() { \
    return new obj; \
  }

#define ImplementSerializable(obj) \
 public: \
  ImplementSerializableDefaultConstructor(obj)


class serializable_builder
{
 public:
  virtual serializable*
  build() const = 0;

  virtual ~serializable_builder(){}

  virtual const char*
  name() const = 0;

  virtual uint32_t
  cls_id() const = 0;

  virtual bool
  sanity(serializable* ser) = 0;
};

template<class T>
class serializable_builder_impl : public serializable_builder
{
 protected:
  static const char* name_;
  static const uint32_t cls_id_;

 public:
  serializable*
  build() const {
    return T::construct_deserialize_stub();
  }

  const char*
  name() const {
    return name_;
  }

  uint32_t
  cls_id() const {
    return cls_id_;
  }

  static uint32_t
  static_cls_id() {
    return cls_id_;
  }

  static const char*
  static_cls_name() {
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
  add_builder(serializable_builder* builder, const char* name);

  static bool
  sanity(serializable* ser, uint32_t cls_id) {
    return (*builders_)[cls_id]->sanity(ser);
  }

  static void
  delete_statics();

};

template<class T> const char* serializable_builder_impl<T>::name_ = typeid(T).name();
template<class T> const uint32_t serializable_builder_impl<T>::cls_id_
  = serializable_factory::add_builder(new serializable_builder_impl<T>,
                                      typeid(T).name());

}

#include <sprockit/serialize_serializable.h>


#endif

