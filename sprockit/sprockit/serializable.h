/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
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