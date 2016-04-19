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

#ifndef SSTMAC_COMMON_MESSAGES_VALUEPAYLOAD_H_INCLUDED
#define SSTMAC_COMMON_MESSAGES_VALUEPAYLOAD_H_INCLUDED

#include <sstmac/common/messages/payload.h>
#include <sstream>

namespace sstmac {

/* Utility classes */

template<typename T>
class bool_result
{
 public:
  static payload::const_ptr
  get_true();
  static payload::const_ptr
  get_false();
};

template<>
class bool_result<int>
{
 public:
  static payload::const_ptr
  get_true();
  static payload::const_ptr
  get_false();
};

template<>
class bool_result<bool>
{
 public:
  static payload::const_ptr
  get_true();
  static payload::const_ptr
  get_false();
};

/**
 * Network payload consisting of a value with sensible copy semantics.
 *
 * Don't use this for pointer types, since you will break assumptions
 * about const-ness or protection of underlying data.
 */
template<typename T>
class value_payload :
  public payload,
  public sprockit::serializable_type<value_payload<T> >
{
  ImplementSerializable(value_payload)



 public:
  typedef sprockit::refcount_ptr<value_payload<T> > ptr;
  typedef sprockit::refcount_ptr<const value_payload<T> > const_ptr;

  virtual
  ~value_payload() {
  }

  /// Construct a new valuepayload.
  static typename value_payload::const_ptr
  construct(const T &inval) {
    return new value_payload<T>(inval);
  }

  static typename value_payload::const_ptr
  construct(const T &inval, size_t s) {
    return new value_payload<T>(inval, s);
  }

  /// Construct a new valuepayload with type conversion.
  template<typename Other>
  static typename value_payload::const_ptr
  construct(const Other &inval) {
    return new value_payload<T>(inval);
  }

  static typename value_payload::ptr
  construct() {
    return new value_payload;
  }

  /// Clone this object.
  payload::const_ptr
  clone() const {
    return new value_payload<T>(*this);
  }

  /// Access the underlying data in a const context.
  T&
  typed_data() const {
    return data_;
  }

  void*
  data() const {
    return &data_;
  }

  virtual long
  byte_length() const {
    return size_;
  }

  /**
   * Add operator
   * @param other
   * @return a smart pointer to a new valuepayload object
   */
  virtual payload::const_ptr
  add(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const value_payload<T>, other);
    T newval;
    if (casted) {
      newval = casted->typed_data() + typed_data();
    }
    else {
      newval = typed_data();
    }
    payload::const_ptr ret = construct(newval);
    return ret;
  }

  virtual payload::const_ptr
  prod(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const value_payload<T>, other);
    T newval;
    if (casted) {
      newval = casted->typed_data() * typed_data();
    }
    else {
      newval = typed_data();
    }
    payload::const_ptr ret = construct(newval);
    return ret;
  }

  /**
   * Less than comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  min(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const value_payload<T>, other);
    return (typed_data() < casted->typed_data()) ? construct(typed_data()) : construct(
             casted->typed_data());

  }

  /**
   * Greater than comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  max(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const value_payload<T>, other);
    if (casted)
      return (typed_data() > casted->typed_data()) ? construct(typed_data()) : construct(
               casted->typed_data());
    else {
      return construct(typed_data());
    }

  }

  /**
   * Equals comparator
   * @param other
   * @return
   */
  virtual bool
  equals(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const value_payload<T>, other);
    return typed_data() == casted->typed_data();
  }

  /**
   * Logical or comparator
   * @param other
   * @return
   */
  payload::const_ptr
  logical_or(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const value_payload<T>, other);
    return (typed_data() || casted->typed_data()) ? bool_result<T>::get_true()
           : bool_result<T>::get_false();
  }

  /**
   * Logical xor comparator
   * @param other
   * @return
   */
  payload::const_ptr
  logical_xor(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const value_payload<T>, other);
    return ((typed_data() || casted->typed_data()) && !(typed_data()
                                            && casted->typed_data())) ? bool_result<
           T>::get_true()
           : bool_result<T>::get_false();
  }

  /**
   * Logical and comparator
   * @param other
   * @return
   */
  payload::const_ptr
  logical_and(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const value_payload<T>, other);
    if (casted)
      return (typed_data() && casted->typed_data()) ? bool_result<T>::get_true()
             : bool_result<T>::get_false();
    else {
      return bool_result<T>::get_true();
    }
  }

  /**
   * Logical or comparator
   * @param other
   * @return
   */
  payload::const_ptr
  bitwise_or(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::spkt_error,
                     "valuepayload::bitwise_or - not sure if I can do that");
  }

  /**
   * Logical xor comparator
   * @param other
   * @return
   */
  payload::const_ptr
  bitwise_xor(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::spkt_error,
                     "valuepayload::bitwise_xor - not sure if I can do that");
  }

  /**
   * Logical and comparator
   * @param other
   * @return
   */
  payload::const_ptr
  bitwise_and(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::spkt_error,
                     "valuepayload::bitwise_and - not sure if I can do that");
  }

  virtual void
  serialize_order(sprockit::serializer& ser) {
    ser & (data_);
    ser & (size_);
  }

  virtual sprockit::serializable*
  serialization_clone() const {
    return new value_payload<T>(*this);
  }

  long
  data_serialization_size() const {
    return size_;
  }

  /**
   * Strinfier
   * @return a std::string description
   */
  virtual std::string
  to_string() const {
    std::stringstream ss;
    ss << "valuepayload(" << data_ << ")";
    return ss.str();
  }

 protected:
  value_payload(const T &inval, size_t size = sizeof(T)) :
    data_(inval), size_(size) {
  }

  /// Construction with type conversion.
  template<typename Other>
  value_payload(const Other &inval, size_t size = sizeof(T)) :
    data_(inval), size_(size) {
  }

  /// Private copy constructor.
  value_payload(const value_payload<T> &other, size_t size = sizeof(T)) :
    data_(other.data_), size_(size) {
  }

 protected:
  /// The data we keep.
  mutable T data_;
  size_t size_;

};

/**
 * Network payload consisting of a value with sensible copy semantics.
 *
 * Don't use this for pointer types, since you will break assumptions
 * about const-ness or protection of underlying data.
 */
template<typename T>
class bitwisevaluepayload : public value_payload<T>
{
 public:
  typedef sprockit::refcount_ptr<bitwisevaluepayload<T> > ptr;
  typedef sprockit::refcount_ptr<const bitwisevaluepayload<T> > const_ptr;

 public:
  virtual ~bitwisevaluepayload() {}

  /// Construct a new valuepayload.
  static typename bitwisevaluepayload::const_ptr
  construct(const T &inval) {
    return new bitwisevaluepayload<T>(inval);
  }

  static typename bitwisevaluepayload::const_ptr
  construct(const T &inval, size_t s) {
    return new bitwisevaluepayload<T>(inval, s);
  }

  /// Construct a new valuepayload with type conversion.
  template<typename Other>
  static typename bitwisevaluepayload::const_ptr
  construct(const Other &inval) {
    return new bitwisevaluepayload<T>(inval);
  }

  static typename bitwisevaluepayload::ptr
  construct() {
    return new bitwisevaluepayload();
  }

  /// Clone this object.
  payload::const_ptr
  clone() const {
    return new bitwisevaluepayload<T>(*this);
  }

  /**
   * Logical or comparator
   * @param other
   * @return
   */
  payload::const_ptr
  bitwise_or(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const bitwisevaluepayload<T>, other);
    T newval;
    if (casted) {
      newval = casted->data() | value_payload<T>::data();
    }
    else {
      newval = value_payload<T>::data();
    }
    payload::const_ptr ret = construct(newval);
    return ret;
  }

  /**
   * Logical xor comparator
   * @param other
   * @return
   */
  payload::const_ptr
  bitwise_xor(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const bitwisevaluepayload<T>, other);
    T newval;
    if (casted) {
      newval = casted->data() ^ value_payload<T>::data();
    }
    else {
      newval = value_payload<T>::data();
    }
    payload::const_ptr ret = construct(newval);
    return ret;
  }

  /**
   * Logical and comparator
   * @param other
   * @return
   */
  payload::const_ptr
  bitwise_and(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const bitwisevaluepayload<T>, other);
    T newval;
    if (casted) {
      newval = casted->data() & value_payload<T>::data();
    }
    else {
      newval = value_payload<T>::data();
    }
    payload::const_ptr ret = construct(newval);
    return ret;
  }

  virtual long
  data_serialization_size() const {
    return value_payload<T>::data_serialization_size();
  }

  virtual void
  serialize_order(sprockit::serializer& ser) {
    value_payload<T>::serialize_order(ser);
  }

  virtual sprockit::serializable*
  serialization_clone() const {
    return new bitwisevaluepayload<T>(*this);
  }

  /**
   * Strinfier
   * @return a std::string description
   */
  virtual std::string
  to_string() const {
    std::stringstream ss;
    ss << "bitwisevaluepayload(" << value_payload<T>::data() << ")";
    return ss.str();
  }

 protected:
  /// Construction time.
  bitwisevaluepayload(const T &inval, size_t size = sizeof(T)) :
    value_payload<T> (inval, size) {
  }

  /// Construction with type conversion.
  template<typename Other>
  bitwisevaluepayload(const Other &inval, size_t size = sizeof(T)) :
    value_payload<T> (inval, size) {
  }

  /// Private copy constructor.
  bitwisevaluepayload(const value_payload<T> &other, size_t size = sizeof(T)) :
    value_payload<T> (other, size) {
  }

  bitwisevaluepayload() :
    value_payload<T> () {
  }

};

/* Utility class definitions */

template<typename T>
inline payload::const_ptr
bool_result<T>::get_true()
{
  throw sprockit::spkt_error("logical operator used with invalid type");
}

template<typename T>
inline payload::const_ptr
bool_result<T>::get_false()
{
  throw sprockit::spkt_error("logical operator used with invalid type");
}

inline payload::const_ptr
bool_result<int>::get_true()
{
  return value_payload<int>::construct(1);
}

inline payload::const_ptr
bool_result<int>::get_false()
{
  return value_payload<int>::construct(0);
}

inline payload::const_ptr
bool_result<bool>::get_true()
{
  return value_payload<bool>::construct(true);
}

inline payload::const_ptr
bool_result<bool>::get_false()
{
  return value_payload<bool>::construct(false);
}

} // end of namespace sstmac
#endif

