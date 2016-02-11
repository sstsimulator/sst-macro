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

#ifndef SSTMAC_COMMON_MESSAGES_WRAPPERPAYLOAD_H_INCLUDED
#define SSTMAC_COMMON_MESSAGES_WRAPPERPAYLOAD_H_INCLUDED

#include <sstmac/common/messages/payload.h>
#include <sprockit/util.h>

namespace sstmac {

/**
 * Network payload that wraps a raw buffer.  Must be
 * careful with this, or else false shared-memory things
 * might happen.
 *
 */
class wrapper_payload :
  public payload,
  public sprockit::serializable_type<wrapper_payload>
{
  ImplementSerializable(wrapper_payload)

  mutable void* base_;
  size_t size_; //in bytes

  /// Construction time.
  wrapper_payload(void *inval, size_t s) :
    base_(inval), size_(s) {

  }

 public:
  typedef sprockit::refcount_ptr<wrapper_payload> ptr;
  typedef sprockit::refcount_ptr<const wrapper_payload> const_ptr;

  virtual
  ~wrapper_payload() {

  }

  static const_ptr
  construct(void *inval, size_t s) {
    return new wrapper_payload(inval, s);
  }

  static ptr
  construct() {
    return new wrapper_payload;
  }

  /// Clone this object.
  payload::const_ptr
  clone() const {
    return new wrapper_payload(base_, size_);
  }

  void*
  data() const {
    return base_;
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
  add(const payload::const_ptr& other) const {

    return other;
  }

  virtual payload::const_ptr
  prod(const payload::const_ptr& other) const {

    return other;
  }

  /**
   * Less than comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  min(const payload::const_ptr& other) const {

    return other;
  }

  /**
   * Greater than comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  max(const payload::const_ptr& other) const {
    return other;

  }

  /**
   * Equals comparator
   * @param other
   * @return
   */
  virtual bool
  equals(const payload::const_ptr& other) const {
    const_ptr casted = ptr_safe_cast(const wrapper_payload, other);
    return base_ == casted->base_ && size_ == casted->size_;
  }

  /**
   * Logical or comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  logical_or(const payload::const_ptr& other) const {
    return other;
  }

  /**
   * Logical xor comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  logical_xor(const payload::const_ptr& other) const {
    return other;
  }

  /**
   * Logical and comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  logical_and(const payload::const_ptr& other) const {
    return other;
  }

  virtual payload::const_ptr
  bitwise_or(const payload::const_ptr& other) const {
    return other;
  }

  /**
   * Logical xor comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  bitwise_xor(const payload::const_ptr& other) const {
    return other;
  }

  /**
   * Logical and comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  bitwise_and(const payload::const_ptr& other) const {
    return other;
  }

  virtual void
  serialize_order(sprockit::serializer& ser);

  /**
   * Strinfier
   * @return a std::string description
   */
  virtual std::string
  to_string() const;

};

} // end of namespace sstmac
#endif

