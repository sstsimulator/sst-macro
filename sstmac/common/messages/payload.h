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

#ifndef SSTMAC_COMMON_MESSAGES_PAYLOAD_H_INCLUDED
#define SSTMAC_COMMON_MESSAGES_PAYLOAD_H_INCLUDED

#include <sstmac/software/api/api_fwd.h>
#include <sstmac/common/serializable.h>
#include <sprockit/ptr_type.h>
#include <typeinfo>

namespace sstmac {

/**
 * Pure abstract base for payload that can be associated with a network send.
 */
class payload :
  public sprockit::printable_ptr_type,
  public serializable
{
 protected:
  /**
   * Empty protected constructor to enforce using construct()
   */
  payload() {
  }

 public:
  typedef sprockit::refcount_ptr<payload> ptr;
  typedef sprockit::refcount_ptr<const payload> const_ptr;

  /// Goodbye.
  virtual
  ~payload() {
  }

  virtual void
  recover(sw::api* api);

  /**
   * Create a copy of this payload
   * @return A smart pointer to a new payload object
   */
  virtual payload::const_ptr
  clone() const = 0;

  /**
   * Stringify
   * @return String description
   */
  virtual std::string
  to_string() const {
    return "payload";
  }

  /**
   *  Convenience function to get a null payload pointer.
   *  @return null smart pointer
   */
  static ptr
  null() {
    return ptr();
  }

  virtual void
  assign(void* data) {
    spkt_throw_printf(sprockit::illformed_error,
                     "payload::assign: assigning to non-vector type");
  }

  virtual void*
  data() const = 0;

  virtual long
  byte_length() const = 0;

  /**
   * Pure virtual add operator
   * @param other Payload object to add to this one
   * @return A smart pointer to a new object which is the sum of this and other
   */
  virtual payload::const_ptr
  add(const payload::const_ptr& other) const = 0;

  virtual payload::const_ptr
  prod(const payload::const_ptr& other) const = 0;

  /**
   * Pure virtual less than (<) operator
   * @param other Payload to compare to
   * @return true if this payload is considered less than other
   */
  virtual payload::const_ptr
  min(const payload::const_ptr& other) const = 0;

  /**
   * Pure virtual greater than (>) operator
   * @param other Payload to compare to
   * @return true if this payload is considered greather than other
   */
  virtual payload::const_ptr
  max(const payload::const_ptr& other) const = 0;

  /**
   * Pure virtual equals operator
   * @param other Payload to compare to
   * @return true if this payload is considered equal to the other
   */
  virtual bool
  equals(const payload::const_ptr& other) const = 0;

  /**
   * Pure virtual logical or operator
   * @param other Payload to compare to
   * @return true if this either payload is considered true
   */
  virtual payload::const_ptr
  logical_or(const payload::const_ptr& other) const = 0;

  /**
   * Pure virtual logical xor operator
   * @param other Payload to compare to
   * @return true if exactly one payload is considered true
   */
  virtual payload::const_ptr
  logical_xor(const payload::const_ptr& other) const = 0;

  /**
   * Pure virtual logical and operator
   * @param other Payload to compare to
   * @return true if both payloads are considered true
   */
  virtual payload::const_ptr
  logical_and(const payload::const_ptr& other) const = 0;

  virtual payload::const_ptr
  bitwise_or(const payload::const_ptr& other) const = 0;

  /**
   * Pure virtual logical xor operator
   * @param other Payload to compare to
   * @return true if exactly one payload is considered true
   */
  virtual payload::const_ptr
  bitwise_xor(const payload::const_ptr& other) const = 0;

  /**
   * Pure virtual logical and operator
   * @param other Payload to compare to
   * @return true if both payloads are considered true
   */
  virtual payload::const_ptr
  bitwise_and(const payload::const_ptr& other) const = 0;



};
} // end of namespace sstmac
#endif

