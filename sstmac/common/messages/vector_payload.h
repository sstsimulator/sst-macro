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

#ifndef SSTMAC_COMMON_MESSAGES_VECTORPAYLOAD_H_INCLUDED
#define SSTMAC_COMMON_MESSAGES_VECTORPAYLOAD_H_INCLUDED

#include <sstream>
#include <sprockit/util.h>
#include <sstmac/common/messages/payload.h>
#include <cstddef>

namespace sstmac {

/**
 * Network payload consisting of a boost multiarray.
 *
 */
template<typename Type, typename VectorType = std::vector<Type> >
class vector1_payload :
  public payload,
  public serializable_type<vector1_payload<Type, VectorType> >
{
  ImplementSerializable(vector1_payload)

 protected:
  /// The data we keep.
  typedef VectorType arraytype;

 public:
  typedef sprockit::refcount_ptr<vector1_payload<Type, VectorType> > ptr;
  typedef sprockit::refcount_ptr<const vector1_payload<Type, VectorType> >
  const_ptr;

 public:
  vector1_payload(){} //needed for serialization

  virtual ~vector1_payload() {
    free(underneath_);
  }

  void
  assign(void *data) {
    Type* casted = (Type*) data;
    int count = dims_[0];
    for (int i = 0; i < count; ++i) {
      underneath_[i] = casted[i];
    }
  }

  static ptr
  construct() {
    return new vector1_payload<Type, VectorType>;
  }

  static const_ptr
  construct(const arraytype &inval, int dim1, ptrdiff_t stride = 1) {
    return new vector1_payload<Type, VectorType> (inval, dim1, stride);
  }

  /// Clone this object.
  payload::const_ptr
  clone() const {
    return new vector1_payload<Type, VectorType> (underneath_, dims_[0], true, 1);
  }

  /// Access the underlying data in a const context.
  void*
  data() const {
    return underneath_;
  }

  const Type*
  typed_data() const {
    return underneath_;
  }

  int
  get_dim() const {
    return dims_[0];
  }

  virtual long
  byte_length() const {
    int dimsize = 1;
    for (int i = 0; i < dims_.size(); i++) {
      dimsize *= dims_[i];
    }
    return sizeof(Type) * dimsize;
  }

  typedef vector1_payload<Type, VectorType> Vector1Payload;
  
  /**
   * Add operator
   * @param other
   * @return a smart pointer to a new valuepayload object
   */
  virtual payload::const_ptr
  add(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const Vector1Payload, other);

    Type* newval = (Type*) malloc(sizeof(Type) * dims_[0]);

    for (int i = 0; i < dims_[0]; i++) {

      newval[i] = underneath_[i] + casted->typed_data()[i];

    }

    payload::const_ptr ret = new Vector1Payload(newval, dims_[0], true, 1);

    free(newval);

    return ret;
  }

  virtual payload::const_ptr
  prod(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const Vector1Payload, other);

    Type* newval = (Type*) malloc(sizeof(Type) * dims_[0]);

    for (int i = 0; i < dims_[0]; i++) {

      newval[i] = underneath_[i] * casted->typed_data()[i];

    }

    payload::const_ptr ret = new Vector1Payload(newval, dims_[0], true, 1);

    free(newval);

    return ret;
  }

  /**
   * Less than comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  min(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const Vector1Payload, other);

    Type* newval = (Type*) malloc(sizeof(Type) * dims_[0]);

    for (int i = 0; i < dims_[0]; i++) {

      newval[i] = (underneath_[i] < casted->typed_data()[i]) ? underneath_[i]
                  : casted->typed_data()[i];

    }

    payload::const_ptr ret = new Vector1Payload(newval, dims_[0], true, 1);

    free(newval);

    return ret;
  }

  /**
   * Greater than comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  max(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const Vector1Payload, other);

    Type* newval = (Type*) malloc(sizeof(Type) * dims_[0]);

    for (int i = 0; i < dims_[0]; i++) {

      newval[i] = (underneath_[i] > casted->typed_data()[i]) ? underneath_[i]
                  : casted->typed_data()[i];

    }

    payload::const_ptr ret = new Vector1Payload(newval, dims_[0], true, 1);

    free(newval);

    return ret;

  }

  /**
   * Equals comparator
   * @param other
   * @return
   */
  virtual bool
  equals(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const Vector1Payload, other);

    for (int i = 0; i < dims_[0]; i++) {
      if (underneath_[i] != casted->typed_data()[i]) {
        return false;
      }
    }

    return true;
  }

  /**
   * Logical or comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  logical_or(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const Vector1Payload, other);

    Type* newval = (Type*) malloc(sizeof(Type) * dims_[0]);

    for (int i = 0; i < dims_[0]; i++) {
      newval[i] = underneath_[i] || casted->typed_data()[i];
    }

    payload::const_ptr ret = new Vector1Payload(newval, dims_[0], true, 1);

    free(newval);

    return ret;
  }

  /**
   * Logical xor comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  logical_xor(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const Vector1Payload, other);

    Type* newval = (Type*) malloc(sizeof(Type) * dims_[0]);

    for (int i = 0; i < dims_[0]; i++) {
      newval[i] = ((underneath_[i] || casted->typed_data()[i])
                   && !(underneath_[i] && casted->typed_data()[i]));
    }

    payload::const_ptr ret = new Vector1Payload(newval, dims_[0], true, 1);

    free(newval);

    return ret;
  }

  /**
   * Logical and comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  logical_and(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(const Vector1Payload, other);

    Type* newval = (Type*) malloc(sizeof(Type) * dims_[0]);

    for (int i = 0; i < dims_[0]; i++) {
      newval[i] = underneath_[i] && casted->typed_data()[i];
    }

    payload::const_ptr ret = new Vector1Payload(newval, dims_[0], true, 1);

    free(newval);

    return ret;
  }

  /**
   * Logical or comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  bitwise_or(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::spkt_error, "vector1_payload::bitwise| not implemented");
  }

  /**
   * Logical xor comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  bitwise_xor(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::spkt_error, "vector1_payload::bitwise^ not implemented");
  }

  /**
   * Logical and comparator
   * @param other
   * @return
   */
  virtual payload::const_ptr
  bitwise_and(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::spkt_error, "vector1_payload::bitwise& not implemented");
  }

  virtual void
  serialize_order(serializer& ser) {
    spkt_throw_printf(sprockit::unimplemented_error, "vector1_payload::serialize");
  }

  /**
   * Strinfier
   * @return a std::string description
   */
  virtual std::string
  to_string() const {
    std::stringstream ss;
    ss << "vector1_payload("
       << typeid(Type).name() << ", " << dims_[0]
       << ")";
    return ss.str();
  }

 protected:
  vector1_payload(const arraytype &inval, int dim1, ptrdiff_t stride) {
    dims_.push_back(dim1);

    //do a deep copy
    underneath_ = (Type*) malloc(sizeof(Type) * dim1);
    for (int i = 0; i < dim1; i++) {
      underneath_[i] = inval[i * stride];
    }
  }

  //Used by clone
  vector1_payload(Type* inval, int dim1, bool cloning, ptrdiff_t stride) {
    dims_.push_back(dim1);

    //do a deep copy
    underneath_ = (Type*) malloc(sizeof(Type) * dim1);
    for (int i = 0; i < dim1; i++) {
      underneath_[i] = inval[i * stride];
    }
  }

 protected:
  std::vector<int> dims_;

  Type* underneath_;
};

} // end of namespace sstmac
#endif

