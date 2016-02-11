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

#ifndef SSTMAC_COMMON_MESSAGES_PAIRPAYLOAD_H_INCLUDED
#define SSTMAC_COMMON_MESSAGES_PAIRPAYLOAD_H_INCLUDED

#include <sstream>

#include <sstmac/common/messages/payload.h>

namespace sstmac {

/**
 * Network payload consisting of a value with sensible copy semantics.
 *
 * Don't use this for pointer types, since you will break assumptions
 * about const-ness or protection of underlying data.
 */
template<typename T1, typename T2>
class pair_payload : public payload
{
  /// The data we keep.
  mutable T1 data1_;
  mutable T2 data2_;
  size_t size_;
  typedef pair_payload<T1, T2> PairPayload;
  
  /// Construction time.
  pair_payload(const T1 &inval1, const T2 &inval2,
               size_t size = sizeof(T1) + sizeof(T2)) :
    data1_(inval1), data2_(inval2), size_(size) {
  }

  /// Private copy constructor.
  pair_payload(const pair_payload<T1, T2> &other,
               size_t size = sizeof(T1) + sizeof(T2)) :
    data1_(other.data1_), data2_(other.data2_), size_(size) {
  }

 public:
  typedef sprockit::refcount_ptr<pair_payload<T1, T2> > ptr;
  typedef sprockit::refcount_ptr<const pair_payload<T1, T2> > const_ptr;

  virtual
  ~pair_payload() {
  }

  /// Construct a new valuepayload.
  static typename pair_payload<T1, T2>::const_ptr
  construct(const T1 &inval1, const T2 &inval2) {
    return new pair_payload<T1, T2>(inval1, inval2);
  }

  /// Clone this object.
  payload::const_ptr
  clone() const {
    return new pair_payload<T1, T2>(*this);
  }

  /// Access the underlying data in a non-const context.
  std::pair<T1, T2>
  data_unconst() {
    return std::pair<T1, T2>(data1_, data2_);
  }

  /// Access the underlying data in a const context.
  const std::pair<T1, T2>
  data() const {
    return std::pair<T1, T2>(data1_, data2_);
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
  virtual const payload::const_ptr
  add(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast<
const pair_payload<T1, T2> >(other);
    T1 newval1 = casted->data().first + data().first;
    T2 newval2 = casted->data().second + data().second;
    payload::const_ptr ret = construct(newval1, newval2);
    return ret;
  }

  /**
   * Less than comparator, to only be used for the minloc mpi operation
   * @param other
   * @return
   */
  virtual const payload::const_ptr
  min(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast<
                       const pair_payload<T1, T2> >(other);
    if (data1_ < casted->data().first) {
      return pair_payload<T1, T2>::construct(data1_, data2_);
    }
    else {
      return pair_payload<T1, T2>::construct(casted->data().first,
                                             casted->data().second);
    }

  }

  /**
   * Greater than comparator, to only be used for the maxloc operation
   * @param other
   * @return
   */
  virtual const payload::const_ptr
  max(const payload::const_ptr &other)  const {
    const_ptr casted = ptr_safe_cast<
                       const pair_payload<T1, T2> >(other);
    if (data1_ > casted->data().first) {
      return pair_payload<T1, T2>::construct(data1_, data2_);
    }
    else {
      return pair_payload<T1, T2>::construct(casted->data().first,
                                             casted->data().second);
    }

  }

  /**
   * Equals comparator
   * @param other
   * @return
   */
  virtual bool
  equals(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast<
                       const pair_payload<T1, T2> >(other);
    return data() == casted->data();

  }

  /**
   * Logical or comparator
   * @param other
   * @return
   */
  const payload::const_ptr
  logical_or(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::unimplemented_error,
                     "pairpayload: logical_or doesn't make sense");
  }

  /**
   * Logical xor comparator
   * @param other
   * @return
   */
  const payload::const_ptr
  logical_xor(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::unimplemented_error,
                     "pairpayload: logical_xor doesn't make sense");
  }

  /**
   * Logical and comparator
   * @param other
   * @return
   */
  const payload::const_ptr
  logical_and(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::unimplemented_error,
                     "pairpayload: logical_and doesn't make sense");
  }

  /**
   * Serialize this object into the spkt_serializer, for
   * running in parallel simulation
   * @param ser the serializer to use
   */
  virtual void
  serialize_order(sprockit::spkt_serializer* ser) const {
    spkt_throw_printf(sprockit::unimplemented_error, "pairpayload: serialize unimplemented");
  }

  /**
   * Strinfier
   * @return a std::string description
   */
  virtual std::string
  to_string() const {
    std::stringstream ss;
    ss << "pairpayload(" << data1_ << ", " << data2_ << ")";
    return ss.str();
  }

};

/**
 * Network payload consisting of a value with sensible copy semantics.
 *
 * Don't use this for pointer types, since you will break assumptions
 * about const-ness or protection of underlying data.
 */
template<typename T1, typename T2>
class pairpayloadvector : public payload
{

  struct structtype {
    T1 a;
    T2 b;
  };

  /// The data we keep.
  void* buf_;
  size_t size_;

  /// Construction time.
  pairpayloadvector(void* start, size_t size, bool copy) :
    size_(size) {
    if (copy) {
      size_t s = (sizeof(T1) + sizeof(T2)) * size;
      buf_ = malloc(s);

      memcpy(buf_, start, s);
    }
    else {
      buf_ = start;
    }
  }

  /// Private copy constructor.
  pairpayloadvector(const pairpayloadvector<T1, T2> &other, size_t size) :
    size_(size) {
    size_t s = (sizeof(T1) + sizeof(T2)) * size;
    buf_ = malloc(s);

    memcpy(buf_, other.buf_, s);
  }

 public:
  typedef sprockit::refcount_ptr<pairpayloadvector<T1, T2> > ptr;
  typedef sprockit::refcount_ptr<const pairpayloadvector<T1, T2> > const_ptr;

  virtual
  ~pairpayloadvector() {
    free(buf_);
  }

  /// Construct a new valuepayload.
  static typename pairpayloadvector<T1, T2>::const_ptr
  construct(void* start, size_t size, bool copy = true) {
    return new pairpayloadvector<T1, T2>(start, size, copy);
  }

  /// Clone this object.
  payload::const_ptr
  clone() const {
    return new pairpayloadvector<T1, T2>(*this);
  }

  /// Access the underlying data in a non-const context.
  void*
  data_unconst() {
    return buf_;
  }

  /// Access the underlying data in a const context.
  const void*
  data() const {
    return buf_;
  }

  virtual long
  byte_length() const {
    return size_ * (sizeof(T1) + sizeof(T2));
  }

  /**
   * Add operator
   * @param other
   * @return a smart pointer to a new valuepayload object
   */
  virtual const payload::const_ptr
  add(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(PairPayload,other);
    size_t s = (sizeof(T1) + sizeof(T2)) * size_;
    structtype* newdata = (structtype*) malloc(s);
    structtype* thisdata = (structtype*) data();
    structtype* thatdata = (structtype*) casted->data();
    for (int i = 0; i < size_; i++) {
      newdata->a = thisdata->a + thatdata->a;
      newdata->b = thisdata->b + thatdata->b;
      newdata++;
      thisdata++;
      thatdata++;
    }
    payload::const_ptr ret = construct(newdata, size_, false);
    return ret;
  }

  /**
   * Less than comparator, to only be used for the minloc mpi operation
   * @param other
   * @return
   */
  virtual const payload::const_ptr
  min(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::unimplemented_error,
                     "pairpayloadvector::less_than - doesn't make sense");

  }

  /**
   * Greater than comparator, to only be used for the maxloc operation
   * @param other
   * @return
   */
  virtual const payload::const_ptr
  max(const payload::const_ptr &other)  const {
    spkt_throw_printf(sprockit::unimplemented_error,
                     "pairpayloadvector::greater_than - doesn't make sense");
  }

  /**
   * Equals comparator
   * @param other
   * @return
   */
  virtual bool
  equals(const payload::const_ptr &other) const {
    const_ptr casted = ptr_safe_cast(PairPayload, other);
    bool ret = true;
    structtype* thisdata = (structtype*) data();
    structtype* thatdata = (structtype*) casted->data();
    for (int i = 0; i < size_ && ret; i++) {
      ret = ret && (thisdata->a == thatdata->a);
      ret = ret && (thisdata->b == thatdata->b);
    }

    return ret;

  }

  /**
   * Logical or comparator
   * @param other
   * @return
   */
  const payload::const_ptr
  logical_or(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::unimplemented_error,
                     "pairpayloadvector: logical_or doesn't make sense");
  }

  /**
   * Logical xor comparator
   * @param other
   * @return
   */
  const payload::const_ptr
  logical_xor(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::unimplemented_error,
                     "pairpayloadvector: logical_xor doesn't make sense");
  }

  /**
   * Logical and comparator
   * @param other
   * @return
   */
  const payload::const_ptr
  logical_and(const payload::const_ptr &other) const {
    spkt_throw_printf(sprockit::unimplemented_error,
                     "pairpayloadvector: logical_and doesn't make sense");
  }

  /**
   * Serialize this object into the spkt_serializer, for
   * running in parallel simulation
   * @param ser the serializer to use
   */
  virtual void
  serialize_order(sprockit::spkt_serializer* ser) const {
    spkt_throw_printf(sprockit::unimplemented_error,
                     "pairpayloadvector: serialize unimplemented");
  }

  /**
   * Strinfier
   * @return a std::string description
   */
  virtual std::string
  to_string() const {
    std::stringstream ss;
    ss << "pairpayloadvector(" << size_ << ")";
    return ss.str();
  }

};

} // end of namespace sstmac
#endif

