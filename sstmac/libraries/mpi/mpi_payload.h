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

#ifndef SSTMAC_SOFTWARE_API_MPI_MPIPAYLOAD_H_INCLUDED
#define SSTMAC_SOFTWARE_API_MPI_MPIPAYLOAD_H_INCLUDED

#include <sstream>
#include <cstring>

#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/messages/payload.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_op.h>
#include <sstmac/libraries/mpi/mpi_api_fwd.h>

namespace sstmac {
namespace sw {
/**
 * Network payload consisting of a boost multiarray.
 *
 */
class mpi_payload :
  public payload,
  public serializable_type<mpi_payload>
{
  ImplementSerializableDefaultConstructor(mpi_payload)

 public:
  typedef sprockit::refcount_ptr<mpi_payload> ptr;
  typedef sprockit::refcount_ptr<const mpi_payload> const_ptr;

  /// Construction time.
  mpi_payload(void* data, mpi_type* t, int count, bool usewrapper);
  //internal construction
  mpi_payload(void* data, mpi_type* t, int count);

  mpi_payload();

  virtual long
  byte_length() const {
    return bufsize_;
  }

  virtual
  ~mpi_payload() {
    if ((given_ || !wrapped_) && bufsize_ > 0) {
      free(buf_);
    }
  }

  /// Clone this object.
  payload::const_ptr
  clone() const {
    return new mpi_payload(buf_, type_, cnt_, wrapped_);
  }

  void*
  data() const {
    return buf_;
  }

  void
  extract(void* data, int bytes, mpi_type* t) const;

  void
  extract(void* data, int bytes, mpi_type* t, int offset) const;

  void
  extract(void* data) const;

  int
  count() const {
    return cnt_;
  }

  mpi_type*
  type() const {
    return type_;
  }


 protected:
  struct payload_op {
    virtual void
    go(void*, const void*, const void*,int,int,mpi_type*,mpi_type*) const = 0;
  };

  struct addstruct : public payload_op {
    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {
      t1->op()->do_add(arg1, arg2, newbuf, cnt1);
    }
  };

  struct prodstruct : public payload_op  {
    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {
      t1->op()->do_prod(arg1, arg2, newbuf, cnt1);
    }
  };

  struct minstruct : public payload_op  {
    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {
      t1->op()->do_min(arg1, arg2, newbuf, cnt1);
    }
  };

  struct maxstruct : public payload_op  {
    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {

      t1->op()->do_max(arg1, arg2, newbuf, cnt1);
    }
  };

  struct replacestruct : public payload_op  {
    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {

      t1->op()->do_replace(arg1, arg2, newbuf, cnt1);
    }
  };

  struct minlocstruct : public payload_op  {
    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {
      t1->op()->do_minloc(arg1, arg2, newbuf, cnt1);
    }
  };

  struct maxlocstruct : public payload_op  {
    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {
      t1->op()->do_maxloc(arg1, arg2, newbuf, cnt1);
    }
  };

  struct orstruct : public payload_op  {
    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {
      t1->op()->do_or(arg1, arg2, newbuf, cnt1);
    }
  };

  struct xorstruct : public payload_op  {

    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {
      t1->op()->do_xor(arg1, arg2, newbuf, cnt1);
    }
  };

  struct andstruct : public payload_op  {

    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {
      t1->op()->do_and(arg1, arg2, newbuf, cnt1);
    }
  };

  struct borstruct : public payload_op  {

    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {
      t1->op()->do_bor(arg1, arg2, newbuf, cnt1);
    }
  };

  struct bxorstruct : public payload_op  {

    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {
      t1->op()->do_bxor(arg1, arg2, newbuf, cnt1);
    }
  };

  struct bandstruct  : public payload_op {

    void
    go(void* newbuf, const void* arg1, const void* arg2, int cnt1,
       int cnt2, mpi_type* t1, mpi_type* t2) const {
      t1->op()->do_band(arg1, arg2, newbuf, cnt1);
    }
  };

  payload::const_ptr
  go_go_gadget_operation(const payload::const_ptr& other, payload_op* op) const;

 public:
  /** The MPI payload does not carry an MPI type unless it needs to - it just carries an MPI type ID
   *  Before doing add,prod,pack operations
   *  The calling object must let the payload get a type object from mpi_api for the type id */
  void
  recover(api* mpi);

  /**
   * Add operator
   * @param other
   * @return a smart pointer to a new valuepayload object
   */
  payload::const_ptr
  add(const payload::const_ptr& other) const {
    addstruct op;
    return go_go_gadget_operation(other, &op);
  }

  payload::const_ptr
  prod(const payload::const_ptr& other) const {
    prodstruct op;
    return go_go_gadget_operation(other, &op);
  }

  payload::const_ptr
  user_op(const payload::const_ptr& other, mpi_op* op) const;

  /**
   * Less than comparator
   * @param other
   * @return
   */
  payload::const_ptr
  min(const payload::const_ptr& other) const {
    minstruct op;
    return go_go_gadget_operation(other, &op);
  }

  payload::const_ptr
  minloc(const payload::const_ptr& other) const {
    minlocstruct op;
    return go_go_gadget_operation(other, &op);
  }

  payload::const_ptr
  maxloc(const payload::const_ptr& other) const {
    maxlocstruct op;
    return go_go_gadget_operation(other, &op);
  }

  /**
   * Greater than comparator
   * @param other
   * @return
   */
  payload::const_ptr
  max(const payload::const_ptr& other) const {
    maxstruct op;
    return go_go_gadget_operation(other, &op);
  }

  const void*
  buf() const {
    return buf_;
  }

  /**
   * This guy is special
   */
  payload::const_ptr
  replace(const payload::const_ptr& other) const {
    replacestruct op;
    return go_go_gadget_operation(other, &op);
  }

  /**
   * Equals comparator
   * @param other
   * @return
   */
  virtual bool
  equals(const payload::const_ptr& other) const;

  /**
   * Logical or comparator
   * @param other
   * @return
   */
  payload::const_ptr
  logical_or(const payload::const_ptr& other) const {
    orstruct op;
    return go_go_gadget_operation(other, &op);
  }

  /**
   * Logical xor comparator
   * @param other
   * @return
   */
  payload::const_ptr
  logical_xor(const payload::const_ptr& other) const {
    xorstruct op;
    return go_go_gadget_operation(other, &op);
  }

  /**
   * Logical and comparator
   * @param other
   * @return
   */
  payload::const_ptr
  logical_and(const payload::const_ptr& other) const {
    andstruct op;
    return go_go_gadget_operation(other, &op);
  }

  payload::const_ptr
  bitwise_or(const payload::const_ptr& other) const {
    borstruct op;
    return go_go_gadget_operation(other, &op);
  }

  /**
   * Logical xor comparator
   * @param other
   * @return
   */
  payload::const_ptr
  bitwise_xor(const payload::const_ptr& other) const {
    bxorstruct op;
    return go_go_gadget_operation(other, &op);
  }

  /**
   * Logical and comparator
   * @param other
   * @return
   */
  payload::const_ptr
  bitwise_and(const payload::const_ptr& other) const {
    bandstruct op;
    return go_go_gadget_operation(other, &op);
  }

  virtual void
  serialize_order(serializer& ser);

  /**
   * Strinfier
   * @return a std::string description
   */
  virtual std::string
  to_string() const;

 protected:
  void* buf_;
  size_t bufsize_; //in bytes
  mpi_type* type_;
  mpi_type_id type_id_;
  int cnt_;
  bool wrapped_;
  bool given_;
  bool real_;

};

} // end of namespace sstmac
}
#endif

