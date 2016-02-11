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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_MPICOLLPAYLOAD_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_MPICOLLPAYLOAD_H_INCLUDED

#include <sstmac/common/messages/payload.h>
#include <vector>

namespace sstmac {
namespace sw {

class mpi_collective_payload :
  public payload,
  public sprockit::serializable_type<mpi_collective_payload>
{
  ImplementSerializable(mpi_collective_payload)

 public:
  typedef sprockit::refcount_ptr<mpi_collective_payload> ptr;
  typedef sprockit::refcount_ptr<const mpi_collective_payload> const_ptr;

 public:
  virtual std::string
  to_string() const {
    return "mpi collective payload";
  }

  mpi_collective_payload(const std::vector<payload::const_ptr>& l) :
    contents_(l) {
  }

  mpi_collective_payload(int num) :
    contents_(num, payload::const_ptr())
  {
  }

  mpi_collective_payload(const mpi_collective_payload::const_ptr& p) :
    contents_(p->get_content()) {
  }

  void
  recover(api* mpi);

  virtual sprockit::serializable*
  serialization_clone() const {
    return new mpi_collective_payload(contents_);
  }

  virtual
  ~mpi_collective_payload() throw () {
  }

  void*
  data() const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:data doesn't make a whole lot of sense");
  }

  std::vector<payload::const_ptr>
  get_content() const {
    return contents_;
  }

  void
  set_content(const payload::const_ptr& p, int index) {
    contents_[index] = p;
  }

  virtual payload::const_ptr
  clone() const {
    return new mpi_collective_payload(contents_);
  }

  virtual long
  byte_length() const {
    long ret = 0;
    int size = contents_.size();
    for (int i = 0; i < size; i++) {
      ret += contents_[i]->byte_length();
    }
    return ret;
  }

  virtual payload::const_ptr
  add(const payload::const_ptr& other) const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:add doesn't make a whole lot of sense");
  }

  virtual payload::const_ptr
  prod(const payload::const_ptr& other) const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:prod doesn't make a whole lot of sense");
  }

  virtual payload::const_ptr
  min(const payload::const_ptr& other) const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:operator< doesn't make a whole lot of sense");
  }

  virtual payload::const_ptr
  max(const payload::const_ptr& other) const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:operator> doesn't make a whole lot of sense");
  }

  virtual bool
  equals(const payload::const_ptr& other) const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:operator== doesn't make a whole lot of sense");
  }

  virtual payload::const_ptr
  logical_or(const payload::const_ptr& other) const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:operator || doesn't make a whole lot of sense");
  }

  virtual payload::const_ptr
  logical_xor(const payload::const_ptr& other) const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:operator XOR doesn't make a whole lot of sense");
  }

  virtual payload::const_ptr
  logical_and(const payload::const_ptr& other) const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:operator && doesn't make a whole lot of sense");
  }

  virtual payload::const_ptr
  bitwise_or(const payload::const_ptr& other) const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:operator | doesn't make a whole lot of sense");
  }

  virtual payload::const_ptr
  bitwise_xor(const payload::const_ptr& other) const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:operator ^ doesn't make a whole lot of sense");
  }

  virtual payload::const_ptr
  bitwise_and(const payload::const_ptr& other) const {
    spkt_throw(sprockit::unimplemented_error,
              "mpi_collective_payload:operator & doesn't make a whole lot of sense");
  }

  virtual void
  serialize_order(sprockit::serializer& ser);

 protected:
  std::vector<payload::const_ptr> contents_;

};

}
}

#endif

