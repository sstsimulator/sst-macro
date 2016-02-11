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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_TYPES_MPITYPE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_TYPES_MPITYPE_H_INCLUDED

#include <dharma/comm_functions.h>
#include <dharma-mpi/sstmac_mpi.h>
#include <sprockit/spkt_config.h>
#include <sprockit/serializable.h>
#include <sprockit/opaque_typedef.h>

#include <iosfwd>
#include <map>

namespace sstmac {
namespace sumi {

class pairdata;
class vecdata;
struct inddata;

}}

namespace sstmac {
namespace sumi {

using dharma::ReduceOp;
using dharma::Add;
using dharma::And;
using dharma::Min;
using dharma::Max;

/// MPI datatypes.
class mpi_type
{
 public:
  enum TYPE_TYPE {
    PRIM, PAIR, VEC, IND, NONE
  };

  mpi_type();

  /** Annoying memory leak avoider */
  void
  init_primitive(const char* label, const int sizeit, int align, int comb);

  void
  init_primitive(const char* label, mpi_type* b1, mpi_type* b2,
           int size, int comb);

  void
  init_primitive(const std::string& labelit, const int sizeit, int align,
           int comb);

  //pair of primitives datatype
  void
  init_primitive(const std::string& labelit, mpi_type* b1, mpi_type* b2,
           int size, int comb);

  void
  init_vector(const std::string &labelit, mpi_type*base, int count,
           int block, int str, bool in_elem, int comb);

  void
  init_indexed(const std::string &labelit,
           inddata* dat,
           int sz, int ext, int numint, int numdtype, int comb);

  // id gets assigned automatically by the constructor.
  MPI_Datatype id;
  std::string label;

  static void
  delete_statics();

 public:
  operator MPI_Datatype() const {
    return id;
  }

  ~mpi_type();

  TYPE_TYPE
  type() const {
    return type_;
  }

  int
  comb() const {
    return combiner_;
  }

  void
  set_comb(int c) {
    combiner_ = c;
  }

  int
  packed_size() const {
    return size_;
  }

  int
  bytes_to_elements(size_t bytes) const;

  //returns the marker if available, true lb if not
  int
  lb() const;

  //disregards the marker, returns lowest displacement
  int
  true_lb() const;

  //returns the marker if available, true ub if not
  int
  ub() const;

  //returns the difference between markers if available, true extent if not
  int
  extent() const;

  //returns the actual difference between min and max displacements (ignore markers)
  int
  true_extent() const;

  void
  envelope(int* numint, int* numaddr, int* numdtype, int* comb);

  void
  contents(int* ints, int* addr, long* dtypes);

  void
  pack(const void *inbuf, void *outbuf, int bytes) const;

  void
  unpack(const void *inbuf, void *outbuf, int bytes) const;

  void
  set_committed(bool flag){
    committed_ = flag;
  }

  bool
  committed() const {
    return committed_;
  }

  bool
  contiguous() const {
    return contiguous_;
  }

  spkt_unordered_map<MPI_Op, dharma::reduce_fxn> fxns_;

  template <typename data_t>
  void
  init_ops(){
    fxns_[MPI_SUM] = &ReduceOp<Add,data_t>::op;
    fxns_[MPI_MAX] = &ReduceOp<Max,data_t>::op;
    fxns_[MPI_MIN] = &ReduceOp<Min,data_t>::op;
    fxns_[MPI_BAND] = &ReduceOp<And,data_t>::op;
  }

  dharma::reduce_fxn
  op(MPI_Op theOp) const {
    return fxns_.at(theOp);
  }

  std::string
  to_string() const;

  // some implementations have other built-in types
  // DUMPI stores them by size
  // this just creates a list of types by size
  // this is a hack since these types cannot be operated on by a reduce
  static std::map<int, mpi_type> builtins;
  static mpi_type* mpi_null;
  static mpi_type* mpi_char;
  static mpi_type* mpi_unsigned_char;
  static mpi_type* mpi_signed_char;
  static mpi_type* mpi_wchar;
  static mpi_type* mpi_unsigned_long_long;
  static mpi_type* mpi_lb;
  static mpi_type* mpi_ub;
  static mpi_type* mpi_byte;
  static mpi_type* mpi_short;
  static mpi_type* mpi_unsigned_short;
  static mpi_type* mpi_int;
  static mpi_type* mpi_unsigned;
  static mpi_type* mpi_long;
  static mpi_type* mpi_unsigned_long;
  static mpi_type* mpi_float;
  static mpi_type* mpi_double;
  static mpi_type* mpi_long_double;
  static mpi_type* mpi_long_long_int;
  static mpi_type* mpi_long_long;
  static mpi_type* mpi_packed;
  static mpi_type* mpi_float_int;
  static mpi_type* mpi_double_int;
  static mpi_type* mpi_long_int;
  static mpi_type* mpi_short_int;
  static mpi_type* mpi_2int;
  static mpi_type* mpi_long_double_int;
  static mpi_type* mpi_complex;
  static mpi_type* mpi_complex8;
  static mpi_type* mpi_complex16;
  static mpi_type* mpi_complex32;
  static mpi_type* mpi_double_complex;
  static mpi_type* mpi_logical;
  static mpi_type* mpi_real;
  static mpi_type* mpi_real4;
  static mpi_type* mpi_real8;
  static mpi_type* mpi_real16;
  static mpi_type* mpi_double_precision;
  static mpi_type* mpi_integer;
  static mpi_type* mpi_integer1;
  static mpi_type* mpi_integer2;
  static mpi_type* mpi_integer4;
  static mpi_type* mpi_integer8;
  static mpi_type* mpi_2integer;
  static mpi_type* mpi_2complex;
  static mpi_type* mpi_2double_complex;
  static mpi_type* mpi_2real;
  static mpi_type* mpi_2double_precision;
  static mpi_type* mpi_character;

 protected:
  void
  pack_action(void* packed_buf, void* unpacked_buf, int bytes, bool pack) const;

  void
  pack_action_primitive(void* packed_buf, void* unpacked_buf, int bytes, bool pack) const;

  void
  pack_action_pair(void* packed_buf, void* unpacked_buf, int bytes, bool pack) const;

  void
  pack_action_vector(void* packed_buf, void* unpacked_buf, int bytes, bool pack) const;

  void
  pack_action_indexed(void* packed_buf, void* unpacked_buf, int bytes, bool pack) const;


 private:
  TYPE_TYPE type_;

  bool contiguous_;

  bool committed_;

  pairdata* pdata_;
  vecdata* vdata_;
  inddata* idata_;

  int size_; //this is the packed size !!!
  size_t extent_; //holds the extent, as defined by the MPI standard

  //stuff for get_envelope
  int combiner_;
  int numints_;
  int numaddr_;
  int numdtype_;
};

struct pairdata
{
  mpi_type* base1;
  mpi_type* base2;
};

struct vecdata
{
  mpi_type* base;
  int count;
  int blocklen;
  int stride; //always in bytes!!!!

};

struct ind_block {
  mpi_type* base;
  int disp; ///always in bytes!!!!
  int num;
};

struct inddata {
  int mindisp; //in bytes, true lb
  int maxbyte; //top byte, true ub
  int ub;
  int lb;
  std::vector<ind_block> blocks;
};

std::ostream&
operator<<(std::ostream &os, mpi_type* type);

}

} // end of namespace sstmac

#endif

