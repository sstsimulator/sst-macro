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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_TYPES_MPITYPE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_TYPES_MPITYPE_H_INCLUDED

#include <sumi/comm_functions.h>
#include <sumi-mpi/mpi_integers.h>
#include <sumi-mpi/mpi_types.h>
#include <sprockit/spkt_config.h>
#include <sprockit/unordered.h>
#include <vector>
#include <string>

#include <iosfwd>
#include <map>

namespace sumi {


struct pairdata;
struct vecdata;
struct inddata;

}


namespace sumi {

using sumi::ReduceOp;
using sumi::Add;
using sumi::And;
using sumi::Min;
using sumi::Max;

/// MPI datatypes.
class mpi_type
{
 public:
  enum TYPE_TYPE {
    PRIM, PAIR, VEC, IND, NONE
  };

  mpi_type();

  void init_primitive(const char* label, const int sizeit);

  void init_primitive(const char* label, mpi_type* b1, mpi_type* b2, int size);

  void init_primitive(const std::string& labelit, const int sizeit, int align);

  //pair of primitives datatype
  void init_primitive(const std::string& labelit, mpi_type* b1, mpi_type* b2, int size);

  void init_vector(const std::string &labelit, mpi_type*base, int count,
                   int block, MPI_Aint byte_stride);

  void init_indexed(const std::string &labelit,
           inddata* dat, int sz, int ext);

  // id gets assigned automatically by the constructor.
  MPI_Datatype id;
  std::string label;

  static void delete_statics();

 public:
  operator MPI_Datatype() const {
    return id;
  }

  ~mpi_type();

  bool builtin() const {
    return builtin_;
  }

  void set_builtin(bool flag){
    builtin_ = flag;
  }

  TYPE_TYPE type() const {
    return type_;
  }

  int packed_size() const {
    return size_;
  }

  void pack_send(void* srcbuf, void* dstbuf, int sendcnt);

  void unpack_recv(void* srcbuf, void* dstbuf, int recvcnt);

  int bytes_to_elements(size_t bytes) const;

  int extent() const {
    return extent_;
  }

  void pack(const void *inbuf, void *outbuf) const;

  void unpack(const void *inbuf, void *outbuf) const;

  void set_committed(bool flag){
    committed_ = flag;
  }

  bool committed() const {
    return committed_;
  }

  bool contiguous() const {
    return contiguous_;
  }

  spkt_unordered_map<MPI_Op, sumi::reduce_fxn> fxns_;

  template <typename data_t>
  void init_integer(const char* name){
    init_primitive(name, sizeof(data_t));
    init_ops<data_t>();
    init_bitwise_ops<data_t>();
  }

  template <typename data_t>
  void init_with_ops(const char* name){
    init_primitive(name, sizeof(data_t));
    init_ops<data_t>();
  }

  void init_op(MPI_Op op, sumi::reduce_fxn fxn){
    fxns_[op] = fxn;
  }

  void init_no_ops(const char* name, int size){
    init_primitive(name, size);
  }

  template <typename data_t>
  void init_ops(){
    fxns_[MPI_SUM] = &ReduceOp<Add,data_t>::op;
    fxns_[MPI_MAX] = &ReduceOp<Max,data_t>::op;
    fxns_[MPI_MIN] = &ReduceOp<Min,data_t>::op;
    fxns_[MPI_LAND] = &ReduceOp<And,data_t>::op;
    fxns_[MPI_LOR] = &ReduceOp<Or,data_t>::op;
    fxns_[MPI_PROD] = &ReduceOp<Prod,data_t>::op;
    fxns_[MPI_LXOR] = &ReduceOp<LXOr,data_t>::op;
  }

  template <typename data_t>
  void init_bitwise_ops(){
    fxns_[MPI_BAND] = &ReduceOp<BAnd,data_t>::op;
    fxns_[MPI_BOR] = &ReduceOp<BOr,data_t>::op;
    fxns_[MPI_BOR] = &ReduceOp<BOr,data_t>::op;
    fxns_[MPI_BXOR] = &ReduceOp<BXOr,data_t>::op;
  }

  sumi::reduce_fxn op(MPI_Op theOp) const;

  std::string to_string() const;

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
  static mpi_type* mpi_int8_t;
  static mpi_type* mpi_int16_t;
  static mpi_type* mpi_int32_t;
  static mpi_type* mpi_int64_t;
  static mpi_type* mpi_uint8_t;
  static mpi_type* mpi_uint16_t;
  static mpi_type* mpi_uint32_t;
  static mpi_type* mpi_uint64_t;

 private:
  void pack_action(void* packed_buf, void* unpacked_buf, bool pack) const;

  void pack_action_primitive(void* packed_buf, void* unpacked_buf, bool pack) const;

  void pack_action_pair(void* packed_buf, void* unpacked_buf, bool pack) const;

  void pack_action_vector(void* packed_buf, void* unpacked_buf, bool pack) const;

  void pack_action_indexed(void* packed_buf, void* unpacked_buf, bool pack) const;


 private:
  TYPE_TYPE type_;

  bool contiguous_;

  bool committed_;

  pairdata* pdata_;
  vecdata* vdata_;
  inddata* idata_;

  bool builtin_;

  int size_; //this is the packed size !!!
  size_t extent_; //holds the extent, as defined by the MPI standard
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
  int byte_stride; //always in bytes!!!!

};

struct ind_block {
  mpi_type* base;
  int byte_disp; ///always in bytes!!!!
  int num;
};

struct inddata {
  std::vector<ind_block> blocks;
};

std::ostream&
operator<<(std::ostream &os, mpi_type* type);

}

#endif