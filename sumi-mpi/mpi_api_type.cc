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

#include <sumi-mpi/mpi_api.h>
#include <climits>

namespace sstmac {
namespace sw {
extern void api_lock();
extern void api_unlock();
} }

namespace sumi {



struct float_int_t {
  float value;
  int index;
};

struct long_double_int_t {
  long double value;
  int index;
};

struct double_int_t {
  double value;
  int index;
};

struct short_int_t {
  short value;
  int index;
};

struct long_int_t {
  long value;
  int index;
};


struct int_int_t {
  int value;
  int index;
};

template <typename data_t>
struct MaxLocPair
{
  typedef data_t type;
  static void
  op(data_t& dst, const data_t& src){
    if (src.value > dst.value){
      dst.index = src.index;
      dst.value = src.value;
    } else if (src.value == dst.value){
      dst.index = std::min(src.index, dst.index);
    }
  }
};

template <typename data_t>
struct MinLocPair
{
  typedef data_t type;
  static void
  op(data_t& dst, const data_t& src){
    if (src.value < dst.value){
      dst.index = src.index;
      dst.value = src.value;
    } else if (src.value == dst.value){
      dst.index = std::min(src.index, dst.index);
    }
  }
};

struct complex {
  float r;
  float i;
};

struct dcomplex {
  double r;
  double i;
};

struct ldcomplex {
  long double r;
  long double i;
};

void
mpi_api::commit_builtin_types()
{
  sstmac::sw::api_lock();

  bool need_init = !mpi_type::mpi_null->committed();

#define int_precommit_type(datatype, typeObj, id) \
  if (need_init) typeObj->init_integer<datatype>(#id); \
  commit_builtin_type(typeObj, id)

#define op_precommit_type(datatype, typeObj, id) \
  if (need_init) typeObj->init_with_ops<datatype>(#id); \
  commit_builtin_type(typeObj, id)

#define noop_precommit_type(size, typeObj, id) \
  if (need_init) typeObj->init_no_ops(#id, size); \
  commit_builtin_type(typeObj, id)

#define index_precommit_type(datatype, typeObj, id) \
  if (need_init) typeObj->init_no_ops(#id, sizeof(datatype)); \
  if (need_init) typeObj->init_op(MPI_MAXLOC, &ReduceOp<MaxLocPair,datatype>::op); \
  if (need_init) typeObj->init_op(MPI_MINLOC, &ReduceOp<MinLocPair,datatype>::op); \
  commit_builtin_type(typeObj, id);

#define precommit_builtin(size) \
  if (need_init) mpi_type::builtins[size].init_no_ops("builtin-" #size, size); \
  allocate_type_id(&mpi_type::builtins[size])


  noop_precommit_type(0, mpi_type::mpi_null, MPI_NULL);

  int_precommit_type(char, mpi_type::mpi_char, MPI_CHAR);

  int_precommit_type(unsigned char, mpi_type::mpi_unsigned_char, MPI_UNSIGNED_CHAR);

  int_precommit_type(char, mpi_type::mpi_signed_char, MPI_SIGNED_CHAR);

  int_precommit_type(char16_t, mpi_type::mpi_wchar, MPI_WCHAR);

  int_precommit_type(unsigned long long, mpi_type::mpi_unsigned_long_long, MPI_UNSIGNED_LONG_LONG);

  noop_precommit_type(0, mpi_type::mpi_lb, MPI_LB);

  noop_precommit_type(0, mpi_type::mpi_ub, MPI_UB);

  int_precommit_type(char, mpi_type::mpi_byte, MPI_BYTE);

  int_precommit_type(int, mpi_type::mpi_int, MPI_INT);

  int_precommit_type(unsigned, mpi_type::mpi_unsigned, MPI_UNSIGNED);

  int_precommit_type(short, mpi_type::mpi_short, MPI_SHORT);

  int_precommit_type(unsigned short, mpi_type::mpi_unsigned_short, MPI_UNSIGNED_SHORT);

  int_precommit_type(long, mpi_type::mpi_long, MPI_LONG);

  int_precommit_type(long long, mpi_type::mpi_long_long_int, MPI_LONG_LONG_INT);

  int_precommit_type(unsigned long, mpi_type::mpi_unsigned_long, MPI_UNSIGNED_LONG);

  int_precommit_type(char, mpi_type::mpi_packed, MPI_PACKED);

  //fortran nonsense
  noop_precommit_type(2*sizeof(float), mpi_type::mpi_complex, MPI_COMPLEX);

  noop_precommit_type(2*sizeof(double), mpi_type::mpi_double_complex, MPI_DOUBLE_COMPLEX);

  op_precommit_type(float, mpi_type::mpi_float, MPI_FLOAT);
  op_precommit_type(float, mpi_type::mpi_real, MPI_REAL);
  op_precommit_type(double, mpi_type::mpi_double_precision, MPI_DOUBLE_PRECISION);
  op_precommit_type(double, mpi_type::mpi_double, MPI_DOUBLE);
  op_precommit_type(float, mpi_type::mpi_real4, MPI_REAL4);
  op_precommit_type(double, mpi_type::mpi_real8, MPI_REAL8);
  op_precommit_type(long double, mpi_type::mpi_long_double, MPI_LONG_DOUBLE);

  int_precommit_type(int, mpi_type::mpi_integer, MPI_INTEGER);
  int_precommit_type(char, mpi_type::mpi_integer1, MPI_INTEGER1);
  int_precommit_type(int16_t, mpi_type::mpi_integer2, MPI_INTEGER2);
  int_precommit_type(int32_t, mpi_type::mpi_integer4, MPI_INTEGER4);
  int_precommit_type(int64_t, mpi_type::mpi_integer8, MPI_INTEGER8);
  int_precommit_type(int8_t, mpi_type::mpi_int8_t, MPI_INT8_T);
  int_precommit_type(int16_t, mpi_type::mpi_int16_t, MPI_INT16_T);
  int_precommit_type(int32_t, mpi_type::mpi_int32_t, MPI_INT32_T);
  int_precommit_type(int64_t, mpi_type::mpi_int64_t, MPI_INT64_T);
  int_precommit_type(uint8_t, mpi_type::mpi_uint8_t, MPI_UINT8_T);
  int_precommit_type(uint16_t, mpi_type::mpi_uint16_t, MPI_UINT16_T);
  int_precommit_type(uint32_t, mpi_type::mpi_uint32_t, MPI_UINT32_T);
  int_precommit_type(uint64_t, mpi_type::mpi_uint64_t, MPI_UINT64_T);
  int_precommit_type(char, mpi_type::mpi_character, MPI_CHARACTER);

  index_precommit_type(int_int_t, mpi_type::mpi_2int, MPI_2INT);
  index_precommit_type(double_int_t, mpi_type::mpi_double_int, MPI_DOUBLE_INT);
  index_precommit_type(float_int_t, mpi_type::mpi_float_int, MPI_FLOAT_INT);
  index_precommit_type(long_int_t, mpi_type::mpi_long_int, MPI_LONG_INT);
  index_precommit_type(short_int_t, mpi_type::mpi_short_int, MPI_SHORT_INT);
  index_precommit_type(long_double_int_t, mpi_type::mpi_long_double_int, MPI_LONG_DOUBLE_INT);

  precommit_builtin(1);
  precommit_builtin(2);
  precommit_builtin(4);
  precommit_builtin(6);
  precommit_builtin(8);
  precommit_builtin(12);
  precommit_builtin(16);
  precommit_builtin(20);
  precommit_builtin(32);
  precommit_builtin(48);
  precommit_builtin(64);


  sstmac::sw::api_unlock();

}

int
mpi_api::pack_size(int incount, MPI_Datatype datatype, MPI_Comm comm, int *size)
{
  type_map::iterator it = known_types_.find(datatype);
  if (it == known_types_.end()){
      return MPI_ERR_TYPE;
  }
  *size = incount * it->second->packed_size();
  return MPI_SUCCESS;
}

int
mpi_api::type_set_name(MPI_Datatype id, const char* name)
{
  mpi_api_debug(sprockit::dbg::mpi,
                "MPI_Type_set_name(%s,%s)",
                type_str(id).c_str(), name);
  type_map::iterator it = known_types_.find(id);
  if (it == known_types_.end()){
      return MPI_ERR_TYPE;
  }
  it->second->label = name;
  return MPI_SUCCESS;
}

int
mpi_api::type_get_name(MPI_Datatype id, char* name, int* resultlen)
{
  mpi_api_debug(sprockit::dbg::mpi,
                "MPI_Type_get_name(%s)",
                type_str(id).c_str());
  type_map::iterator it = known_types_.find(id);
  if (it == known_types_.end()){
      return MPI_ERR_TYPE;
  }
  const std::string& label = it->second->label;
  ::strcpy(name, label.c_str());
  *resultlen = label.size();
  return MPI_SUCCESS;
}

int
mpi_api::op_create(MPI_User_function *user_fn, int commute, MPI_Op *op)
{
  if (!commute){
    spkt_throw_printf(sprockit::unimplemented_error,
                      "mpi_api::op_create: non-commutative operations");
  }
  *op = next_op_id_++;
  custom_ops_[*op] = user_fn;
  return MPI_SUCCESS;
}

int
mpi_api::op_free(MPI_Op *op)
{
  custom_ops_.erase(*op);
  return MPI_SUCCESS;
}

int
mpi_api::do_type_hvector(int count, int blocklength, MPI_Aint stride,
                         mpi_type* old, MPI_Datatype* new_type)
{
  std::stringstream ss;
  ss << "vector-" << old->label << "\n";

  mpi_type* new_type_obj = new mpi_type;
  new_type_obj->init_vector(ss.str(), old,
                        count, blocklength, stride);

  allocate_type_id(new_type_obj);
  *new_type = new_type_obj->id;
  mpi_api_debug(sprockit::dbg::mpi,
                "MPI_Type_vector(%d,%d,%d,%s,*%s)",
                count, blocklength, stride,
                type_str(old->id).c_str(), type_str(*new_type).c_str());

  return MPI_SUCCESS;
}

int
mpi_api::type_hvector(int count, int blocklength, MPI_Aint stride,
                     MPI_Datatype old_type, MPI_Datatype* new_type)
{
  return do_type_hvector(count, blocklength, stride, type_from_id(old_type), new_type);
}

/// Creates a vector (strided) datatype
int
mpi_api::type_vector(int count, int blocklength, int stride,
                     MPI_Datatype old_type, MPI_Datatype* new_type)
{
  mpi_type* old = type_from_id(old_type);
  MPI_Aint byte_stride = stride * old->extent();
  return do_type_hvector(count, blocklength, byte_stride, old, new_type);
}

int
mpi_api::do_type_hindexed(int count, const int lens[],
                          const MPI_Aint* displs, mpi_type *in_type_obj,
                          MPI_Datatype *outtype)
{
  mpi_type* out_type_obj = new mpi_type;

  inddata* idata = new inddata;
  int packed_size = 0;
  int extent = 0;
  int index = 0;

  for (int i = 0; i < count; i++) {
    if (lens[i] > 0) {
      index++;
    }
  }

  idata->blocks.resize(index);
  index = 0;
  for (int i = 0; i < count; i++) {
    if (lens[i] > 0) {
      ind_block& next = idata->blocks[index];
      next.base = in_type_obj;
      next.byte_disp = extent;
      next.num = lens[i];
      packed_size += lens[i] * in_type_obj->packed_size();
      extent += displs[i];
      index++;
    }
  }

  out_type_obj->init_indexed(in_type_obj->label, idata, packed_size, extent);

  allocate_type_id(out_type_obj);
  *outtype = out_type_obj->id;

  mpi_api_debug(sprockit::dbg::mpi,
                "MPI_Type_indexed(%d,<...>,<...>,%s,*%s)",
                count, type_str(in_type_obj->id).c_str(), type_str(*outtype).c_str());

  return MPI_SUCCESS;
}

int
mpi_api::type_hindexed(int count, const int lens[], const MPI_Aint* displs,
                      MPI_Datatype intype, MPI_Datatype* outtype)
{
  return do_type_hindexed(count, lens, displs, type_from_id(intype), outtype);
}

int
mpi_api::type_indexed(int count, const int lens[], const int* displs,
                      MPI_Datatype intype, MPI_Datatype* outtype)
{
  mpi_type* in_type_obj = type_from_id(intype);
  int type_extent = in_type_obj->extent();
  MPI_Aint byte_displs[count];
  for (int i=0; i < count; ++i){
    byte_displs[i] = displs[i] * type_extent;
  }
  return do_type_hindexed(count, lens, byte_displs, in_type_obj, outtype);
}

std::string
mpi_api::type_label(MPI_Datatype tid)
{
  mpi_type* ty = type_from_id(tid);
  return ty->label;
}

//
// A datatype object has to be committed before use in communication.
//
int
mpi_api::type_commit(MPI_Datatype* type)
{
  mpi_type* type_obj = type_from_id(*type);
  type_obj->set_committed(true);
  return MPI_SUCCESS;
}

void
mpi_api::allocate_type_id(mpi_type* type)
{
  type_map::iterator it, end = known_types_.end();
  while ((it = known_types_.find(next_type_id_)) != end){
    ++next_type_id_;
  }
  type->id = next_type_id_;
  known_types_[type->id] = type;
}

void
mpi_api::commit_builtin_type(mpi_type* type, MPI_Datatype id)
{
  if (known_types_.find(id) != known_types_.end()){
    spkt_throw_printf(sprockit::value_error,
      "mpi_api::precommit_type: %d already exists", id);
  }
  type->id = id;
  type->set_builtin(true);
  known_types_[id] = type;
  known_types_[id]->set_committed(true);
}

//
// Creates a contiguous datatype
//
int
mpi_api::type_contiguous(int count, MPI_Datatype old_type,
                         MPI_Datatype* new_type)
{
  mpi_type* new_type_obj = new mpi_type;
  mpi_type* old_type_obj = type_from_id(old_type);
  MPI_Aint byte_stride = count * old_type_obj->extent();
  new_type_obj->init_vector("contiguous-" + old_type_obj->label,
                        old_type_obj,
                        count, 1, byte_stride);

  allocate_type_id(new_type_obj);
  *new_type = new_type_obj->id;
  return MPI_SUCCESS;
}

int
mpi_api::type_create_struct(const int count, const int* blocklens,
                     const MPI_Aint* indices, const MPI_Datatype* old_types,
                     MPI_Datatype* newtype)
{
  int new_ind[count];
  for (int i=0; i < count; ++i){
    new_ind[i] = indices[i];
  }
  return type_create_struct(count, blocklens, new_ind, old_types, newtype);
}

//
// Creates a struct datatype
//
int
mpi_api::type_create_struct(const int count, const int* blocklens,
                     const int* indices, const MPI_Datatype* old_types,
                     MPI_Datatype* newtype)
{
  mpi_type* new_type_obj = new mpi_type;

  inddata* idata = new inddata;

  int packed_size = 0;
  int extent = 0;
  int index = 0;

  for (int i = 0; i < count; i++) {
    if (blocklens[i] > 0) {
      ++index;
    }
  }

  idata->blocks.resize(index);
  index = 0;

  for (int i = 0; i < count; i++) {
    if (blocklens[i] > 0) {
      mpi_type* old_type_obj = type_from_id(old_types[i]);
      ind_block& next = idata->blocks[index];
      next.base = old_type_obj;
      next.byte_disp = indices[i];
      next.num = blocklens[i];
      packed_size += old_type_obj->packed_size() * blocklens[i];
      extent = next.byte_disp + old_type_obj->packed_size() * blocklens[i];
      index++;
    }
  }

  new_type_obj->init_indexed("struct", idata, packed_size, extent);

  allocate_type_id(new_type_obj);
  *newtype = new_type_obj->id;

  mpi_api_debug(sprockit::dbg::mpi,
                "MPI_Type_struct(%d,<...>,<...>,<...>,*%s)",
                count, type_str(*newtype).c_str());

  return MPI_SUCCESS;
}

int
mpi_api::type_size(MPI_Datatype type, int* size)
{
  *size = type_from_id(type)->packed_size();
  return MPI_SUCCESS;
}

int
mpi_api::type_extent(MPI_Datatype type, MPI_Aint *extent)
{
  *extent = type_from_id(type)->extent();
  return MPI_SUCCESS;
}

int
mpi_api::type_dup(MPI_Datatype intype, MPI_Datatype* outtype)
{
  mpi_type* new_type_obj = type_from_id(intype);
  allocate_type_id(new_type_obj);
  *outtype = new_type_obj->id;
  mpi_api_debug(sprockit::dbg::mpi,
                "MPI_Type_dup(%s,*%s)",
                type_str(intype).c_str(), type_str(*outtype).c_str());
  return MPI_SUCCESS;
}


//
// Mark datatype for deallocation.
//
int
mpi_api::type_free(MPI_Datatype* type)
{
  mpi_api_debug(sprockit::dbg::mpi,
                "MPI_Type_free(%s)",
                type_str(*type).c_str());
  auto iter = known_types_.find(*type);
  if (iter != known_types_.end()){
    mpi_type* obj = iter->second;
    known_types_.erase(iter);
    delete obj;
  }
  return MPI_SUCCESS;
}

//
// Get the derived mpitype mapped to an id
//
mpi_type*
mpi_api::type_from_id(MPI_Datatype id)
{
  type_map::iterator it = known_types_.find(id);
  if (it == known_types_.end()){
    spkt_throw_printf(sprockit::invalid_key_error,
        "mpi_api: unknown type id %d",
        int(id));
  }
  return it->second;
}

}