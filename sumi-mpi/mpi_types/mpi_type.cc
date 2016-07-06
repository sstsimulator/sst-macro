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

#include <sprockit/errors.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/statics.h>
#include <sumi-mpi/mpi_types/mpi_type.h>
#include <sumi-mpi/mpi_types.h>
#include <iostream>
#include <sstream>
#include <cstring>

namespace sumi {


mpi_type::mpi_type() :
  id(-1),
  committed_(false),
  contiguous_(true),
  type_(NONE),
  size_(-1),
  pdata_(0),
  idata_(0),
  vdata_(0)
{
}

void
mpi_type::delete_statics()
{
  free_static_ptr(mpi_null);
  free_static_ptr(mpi_char);
  free_static_ptr(mpi_unsigned_char);
  free_static_ptr(mpi_signed_char);
  free_static_ptr(mpi_wchar);
  free_static_ptr(mpi_unsigned_long_long);
  free_static_ptr(mpi_lb);
  free_static_ptr(mpi_ub);
  free_static_ptr(mpi_byte);
  free_static_ptr(mpi_short);
  free_static_ptr(mpi_unsigned_short);
  free_static_ptr(mpi_int);
  free_static_ptr(mpi_unsigned);
  free_static_ptr(mpi_long);
  free_static_ptr(mpi_unsigned_long);
  free_static_ptr(mpi_float);
  free_static_ptr(mpi_double);
  free_static_ptr(mpi_long_double);
  free_static_ptr(mpi_long_long_int);
  free_static_ptr(mpi_long_long);
  free_static_ptr(mpi_packed);
  free_static_ptr(mpi_float_int);
  free_static_ptr(mpi_double_int);
  free_static_ptr(mpi_long_int);
  free_static_ptr(mpi_short_int);
  free_static_ptr(mpi_2int);
  free_static_ptr(mpi_long_double_int);
  free_static_ptr(mpi_complex);
  free_static_ptr(mpi_complex8);
  free_static_ptr(mpi_complex16);
  free_static_ptr(mpi_complex32);
  free_static_ptr(mpi_double_complex);
  free_static_ptr(mpi_logical);
  free_static_ptr(mpi_real);
  free_static_ptr(mpi_real4);
  free_static_ptr(mpi_real8);
  free_static_ptr(mpi_real16);
  free_static_ptr(mpi_double_precision);
  free_static_ptr(mpi_integer);
  free_static_ptr(mpi_integer1);
  free_static_ptr(mpi_integer2);
  free_static_ptr(mpi_integer4);
  free_static_ptr(mpi_integer8);
  free_static_ptr(mpi_2integer);
  free_static_ptr(mpi_2complex);
  free_static_ptr(mpi_2double_complex);
  free_static_ptr(mpi_2real);
  free_static_ptr(mpi_2double_precision);
  free_static_ptr(mpi_character);
  free_static_ptr(mpi_int8_t);
  free_static_ptr(mpi_int16_t);
  free_static_ptr(mpi_int32_t);
  free_static_ptr(mpi_int64_t);
  free_static_ptr(mpi_uint8_t);
  free_static_ptr(mpi_uint16_t);
  free_static_ptr(mpi_uint32_t);
  free_static_ptr(mpi_uint64_t);
}

void
mpi_type::init_primitive(const char* labelit, int size)
{
  extent_ = size;
  size_ = size;
  type_ = PRIM;
  label = labelit;
}

//
// MPI datatypes.
//
void
mpi_type::init_primitive(const std::string& labelit, const int sizeit, int align)
{
  init_primitive(labelit.c_str(), sizeit, align);
}

void
mpi_type::init_primitive(const char* labelit, mpi_type* b1, mpi_type* b2, int s)
{
  label = labelit;
  type_ = PAIR;
  extent_ = s;
  size_ = b1->size_ + b2->size_;
  pdata_ = new pairdata;
  pdata_->base1 = b1;
  pdata_->base2 = b2;
}

void
mpi_type::init_primitive(const std::string &labelit, mpi_type* b1,
                   mpi_type* b2, int s)
{
  init_primitive(labelit.c_str(), b1, b2, s);
}

sumi::reduce_fxn
mpi_type::op(MPI_Op theOp) const
{
  spkt_unordered_map<MPI_Op, sumi::reduce_fxn>::const_iterator it = fxns_.find(theOp);
  if (it == fxns_.end()){
    spkt_throw_printf(sprockit::value_error, "type %s has no operator %d",
           to_string().c_str(), theOp);
  }
  return it->second;
}

void
mpi_type::init_vector(const std::string &labelit, mpi_type* base,
                   int count, int block, MPI_Aint byte_stride)
{
  if (base->id == -1){
    spkt_throw_printf(sprockit::value_error,
        "mpi_type::init_vector: unitialized base type %s",
        base->label.c_str());
  }
  label = labelit;
  type_ = VEC;
  vdata_ = new vecdata();
  pdata_ = NULL;
  vdata_->base = base;
  vdata_->count = count;
  vdata_->blocklen = (block);
  vdata_->byte_stride = byte_stride;

  extent_ = byte_stride;
  size_ = count * block * base->size_;
  int block_extent = block*base->extent();

  if (byte_stride < block_extent){
    spkt_throw_printf(sprockit::value_error,
      "vector stride=%d must be at least as big as blocksize=%d",
     byte_stride, block*base->extent());
  }

  //if the byte_stride matches the blocksize
  //and the underlying type is contiguous
  //then this type is again contiguous
  contiguous_ = byte_stride == block_extent && base->contiguous();

}

void
mpi_type::init_indexed(const std::string &labelit,
                   inddata* dat, int sz, int ext)
{
  label = labelit;
  type_ = IND;
  size_ = sz;
  extent_ = ext;
  idata_ = dat;
}

mpi_type::~mpi_type()
{
 if (idata_) delete idata_;
 if (vdata_) delete vdata_;
 if (pdata_) delete pdata_;
}

int
mpi_type::bytes_to_elements(size_t bytes) const
{
  if (size_ == 0) {
    return 0;
  }

  if (type_ == PRIM) {
    return bytes / size_;
  }
  else if (type_ == PAIR) {
    int ret = 0;
    size_t total = 0;
    bool one = true;
    while (total <= bytes) {
      if (one) {
        total += pdata_->base1->size_;
      }
      else {
        total += pdata_->base2->size_;
      }

      ret++;
      one = !one;
    }
    return ret - 1;
  }
  else if (type_ == VEC) {
    if (vdata_->blocklen == 0) {
      return 0;
    }

    int ret = 0;
    size_t total = 0;
    while (total < bytes) {
      int before = total;
      for (int i = 0; i < vdata_->blocklen && total < bytes; i++) {
        ret += vdata_->base->bytes_to_elements(
                 vdata_->base->packed_size());
        total += vdata_->base->packed_size();
        //ret++;

      }
      if (total == before) {
        return 0;  //stupid check for an empty struct
      }
    }

    return ret;

  }
  else if (type_ == IND) {

    int ret = 0;
    size_t total = 0;
    while (total < bytes) {
      int before = total;
      for (int i = 0; i < idata_->blocks.size() && total < bytes; i++) {
        for (int j = 0; j < idata_->blocks[i].num && total < bytes; j++) {
          ret += idata_->blocks[i].base->bytes_to_elements(
                   idata_->blocks[i].base->packed_size());
          total += idata_->blocks[i].base->packed_size();
        }
      }
      if (total == before) {
        return 0;  //stupid check for an empty struct
      }
    }

    return ret;
  }
  spkt_throw_printf(sprockit::value_error,
                   "mpitype::bytes_to_elements: invalid type %d",
                   type_);
  return 0;
}

void
mpi_type::pack(const void* inbuf, void *outbuf) const
{
  //we are packing from inbuf into outbuf
  pack_action(outbuf, const_cast<void*>(inbuf), true);
}

void
mpi_type::unpack(const void* inbuf, void *outbuf) const
{
  //we are unpacking from inbuf into outbuf
  //false = unpack
  pack_action(const_cast<void*>(inbuf), outbuf, false);
}

void
mpi_type::pack_action_primitive(void* packed_buf, void* unpacked_buf, bool pack) const
{
  char* packed_ptr = (char*) packed_buf;
  char* unpacked_ptr = (char*) unpacked_buf;
  if (pack){
    //copy into packed array
    ::memcpy(packed_ptr, unpacked_ptr, size_);
  }
  else {
    //copy into unpacked array
    ::memcpy(unpacked_ptr, packed_ptr, size_);
  }
}

void
mpi_type::pack_action_vector(void* packed_buf, void* unpacked_buf, bool pack) const
{
  char* packed_ptr = (char*) packed_buf;
  char* unpacked_ptr = (char*) unpacked_buf;
  int packed_stride = vdata_->base->packed_size();
  for (int j=0; j < vdata_->count; ++j){
    char* this_unpacked_ptr = unpacked_ptr + vdata_->byte_stride * j;
    vdata_->base->pack_action(packed_ptr, this_unpacked_ptr, pack);
    packed_ptr += packed_stride;
  }
  unpacked_ptr += extent_;
}

void
mpi_type::pack_action_indexed(void* packed_buf, void* unpacked_buf, bool pack) const
{
  char* packed_ptr = (char*) packed_buf;
  char* unpacked_ptr = (char*) unpacked_buf;
  for (int j=0; j < idata_->blocks.size(); ++j){
    const ind_block& next_block = idata_->blocks[j];
    char* this_unpacked_ptr = unpacked_ptr + next_block.byte_disp;
    next_block.base->pack_action(packed_ptr, this_unpacked_ptr, pack);
    //std::cout << sprockit::printf("copying %d bytes at displacement %d, packed=%p unpacked=%p %d bytes left\n",
    //                  bytes_this_round, next_block.disp, packed*, this_unpacked*, bytes_left);
    packed_ptr += next_block.base->packed_size();
  }
  unpacked_ptr += extent_;
}

void
mpi_type::pack_action_pair(void* packed_buf, void* unpacked_buf, bool pack) const
{
  int bytes_done = 0;
  int first_size = pdata_->base1->size_;
  int second_size = pdata_->base2->size_;
  int pair_alignment = first_size;

  char* packed_ptr = (char*) packed_buf;
  char* unpacked_ptr = (char*) unpacked_buf;

  if (pack){
    ::memcpy(packed_ptr, unpacked_ptr, first_size);
    ::memcpy(packed_ptr + first_size, unpacked_ptr + pair_alignment, second_size);
  }
  else {
    ::memcpy(unpacked_ptr, packed_ptr, first_size);
    ::memcpy(unpacked_ptr + pair_alignment, packed_ptr + first_size, second_size);
  }
  unpacked_ptr += extent_;
  packed_ptr += size_;
  bytes_done += (first_size + second_size);
}

void
mpi_type::pack_action(void* packed_buf, void* unpacked_buf, bool pack) const
{
  switch(type_)
  {
  case PRIM: {
    pack_action_primitive(packed_buf, unpacked_buf, pack);
    break;
  }
  case PAIR: {
    pack_action_pair(packed_buf, unpacked_buf, pack);
    break;
  }
  case VEC: {
    pack_action_vector(packed_buf, unpacked_buf, pack);
    break;
  }
  case IND: {
    pack_action_indexed(packed_buf, unpacked_buf, pack);
    break;
  }
  case NONE: {
    spkt_throw(sprockit::illformed_error,
        "mpi_type::pack_action: cannot pack NONE type");
  }
  }
}

std::string
mpi_type::to_string() const
{
  std::stringstream ss(std::stringstream::in | std::stringstream::out);
  if (type_ == PRIM) {
    ss << "mpitype(primitive, label=\"" << label << "\", size=" << size_
       << ")";
  }
  if (type_ == PAIR) {
    ss << "mpitype(pair, label=\"" << label << "\", size=" << size_ << ")";
  }
  else if (type_ == VEC) {
    ss << "mpitype(vector, label=\"" << label << "\", size=" << size_
       << ", base=" << vdata_->base->to_string() << ", count="
       << vdata_->count << ", blocklen=" << vdata_->blocklen
       << ", stride=" << vdata_->byte_stride << ")";
  }
  else if (type_ == IND) {
    ss << "mpitype(indexed/struct, label=\"" << label << "\", size="
       << size_ << ", base=" << idata_->blocks[0].base->to_string()
       << ", blocks=" << idata_->blocks.size() << ")";

  }
  return ss.str();
}

//
// Fairly self-explanatory.
//
std::ostream&
operator<<(std::ostream &os, mpi_type* type)
{
  os << type->to_string();
  return os;
}

//
// The data types defined by the MPI standard.
//
mpi_type* mpi_type::mpi_null = new mpi_type;
mpi_type* mpi_type::mpi_char = new mpi_type;
mpi_type* mpi_type::mpi_signed_char = new mpi_type;
mpi_type* mpi_type::mpi_wchar = new mpi_type;
mpi_type* mpi_type::mpi_unsigned_long_long = new mpi_type;
mpi_type* mpi_type::mpi_lb = new mpi_type;
mpi_type* mpi_type::mpi_ub = new mpi_type;
mpi_type* mpi_type::mpi_unsigned_char = new mpi_type;
mpi_type* mpi_type::mpi_byte = new mpi_type;
mpi_type* mpi_type::mpi_short = new mpi_type;
mpi_type* mpi_type::mpi_unsigned_short = new mpi_type;
mpi_type* mpi_type::mpi_int = new mpi_type;
mpi_type* mpi_type::mpi_unsigned = new mpi_type;
mpi_type* mpi_type::mpi_long = new mpi_type;
mpi_type* mpi_type::mpi_unsigned_long = new mpi_type;
mpi_type* mpi_type::mpi_float = new mpi_type;
mpi_type* mpi_type::mpi_double = new mpi_type;
mpi_type* mpi_type::mpi_long_double = new mpi_type;
mpi_type* mpi_type::mpi_long_long_int = new mpi_type;
mpi_type* mpi_type::mpi_long_long = new mpi_type;
mpi_type* mpi_type::mpi_packed = new mpi_type;

//pair types
mpi_type* mpi_type::mpi_float_int = new mpi_type;
mpi_type* mpi_type::mpi_double_int = new mpi_type;
mpi_type* mpi_type::mpi_long_int = new mpi_type;
mpi_type* mpi_type::mpi_short_int = new mpi_type;
mpi_type* mpi_type::mpi_2int = new mpi_type;
mpi_type* mpi_type::mpi_long_double_int = new mpi_type;

//fortran nonsense
mpi_type* mpi_type::mpi_complex = new mpi_type;
mpi_type* mpi_type::mpi_double_complex = new mpi_type;
mpi_type* mpi_type::mpi_logical = new mpi_type;
mpi_type* mpi_type::mpi_real = new mpi_type;
mpi_type* mpi_type::mpi_double_precision = new mpi_type;
mpi_type* mpi_type::mpi_integer = new mpi_type;

mpi_type* mpi_type::mpi_integer1 = new mpi_type;
mpi_type* mpi_type::mpi_integer2 = new mpi_type;
mpi_type* mpi_type::mpi_integer4 = new mpi_type;
mpi_type* mpi_type::mpi_integer8 = new mpi_type;
mpi_type* mpi_type::mpi_real4 = new mpi_type;
mpi_type* mpi_type::mpi_real8 = new mpi_type;
mpi_type* mpi_type::mpi_real16 = new mpi_type;

mpi_type* mpi_type::mpi_complex8 = new mpi_type;
mpi_type* mpi_type::mpi_complex16 = new mpi_type;
mpi_type* mpi_type::mpi_complex32 = new mpi_type;

mpi_type* mpi_type::mpi_int8_t = new mpi_type;
mpi_type* mpi_type::mpi_int16_t = new mpi_type;
mpi_type* mpi_type::mpi_int32_t = new mpi_type;
mpi_type* mpi_type::mpi_int64_t = new mpi_type;

mpi_type* mpi_type::mpi_uint8_t = new mpi_type;
mpi_type* mpi_type::mpi_uint16_t = new mpi_type;
mpi_type* mpi_type::mpi_uint32_t = new mpi_type;
mpi_type* mpi_type::mpi_uint64_t = new mpi_type;

//fortran pairs
mpi_type* mpi_type::mpi_2integer = new mpi_type;
mpi_type* mpi_type::mpi_2complex = new mpi_type;
mpi_type* mpi_type::mpi_2double_complex = new mpi_type;
mpi_type* mpi_type::mpi_2real = new mpi_type;
mpi_type* mpi_type::mpi_2double_precision = new mpi_type;
mpi_type* mpi_type::mpi_character = new mpi_type;

std::map<int, mpi_type> mpi_type::builtins;

}

