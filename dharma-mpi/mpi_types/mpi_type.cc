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

#include <sprockit/serializer.h>
#include <sprockit/errors.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/statics.h>
#include <dharma-mpi/mpi_types/mpi_type.h>
#include <dharma-mpi/mpi_types.h>
#include <iostream>
#include <sstream>


namespace sstmac {
namespace sumi {

mpi_type::mpi_type() :
  id(-1),
  committed_(false),
  contiguous_(true),
  type_(NONE),
  size_(-1),
  combiner_(-1),
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
}

void
mpi_type::init_primitive(const char* labelit, const int sizeit, int align, int comb)
{
  extent_ = sizeit;
  numints_ = 0;
  numaddr_ = 0;
  numdtype_ = 1;
  combiner_ = comb;
  size_ = sizeit;
  type_ = PRIM;
  label = labelit;
}

//
// MPI datatypes.
//
void
mpi_type::init_primitive(const std::string& labelit, const int sizeit, int align,
                   int comb)
{
  init_primitive(labelit.c_str(), sizeit, align, comb);
}

void
mpi_type::init_primitive(const char* labelit, mpi_type* b1,
                   mpi_type* b2, int s, int comb)
{
  label = labelit;
  type_ = PAIR;
  extent_ = s;
  size_ = b1->size_ + b2->size_;
  numints_ = 0;
  numaddr_ = 0;
  numdtype_ = 1;
  combiner_ = comb;
  pdata_ = new pairdata;
  pdata_->base1 = b1;
  pdata_->base2 = b2;
}

void
mpi_type::init_primitive(const std::string &labelit, mpi_type* b1,
                   mpi_type* b2, int s, int comb)
{
  init_primitive(labelit.c_str(), b1, b2, s, comb);
}

void
mpi_type::init_vector(const std::string &labelit, mpi_type* base,
                   int count, int block, int str, bool in_elem, int comb)
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
  if (in_elem) {
    vdata_->stride = (str) * base->extent();
  }
  else {
    vdata_->stride = str;
  }

  extent_ = (count - 1) * abs(vdata_->stride) + base->extent_;
  size_ = count * block * base->size_;

  numints_ = 3;
  numaddr_ = 0;
  numdtype_ = 1;
  combiner_ = comb;
}

void
mpi_type::init_indexed(const std::string &labelit,
                   inddata* dat, int sz, int ext, int numint,
                   int numdtype, int comb)
{
  label = labelit;
  type_ = IND;
  size_ = sz;
  extent_ = ext;
  numints_ = numint;
  numdtype_ = numdtype;
  idata_ = dat;
  numaddr_ = 0;
  combiner_ = comb;
}

mpi_type::~mpi_type()
{
 if (idata_) delete idata_;
 if (vdata_) delete vdata_;
 if (pdata_) delete pdata_;
}

int
mpi_type::lb() const
{
  if (type_ == PRIM || type_ == PAIR) {
    return 0;
  }
  else if (type_ == VEC) {
    return vdata_->base->lb();
  }
  else if (type_ == IND) {
    return idata_->lb;
  }
  else {
    spkt_throw_printf(sprockit::value_error, "mpitype::get_lb - unknown type category");
  }
}

int
mpi_type::true_lb() const
{
  if (type_ == PRIM || type_ == PAIR || type_ == VEC) {
    return 0;
  }
  else if (type_ == IND) {
    return idata_->mindisp;
  }
  else {
    spkt_throw_printf(sprockit::value_error, "mpitype::get_lb - unknown type category");
  }
}

int
mpi_type::ub() const
{
  if (type_ == PRIM || type_ == PAIR || type_ == VEC) {
    return extent_;
  }
  else if (type_ == IND) {
    return idata_->ub;
  }
  else {
    spkt_throw_printf(sprockit::value_error, "mpitype::get_lb - unknown type category");
  }
}

int
mpi_type::extent() const
{
  if (type_ == PRIM || type_ == PAIR || type_ == VEC) {
    return extent_;
  }
  else if (type_ == IND) {
    return idata_->ub - idata_->lb;
  }
  else {
    spkt_throw_printf(sprockit::value_error, "mpitype::get_lb - unknown type category");
  }
}

int
mpi_type::true_extent() const
{
  if (type_ == PRIM || type_ == PAIR || type_ == VEC) {
    return extent_;
  }
  else if (type_ == IND) {
    return idata_->maxbyte - idata_->mindisp;
  }
  else {
    spkt_throw_printf(sprockit::value_error, "mpitype::get_lb - unknown type category");
  }
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
mpi_type::envelope(int* numint, int* numaddr, int* numdtype, int* comb)
{
  *comb = combiner_;
  *numint = numints_;
  *numaddr = numaddr_;
  *numdtype = numdtype_;
}

void
mpi_type::contents(int* ints, int* addr, long* dtypes)
{
  if (type_ == PRIM || type_ == PAIR) {
    dtypes[0] = id;
  }
  else if (type_ == VEC) {
    ints[0] = vdata_->count;
    ints[1] = vdata_->blocklen;
    if (combiner_ == MPI_COMBINER_HVECTOR) {
      ints[2] = vdata_->stride;
    }
    else {
      ints[2] = vdata_->stride / vdata_->base->extent();
    }
    dtypes[0] = vdata_->base->id;
  }
  else if (type_ == IND) {
    ints[0] = idata_->blocks.size();

    for (int i = 0; i < ints[0]; i++) {
      ints[i + 1] = idata_->blocks[i].num;
      if (combiner_ == MPI_COMBINER_HINDEXED || combiner_
          == MPIX_COMBINER_HINDEXED_BLOCK || combiner_
          == MPI_COMBINER_STRUCT) {
        ints[i + 1 + ints[0]] = idata_->blocks[i].disp;
      }
      else
        ints[i + 1 + ints[0]] = idata_->blocks[i].disp
                                / idata_->blocks[i].base->extent();
      if (numdtype_ > i) {
        dtypes[i] = idata_->blocks[i].base->id;
      }
    }

  }
}

void
mpi_type::pack(const void *inbuf, void *outbuf, int bytes) const
{
  //we are packing from inbuf into outbuf
  pack_action(outbuf, const_cast<void*>(inbuf), bytes, true);
}

void
mpi_type::unpack(const void *inbuf, void *outbuf, int bytes) const
{
  //we are unpacking from inbuf into outbuf
  //false = unpack
  pack_action(const_cast<void*>(inbuf), outbuf, bytes, false);
}

void
mpi_type::pack_action_primitive(void* packed_buf, void* unpacked_buf, int bytes, bool pack) const
{
  char* packed_ptr = (char*) packed_buf;
  char* unpacked_ptr = (char*) unpacked_buf;
  if (pack){
    //copy into packed array
    ::memcpy(packed_ptr, unpacked_ptr, bytes);
  }
  else {
    //copy into unpacked array
    ::memcpy(unpacked_ptr, packed_ptr, bytes);
  }
}

void
mpi_type::pack_action_vector(void* packed_buf, void* unpacked_buf, int bytes, bool pack) const
{
  int bytes_left = bytes;
  char* packed_ptr = (char*) packed_buf;
  char* unpacked_ptr = (char*) unpacked_buf;
  while (bytes_left > 0)
  {
    int bytes_this_round = vdata_->blocklen * vdata_->base->packed_size();
    for (int j=0; j < vdata_->count && bytes_left > 0; ++j){
      char* this_unpacked_ptr = unpacked_ptr + vdata_->stride * j;
      bytes_this_round = std::min(bytes_this_round, bytes_left);
      vdata_->base->pack_action(packed_ptr, this_unpacked_ptr, bytes_this_round, pack);
      packed_ptr += bytes_this_round;
      bytes_left -= bytes_this_round;
    }
    unpacked_ptr += extent_;
  }
}

void
mpi_type::pack_action_indexed(void* packed_buf, void* unpacked_buf, int bytes, bool pack) const
{
  int bytes_left = bytes;
  char* packed_ptr = (char*) packed_buf;
  char* unpacked_ptr = (char*) unpacked_buf;
  while (bytes_left > 0)
  {
    for (int j=0; j < idata_->blocks.size() && bytes_left > 0; ++j){
      const ind_block& next_block = idata_->blocks[j];
      char* this_unpacked_ptr = unpacked_ptr + next_block.disp;
      int bytes_this_round = next_block.num * next_block.base->packed_size();
      bytes_this_round = std::min(bytes_this_round, bytes_left);
      next_block.base->pack_action(packed_ptr, this_unpacked_ptr, bytes_this_round, pack);
      bytes_left -= bytes_this_round;
      //std::cout << sprockit::printf("copying %d bytes at displacement %d, packed=%p unpacked=%p %d bytes left\n",
      //                  bytes_this_round, next_block.disp, packed*, this_unpacked*, bytes_left);
      packed_ptr += bytes_this_round;
    }
    unpacked_ptr += extent_;
  }
}

void
mpi_type::pack_action_pair(void* packed_buf, void* unpacked_buf, int bytes, bool pack) const
{
  int bytes_done = 0;
  int first_size = pdata_->base1->size_;
  int second_size = pdata_->base2->size_;
  int pair_alignment = first_size;

  char* packed_ptr = (char*) packed_buf;
  char* unpacked_ptr = (char*) unpacked_buf;

  while (bytes_done < bytes){
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
}

void
mpi_type::pack_action(void* packed_buf, void* unpacked_buf, int bytes, bool pack) const
{
  switch(type_)
  {
  case PRIM: {
    pack_action_primitive(packed_buf, unpacked_buf, bytes, pack);
    break;
  }
  case PAIR: {
    pack_action_pair(packed_buf, unpacked_buf, bytes, pack);
    break;
  }
  case VEC: {
    pack_action_vector(packed_buf, unpacked_buf, bytes, pack);
    break;
  }
  case IND: {
    pack_action_indexed(packed_buf, unpacked_buf, bytes, pack);
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
       << ", stride=" << vdata_->stride << ")";
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

//fortran pairs
mpi_type* mpi_type::mpi_2integer = new mpi_type;
mpi_type* mpi_type::mpi_2complex = new mpi_type;
mpi_type* mpi_type::mpi_2double_complex = new mpi_type;
mpi_type* mpi_type::mpi_2real = new mpi_type;
mpi_type* mpi_type::mpi_2double_precision = new mpi_type;
mpi_type* mpi_type::mpi_character = new mpi_type;

std::map<int, mpi_type> mpi_type::builtins;

}
} // end of namespace sstmac

