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

#include <sstmac/libraries/mpi/mpi_payload.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sprockit/util.h>

DeclareSerializable(sstmac::sw::mpi_payload);

namespace sstmac {
namespace sw {

/// Construction time.
mpi_payload:: mpi_payload(void* data, mpi_type* t, int count, bool usewrapper) :
  type_id_(t->id),
  cnt_(count),
  wrapped_(usewrapper),
  given_(false),
  real_(true),
  type_(t)
{
  bufsize_ = count * t->packed_size();
  if (usewrapper) {
    buf_ = data;
  }
  else if (count > 0) {
    buf_ = malloc(sizeof(char) * bufsize_);
    t->pack(data, buf_, bufsize_);
  }
  else {
    buf_ = 0;
  }
}

void
mpi_payload::recover(api* a)
{
  if (type_ == 0){
    mpi_api* mpi = safe_cast(mpi_api, a);
    type_ = mpi->type_from_id(type_id_);
  }
}

payload::const_ptr
mpi_payload::go_go_gadget_operation(const payload::const_ptr& other, payload_op* op) const
{
  mpi_payload::const_ptr oth = ptr_safe_cast(const mpi_payload, other,
      "mpipayload::operation - payload didn't cast to mpipayload");

  if ((type_->type() == mpi_type::PRIM || type_->type()
       == mpi_type::PAIR) && (oth->type_->type() == mpi_type::PRIM
                              || oth->type_->type() == mpi_type::PAIR)) {
    void* resbuf;

    void* arg1 = buf_;
    void* arg2 = oth->buf_;

    if (type_->op()->needs_unpack() || oth->type_->op()->needs_unpack()) {
      resbuf = (void*) malloc(type_->extent() * cnt_);
      arg1 = (void*) malloc(type_->extent() * cnt_);
      arg2 = (void*) malloc(oth->type_->extent() * oth->cnt_);

      int pos = 0;
      type_->unpack(buf_, arg1, bufsize_);
      pos = 0;
      oth->type_->unpack(oth->buf_, arg2, oth->bufsize_);

    }
    else {
      resbuf = (void*) malloc(bufsize_);
    }

    if (!type_->op()) {
      spkt_throw_printf(sprockit::spkt_error,
                       "mpipayload::go_operation - operation for type %s is null",
                       type_->to_string().c_str());
    }
    op->go(resbuf, arg1, arg2, cnt_, oth->cnt_, type_, oth->type_);

    void* outgoing = resbuf;

    if (type_->op()->needs_unpack() || oth->type_->op()->needs_unpack()) {
      outgoing = (void*) malloc(bufsize_);
      type_->pack(resbuf, outgoing, bufsize_);

      free(arg1);
      free(arg2);
      free(resbuf);
    }

    return new mpi_payload(outgoing, type_, cnt_);
  }
  else {
    spkt_throw_printf(sprockit::spkt_error,
                     "mpipayload::operation - on mismatched basic types isn't implemented yet, %d != %d",
                     type_->type(), oth->type_->type());
  }
}

bool
mpi_payload::equals(const payload::const_ptr& other) const
{
  mpi_payload::const_ptr oth = ptr_safe_cast(const mpi_payload, other);

  bool ret = true;
  char* othbuf = (char*) oth->buf_;
  char* mybuf = (char*) buf_;
  for (int i = 0; i < cnt_ * type_->packed_size() && ret; i++) {
    ret = ret && (mybuf[i] == othbuf[i]);
  }

  return ret;
}

payload::const_ptr
mpi_payload::user_op(const payload::const_ptr& other, mpi_op* op) const
{
  mpi_payload::const_ptr oth = ptr_safe_cast(const mpi_payload, other,
      "mpipayload::operation: payload didn't cast to mpipayload");

  void* newbuf = (void*) malloc(cnt_ * type_->extent());
  void* otherbuf = (void*) malloc(cnt_ * type_->extent());

  type_->unpack(buf_, newbuf, cnt_ * type_->packed_size());
  type_->unpack(oth->buf_, otherbuf, cnt_ * type_->packed_size());

  int c = cnt_;
  long t = type_->id;
  (*op->userfunc_)(otherbuf, newbuf, &c, &t);
  payload::const_ptr p = new mpi_payload(newbuf, type_, cnt_, false);
  free(newbuf);
  free(otherbuf);
  return p;
}

//internal construction
mpi_payload::mpi_payload(void* data, mpi_type* t, int count) :
  type_(t),
  type_id_(t->id),
  cnt_(count),
  wrapped_(false),
  given_(true),
  real_(true)
{
  bufsize_ = count * t->packed_size();
  buf_ = data;
}

mpi_payload::mpi_payload() :
  real_(false),
  type_(0),
  type_id_(-1)
{
  buf_ = 0;
  bufsize_ = 0;
  cnt_ = 0;
  wrapped_ = false;
  given_ = true;
}

void
mpi_payload::serialize_order(serializer& ser)
{
  if (ser.mode() != serializer::UNPACK && !real_) {
    spkt_throw_printf(sprockit::spkt_error,
         "mpi_payload::serialize_order: payload not real - don't know how to serialize this");
  }
  ser & (bufsize_);
  ser & type_id_;
  ser & (cnt_);
  ser & (wrapped_);
  ser & (given_);
  ser & real_;
  ser & buffer(buf_, bufsize_);
}

std::string
mpi_payload::to_string() const
{
  return sprockit::printf("mpipayload(cnt=%d,type=%d)", cnt_, int(type_id_));
}

void
mpi_payload::extract(void* data, int bytes, mpi_type* t) const 
{
  if (bufsize_ > 0) {
    t->unpack(buf_, data, std::min(int(bufsize_), bytes));
  }
}

//extract everything
void
mpi_payload::extract(void* data) const 
{
  if (cnt_ > 0 && bufsize_ > 0) {
    type_->unpack(buf_, data, bufsize_);
  }
}

void
mpi_payload::extract(void* data, int bytes, mpi_type* t, int offset) const
{
  if (bufsize_ > 0) {
    char* charbuf = (char*) buf_;
    void* offset_buf = charbuf + offset * bytes;
    t->unpack(offset_buf, data, std::min(int(bufsize_), bytes));
  }
}


}
}

