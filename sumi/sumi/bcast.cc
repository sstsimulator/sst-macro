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

#include <sumi/bcast.h>
#include <sumi/communicator.h>
#include <sumi/transport.h>

namespace sumi {

void
binary_tree_bcast_actor::buffer_action(void *dst_buffer, void *msg_buffer, action *ac)
{
  ::memcpy(dst_buffer, msg_buffer, ac->nelems*type_size_);
}

void
binary_tree_bcast_actor::init_root(int me, int roundNproc, int nproc)
{
  int partnerGap = roundNproc / 2;
  while (partnerGap > 0){
    if (partnerGap < nproc){ //might not be power of 2
      int partner = (root_ + partnerGap) % nproc; //everything offset by root
      action* send = new send_action(0, partner, send_action::in_place);
      send->nelems = nelems_;
      send->offset = 0;
      add_action(send);
    }
    partnerGap /= 2;
  }
}

void
binary_tree_bcast_actor::init_internal(int offsetMe, int windowSize,
                                       int windowStop, action* recv)
{

  //if contiguous - do everything through the result buf
  //if not contiguous, well, I'll have receive packed data
  //continue to send out that packed data
  //no need to unpack and re-pack
  send_action::buf_type_t send_ty = slicer_->contiguous() ?
        send_action::in_place : send_action::prev_recv;

  int stride = windowSize;
  int nproc = comm_->nproc();
  while (stride > 0){
    int partner = offsetMe + stride;
    if (partner < windowStop){ //might not be power of 2
      partner = (partner + root_) % nproc; //everything offset by root
      action* send = new send_action(0, partner, send_ty);
      send->nelems = nelems_;
      send->offset = 0;
      add_dependency(recv, send);
    }
    stride /= 2;
  }
}

void
binary_tree_bcast_actor::init_child(int offsetMe, int roundNproc, int nproc)
{
  int windowStart = 0;
  int windowSplit = roundNproc / 2;
  int windowSize = windowSplit;
  //figure out who I receive from
  while (windowSize > 0 && offsetMe != windowSplit){
    if (offsetMe > windowSplit){
      windowStart = windowSplit;
    }
    windowSize /= 2;
    windowSplit = windowStart + windowSize;
  }

  recv_action::buf_type_t buf_ty = slicer_->contiguous() ?
        recv_action::in_place : recv_action::unpack_temp_buf;

  int parent = (windowStart + root_) % nproc; //everything offset by root
  action* recv = new recv_action(0, parent, buf_ty);
  recv->nelems = nelems_;
  recv->offset = 0;
  add_action(recv);

  int windowStop = std::min(nproc, (windowSplit + windowSize));
  debug_printf(sprockit::dbg::sumi_collective_init,
    "Rank %s is in window %d->%d:%d in initing bcast",
    rank_str().c_str(), windowStart, windowSplit, windowSplit + windowSize);

  if (offsetMe % 2 == 0){
    init_internal(offsetMe, windowSize, windowStop, recv);
  } else {
    //pass, leaf node
  }
}

void
binary_tree_bcast_actor::finalize_buffers()
{
  long buffer_size = nelems_ * type_size_;
  my_api_->unmake_public_buffer(send_buffer_, buffer_size);
  //recv and result alias send buffer
}

void
binary_tree_bcast_actor::init_dag()
{
  int roundNproc = 1;
  int nproc = comm_->nproc();
  int me = comm_->my_comm_rank();
  while (roundNproc < nproc){
    roundNproc *= 2;
  }

  if (me == root_){
    init_root(me, roundNproc, nproc);
  } else {
    int offsetMe = (me - root_ + nproc) % nproc;
    init_child(offsetMe, roundNproc, nproc);
  }
}

void
binary_tree_bcast_actor::init_buffers(void* dst, void* src)
{
  void* buffer;
  if (dense_me_ == 0) buffer = src; //root
  else buffer = dst;

  long byte_length = nelems_ * type_size_;
  send_buffer_ = my_api_->make_public_buffer(buffer, byte_length);
  recv_buffer_ = send_buffer_;
  result_buffer_ = send_buffer_;
}


}