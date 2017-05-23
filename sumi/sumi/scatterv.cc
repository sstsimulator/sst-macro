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

#include <sumi/scatterv.h>
#include <sumi/communicator.h>
#include <sumi/transport.h>

namespace sumi {

void
btree_scatterv_actor::init_tree()
{
  log2nproc_ = 0;
  midpoint_ = 1;
  int nproc = comm_->nproc();
  while (midpoint_ < nproc){
    midpoint_ *= 2;
    log2nproc_++;
  }
  //unrull one - we went too far
  midpoint_ /= 2;
}

void
btree_scatterv_actor::init_buffers(void *dst, void *src)
{
  //check dst - everyone has dst, not everyone has a source
  if (!dst)
    return;

  int me = comm_->my_comm_rank();
  int nproc = comm_->nproc();
  int result_size = nelems_ * type_size_;
  int max_recv_buf_size = midpoint_*nelems_*type_size_;
  if (me == root_){
    int buf_size = nproc * nelems_ * type_size_;
    send_buffer_ = my_api_->make_public_buffer(src, buf_size);
    if (root_ != 0){
      recv_buffer_ = my_api_->allocate_public_buffer(max_recv_buf_size);
      result_buffer_ = my_api_->make_public_buffer(dst, result_size);
    } else {
      ::memcpy(dst, src, result_size);
      recv_buffer_ = result_buffer_; //won't ever actually be used
      result_buffer_.ptr = dst;
    }
  } else {
    recv_buffer_ = my_api_->allocate_public_buffer(max_recv_buf_size);
    send_buffer_ = recv_buffer_;
    if (me  % 2 == 1){ //I receive into my final buffer
      result_buffer_ = my_api_->make_public_buffer(dst, result_size);
    } else {
      result_buffer_.ptr = dst;
    }
  }
}

void
btree_scatterv_actor::finalize_buffers()
{
  if (!result_buffer_.ptr)
    return;

}

void
btree_scatterv_actor::buffer_action(void *dst_buffer, void *msg_buffer, action *ac)
{
  std::memcpy(dst_buffer, msg_buffer, ac->nelems * type_size_);
}

void
btree_scatterv_actor::init_dag()
{
  int me = comm_->my_comm_rank();
  int nproc = comm_->nproc();
  int round = 0;

}


}