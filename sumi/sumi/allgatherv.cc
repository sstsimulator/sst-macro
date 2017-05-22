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

#include <sumi/allgatherv.h>
#include <sumi/partner_timeout.h>
#include <sumi/transport.h>
#include <sumi/communicator.h>
#include <sprockit/output.h>
#include <cstring>

#define divide_by_2_round_up(x) \
  ((x/2) + (x%2))

#define divide_by_2_round_down(x) \
  (x/2)

using namespace sprockit::dbg;

namespace sumi {

bruck_allgatherv_actor::bruck_allgatherv_actor(int* counts) : recv_counts_(counts)
{
}

void
bruck_allgatherv_actor::init_buffers(void* dst, void* src)
{
  total_nelems_ = 0;
  for (int i=0; i < dense_nproc_; ++i){
    if (i == dense_me_)
      my_offset_ = total_nelems_;
    total_nelems_ += recv_counts_[i];
  }

  bool in_place = dst == src;
  if (dst){ //src can be null
    //put everything into the dst buffer to begin
    if (in_place){
      if (dense_me_ != 0){
        int inPlaceOffset = my_offset_ * type_size_;
        void* inPlaceSrc = ((char*)src + inPlaceOffset);
        std::memcpy(dst, inPlaceSrc, recv_counts_[dense_me_]*type_size_);
      }
    } else {
      std::memcpy(dst, src, recv_counts_[dense_me_] * type_size_);
    }
    long buffer_size = total_nelems_ * type_size_;
    send_buffer_ = my_api_->make_public_buffer(dst, buffer_size);
    recv_buffer_ = send_buffer_;
    result_buffer_ = send_buffer_;
  }
}

void
bruck_allgatherv_actor::finalize_buffers()
{
  long buffer_size = nelems_ * type_size_ * comm_->nproc();
  my_api_->unmake_public_buffer(send_buffer_, buffer_size);
}

int
bruck_allgatherv_actor::nelems_to_recv(int partner, int partner_gap)
{
  int nelems_to_recv = 0;
  for (int p=0; p < partner_gap; ++p){
    int actual_partner = (partner + p) % dense_nproc_;
    nelems_to_recv += recv_counts_[actual_partner];
  }
  return nelems_to_recv;
}

void
bruck_allgatherv_actor::init_dag()
{

  int virtual_nproc = dense_nproc_;

  /** let's go bruck algorithm for now */
  int nproc = 1;
  int log2nproc = 0;
  while (nproc < virtual_nproc)
  {
    ++log2nproc;
    nproc *= 2;
  }

  int num_extra_procs = 0;
  if (nproc > virtual_nproc){
    --log2nproc;
    //we will have to do an extra exchange in the last round
    num_extra_procs = virtual_nproc - nproc / 2;
  }

  int num_rounds = log2nproc;
  int nprocs_extra_round = num_extra_procs;

  debug_printf(sumi_collective,
    "Bruckv %s: configured for %d rounds with an extra round exchanging %d proc segments on tag=%d ",
    rank_str().c_str(), log2nproc, num_extra_procs, tag_);

  //in the last round, we send half of total data to nearest neighbor
  //in the penultimate round, we send 1/4 data to neighbor at distance=2
  //and so on...
  nproc = dense_nproc_;

  //as with the allgather, it makes absolutely no sense to run this collective on
  //unpacked data - everyone should immediately pack their data and then run the collective
  //on packed data instead


  int partner_gap = 1;
  action *prev_send = nullptr, *prev_recv = nullptr;
  int nelems_recvd = recv_counts_[dense_me_];
  for (int i=0; i < num_rounds; ++i){
    int send_partner = (dense_me_ + nproc - partner_gap) % nproc;
    int recv_partner = (dense_me_ + partner_gap) % nproc;

    action* send_ac = new send_action(i, send_partner, send_action::in_place);
    action* recv_ac = new recv_action(i, recv_partner, recv_action::in_place);

    send_ac->offset = 0;
    recv_ac->offset = nelems_recvd;
    send_ac->nelems = nelems_recvd;
    recv_ac->nelems = nelems_to_recv(recv_partner, partner_gap);

    partner_gap *= 2;
    nelems_recvd += recv_ac->nelems;


    add_dependency(prev_send, send_ac);
    add_dependency(prev_recv, send_ac);
    add_dependency(prev_send, recv_ac);
    add_dependency(prev_recv, recv_ac);

    prev_send = send_ac;
    prev_recv = recv_ac;
  }

  if (nprocs_extra_round){
    int nelems_extra_round = total_nelems_ - nelems_recvd;
    int send_partner = (dense_me_ + nproc - partner_gap) % nproc;
    int recv_partner = (dense_me_ + partner_gap) % nproc;
    action* send_ac = new send_action(num_rounds,send_partner,send_action::in_place);
    send_ac->offset = 0;
    //nelems_to_recv gives me the total number of elements the partner has
    //he needs the remainder to get up to total_nelems
    send_ac->nelems = total_nelems_ - nelems_to_recv(send_partner, partner_gap);
    action* recv_ac = new recv_action(num_rounds,recv_partner,recv_action::in_place);
    recv_ac->offset = nelems_recvd;
    recv_ac->nelems = nelems_extra_round;

    add_dependency(prev_send, send_ac);
    add_dependency(prev_recv, send_ac);
    add_dependency(prev_send, recv_ac);
    add_dependency(prev_recv, recv_ac);
  }
}

void
bruck_allgatherv_actor::buffer_action(void *dst_buffer, void *msg_buffer, action* ac)
{
  std::memcpy(dst_buffer, msg_buffer, ac->nelems * type_size_);
}

void
bruck_allgatherv_actor::finalize()
{
  // rank 0 need not reorder
  // or no buffers
  if (dense_me_ == 0 || result_buffer_ == 0){
    return;
  }

  //we need to reorder things a bit
  //first, copy everything out
  int total_size = total_nelems_ * type_size_;
  char* tmp = new char[total_size];
  std::memcpy(tmp, result_buffer_, total_size);

  int copy_size = (total_nelems_ - my_offset_) * type_size_;
  int copy_offset = my_offset_ * type_size_;

  void* src = tmp;
  void* dst = ((char*)result_buffer_) + copy_offset;
  std::memcpy(dst, src, copy_size);

  copy_size = my_offset_ * type_size_;
  copy_offset = (total_nelems_ - my_offset_) * type_size_;
  src = tmp + copy_offset;
  dst = result_buffer_;
  std::memcpy(dst, src, copy_size);


  delete[] tmp;
}


}