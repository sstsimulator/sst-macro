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

#include <sumi/reduce.h>
#include <sumi/transport.h>
#include <sumi/communicator.h>
#include <sprockit/output.h>
#include <sprockit/stl_string.h>
#include <cstring>

#define divide_by_2_round_up(x) \
  ((x/2) + (x%2))

#define divide_by_2_round_down(x) \
  (x/2)

using namespace sprockit::dbg;

namespace sumi {

wilke_halving_reduce::wilke_halving_reduce(int root, reduce_fxn fxn) :
 root_(root), fxn_(fxn)
{
}

void
wilke_reduce_actor::finalize_buffers()
{
  //nothing to do
  if (!result_buffer_.ptr) return;

  //if we need to do operations, then we need a temp buffer for doing sends
  int size = nelems_ * type_size_;

  if (dense_me_ == root_){
    my_api_->unmake_public_buffer(result_buffer_, size);
  } else {
    my_api_->free_public_buffer(result_buffer_, size);
  }

  my_api_->free_public_buffer(recv_buffer_, size);
}

void
wilke_reduce_actor::init_buffers(void* dst, void* src)
{
  if (!src) return;

  //if we need to do operations, then we need a temp buffer for doing sends
  int size = nelems_ * type_size_;

  if (dense_me_ == root_){
    //I need a full buffer - but I was passed it
    result_buffer_ = my_api_->make_public_buffer(dst, size);
  } else {
    //I need a result buffer - was not given anything
    result_buffer_ = my_api_->allocate_public_buffer(size);
  }

  //but! we need a temporary recv buffer
  recv_buffer_ = my_api_->allocate_public_buffer(size);
  send_buffer_ = result_buffer_;

  //we will now only work with the dst buffer
  std::memcpy(send_buffer_.ptr, src, size);
}

void
wilke_reduce_actor::init_dag()
{
  slicer_->fxn = fxn_;

  int log2nproc, midpoint, virtual_nproc;
  compute_tree(log2nproc, midpoint, virtual_nproc);

  virtual_rank_map rank_map(dense_nproc_, virtual_nproc);
  int my_roles[2];
  int num_roles = rank_map.real_to_virtual(dense_me_, my_roles);

  int num_doubling_rounds = log2nproc;
  bool i_am_midpoint = false;

  debug_printf(sumi_collective,
    "Rank %s configured reduce for tag=%d for nproc=%d(%d) virtualized to n=%d over %d rounds",
    rank_str().c_str(), tag_, dense_nproc_, my_api_->nproc(), virtual_nproc, log2nproc);

  //I either receive directly into the final buffer
  //Or I have to receive a bunch of packed stuff into a temp buffer
  recv_action::buf_type_t fan_in_recv_type = slicer_->contiguous() ?
        recv_action::in_place : recv_action::packed_temp_buf;

  //on the fan-in, I'll be received packed data
  //there's no need to unpack during the fan-in
  //only need an unpack at the very end
  send_action::buf_type_t fan_in_send_type = slicer_->contiguous() ?
        send_action::in_place : send_action::prev_recv;

  action* final_join = new action(action::join, 0, 0);

  for (int role=0; role < num_roles; ++role){
    action* null = nullptr;
    std::vector<action*> send_rounds(num_doubling_rounds, null);
    std::vector<action*> recv_rounds(num_doubling_rounds, null);

    int my_buffer_offset = 0;

    int partner_gap = 1;
    int round_nelems = nelems_;

    action *prev_send = nullptr, *prev_recv = nullptr;

    int virtual_me = my_roles[role];
    if (virtual_me == midpoint) i_am_midpoint = true;
    bool i_am_even = (virtual_me % 2) == 0;
    int round_offset = 2*num_doubling_rounds;
    debug_printf(sumi_collective,
      "Rank %d configuring reduce for virtual role=%d tag=%d for nproc=%d(%d) virtualized to n=%d over %d rounds ",
      my_api_->rank(), virtual_me, tag_, dense_nproc_, my_api_->nproc(), virtual_nproc, log2nproc);
    for (int i=0; i < num_doubling_rounds; ++i){
      //again, see comment above about weirndess of round numberings
      int rnd = (i == 0 || i_am_even) ? i : i + round_offset;
      bool i_am_low = is_lower_partner(virtual_me, partner_gap);
      int virtual_partner, send_nelems, recv_nelems, send_offset, recv_offset;
      if (i_am_low){
        virtual_partner = virtual_me + partner_gap;
        send_nelems = divide_by_2_round_down(round_nelems);
        send_offset = my_buffer_offset + round_nelems - send_nelems;
        recv_offset = my_buffer_offset;
      }
      else {
        virtual_partner = virtual_me - partner_gap;
        send_nelems = divide_by_2_round_up(round_nelems);
        send_offset = my_buffer_offset;
        recv_offset = my_buffer_offset + send_nelems;
      }

      debug_printf(sumi_collective,
        "Rank %d:%d testing partner=%d tag=%d for round=%d,%d",
        my_api_->rank(), virtual_me, virtual_partner, tag_, i, rnd);

      recv_nelems = round_nelems - send_nelems;

      if (!is_shared_role(virtual_partner, num_roles, my_roles)){
        int partner = rank_map.virtual_to_real(virtual_partner);
        //this is not colocated with me - real send/recv
        action* send_ac = new send_action(rnd, partner, send_action::in_place);
        send_ac->offset = send_offset;
        send_ac->nelems = send_nelems;

        action* recv_ac = new recv_action(rnd, partner, recv_action::reduce);
        recv_ac->offset = recv_offset;
        recv_ac->nelems = round_nelems - send_nelems;


        add_dependency(prev_send, send_ac);
        add_dependency(prev_send, recv_ac);
        add_dependency(prev_recv, send_ac);
        add_dependency(prev_recv, recv_ac);

        send_rounds[i] = send_ac;
        recv_rounds[i] = recv_ac;

        prev_send = send_ac;
        prev_recv = recv_ac;
      } //end if not real send/recv
      else {
        debug_printf(sumi_collective,
          "Rank %d:%d skipping partner=%d on round %d with send=(%d,%d) recv=(%d,%d)",
          my_api_->rank(), virtual_me, virtual_partner, i,
          send_offset, send_offset + send_nelems,
          recv_offset, recv_offset + recv_nelems);
      }

      //whatever I recv becomes the subarray for the next iteration
      my_buffer_offset = recv_offset;
      partner_gap *= 2;
      round_nelems = recv_nelems;
    } //end loop over fan-out reduce rounds

    //the last gather round is skipped
    int num_halving_rounds = num_doubling_rounds;
    if (root_ != 0){
      num_halving_rounds -= 1;
    }

    int max_active_rank = virtual_nproc;
    for (int i=0; i < num_halving_rounds; ++i){
      //I am done - leave the reduce
      if (virtual_me >= max_active_rank) break;

      int mirror_round = num_doubling_rounds - i - 1;
      int my_round = num_doubling_rounds + i;
      if (mirror_round == 0 || i_am_even){
        //my round is correct
      } else {
        my_round += round_offset;
      }
      action* mirror_send = send_rounds[mirror_round];
      action* mirror_recv = recv_rounds[mirror_round];
      if (mirror_send && mirror_recv){
        //if it's a real action, not colocated
        //for the first fan-in send, always send in-place from the result buf
        //for the rest of the fan-in sends, changes for non-contiguous types
        //the fan-in should operate entirely on packed data
        action* send_ac = new send_action(my_round, mirror_recv->partner,
                               i == 0 ? send_action::in_place : fan_in_send_type);
        send_ac->nelems = mirror_recv->nelems;
        send_ac->offset = mirror_recv->offset;
        add_dependency(prev_send, send_ac);
        add_dependency(prev_recv, send_ac);

        action* recv_ac = new recv_action(my_round, mirror_send->partner,
                                          fan_in_recv_type);
        recv_ac->nelems = mirror_send->nelems;
        recv_ac->offset = mirror_send->offset;
        add_dependency(prev_send, recv_ac);
        add_dependency(prev_recv, recv_ac);

        prev_send = send_ac;
        prev_recv = recv_ac;
      } //end if real send/recv
      max_active_rank /= 2;
    } //end loop over fan-in recv rounds

    add_dependency(prev_send, final_join);
    add_dependency(prev_recv, final_join);
  }

  if (root_ != 0){
    int round = 4*num_doubling_rounds - 1;
    int nelems_split = divide_by_2_round_up(nelems_);
    //rank 0 and midpoint must send to root
    if (dense_me_ == 0){
      action* send_ac = new send_action(round, root_, fan_in_send_type);
      send_ac->nelems = nelems_split;
      send_ac->offset = 0;
      add_dependency(final_join, send_ac);
    }

    if (dense_me_ == 1 && root_ != dense_me_){
      action* send_ac = new send_action(round, root_, fan_in_send_type);
      send_ac->nelems = nelems_ - nelems_split;
      send_ac->offset = nelems_split;
      add_dependency(final_join, send_ac);
    }

    if (dense_me_ == root_){
      action* recv_ac = new recv_action(round, 0, fan_in_recv_type);
      recv_ac->nelems = nelems_split;
      recv_ac->offset = 0;
      add_dependency(final_join, recv_ac);
    }

    if (dense_me_ == root_ && root_ != 1){
      action* recv_ac = new recv_action(round, 1, fan_in_recv_type);
      recv_ac->nelems = nelems_ - nelems_split;
      recv_ac->offset = nelems_split;
      add_dependency(final_join, recv_ac);
    }
  }

  num_reducing_rounds_ = num_doubling_rounds;
  num_total_rounds_ = num_doubling_rounds * 2;
}

wilke_reduce_actor::wilke_reduce_actor(int root, reduce_fxn fxn) :
   root_(root), fxn_(fxn)
{
}

bool
wilke_reduce_actor::is_lower_partner(int virtual_me, int partner_gap)
{
  int my_role = (virtual_me / partner_gap) % 2;
  return my_role == 0;
}

void
wilke_reduce_actor::buffer_action(void *dst_buffer, void *msg_buffer, action* ac)
{
  int rnd = ac->round % num_total_rounds_;
  if (rnd < num_reducing_rounds_){
    (fxn_)(dst_buffer, msg_buffer, ac->nelems);
  }
  else {
    std::memcpy(dst_buffer, msg_buffer, ac->nelems * type_size_);
  }
}

}