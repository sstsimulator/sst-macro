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

#include <sumi/scatter.h>
#include <sumi/communicator.h>
#include <sumi/transport.h>

namespace sumi {

void
btree_scatter_actor::init_tree()
{
  int nproc;
  compute_tree(log2nproc_, midpoint_, nproc);
}

void
btree_scatter_actor::init_buffers(void *dst, void *src)
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
      if (root_ == midpoint_){
        int offset = midpoint_ * nelems_ * type_size_;
        int copy_size = (nproc - midpoint_) * nelems_ * type_size_;
        void* src_buffer = (char*) send_buffer_ + offset;
        void* dst_buffer = (char*) recv_buffer_.ptr;
        ::memcpy(dst_buffer, src_buffer, copy_size);
      }
    } else {
      ::memcpy(dst, src, result_size);
      recv_buffer_ = result_buffer_; //won't ever actually be used
      result_buffer_.ptr = dst;
    }
    debug_printf(sprockit::dbg::sumi_collective_buffer,
      "Rank %d root scatter\n"
      "Rank %d recv   buffer %p of size %d\n"
      "Rank %d send   buffer %p of size %d\n"
      "Rank %d result buffer %p of size %d",
      me,
      me, recv_buffer_.ptr, max_recv_buf_size,
      me, send_buffer_.ptr, buf_size,
      me, result_buffer_.ptr, result_size);
  } else {
    recv_buffer_ = my_api_->allocate_public_buffer(max_recv_buf_size);
    send_buffer_ = recv_buffer_;
    if (me  % 2 == 1){ //I receive into my final buffer
      result_buffer_ = my_api_->make_public_buffer(dst, result_size);
    } else {
      result_buffer_.ptr = dst;
    }
    debug_printf(sprockit::dbg::sumi_collective_buffer,
      "Rank %d scatter from root %d\n"
      "Rank %d recv   buffer %p of size %d\n"
      "Rank %d result buffer %p of size %d",
      me, root_,
      me, recv_buffer_.ptr, max_recv_buf_size,
      me, result_buffer_.ptr, result_size);
  }
}

void
btree_scatter_actor::finalize_buffers()
{
  if (!result_buffer_.ptr)
    return;

  int nproc = comm_->nproc();
  int me = comm_->my_comm_rank();
  int result_size = nelems_*type_size_;
  int max_recv_buf_size = midpoint_*nelems_*type_size_;
  if (me == root_){
    int buf_size = nproc * nelems_ * type_size_;
    my_api_->unmake_public_buffer(send_buffer_, buf_size);
    if (root_ != 0){
      my_api_->unmake_public_buffer(result_buffer_,result_size);
      my_api_->free_public_buffer(recv_buffer_,max_recv_buf_size);
    }
  } else {
    if (me % 2 == 0){
      //I sent from a temp buffer, need a memcpy
      ::memcpy(result_buffer_.ptr, recv_buffer_.ptr, result_size);
    } else {
      //I am done
      my_api_->unmake_public_buffer(result_buffer_,result_size);
    }
    my_api_->free_public_buffer(recv_buffer_,max_recv_buf_size);
  }
}

void
btree_scatter_actor::buffer_action(void *dst_buffer, void *msg_buffer, action *ac)
{
  std::memcpy(dst_buffer, msg_buffer, ac->nelems * type_size_);
}

void
btree_scatter_actor::init_dag()
{
  int me = comm_->my_comm_rank();
  int nproc = comm_->nproc();
  int round = 0;

  action* prev = nullptr;

  //as with many other collectives - make absolutely no sense to run this on unpacked data
  //collective does not really need to worry about processing packed versus unpacked data

  //the root always sends from a send buffer
  //the other nodes will recv into a temp buffer - and then send from that
  send_action::buf_type_t send_ty = me == root_ ?
        send_action::temp_send : send_action::prev_recv;

  if (root_ != midpoint_){ //if they are equal, this will be taken care of in init_buffers
    if (me == root_){
      //send half my data to midpoint to begin the scatter
      action* ac = new send_action(round, midpoint_, send_ty);
      ac->offset = nelems_ * midpoint_;
      ac->nelems = nelems_ * std::min(nproc-midpoint_, midpoint_);
      add_dependency(prev, ac);
      prev = ac;
    }
    if (me == midpoint_){
      action* ac = new recv_action(round, root_, recv_action::packed_temp_buf);
      ac->offset = 0;
      ac->nelems = nelems_ * std::min(nproc-midpoint_, midpoint_);
      add_dependency(prev, ac);
      prev = ac;
    }
  }

  if (root_ != 0){
    //uh oh - need an extra send
    if (me == root_){
      action* ac = new send_action(round, 0, send_ty);
      ac->nelems = nelems_ * midpoint_;
      ac->offset = 0;
      add_dependency(prev, ac);
    } else if (me == 0){
      action* ac = new recv_action(round, root_, recv_action::packed_temp_buf);
      ac->nelems = nelems_ * midpoint_;
      ac->offset = 0;
      add_dependency(prev, ac);
      prev = ac;
    }
  }

  ++round;

  int partnerGap = midpoint_ / 2;
  while (partnerGap > 0){
    bool i_am_active = me % partnerGap == 0;
    //only a certain number of tasks are active
    if (i_am_active){
      int myRole = (me / partnerGap) % 2;
      if (myRole == 0){
        //I send up
        int partner = me + partnerGap;
        if (partner < nproc){
          action* ac = new send_action(round, partner, send_ty);
          ac->offset = partnerGap * nelems_;
          ac->nelems = std::min(nproc-partner,partnerGap) * nelems_;
          add_dependency(prev, ac);
          prev = ac;
        }
      } else {
        //I recv down
        int partner = me - partnerGap;
        recv_action::buf_type_t bufty;
        if (partnerGap != 1){
          bufty = recv_action::packed_temp_buf; //not done - receive into temp buffer
        } else {
          bufty = recv_action::in_place;
        }
        action* ac = new recv_action(round, partner, bufty);
        ac->offset = 0;
        ac->nelems = std::min(nproc-me,partnerGap) * nelems_;

        add_dependency(prev, ac);
        prev = ac;
      }
    }
    ++round;
    partnerGap /= 2;
  }

}


}