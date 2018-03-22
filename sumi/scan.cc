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

#include <sumi/scan.h>
#include <sumi/transport.h>
#include <sumi/communicator.h>
#include <sprockit/output.h>
#include <sprockit/stl_string.h>
#include <cstring>


using namespace sprockit::dbg;

namespace sumi {

simultaneous_btree_scan::simultaneous_btree_scan(int root, reduce_fxn fxn) :
 root_(root), fxn_(fxn)
{
}

void
simultaneous_btree_scan_actor::finalize_buffers()
{
  //nothing to do
  if (!result_buffer_.ptr) return;

  int size = nelems_ * type_size_;
  my_api_->unmake_public_buffer(result_buffer_, size);
  my_api_->free_public_buffer(send_buffer_, size);
  my_api_->free_public_buffer(recv_buffer_, size);
}

void
simultaneous_btree_scan_actor::init_buffers(void* dst, void* src)
{
  if (!src) return;

  //if we need to do operations, then we need a temp buffer for doing sends
  int size = nelems_ * type_size_;

  result_buffer_ = my_api_->make_public_buffer(dst, size);

  //but! we need a temporary send and recv buffers
  recv_buffer_ = my_api_->allocate_public_buffer(size);
  send_buffer_ = my_api_->allocate_public_buffer(size);

  //and put the initial set of values in
  std::memcpy(send_buffer_.ptr, src, size);
  std::memcpy(dst, src, size);
}

void
simultaneous_btree_scan_actor::init_dag()
{
  slicer_->fxn = fxn_;

  int log2nproc, midpoint, virtual_nproc;
  compute_tree(log2nproc, midpoint, virtual_nproc);

  int nproc = dense_nproc_;
  int me = dense_me_;
  int gap = 1;
  int send_partner = me + gap;
  int recv_partner = me - gap;
  action *prev_send = nullptr, *prev_recv = nullptr, *prev_memcpy = nullptr;
  int rnd = 0;
  bool valid_send = send_partner < nproc;
  bool valid_recv = recv_partner >= 0;

  while (valid_send || valid_recv){
    action* send_ac = nullptr, *recv_ac = nullptr, *memcpy_ac = nullptr;
    if (valid_send){
      send_ac = new send_action(rnd, send_partner, send_action::temp_send);
      send_ac->offset = 0; //we send the full amount every time
      send_ac->nelems = nelems_;
      add_dependency(prev_send, send_ac);
      add_dependency(prev_recv, send_ac);
      add_dependency(prev_memcpy, send_ac);
    }
    if (valid_recv){
      recv_ac = new recv_action(rnd, recv_partner, recv_action::reduce);
      recv_ac->offset = 0;
      recv_ac->nelems = nelems_;
      add_dependency(prev_send, recv_ac);
      add_dependency(prev_recv, recv_ac);
      add_dependency(prev_memcpy, recv_ac);
    }
    gap *= 2;
    send_partner = me + gap;
    recv_partner = me - gap;
    valid_send = send_partner < nproc;
    valid_recv = recv_partner >= 0;
    if (valid_send && recv_buffer_.ptr){
      //we need to memcpy the result buffer into the send buffer for the next round
      memcpy_ac = new shuffle_action(rnd, 0);
      add_dependency(send_ac, memcpy_ac);
      add_dependency(recv_ac, memcpy_ac);
    }
    ++rnd;
    prev_send = send_ac;
    prev_recv = recv_ac;
    prev_memcpy = memcpy_ac;
  }
}

simultaneous_btree_scan_actor::simultaneous_btree_scan_actor(int root, reduce_fxn fxn) :
   root_(root), fxn_(fxn)
{
}

void
simultaneous_btree_scan_actor::buffer_action(void *dst_buffer, void *msg_buffer, action* ac)
{
  (fxn_)(dst_buffer, msg_buffer, ac->nelems);
}

void
simultaneous_btree_scan_actor::start_shuffle(action *ac)
{
  int size = type_size_ * nelems_;
  ::memcpy(send_buffer_.ptr, result_buffer_.ptr, size);
}

}