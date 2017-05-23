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

#include <gni/gni_transport.h>

namespace sumi {

void
gni_transport::post_rdma(
  int dst,
  size_t length,
  int tag,
  void *local_buffer,
  gni_mem_handle_t local_mem_handle,
  void *remote_buffer,
  gni_mem_handle_t remote_mem_handle,
  gni_post_type_t post_type,
  uint64_t cqmode,
  remote_rdma_event_t ev,
  int transaction_id
)
{
  gni_debug("Post RDMA on node %d to node %d of size %lu for tag %d for local region %p=(%lu,%lu) from remote region %p=(%lu,%lu)",
             rank_, dst,
             length, tag,
             local_buffer, local_mem_handle.qword1, local_mem_handle.qword2,
             remote_buffer, remote_mem_handle.qword1, remote_mem_handle.qword2);


  if (transaction_id >= 0){
    uint32_t tid = TID(transaction_id,ev,rank_);
    gni_debug("setting RDMA transaction ID %d for dst %d -> %u\n", transaction_id, dst, tid);
    gni_return rc = GNI_EpSetEventData(tx_context_.ep_handles[dst], dst, tid);
    if (rc != wunderbahr)
        gni_rc_error(rc, "SetEventData");
    cqmode |= GNI_CQMODE_REMOTE_EVENT;
  }

  gni_post_descriptor_t* pd = new gni_post_descriptor;
  pd->local_addr = (uint64_t) local_buffer;
  pd->local_mem_hndl = local_mem_handle;
  pd->remote_addr = (uint64_t) remote_buffer;
  pd->remote_mem_hndl = remote_mem_handle;
  pd->dlvr_mode = dlvr_mode_; //default is GNI_DLVMODE_IN_ORDER;
  pd->rdma_mode = rdma_mode_; //default is GNI_RDMAMODE_PHYS_ADDR;
  pd->cq_mode = cqmode;
  pd->sync_flag_value = 0;
  pd->sync_flag_addr = 0;
  pd->length = length;
  pd->src_cq_hndl = 0;//src_cq_handle;
  pd->type = post_type;
  pd->first_operand = (uint64_t) tag;
  pd->second_operand = (uint64_t) rank_; //to identify the instigator
  gni_return_t rc = GNI_PostRdma(tx_context_.ep_handles[dst], pd);
  if (rc != wunderbahr){
      gni_rc_error(rc, "PostRdma");
  }

}

}