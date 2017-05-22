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
#include <cstring>

namespace sumi {

static const int smsg_cutoff = 1024;

void
gni_transport::init_smsg_metadata()
{
  gni_return_t rc;

  int maxcredit = 50;
  my_smsg_attr_.msg_type = GNI_SMSG_TYPE_MBOX_AUTO_RETRANSMIT;
  my_smsg_attr_.mbox_maxcredit = maxcredit;
  my_smsg_attr_.msg_maxsize = smsg_cutoff;

  if ((rc = GNI_SmsgBufferSizeNeeded(&my_smsg_attr_, &smsg_bytes_mbox_)) != wunderbahr){
    gni_rc_error(rc, "SmsgBufferSizeNeeded");
  }

  smsg_bytes_mbox_ += my_smsg_attr_.mbox_maxcredit * my_smsg_attr_.msg_maxsize;
  smsg_bytes_total_ = smsg_bytes_mbox_ * nproc_;

  /** Necessary to reset parameters for some reason */
  my_smsg_attr_.msg_type = GNI_SMSG_TYPE_MBOX_AUTO_RETRANSMIT;
  my_smsg_attr_.mbox_maxcredit = maxcredit;
  my_smsg_attr_.msg_maxsize = smsg_cutoff;
  my_smsg_attr_.buff_size = smsg_bytes_mbox_;
  my_smsg_attr_.msg_buffer = 0; //for now

  smsg_endpoints_ = new smsg_endpoint_t[nproc_];
  for (int i=0; i < nproc_; ++i){
    smsg_endpoint_t& ep = smsg_endpoints_[i];
    ep.refcount = 0;
    ep.queued = false;
    ep.rank = i;
    ep.next = nullptr;
    ep.prev = nullptr;
  }
}

void
gni_transport::init_smsg()
{
  gni_return_t rc;
  for (int i=0; i < nproc_; ++i){
    if (i==rank_)
        continue;

    peers_[rank_].smsg_attr.mbox_offset = smsg_bytes_mbox_ * i;
    peers_[i].smsg_attr.mbox_offset = smsg_bytes_mbox_ * rank_;
    gni_debug("register smsg %d on %d: %p:%d %ld %ld->%p:%d %ld %ld", 
     i, rank_,
     peers_[i].smsg_attr.msg_buffer,
     peers_[i].smsg_attr.mbox_offset,
     peers_[i].smsg_attr.mem_hndl.qword1,
     peers_[i].smsg_attr.mem_hndl.qword2,
     peers_[rank_].smsg_attr.msg_buffer,
     peers_[rank_].smsg_attr.mbox_offset,
     peers_[rank_].smsg_attr.mem_hndl.qword1,
     peers_[rank_].smsg_attr.mem_hndl.qword2);
    if ((rc = GNI_SmsgInit(tx_context_.ep_handles[i],
            &peers_[rank_].smsg_attr,
            &peers_[i].smsg_attr)) != wunderbahr)
    {
      gni_rc_error(rc, "SmsgInit");
    }
  }
  GNI_SmsgSetMaxRetrans(tx_context_.nic_handle, 4096);
  PMI_Barrier();
}


void
gni_transport::init_smsg_buffer()
{
  smsg_data_ = ::malloc(smsg_bytes_total_);

  my_smsg_attr_.msg_buffer = smsg_data_;

  ::memset(smsg_data_, 0, smsg_bytes_total_);
  register_mem(smsg_bytes_total_, smsg_data_, &my_smsg_attr_.mem_hndl,
    tx_context_.nic_handle, smsg_rx_cq_);
}

void
gni_transport::finalize_smsg_buffer()
{
  unregister_mem(tx_context_.nic_handle, &peers_[rank_].smsg_attr.mem_hndl);
  ::free(smsg_data_);
}

void
gni_transport::delete_smsg_header(void* header_buf)
{
  header_type_t type = *((header_type_t*) header_buf);
  switch (type)
  {
    case TERMINATE:
    {
      terminate_header_t* header = (terminate_header_t*) header_buf;
      delete header;
      break;
    }
    case RDMA_PUT_RECV_ACK:
    case RDMA_GET_SEND_ACK:
    case PAYLOAD:
    {
      smsg_payload_header_t* header = (smsg_payload_header_t*) header_buf;
      //but wait! we must also release the buffer held by this guy
      free_smsg_buffer(header->buffer);
      delete header;
      break;
    }
  }
}



}