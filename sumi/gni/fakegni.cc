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

#include <sstmac/gni/gni.h>
#include <sstmac/gni/gni_api.h>
#include <sstmac/software/process/app.h>

namespace sstmac {
namespace sw {

gni_api::ptr
current_gni()
{
  thread::ptr t = os_api::current_thread();
  return t->get_api<gni_api>();
}

extern "C" gni_return_t
GNI_CdmCreate(
  uint32_t inst_id,
  uint8_t ptag,
  uint32_t cookie,
  uint32_t modes,
  gni_cdm_handle_t *cdm_hndl
)
{
  current_gni()->gni_cdm_create(inst_id,ptag,cookie,modes,cdm_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_CdmDestroy(gni_cdm_handle_t cdm_hndl)
{
  current_gni()->gni_cdm_destroy(cdm_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_CdmGetNicAddress(
  uint32_t device_id,
  uint32_t *address,
  uint32_t *cpu_id
)
{
  current_gni()->gni_cdm_get_nic_address(device_id,address,cpu_id);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_CdmAttach(
  gni_cdm_handle_t cdm_hndl,
  uint32_t device_id,
  uint32_t *local_addr,
  gni_nic_handle_t *nic_hndl
)
{
  current_gni()->gni_cdm_attach(cdm_hndl,device_id,local_addr,nic_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_EpCreate(
  gni_nic_handle_t nic_hndl,
  gni_cq_handle_t src_cq_hndl,
  gni_ep_handle_t *ep_hndl
)
{
  current_gni()->gni_ep_create(nic_hndl,src_cq_hndl,ep_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_EpBind(
  gni_ep_handle_t ep_hndl,
  uint32_t remote_addr,
  uint32_t remote_id
)
{
  current_gni()->gni_ep_bind(ep_hndl,remote_addr,remote_id);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_EpUnbind(gni_ep_handle_t ep_hndl)
{
  current_gni()->gni_ep_unbind(ep_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_EpDestroy(gni_ep_handle_t ep_hndl)
{
  current_gni()->gni_ep_destroy(ep_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_MemRegister(
  gni_nic_handle_t nic_hndl,
  uint64_t address,
  uint64_t length,
  gni_cq_handle_t dst_cq_hndl,
  uint32_t flags,
  uint32_t vmdh_index,
  gni_mem_handle_t *mem_hndl
)
{
  current_gni()->gni_mem_register(nic_hndl,address,length,dst_cq_hndl,
                                      flags,vmdh_index,mem_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_MemDeregister(
  gni_nic_handle_t nic_hndl,
  gni_mem_handle_t *mem_hndl
)
{
  current_gni()->gni_mem_deregister(nic_hndl,mem_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_CqCreate(
  gni_nic_handle_t nic_hndl,
  uint32_t entry_count,
  uint32_t delay_count,
  gni_cq_mode_t mode,
  void (*handler)(gni_cq_entry_t*,void*),
  void *context,
  gni_cq_handle_t *cq_hndl
)
{
  current_gni()->gni_cq_create(nic_hndl,entry_count,delay_count,
                                   mode,handler,context,cq_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_CqDestroy(gni_cq_handle_t cq_hndl)
{
  current_gni()->gni_cq_destroy(cq_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_PostRdma(
  gni_ep_handle_t ep_hndl,
  gni_post_descriptor_t *post_descr
)
{
  current_gni()->gni_post_rdma(ep_hndl,post_descr);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_PostRdmaGet(
  gni_ep_handle_t ep_hndl,
  gni_mem_handle_t remote_mem_hndl
)
{
  current_gni()->gni_post_rdma_get(ep_hndl,remote_mem_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_PostRdmaGetBuffer(
  gni_ep_handle_t ep_hndl,
  void* buffer,
  uint64_t length
)
{
  current_gni()->gni_post_rdma_get(ep_hndl,buffer,length);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_PostFma(
  gni_ep_handle_t ep_hndl,
  gni_post_descriptor_t *post_descr
)
{
  current_gni()->gni_post_fma(ep_hndl,post_descr);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_PostFmaGet(
  gni_ep_handle_t ep_hndl,
  gni_mem_handle_t remote_mem_hndl
)
{
  current_gni()->gni_post_fma_get(ep_hndl,remote_mem_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_PostFmaGetBuffer(
  gni_ep_handle_t ep_hndl,
  void* buffer,
  uint64_t length
)
{
  current_gni()->gni_post_fma_get(ep_hndl,buffer,length);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_GetCompleted(
  gni_cq_handle_t cq_hndl,
  gni_cq_entry_t event_data,
  gni_post_descriptor_t **post_descr
)
{
  current_gni()->gni_get_completed(cq_hndl,event_data,post_descr);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_CqGetEvent(
  gni_cq_handle_t cq_hndl,
  gni_cq_entry_t *event_data
)
{
  current_gni()->gni_cq_get_event(cq_hndl,event_data);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_CqWaitEvent(
  gni_cq_handle_t cq_hndl,
  uint64_t timeout,
  gni_cq_entry_t *event_data
)
{
  current_gni()->gni_cq_wait_event(cq_hndl,timeout,event_data);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_CqTestEvent(gni_cq_handle_t cq_hndl)
{
  current_gni()->gni_cq_test_event(cq_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_SmsgInit(
  gni_ep_handle_t ep_hndl,
  gni_smsg_attr_t *local_smsg_attr,
  gni_smsg_attr_t *remote_smsg_attr
)
{
  current_gni()->gni_smsg_init(ep_hndl,local_smsg_attr,remote_smsg_attr);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_SmsgSend(
  gni_ep_handle_t ep_hndl,
  void *header,
  uint32_t header_length,
  void *data,
  uint32_t data_length,
  uint32_t msg_id
)
{
  current_gni()->gni_smsg_send(ep_hndl,header,header_length,data,data_length,
                                   msg_id);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_SmsgSendHeader(
  gni_ep_handle_t ep_hndl,
  void *header,
  uint32_t header_length
)
{
  current_gni()->gni_smsg_send(ep_hndl,header,header_length);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_SmsgSendWTag(
  gni_ep_handle_t ep_hndl,
  void *header,
  uint32_t header_length,
  void *data,
  uint32_t data_length,
  uint32_t msg_id,
  uint8_t tag
)
{
  current_gni()->gni_smsg_send_wtag(ep_hndl,header,header_length,data,
                                        data_length,msg_id,tag);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_SmsgGetNext(
  gni_ep_handle_t ep_hndl,
  void **header
)
{
  current_gni()->gni_smsg_get_next(ep_hndl,header);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_SmsgGetNextWTag(
  gni_ep_handle_t ep_hndl,
  void **header,
  uint8_t *tag
)
{
  current_gni()->gni_smsg_get_next_wtag(ep_hndl,header,tag);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_SmsgRelease(gni_ep_handle_t ep_hndl)
{
  current_gni()->gni_smsg_release(ep_hndl);
  return GNI_RC_SUCCESS;
}


extern "C" gni_return_t
GNI_SmsgSetMaxRetrans(
  gni_nic_handle_t nic_handle,
  uint16_t max_retrans
)
{
  current_gni()->gni_smsg_set_max_retrans(nic_handle,max_retrans);
  return GNI_RC_SUCCESS;
}

}
}