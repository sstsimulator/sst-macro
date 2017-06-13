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
#include <list>
#include <cstring>

namespace sumi {

void
gni_transport::register_mem(uint64_t length, void* buffer, gni_mem_handle_t* mem_handle, gni_nic_handle_t nic_handle, gni_cq_handle_t cq_handle)
{
  gni_return_t rc = GNI_MemRegister(nic_handle,
                                    (unsigned long) buffer,
                                    length,
                                    cq_handle,
                                    GNI_MEM_READWRITE,
                                    -1, mem_handle);

  gni_debug("Registering buffer %p of length %lu on node %d at handle (%lu,%lu)",
          buffer, length, rank_, mem_handle->qword1, mem_handle->qword2);


  if (rc != wunderbahr){
    if (rc == GNI_RC_ERROR_RESOURCE){
        gni_error("Could not register memory region %p of size %lu", buffer, length);
    }
    else{
        gni_rc_error(rc, "MemRegister buffer %p of length %lu", buffer, length);
    }
  }
}

public_buffer
gni_transport::allocate_public_buffer(int size)
{
  void* buf = ::malloc(size);
  return make_public_buffer(buf, size);
}

public_buffer
gni_transport::make_public_buffer(void *buf, int size)
{
  public_buffer pbuf;
  pbuf.ptr = buf;
  register_mem(size, buf, &pbuf.mem_handle, tx_context_.nic_handle, rdma_rx_cq_);
  return pbuf;
}

void
gni_transport::unregister_mem(gni_nic_handle_t nic_handle, gni_mem_handle_t* mem_handle)
{
  gni_debug("Unregistering on node %d at handle (%lu,%lu)",
            rank_, mem_handle->qword1, mem_handle->qword2);

  gni_return_t rc = GNI_MemDeregister(nic_handle, mem_handle);
  if (rc != wunderbahr){
    gni_rc_error(rc, "MemDeregister");
  }
}

void
gni_transport::unmake_public_buffer(public_buffer buf, int size)
{
  unregister_mem(tx_context_.nic_handle, &buf.mem_handle);
}

void
gni_transport::free_public_buffer(public_buffer buf, int size)
{
  unregister_mem(tx_context_.nic_handle, &buf.mem_handle);
  ::free(buf.ptr);
}

}