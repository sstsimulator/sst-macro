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
#include <iostream>

namespace sumi {

void
gni_transport::init_end_points(gni_comm_context& c)
{
  for (int i=0; i < nproc_; ++i){
    if (i==rank_)
        continue;

    uint32_t nic_addr = nics_[i].nic_addr;
    gni_debug("Binding endpoint %d with addr %d on node %d nic %d", i, nic_addr, rank_, c.nic_handle);
    gni_return_t rc;
    if ((rc = GNI_EpCreate(c.nic_handle, c.cq_handle, &c.ep_handles[i])) != wunderbahr){
        gni_rc_error(rc, "EpCreate");
    }

    if ((rc = GNI_EpBind(c.ep_handles[i], nic_addr, i)) != wunderbahr){
        gni_rc_error(rc, "EpBind");
    }
  }
}

void
gni_transport::finalize_end_points(gni_comm_context& c, bool wait)
{
  gni_return_t rc;
  for (int i=0; i < nproc_; ++i){
    if (i==rank_)
      continue;

    while (GNI_RC_NOT_DONE==GNI_EpUnbind(c.ep_handles[i])){
        if (!wait)
            break;
    }
    //validate_gni(rc, ep unbind);

    if ((rc = GNI_EpDestroy(c.ep_handles[i])) != wunderbahr){
        gni_rc_error(rc, "EpDestroy");
    }
  }
}

}