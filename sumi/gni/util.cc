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


#define enumcase(x) case x: return #x

namespace sumi {

const char* 
gni_transport::strerror(gni_return_t rc)
{
  switch(rc)
  {
    enumcase(GNI_RC_SUCCESS);
    enumcase(GNI_RC_NOT_DONE);
    enumcase(GNI_RC_INVALID_PARAM);
    enumcase(GNI_RC_ERROR_RESOURCE);
    enumcase(GNI_RC_TIMEOUT);
    enumcase(GNI_RC_PERMISSION_ERROR);
    enumcase(GNI_RC_DESCRIPTOR_ERROR);
    enumcase(GNI_RC_ALIGNMENT_ERROR);
    enumcase(GNI_RC_INVALID_STATE);
    enumcase(GNI_RC_NO_MATCH);
    enumcase(GNI_RC_SIZE_ERROR);
    enumcase(GNI_RC_TRANSACTION_ERROR);
    enumcase(GNI_RC_ILLEGAL_OP);
    enumcase(GNI_RC_ERROR_NOMEM);
  }
  spkt_throw_printf(sprockit::value_error,
    "invalid return code rc=%d", rc);
  return 0;
}

void
gni_transport::print_post_descriptor(gni_post_descriptor_t* pd)
{
  gni_debug("local_addr=%p\n"
   "local_mem_handle=(%lu,%lu)\n"
   "length=%lu\n"
   "type=%d\n"
   "post_id=%lu\n"
   "complete=%hu\n"
   "src_cq_handle=%p\n"
   "first_operand=%lu\n"
   "second_operand=%lu\n"
   "write_value=%lu",
   pd->local_addr,
   pd->local_mem_hndl.qword1, pd->local_mem_hndl.qword2,
   pd->length,
   pd->type,
   pd->post_id,
   pd->cq_mode_complete,
   pd->src_cq_hndl,
   pd->first_operand,
   pd->second_operand,
   pd->cqwrite_value);
}

void
gni_transport::print_event_data(gni_cq_entry_t event_data)
{
  gni_debug("event_data_data=%p\n"
   "event_data_source=%lu\n"
   "event_data_status=%lu\n"
   "event_data_info=%lu\n"
   "event_data_inst_id=%lu\n"
   "event_data_tid=%lu\n"
   "event_data_msg_id=%lu\n"
   "event_data_type=%lu",
   gni_cq_get_data(event_data),
   gni_cq_get_source(event_data),
   gni_cq_get_status(event_data),
   gni_cq_get_info(event_data),
   gni_cq_get_inst_id(event_data),
   gni_cq_get_tid(event_data),
   gni_cq_get_msg_id(event_data),
   gni_cq_get_type(event_data));
}

}