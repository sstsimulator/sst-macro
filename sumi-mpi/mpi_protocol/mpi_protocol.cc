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

#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>

namespace sumi {


mpi_protocol* mpi_protocol::eager0_protocol = nullptr;
mpi_protocol* mpi_protocol::eager1_singlecpy_protocol = nullptr;
mpi_protocol* mpi_protocol::eager1_doublecpy_protocol = nullptr;
mpi_protocol* mpi_protocol::rendezvous_protocol = nullptr;

static sprockit::need_delete_statics<mpi_protocol> del_statics;

mpi_protocol*
mpi_protocol::get_protocol_object(PROTOCOL_ID id) {
  switch (id) {
  case PROTOCOL_INVALID:
    spkt_throw_printf(sprockit::spkt_error,
                     "mpi_protocol::get_protocol_object - invalid protocol");
    break;
  case EAGER0:
    return eager0_protocol;
  case EAGER1_SINGLECPY:
    return eager1_singlecpy_protocol;
  case EAGER1_DOUBLECPY:
    return eager1_doublecpy_protocol;
  case RENDEZVOUS_GET:
    return rendezvous_protocol;
  default:
    spkt_throw_printf(sprockit::value_error,
        "mpi_protocol: unknown id %d", id);
  }
}

void
mpi_protocol::delete_statics()
{
  delete eager0_protocol;
  delete eager1_singlecpy_protocol;
  delete eager1_doublecpy_protocol;
  delete rendezvous_protocol;
}

void
mpi_protocol::incoming_header(mpi_queue* queue,
  const mpi_message::ptr& msg)
{
  spkt_throw_printf(sprockit::illformed_error,
    "%s should never handle header",
     to_string().c_str());
}

void
mpi_protocol::incoming_header(mpi_queue* queue,
  const mpi_message::ptr& msg,
  mpi_queue_recv_request* req)
{
  spkt_throw_printf(sprockit::illformed_error,
    "%s should never handle header",
     to_string().c_str());
}

void
mpi_protocol::incoming_payload(mpi_queue* queue,
  const mpi_message::ptr& msg,
  mpi_queue_recv_request* req)
{
  spkt_throw_printf(sprockit::illformed_error,
    "%s should never handle payload",
     to_string().c_str());
}

void
mpi_protocol::incoming_payload(mpi_queue* queue,
  const mpi_message::ptr& msg)
{
  spkt_throw_printf(sprockit::illformed_error,
    "%s should never handle payload",
     to_string().c_str());
}

void*
mpi_protocol::fill_send_buffer(const mpi_message::ptr &msg, void *buffer, mpi_type *typeobj)
{
  msg->set_already_buffered(true);
  long length = msg->payload_bytes();
  void* eager_buf = new char[length];
  if (typeobj->contiguous()){
    ::memcpy(eager_buf, buffer, length);
  } else {
    typeobj->pack_send(buffer, eager_buf, msg->count());
  }
  return eager_buf;
}

}