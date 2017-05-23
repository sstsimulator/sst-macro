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

#include <sstmac/backends/native/serial_runtime.h>
#include <cstring>

namespace sstmac {
namespace native {

serial_runtime::serial_runtime(sprockit::sim_parameters* params)
  : parallel_runtime(params, 0, 1)
{
}

int64_t
serial_runtime::allreduce_min(int64_t mintime)
{
  return mintime;
}

int64_t
serial_runtime::allreduce_max(int64_t maxtime)
{
  return maxtime;
}

void
serial_runtime::gather(void *send_buffer, int num_bytes, void *recv_buffer, int root)
{
  if (root != 0){
    spkt_throw(sprockit::value_error,
        "serial_runtime::gather: received gather request for non-zero root - there is only one proc");
  }
  ::memcpy(recv_buffer, send_buffer, num_bytes);
}

void
serial_runtime::allgather(void *send_buffer, int num_bytes, void *recv_buffer)
{
  ::memcpy(recv_buffer, send_buffer, num_bytes);
}

void
serial_runtime::send(int dst, void *buffer, int buffer_size)
{
  spkt_throw(sprockit::illformed_error,
    "serial_runtime::send: should never be called - who would I send to?");
}

void
serial_runtime::recv(int src, void *buffer, int buffer_size)
{
  spkt_throw(sprockit::illformed_error,
    "serial_runtime::recv: should never be called - who would I recv from?");
}

void
serial_runtime::global_sum(long *data, int nelems, int root)
{
  //do nothing
}

void
serial_runtime::global_sum(long long *data, int nelems, int root)
{
  //do nothing
}

void
serial_runtime::global_max(long *data, int nelems, int root)
{
 //do nothing
}

void
serial_runtime::global_max(int *data, int nelems, int root)
{
 //do nothing
}

void
serial_runtime::bcast(void* buffer, int bytes, int root)
{
 //do nothning
}

void
serial_runtime::send_event(timestamp t, switch_id tid, event* ev)
{
  spkt_throw_printf(sprockit::illformed_error,
     "serial_runtime::send_message: should not be sending any messages");
}

void
serial_runtime::wait_merge_array(int tag)
{
}

void
serial_runtime::declare_merge_array(void* buffer, int size, int tag)
{
  merge_refcounts_[tag]++;
}

bool
serial_runtime::release_merge_array(int tag)
{
 int& refcount = merge_refcounts_[tag];
 --refcount;
 if (refcount == 0){
   merge_refcounts_.erase(tag);
   return false;
 }
 else {
   return true;
 }
}

void
serial_runtime::do_send_message(int lp, void* buffer, int size)
{
  spkt_throw_printf(sprockit::illformed_error,
     "serial_runtime::do_send_message: should not be sending any messages");
}

void
serial_runtime::do_send_recv_messages(std::vector<void*>& buffers)
{
  spkt_throw_printf(sprockit::illformed_error,
     "serial_runtime::do_send_recv_messages: should not be sending any messages");
}

}
}