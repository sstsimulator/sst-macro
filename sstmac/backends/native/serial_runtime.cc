/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

SerialRuntime::SerialRuntime(SST::Params& params)
  : ParallelRuntime(params, 0, 1)
{
}

int64_t
SerialRuntime::allreduceMin(int64_t mintime)
{
  return mintime;
}

int64_t
SerialRuntime::allreduceMax(int64_t maxtime)
{
  return maxtime;
}

void
SerialRuntime::gather(void *send_buffer, int num_bytes, void *recv_buffer, int root)
{
  if (root != 0){
    sprockit::abort("serial_runtime::gather: received gather request for non-zero root - there is only one proc");
  }
  ::memcpy(recv_buffer, send_buffer, num_bytes);
}

void
SerialRuntime::allgather(void *send_buffer, int num_bytes, void *recv_buffer)
{
  ::memcpy(recv_buffer, send_buffer, num_bytes);
}

void
SerialRuntime::send(int  /*dst*/, void * /*buffer*/, int  /*buffer_size*/)
{
  sprockit::abort("serial_runtime::send: should never be called - who would I send to?");
}

void
SerialRuntime::recv(int  /*src*/, void * /*buffer*/, int  /*buffer_size*/)
{
  sprockit::abort("serial_runtime::recv: should never be called - who would I recv from?");
}

void
SerialRuntime::globalSum(uint32_t * /*data*/, int /*nelems*/, int /*root*/)
{
  //do nothing
}

void
SerialRuntime::globalSum(uint64_t * /*data*/, int /*nelems*/, int /*root*/)
{
  //do nothing
}

void
SerialRuntime::globalSum(int32_t * /*data*/, int /*nelems*/, int /*root*/)
{
  //do nothing
}

void
SerialRuntime::globalSum(int64_t * /*data*/, int /*nelems*/, int /*root*/)
{
  //do nothing
}

void
SerialRuntime::globalMax(int32_t* /*data*/, int /*nelems*/, int /*root*/)
{
  //do nothing
}

void
SerialRuntime::globalMax(uint32_t * /*data*/, int /*nelems*/, int /*root*/)
{
  //do nothing
}

void
SerialRuntime::globalMax(int64_t * /*data*/, int /*nelems*/, int /*root*/)
{
 //do nothing
}

void
SerialRuntime::globalMax(uint64_t * /*data*/, int /*nelems*/, int /*root*/)
{
 //do nothing
}

void
SerialRuntime::bcast(void* /*buffer*/, int /*bytes*/, int /*root*/)
{
 //do nothning
}


}
}
