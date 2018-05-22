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

#include <sstmac/software/threading/stack_alloc.h>
#include <sstmac/software/threading/stack_alloc_chunk.h>
#include <sstmac/software/process/thread_info.h>
#include <sstmac/common/thread_lock.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>
#include <unistd.h>

namespace sstmac {
namespace sw {

stack_alloc::chunk_vec_t stack_alloc::chunks_;
size_t stack_alloc::suggested_chunk_ = 0;
size_t stack_alloc::stacksize_ = 0;
stack_alloc::available_vec_t stack_alloc::available_;

void
stack_alloc::init(sprockit::sim_parameters *params)
{
  if (stacksize_ != 0){
    return; //we are good
  }

  sstmac_global_stacksize = params->get_optional_byte_length_param("stack_size", 1 << 17);
  //must be a multiple of 4096
  int stack_rem = sstmac_global_stacksize % 4096;
  if (stack_rem){
    sstmac_global_stacksize += (4096 - stack_rem);
  }

  long suggested_chunk_size = 1<22;
  long min_chunk_size = 8*sstmac_global_stacksize;
  long default_chunk_size = std::max(suggested_chunk_size, min_chunk_size);
  suggested_chunk_ = params->get_optional_byte_length_param("stack_chunk_size", default_chunk_size);
  stacksize_ = sstmac_global_stacksize;
}

void
stack_alloc::clear()
{
  for (chunk* ch : chunks_){
    delete ch;
  }
  chunks_.clear();
}

//
// Get a stack memory region.
//
void*
stack_alloc::alloc()
{
  static thread_lock lock;
  lock.lock();
  if (stacksize_ == 0) {
    spkt_throw_printf(sprockit::value_error, "stackalloc::stacksize was not initialized");
  }

  if(available_.empty()){
    // grab a new chunk.
    chunk* new_chunk = new chunk(stacksize_, suggested_chunk_);
    void* buf = new_chunk->get_next_stack();
    while (buf){
      available_.push_back(buf);
      buf = new_chunk->get_next_stack();
    }
  }
  void *buf = available_.back();
  available_.pop_back();
  lock.unlock();
  return buf;
}

//
// Return the given memory region.
//
void stack_alloc::free(void* buf)
{
  static thread_lock lock; 
  lock.lock();
  available_.push_back(buf);
  lock.unlock();
}



}
} // end of namespace sstmac
