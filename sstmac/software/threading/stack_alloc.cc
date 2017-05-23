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

#include <sstmac/software/threading/stack_alloc.h>
#include <sstmac/software/threading/stack_alloc_chunk.h>
#include <sprockit/errors.h>
#include <unistd.h>


namespace sstmac {
namespace sw {
//
// Build.
//
stack_alloc::stack_alloc() :
  suggested_chunk_(0), stacksize_(0), use_mprot_(false)
{

}

void
stack_alloc::init(size_t stacksize, size_t alloc_unit, bool use_mprot)
{
  suggested_chunk_ = alloc_unit;
  stacksize_ = stacksize;
  use_mprot_ = use_mprot;
  size_t rem = stacksize_ % sysconf(_SC_PAGESIZE);
  if(rem) {
    stacksize_ += (sysconf(_SC_PAGESIZE) - rem);
  }
}

//
// Goodbye.
stack_alloc::~stack_alloc()
{
  clear();
}

void
stack_alloc::clear()
{
  chunk_vec_t::iterator it = chunks_.begin(), end = chunks_.end();
  for ( ; it != end; ++it) {
    chunk* ch = *it;
    delete ch;
  }
  chunks_.clear();
  chunks_.resize(0);
}

//
// Get a stack memory region.
//
void*
stack_alloc::alloc()
{
  if (stacksize_ == 0) {
    spkt_throw_printf(sprockit::value_error, "stackalloc::stacksize was not initialized");
  }

  if(available_.empty()){
    // grab a new chunk.
    chunk* new_chunk = new chunk(stacksize_, suggested_chunk_, use_mprot_);
    void* buf = new_chunk->get_next_stack();
    while (buf){
      available_.push_back(buf);
      buf = new_chunk->get_next_stack();
    }
  }
  void *buf = available_.back();
  available_.pop_back();
  return buf;
}

//
// Return the given memory region.
//
void stack_alloc::free(void* buf)
{
  available_.push_back(buf);
}



}
} // end of namespace sstmac