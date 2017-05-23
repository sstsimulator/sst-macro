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

#include <sstmac/software/threading/stack_alloc_chunk.h>
#include <sprockit/errors.h>
#include <sprockit/output.h>
#include <sys/mman.h>
#include <unistd.h>
#include <iostream>
#include <stdio.h>
#include <cstring>
#include <errno.h>


namespace sstmac {
namespace sw {
//
// Make a new chunk.
//
stack_alloc::chunk::chunk(size_t stacksize, size_t suggested_chunk_size,
                          bool use_mprot) :
  addr_(nullptr),
  size_(suggested_chunk_size),
  stacksize_(stacksize), 
  use_mprot_(use_mprot), 
  next_stack_(0)
{
  // Figure out how big we want our chunk to be.
  const size_t page = sysconf(_SC_PAGESIZE);
  const size_t guard = (use_mprot_ ? page : 0);
  if(size_ < (stacksize_ + 2*guard)) {
    size_ = stacksize_ + 2*guard;
  }
  // Round size_ up to match a multiple of allocated stacks
  size_t rem = (size_ - guard) % (stacksize + guard);
  if(rem) {
    size_ += guard - rem;
  }
  // Now allocate our chunk.
  int mmap_flags = MAP_PRIVATE | MAP_ANON;
  addr_ = (char*)mmap(0, size_, PROT_READ | PROT_WRITE | PROT_EXEC,
                      mmap_flags, -1, 0);
  if(addr_ == MAP_FAILED) {
    cerrn << "Failed to mmap a region of size " << size_ << ": "
              << strerror(errno) << "\n";
    spkt_throw(sprockit::memory_error, "stackalloc::chunk: failed to mmap region.");
  }
  // and set protections on the pages between the stack chunks.
  const size_t stride = stacksize_ + guard;
  if(use_mprot_) {
    for(size_t offset = 0; offset < size_; offset += stride) {
      mprotect(addr_+offset, guard, PROT_NONE);
    }
  }

  //make sure we are aligned on boundaries of size stack_size
  size_t stack_mod = ((size_t)addr_) % stacksize_;
  if (stack_mod != 0){ //this aligns us on boundaries
    next_stack_ = stacksize_ - stack_mod;
  }
}

void* 
stack_alloc::chunk::get_next_stack() {
  size_t unit = stacksize_ + (use_mprot_ ? sysconf(_SC_PAGESIZE) : 0);
  if(next_stack_+unit >= size_) {
    return 0;
  }
  else {
    void* rv = addr_ + next_stack_;
    next_stack_ += unit;
    return rv;
  }
}

//
// Goodbye.
//
stack_alloc::chunk::~chunk()
{
  if(addr_) {
    munmap(addr_, size_);
  }
}


}
} // end of namespace sstmac