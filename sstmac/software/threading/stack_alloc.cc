/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
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

