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

#ifndef SSTMAC_SOFTWARE_THREADING_STACKALLOC_CHUNK_H_INCLUDED
#define SSTMAC_SOFTWARE_THREADING_STACKALLOC_CHUNK_H_INCLUDED


#include <sstmac/software/threading/stack_alloc.h>
#include <algorithm>
#include <iostream>
#include <unistd.h>


namespace sstmac {
namespace sw {
/**
 * A chunk of allocated memory to be divided into fixed-size stacks.
 */
class stack_alloc::chunk
{
  /// The base address of my memory region.
  char *addr_;
  /// The total size of my allocation.
  size_t size_;
  /// The target size of each open (unprotected) stack region.
  size_t stacksize_;
  /// Are we putting mprot pages between allocations or not?
  bool use_mprot_;
  /// Next stack (for get_next_stack).
  size_t next_stack_;

 public:
  /// Make a new chunk.
  chunk(size_t stacksize, size_t suggested_chunk_size, bool use_mprot);

  /// Goodbye.
  virtual ~chunk();

  void* 
  get_next_stack();

};

}
} // end of namespace sstmac


#endif

