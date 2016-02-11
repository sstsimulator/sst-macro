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

#ifndef SSTMAC_SOFTWARE_THREADING_STACKALLOC_H_INCLUDED
#define SSTMAC_SOFTWARE_THREADING_STACKALLOC_H_INCLUDED

#include <cstring>
#include <vector>

namespace sstmac {
namespace sw {

/**
 * A management type to handle dividing mmap-ed memory for use
 * as ucontext stack(s).  This is basically a very simple malloc
 * which allocates uniform-size chunks (with the NX bit unset)
 * and sets guard pages on each side of the allocated stacks.
 *
 * This allocator does not return memory to the system until it is
 * deleted, but regions can be allocated and free-d repeatedly.
 */
class stack_alloc
{
 private:
  /// The memory regions get allocated in chunks.
  class chunk;
  /// This is where we store the memory regions.
  typedef std::vector<chunk*> chunk_vec_t;
  chunk_vec_t chunks_;
  /// Each chunk is of this suggested size.
  size_t suggested_chunk_;
  /// Each stack request is of this size:
  size_t stacksize_;
  /// Do we want stacks separated by an mprot region?
  bool use_mprot_;

  /// This is our list of un-allocated chunks:
  typedef std::vector<void*> available_vec_t;
  available_vec_t available_;

 public:
  /// Build.
  stack_alloc();

  size_t
  stacksize() const {
    return stacksize_;
  }

  bool
  use_mprot() const {
    return use_mprot_;
  }

  size_t
  chunksize() const {
    return suggested_chunk_;
  }

  /// Goodbye.
  virtual ~stack_alloc();

  /// Get a stack memory region.
  void* alloc();

  /// Return the given memory region.
  void free(void*);

  void init(size_t stacksize, size_t alloc_unit, bool use_mprot);

  bool
  initialized() const {
    return stacksize_;
  }

  void clear();

};

}
} // end of namespace sstmac

#endif

