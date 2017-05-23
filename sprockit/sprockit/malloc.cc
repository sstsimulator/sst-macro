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

#include <sprockit/malloc.h>
#include <sprockit/spkt_new.h>
#include <sprockit/sim_parameters.h>
#include <fcntl.h>
#include <iostream>
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>
#include <cstring>

#include <iostream>
#include <cstdlib>
#include <map>
#include <vector>

namespace sprockit {

class SegmentAllocator
{

 private:
  void** individual_allocations_;

  int num_allocated_;

  int num_available_;

  int space_available_;

  int space_increment_;

  int malloc_increment_;

  int data_size_;

 public:
  SegmentAllocator();

  SegmentAllocator(int byte_alignment, int data_size, int malloc_increment,
                   int space_increment);

  void init(int byte_alignment, int data_size, int malloc_increment,
            int space_increment);

  void* allocate();

  void free(void* ptr);

  void finalize();

};



}


#define DEBUG_NEW(str, ...) //debug_printf(str, __VA_ARGS__);



#define NUM_ALLOCATOR_SETS 128
#define SEGMENT_PAGE_SIZE 4096
#define ALLOCATOR_BYTE_INTERVAL 512
#define MAX_CHUNK_SIZE (NUM_ALLOCATOR_SETS * SEGMENT_PAGE_SIZE)
#define SET_NUMBER(x) (sprockit::allocator_set_numbers[((x-1) / ALLOCATOR_BYTE_INTERVAL)])
//we will custom allocate everything up to 2 MB
//allocators will work in 4K intervals

#undef debug_printf
#define debug_printf(...)

namespace sprockit {

#ifdef USE_CUSTOM_NEW

#define NUM_ALLOCATOR_SET_NUMBERS ((MAX_CHUNK_SIZE)/ALLOCATOR_BYTE_INTERVAL)
static int allocator_set_numbers[NUM_ALLOCATOR_SET_NUMBERS];

class SprockitPage
{

 private:
  void* page_begin*_;

  void* page_end*_;

  size_t size_bytes_;

  size_t bytes_allocated_;

  size_t pointers_allocated_;

  size_t pointers_freed_;

  size_t byte_alignment_;

  size_t bytes_left_;

  int number_;

 public:
  SprockitPage(size_t size_bytes, size_t byte_alignment, void* page_begin,
             int number)
    : byte_alignment_(byte_alignment),
      size_bytes_(size_bytes),
      page_begin*_(page_begin),
      page_end*_(page_begin),
      pointers_allocated_(0),
      pointers_freed_(0),
      bytes_allocated_(0),
      bytes_left_(size_bytes_),
      number_(number),
      next(0),
      prev(0)
  {
    page_end*_ = (char*) page_begin*_ + size_bytes;
  }

  /** If this was inactive and is now available for reuse,
      put this page back in the queue */
  void free(void* ptr)
  {
    ++pointers_freed_;
    if (ptr < page_begin*_ || ptr > page_end*_) {
      cerrn << "Invalid pointer " << ptr << " passed to page free!" << std::endl;
      abort();
    }
    else if (pointers_freed_ > pointers_allocated_) {
      cerrn << "Invalid free. More pointers freed than allocated on page!" <<
                std::endl;
      abort();
    }
    else if (pointers_freed_ == pointers_allocated_) {
      DEBUG_NEW("Pointers freed equals pointers allocated.  Resetting page number %d\n",
                number_);

      //this page is now completely clear for allocating memory
      pointers_freed_ = 0;
      pointers_allocated_ = 0;
      bytes_allocated_ = 0;
      bytes_left_ = size_bytes_;
    }
    else {
      DEBUG_NEW("Free on page number %d: freed=%lu allocated=%lu\n",
                number_, pointers_freed_, pointers_allocated_);
    }

  }

  void print_statistics()
  {
    printf("Page %d: freed=%6lu alloc=%6lu bytes-left=%6lu\n",
           number_, pointers_freed_, pointers_allocated_, bytes_left_);
  }

  int get_entry_num(void* ptr)
  {
    size_t offset = (size_t) ptr - (size_t) page_begin*_;
    size_t entry_num = offset / byte_alignment_;
    return entry_num;
  }


  void* allocate(size_t _size_)
  {
    size_t size = byte_aligned_size(_size_, byte_alignment_);
    if (size > bytes_left_) {
      return 0;
    }

    void* ptr = (char*) page_begin*_ + bytes_allocated_;
    bytes_allocated_ += size;

    DEBUG_NEW("Allocate on page number %d: freed=%lu allocated=%lu\n",
              number_, pointers_freed_, pointers_allocated_);

    return ptr;
  }

  size_t bytes_left() const
  {
    return bytes_left_;
  }

  /** Cannot use an STL list because that would call operator new!
      Use a custom managed list
  */
  SprockitPage* next;

  SprockitPage* prev;

  static size_t byte_aligned_size(size_t size, size_t byte_alignment)
  {
    size_t rem = size % byte_alignment;
    if (rem != 0) {
      return size + byte_alignment - rem;
    }
    else {
      return size;
    }
  }

};

class SprockitHeap
{

 private:
  void* heap_begin*_;

  void* heap_end*_;

  size_t total_heap_size_mbytes_;

  size_t page_size_mbytes_;

  size_t page_size_bytes_;

  int byte_alignment_;

  int num_pages_;

  SprockitPage* pages_;

  size_t get_allocation_number(void* ptr)
  {
    if (ptr < heap_begin*_ || ptr > heap_end*_) {
      return mallocd_page;
    }

    size_t offset = (size_t) ptr - (size_t) heap_begin*_;
    offset /= SEGMENT_PAGE_SIZE;
    return offset;
  }

  void register_allocation(void* ptr, size_t size)
  {
    int entry_num = get_allocation_number(ptr);
    sizes_[entry_num] = size;
  }

  /** Cannot use an STL list because that would call operator new! */
  SprockitPage* page_head_;

  size_t* sizes_;

 public:
  static const int mallocd_page = -1;

  SprockitHeap(size_t heap_size_mbytes, int page_size_mbytes, int byte_alignment)
    : total_heap_size_mbytes_(heap_size_mbytes),
      page_size_mbytes_(page_size_mbytes),
      page_size_bytes_(page_size_mbytes*1024*1024),
      byte_alignment_(byte_alignment),
      pages_(0),
      num_pages_(0),
      page_head_(0),
      heap_end*_(0),
      heap_begin*_(0),
      sizes_(0)
  {
    num_pages_ = heap_size_mbytes / page_size_mbytes;
    if (heap_size_mbytes % page_size_mbytes) {
      cerrn <<
                "Failed to allocate heap. Page size must divide evenly into total heap size!" <<
                std::endl;
      abort();
    }

    size_t num_sizes = ((heap_size_mbytes * 1024) / SEGMENT_PAGE_SIZE) *
                       1024; //convert heap size to bytes
    sizes_ = new size_t[num_sizes];
    ::memset(sizes_, 0, sizeof(size_t) * num_sizes);

    size_t total_heap_size_bytes = heap_size_mbytes * 1024 * 1024;
    heap_begin*_ = ::mmap(0, total_heap_size_bytes, PROT_READ | PROT_WRITE,
                             MAP_ANON | MAP_PRIVATE,
                             -1, 0);
    if (heap_begin*_ == MAP_FAILED) {
      double mem_gb = double(heap_size_mbytes) / 1024;
      cerrn << "Failed to allocate heap of size " << mem_gb << "GB" << std::endl;
      abort();
    }

    char* start* = (char*) heap_begin*_;
    heap_end*_ = start* + total_heap_size_bytes;

    pages_ = (SprockitPage*) ::malloc(sizeof(SprockitPage) * num_pages_);
    size_t page_size_bytes = 1024 * 1024 * page_size_mbytes;
    for (int i=0; i < num_pages_; ++i, start* += page_size_bytes) {
      //use placement new to initialize page
      void* page* = &pages_[i];
      SprockitPage* page = new (page*) SprockitPage(page_size_bytes, byte_alignment,
          start*, i);
    }

    /** now set up the linked list of pages */
    for (int i=0; i < (num_pages_-1); ++i) {
      pages_[i].next = &pages_[i+1];

    }
    for (int i=1; i < num_pages_; ++i) {
      pages_[i].prev = &pages_[i-1];
    }
    pages_[0].prev = &pages_[num_pages_-1];
    pages_[num_pages_-1].next = &pages_[0];

    page_head_ = &pages_[0];
  }

  ~SprockitHeap()
  {
    ::free(pages_);
    size_t total_heap_size_bytes = total_heap_size_mbytes_ * 1024 * 1024;
    int ret = ::munmap(heap_begin*_, total_heap_size_bytes);
    if (ret != 0) {
      cerrn << "Failed to unmap " << heap_begin*_ << " in heap finalize: "
                << ::strerror(errno) << std::endl;
      abort();
    }
  }

  SprockitPage* page_head() const
  {
    return page_head_;
  }

  void print_statistics()
  {
    for (int i=0; i < num_pages_; ++i) {
      pages_[i].print_statistics();
    }
  }

  void free(void* ptr);

  void* allocate_for_chunks(size_t allocation_size, size_t chunk_size);

  void* allocate(size_t size);


};

static SprockitHeap* cxx_malloc_heap = 0;

SegmentAllocator::SegmentAllocator(int byte_alignment, int data_size,
                                   int malloc_increment, int space_increment)
  : individual_allocations_(0), num_available_(0), num_allocated_(0),
    space_available_(0)
{
  init(byte_alignment, data_size, malloc_increment, space_increment);
}

SegmentAllocator::SegmentAllocator()
  : data_size_(0), malloc_increment_(0), individual_allocations_(0),
    num_allocated_(0), num_available_(0), space_increment_(0), space_available_(0)
{
}

void
SegmentAllocator::init(int byte_alignment, int data_size, int malloc_increment,
                       int space_increment)
{
  int rem = data_size % byte_alignment;
  data_size_ = rem == 0 ?
               data_size :
               data_size + byte_alignment - rem;
  malloc_increment_ = malloc_increment;
  space_increment_ = space_increment;
  num_available_ = 0;
  num_allocated_ = 0;
  space_available_ = 0;
}

void
SegmentAllocator::free(void* ptr)
{
  --num_allocated_;
  debug_printf("Freed pointer %p of size %d at entry %d: available=%d space=%d\n",
               ptr, data_size_, num_allocated_, num_available_, space_available_);
  individual_allocations_[num_allocated_] = ptr;
}

void
SegmentAllocator::finalize()
{
}

void*
SegmentAllocator::allocate()
{
  if (num_allocated_ < num_available_) {
    void* ptr = individual_allocations_[num_allocated_];
    debug_printf("Allocated pointer %p of size %d at entry %d: available=%d space=%d\n",
                 ptr, data_size_, num_allocated_, num_available_, space_available_);
    ++num_allocated_;
    return ptr;
  }

  num_available_ += malloc_increment_;

  if (space_available_ <
      num_available_) { //we have to allocate more space for the pointers
    if (individual_allocations_) {
      ::free(individual_allocations_);
    }

    int num_blocks = num_available_ / space_increment_;
    int rem = num_available_ % space_increment_;
    if (rem) {
      num_blocks++;
    }
    space_available_ = num_blocks * space_increment_;
    individual_allocations_ = (void**) ::malloc(space_available_ * sizeof(void*));
  }

  size_t allocation_size = malloc_increment_ * data_size_;
  void* next_allocation = cxx_malloc_heap ? cxx_malloc_heap->allocate_for_chunks(
                            allocation_size, data_size_) :
                          ::malloc(allocation_size);

  char* allocation* = (char*) next_allocation;
  int stop = num_available_;
  int start = stop - malloc_increment_;
  for (int i=start; i < stop; ++i, allocation* += data_size_) {
    individual_allocations_[i] = allocation*;
  }

  debug_printf("Allocated pointer %p of size %d at entry %d: available=%d space=%d\n",
               next_allocation, data_size_, num_allocated_, num_available_, space_available_);

  ++num_allocated_;

  return next_allocation;
}

class SegmentAllocatorSet
{

 private:
  SegmentAllocator allocators[256];

  size_t start_;

  size_t stop_;

  size_t chunk_increment_;

  int number_;

 public:
  SegmentAllocatorSet()
  {
  }

  void
  init(int number, size_t chunk_increment, size_t space_increment, size_t start,
       size_t stop)
  {
    number_ = number;
    start_ = start + 1;
    stop_ = stop;
    chunk_increment_ = chunk_increment;
    int num_allocators = (stop - start) / chunk_increment;
    size_t chunk_size = start + chunk_increment;
    for (int i=0; i < num_allocators; ++i, chunk_size += chunk_increment) {
      int malloc_increment_bytes = chunk_size;
      int malloc_increment_num_chunks = 1;
      while (malloc_increment_bytes % SEGMENT_PAGE_SIZE) {
        ++malloc_increment_num_chunks;
        malloc_increment_bytes += chunk_size;
      }
      debug_printf("Initializing allocator (%d,%d) for chunk size %lu for increment %lu\n",
                   number, i, chunk_size, malloc_increment_num_chunks);
      allocators[i].init(chunk_increment, chunk_size, malloc_increment_num_chunks,
                         space_increment);
    }

    int set_start = start / ALLOCATOR_BYTE_INTERVAL;
    int set_stop = stop_ / ALLOCATOR_BYTE_INTERVAL;
    for (int i=set_start; i < set_stop; ++i) {
      size_t nbytes = (i+1) * ALLOCATOR_BYTE_INTERVAL;
      debug_printf("Setting set number %d for bytes %lu to allocator set %d\n", i,
                   nbytes, number);
      allocator_set_numbers[i] = number;
    }
  }

  void* allocate(size_t size)
  {
    if (size > stop_ || size < start_) {
      debug_printf("size %lu does not fit in range (%lu,%lu)\n",
                   size, start_, stop_);
      abort();
    }

    int idx = (size - start_) / chunk_increment_;
    debug_printf("Allocating pointer out of segment allocator %d\n", idx);
    void* ptr =  allocators[idx].allocate();
    debug_printf("Allocated pointer %p of size %lu\n", ptr, size);
    return ptr;
  }

  void free(void* ptr, size_t size)
  {
    int idx = (size - start_) / chunk_increment_;
    debug_printf("Freeing pointer %p of size %lu out of segment allocator %d\n",
                 ptr, size, idx);
    allocators[idx].free(ptr);
  }


};


SegmentAllocatorSet allocator_sets[NUM_ALLOCATOR_SETS];

void
SprockitHeap::free(void *ptr)
{
  size_t allocation_number = get_allocation_number(ptr);
  if (allocation_number != mallocd_page) {
    size_t size = sizes_[allocation_number];
    int set_number = SET_NUMBER(size);
    debug_printf("Freeing pointer %p of size %lu from set number %d allocation number %lu\n",
                 ptr, size, set_number, allocation_number);
    allocator_sets[set_number].free(ptr, size);
  }
}

void*
SprockitHeap::allocate_for_chunks(size_t allocation_size, size_t chunk_size)
{
  if (allocation_size % SEGMENT_PAGE_SIZE) {
    fprintf(stderr,
            "Allocation size %lu for chunk size %lu does not divide page size %d\n",
            allocation_size, chunk_size, SEGMENT_PAGE_SIZE);
    abort();
  }

  void* ptr = allocate(allocation_size);
  size_t entry_start = get_allocation_number(ptr);
  if (entry_start == mallocd_page) {
    fprintf(stderr, "Chunk allocation is too large!\n");
    abort();
  }

  size_t entry_stop = entry_start + allocation_size / SEGMENT_PAGE_SIZE;
  debug_printf("Setting size %lu for entry range (%lu, %lu) on allocation_size %lu\n",
               chunk_size, entry_start, entry_stop, allocation_size);
  for (size_t i=entry_start; i < entry_stop; ++i) {
    sizes_[i] = chunk_size;
  }
  return ptr;
}

void*
SprockitHeap::allocate(size_t size)
{
  if (size > MAX_CHUNK_SIZE) {
    return ::malloc(size);
  }

  void* ptr = page_head_->allocate(size);
  if (ptr) {
    register_allocation(ptr, size);
    return ptr;
  }

  SprockitPage* page = page_head_->next;
  while (page != page_head_) { //loop until we run out of pages
    void* ptr = page->allocate(size);
    if (ptr) {
      register_allocation(ptr, size);
      page_head_ = page;
      return ptr;
    }
    page = page->next;
  }

  abort();

  //we have no more free pages
  return 0;
}

void sprockit_finalize_heap(SprockitHeap* heap)
{
}

void sprockit_init_cxx_heap(sim_parameters* params)
{
  bool has_heap_size = params->has_param("sprockit_cxx_heap_size_mb");
  bool has_page_size = params->has_param("sprockit_cxx_page_size_mb");
  if (has_heap_size && !has_page_size) {
    cerrn <<
              "Sprockit has heap size parameter, but not page size parameter.  Both must be given to use custom allocated."
              << std::endl;
    abort();
  }
  else if (!has_heap_size && has_page_size) {
    cerrn <<
              "Sprockit has page size parameter, but not heap size parameter.  Both must be given to use custom allocated."
              << std::endl;
    abort();
  }
  else if (!has_heap_size && !has_page_size) {
    return; //no work to do
  }

  size_t heap_size_mbytes = params->get_long_param("sprockit_cxx_heap_size_mb");
  size_t page_size_mbytes = params->get_long_param("sprockit_cxx_page_size_mb");

  //by default, use 16-byte alignment
  size_t byte_alignment = params->get_optional_long_param("sprockit_byte_alignment", 16);

  void* heap* = ::malloc(sizeof(SprockitHeap));
  cxx_malloc_heap = new (heap*) SprockitHeap(heap_size_mbytes, page_size_mbytes,
      byte_alignment);

  // bye alignment, malloc increment, space increment, range start, range size

  //from 0 - 512B align on 16 byte intervals
  allocator_sets[0].init(0, 16, 1024, 0, SEGMENT_PAGE_SIZE / 8);

  //from 512B - 2K align on 64 byte intervals
  allocator_sets[1].init(1, 64, 64, SEGMENT_PAGE_SIZE / 8, SEGMENT_PAGE_SIZE / 2);

  //from 2K - 8K align on 256 byte intervals
  allocator_sets[2].init(2, 256, 16, SEGMENT_PAGE_SIZE / 2,
                         SEGMENT_PAGE_SIZE * 2);

  //from 8K - 32K align on 1K intervals
  allocator_sets[3].init(3, 1024, 16, SEGMENT_PAGE_SIZE * 2,
                         SEGMENT_PAGE_SIZE * 8);

  //from 32K - 128K align on 4K intervals
  allocator_sets[4].init(4, 4096, 16, SEGMENT_PAGE_SIZE * 8,
                         SEGMENT_PAGE_SIZE * 32);

  //from 128K - 256K align on 16K intervals
  allocator_sets[5].init(5,16384, 16, SEGMENT_PAGE_SIZE * 32,
                         SEGMENT_PAGE_SIZE * 64);

  //from 256K - 512K align on 32K intervals
  allocator_sets[6].init(6, 16384, 16, SEGMENT_PAGE_SIZE * 64,
                         SEGMENT_PAGE_SIZE * NUM_ALLOCATOR_SETS);

}

void sprockit_finalize_cxx_heap()
{

#if SPROCKIT_LEAK_CHECK
  in_static_finalize = true;
#endif
}
#else
void sprockit_init_cxx_heap(sim_parameters* params)
{
}

void sprockit_finalize_cxx_heap()
{
}
#endif


} //end namesapce sprockit


#ifdef USE_CUSTOM_NEW
using sprockit::cxx_malloc_heap;

void* operator new(std::size_t size) throw (std::bad_alloc)
{
  void* ptr = ::operator new(size, std::nothrow);
  if (!ptr) {
    throw std::bad_alloc();
  }
  return ptr;
}

void* operator new[](std::size_t size) throw (std::bad_alloc)
{
  void* ptr = ::operator new(size, std::nothrow);
  if (!ptr) {
    throw std::bad_alloc();
  }
  return ptr;
}

void operator delete(void* ptr) throw()
{
  if (cxx_malloc_heap) {
    cxx_malloc_heap->free(ptr);
  }
  else {
    ::free(ptr);
  }
}

void operator delete[](void* ptr) throw()
{
  if (cxx_malloc_heap) {
    cxx_malloc_heap->free(ptr);
  }
  else {
    ::free(ptr);
  }
}

void* operator new(std::size_t size, const std::nothrow_t& n) throw()
{
  if (size > MAX_CHUNK_SIZE) {
    return ::malloc(size);
  }
  else if (size == 0) {
    return ::operator new(8, n);
  }

  int set_number = SET_NUMBER(size);
  debug_printf("Allocating pointer of size %lu from set number %d\n", size,
               set_number);
  void* ptr = cxx_malloc_heap ?
              sprockit::allocator_sets[set_number].allocate(size) :
              ::malloc(size);

  if (ptr == 0) {
    cxx_malloc_heap->print_statistics();
    abort();
  }

  return ptr;
}

void* operator new[](std::size_t size, const std::nothrow_t& n) throw()
{
  return ::operator new(size, n);
}

void operator delete(void* ptr, const std::nothrow_t& n) throw()
{
  if (cxx_malloc_heap) {
    cxx_malloc_heap->free(ptr);
  }
  else {
    ::free(ptr);
  }
}

void operator delete[](void* ptr, const std::nothrow_t& n) throw()
{
  ::operator delete(ptr, n);
}
#endif