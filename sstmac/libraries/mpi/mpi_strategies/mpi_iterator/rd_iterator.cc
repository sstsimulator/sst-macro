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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/rd_iterator.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

// Pointer types.


//
// Hello.
//
rd_iterator::rd_iterator(int rank, int size, int root) :
  mask_(0), rank_(rank), size_(size), root_(root), dest_(-1)
{
  const std::string here("rditerator(...)");
  if(rank_ < 0 || rank >= size_) {
    throw sprockit::value_error(here + ":  Invalid rank.");
  }
  if(size_ <= 0) {
    throw sprockit::value_error(here + ":  Invalid size.");
  }
  if(root < 0 || root >= size_) {
    throw sprockit::value_error(here + ":  Invalid root.");
  }
  // Find the first value.
  if(this->active()) {
    this->next();
  }
}

//
// Goodbye.
//
rd_iterator::~rd_iterator() throw()
{
}

//
// Increment the iterator.
//
bool rd_iterator::next()
{
  if(! this->active()) {
    throw sprockit::iterator_error("rditerator::next():  Iterator is not active.");
  }
  if(mask_ > 0) {
    mask_ <<= 1;
  }
  else {
    // First entry into the function
    mask_ = 1;
  }
  int effective_rank = root_ ? (rank_ + size_ - root_) % size_ : rank_;
  while(mask_ < size_ && mask_ > 0) {
    dest_ = effective_rank ^ mask_;
    if(dest_ < size_) {
      break;
    }
    mask_ <<= 1;
  }
  // Fix the destination if needed.
  if(root_) {
    dest_ = (dest_ + root_) % size_;
  }
  return this->active();
}

//
// Test whether this iterator is still active (has a valid neighbor).
//
bool rd_iterator::active() const
{
  return (mask_ >= 0 && size_ > 0 && mask_ < size_ && dest_ < size_);
}

//
// Return the index of the node we are receiving from.
//
int rd_iterator::source() const
{
  if(! this->active()) {
    throw sprockit::iterator_error("rditerator::source():  Iterator is not active.");
  }
  return dest_;
}

/// Return the index of the node we are sending to.
/// \throw iteratorerror if this->sending() returns false.
int rd_iterator::dest() const
{
  if(! this->active()) {
    throw sprockit::iterator_error("rditerator::source():  Iterator is not active.");
  }
  return dest_;
}

}
} // end of namespace sstmac

