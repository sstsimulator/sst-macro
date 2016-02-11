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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/hfm_iterator.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

//
// Hello.
//
hfm_iterator::hfm_iterator(int rank, int size) :
  mask_(0), rank_(rank), size_(size), source_(-1), dest_(-1)
{
  const std::string here("hfmiterator(...)");
  if(rank_ < 0 || rank >= size_) {
    spkt_throw_printf(sprockit::value_error, "invalid rank %d", rank_);
  }
  if(size_ < 0) {
    spkt_throw_printf(sprockit::value_error, "invalid size %d", size_);
  }
  if(this->active()) {
    this->next();
  }
}

//
// Goodbye.
//
hfm_iterator::~hfm_iterator() throw()
{
}

//
// Increment the iterator.
//
bool hfm_iterator::next()
{
  if(! this->active()) {
    spkt_throw(sprockit::iterator_error, "hfmiterator::next(): iterator is not active");
  }
  // Go.
  if(mask_ > 0) {
    mask_ <<= 1;
  }
  else {
    // First entry.
    mask_ = 1;
  }
  dest_ = (rank_ + mask_) % size_;
  source_ = (rank_ - mask_ + size_) % size_;
  return this->active();
}

//
// Test whether this iterator is still active (has valid neighbors).
//
bool hfm_iterator::active() const
{
  return (mask_ ?
          (mask_ > 0 && size_ > 0 && mask_ < size_ &&
           dest_ >= 0 && dest_ < size_ &&
           source_ >= 0 && source_ < size_) :
          true);
}

//
// Return the index of the node we are receiving from.
//
int hfm_iterator::source() const
{
  if(! this->active()) {
    spkt_throw(sprockit::iterator_error, "hfmiterator::source(): iterator is not active");
  }
  return source_;
}

//
// Return the index of the node we are sending to.
//
int hfm_iterator::dest() const
{
  if(! this->active()) {
    spkt_throw(sprockit::iterator_error, "hfmiterator::dest(): iterator is not active");
  }
  return dest_;
}


}
} // end of namespace sstmac

