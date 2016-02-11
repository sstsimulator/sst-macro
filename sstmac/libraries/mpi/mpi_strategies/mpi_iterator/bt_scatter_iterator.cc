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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/bt_scatter_iterator.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

//
// Hi.
//
bt_scatter_iterator::bt_scatter_iterator(int rank, int size, int root) :
  src_(-1), dst_(-1), mask_(1),
  rank_(rank), size_(size), root_(root)
{
  // Sanity check.
  if(size_ <= 0) {
    spkt_throw(sprockit::value_error, "bt_scatter_iterator: invalid zero or negative size");
  }
  if(rank_ < 0 || rank >= size_) {
    spkt_throw(sprockit::value_error, "bt_scatter_iterator: invalid rank");
  }
  if(root_ < 0 || root >= size_) {
    spkt_throw(sprockit::value_error, "bt_scatter_iterator: invalid root");
  }
  //
  // Compute where we will be receiving data from.
  int relative_rank = (rank_ >= root_ ? rank_-root_ : rank_-root_+size_);
  while(mask_ < size_) {
    if(relative_rank & mask_) {
      src_ = rank_ - mask_;
      if(src_ < 0) {
        src_ += size_;
      }
      break;
    }
    mask_ <<= 1;
  }
  // The root node needs to set up for the first send.
  if(src_ < 0) {
    mask_ >>= 1;
    while(mask_ > 0) {
      if(relative_rank + mask_ < size_) {
        dst_ = rank_ + mask_;
        if(dst_ >= size_) {
          dst_ -= size_;
        }
        break;
      }
      mask_ >>= 1;
    }
  }
}

//
// Goodbye.
//
bt_scatter_iterator::~bt_scatter_iterator() throw()
{
}

//
// Increment the iterator.
//
bool bt_scatter_iterator::next()
{
  if(! this->active()) {
    spkt_throw(sprockit::iterator_error, "btscatteriterator::next(): iterator is not active");
  }
  src_ = -1;
  int relative_rank = (rank_ >= root_ ? rank_-root_ : rank_-root_+size_);
  mask_ >>= 1;
  while(mask_ > 0) {
    if(relative_rank + mask_ < size_) {
      dst_ = rank_ + mask_;
      if(dst_ >= size_) {
        dst_ -= size_;
      }
      break;
    }
    mask_ >>= 1;
  }
  return this->active();
}

//
// Test whether this iterator is still active (has a valid neighbor).
//
bool bt_scatter_iterator::active() const
{
  return (0 < mask_ && mask_ < size_);
}

//
// Are we sending?
//
bool bt_scatter_iterator::sending() const
{
  return (this->active() && dst_ >= 0);
}

//
// Are we receiving?
//
bool bt_scatter_iterator::receiving() const
{
  return (this->active() && src_ >= 0);
}

//
// Return the index of the node we are receiving from.
//
int bt_scatter_iterator::source() const
{
  if(! this->receiving()) {
    spkt_throw(sprockit::iterator_error, "btscatteriterator::source():  not receiving");
  }
  return src_;
}

//
// Return the index of the node we are sending to.
//
int bt_scatter_iterator::dest() const
{
  if(! this->sending()) {
    spkt_throw(sprockit::iterator_error, "btscatteriterator::dest():  not sending");
  }
  return dst_;
}

}
} // end of namespace sstmac

