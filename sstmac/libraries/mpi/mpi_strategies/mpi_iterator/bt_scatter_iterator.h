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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_ITERATOR_BTSCATTERITERATOR_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_ITERATOR_BTSCATTERITERATOR_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/topology_iterator.h>

namespace sstmac {
namespace sw {

/**
 * A binomial tree iterator for scattering data.
 *
 * The basic algorithm is obtained from mpich2-1.0.8 (bcast.c).
 *
 * <pre>
 * if (rank >= root) {
 *     relative_rank = rank - root;
 * }
 * else {
 *     relative_rank = rank - root + size;
 * }
 * mask = 1;
 * while (mask < size) {
 *   if (relative_rank & mask) {
 *     src = rank - mask;
 *     if (src < 0) {
 *       src += size;
 *     }
 *     <rank receves from src>;
 *     break;
 *   }
 *   mask <<= 1;
 * }
 * mask >>= 1;
 * while (mask > 0) {
 *   if (relative_rank + mask < size) {
 *     dst = rank + mask;
 *     if (dst >= size) {
 *       dst -= size;
 *     }
 *     <rank sends to dst>;
 *   }
 *   mask >>= 1;
 * }
 * </pre>
 */
class bt_scatter_iterator : public topology_iterator
{

 public:
  /// Hi.
  bt_scatter_iterator(int rank, int size, int root);

  /// Goodbye.
  virtual ~bt_scatter_iterator() throw();

  /// Increment the iterator.
  /// \return this->active() on the newly advanced iterator.
  /// \throw iteratorerror if this->active() is false prior to advancing.
  virtual bool next();

  /// Test whether this iterator is still active (has a valid neighbor).
  virtual bool active() const;

  /// Are we sending?
  virtual bool sending() const;

  /// Are we receiving?
  virtual bool receiving() const;

  /// Return the index of the node we are receiving from.
  /// \throw iteratorerror if this->receiving() returns false.
  virtual int source() const;

  /// Return the index of the node we are sending to.
  /// \throw iteratorerror if this->sending() returns false.
  virtual int dest() const;

 protected:
  /// The rank from which we will be receiving data
  /// (this iterator always starts with a single receive call).
  int src_;
  /// The rank to which we will be sending data.
  int dst_;
  /// The mask used to compute the next neighbor to send to
  int mask_;
  /// The rank of this peer in the communication group.
  int rank_;
  /// The total size of the communication group.
  int size_;
  /// The root rank for the scatter.
  int root_;

};

}
} // end of namespace sstmac

#endif

