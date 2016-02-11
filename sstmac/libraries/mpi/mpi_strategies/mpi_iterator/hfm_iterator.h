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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_ITERATOR_HFMITERATOR_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_ITERATOR_HFMITERATOR_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/topology_iterator.h>

namespace sstmac {
namespace sw {

/**
 * An iterator to do data exchanges between nodes (for barrier etc.).
 *
 * Algorithm obtained from mpich2-1.0.8 (barrier.c):
 *
 * Algorithm: MPI_Barrier
 *
 * We use the dissemination algorithm described in:
 * Debra Hensgen, Raphael Finkel, and Udi Manbet, "Two Algorithms for
 * Barrier Synchronization," International Journal of Parallel
 * Programming, 17(1):1-17, 1988.
 *
 * It uses ceiling(lgp) steps. In step k, 0 <= k <= (ceiling(lgp)-1),
 * process i sends to process (i + 2^k) % p and receives from process
 * (i - 2^k + p) % p.
 */
class hfm_iterator : public topology_iterator
{
 public:
  /// Hello.
  hfm_iterator(int rank, int size);

  /// Goodbye.
  virtual ~hfm_iterator() throw();

  /// Increment the iterator.
  /// \return this->active() on the newly advanced iterator.
  /// \throw iteratorerror if this->active() is false prior to advancing.
  virtual bool next();

  /// Test whether this iterator is still active (has valid neighbors).
  virtual bool active() const;

  /// Are we sending?
  virtual bool sending() const {
    return this->active();
  }

  /// Are we receiving?
  virtual bool receiving() const {
    return this->active();
  }

  /// Return the index of the node we are receiving from.
  /// \throw iteratorerror if this->receiving() returns false.
  virtual int source() const;

  /// Return the index of the node we are sending to.
  /// \throw iteratorerror if this->sending() returns false.
  virtual int dest() const;

 protected:
  /// The mask to figure out our next send/receive pair.
  int mask_;
  /// The rank of this peer in the communication group.
  int rank_;
  /// The total size of the communication group.
  int size_;
  /// The destination for next send.
  int source_;
  /// The source for next receive.
  int dest_;

};

}
} // end of namespace sstmac

#endif

