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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_ITERATOR_RDITERATOR_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_ITERATOR_RDITERATOR_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_iterator/topology_iterator.h>

namespace sstmac {
namespace sw {

/**
 * A topology iterator that uses recursive doubling to decide on the
 * next communication partners.
 *
 * The basic algorithm was obtained from mpich2-1.0.8 (src/mpi/coll/scan.cc):
 * <pre>
 * recvbuf = sendbuf;
 * partial_scan = sendbuf;
 * mask = 0x1;
 * while (mask < size) {
 *    dst = rank^mask;
 *    if (dst < size) {
 *       send partial_scan to dst;
 *       recv from dst into tmp_buf;
 *       if (rank > dst) {
 *          partial_scan = tmp_buf + partial_scan;
 *          recvbuf = tmp_buf + recvbuf;
 *       }
 *       else {
 *          if (op is commutative)
 *             partial_scan = tmp_buf + partial_scan;
 *          else {
 *             tmp_buf = partial_scan + tmp_buf;
 *             partial_scan = tmp_buf;
 *          }
 *       }
 *    }
 *    mask <<= 1;
 * }
 * </pre>
 */
class rd_iterator : public topology_iterator
{


 public:

  /// Construct a new iterator.
  static rd_iterator*
  construct(int rank, int size, int root = 0);

  /// Goodbye.
  virtual ~rd_iterator() throw();

  /// Increment the iterator.
  /// \return this->active() on the newly advanced iterator.
  /// \throw iteratorerror if this->active() is false prior to advancing.
  virtual bool next();

  /// Test whether this iterator is still active (has a valid neighbor).
  virtual bool active() const;

  /// Are we sending?
  virtual bool sending() const {
    return (this->active() && rank_ < dest_);
  }

  /// Are we receiving?
  virtual bool receiving() const {
    return (this->active() && rank_ > dest_);
  }

  /// Return the index of the node we are receiving from.
  /// \throw iteratorerror if this->receiving() returns false.
  virtual int source() const;

  /// Return the index of the node we are sending to.
  /// \throw iteratorerror if this->sending() returns false.
  virtual int dest() const;

 protected:
  /// Hello.
  rd_iterator(int rank, int size, int root = 0);

 protected:
  /// The mask to figure out our next neighbor.
  int mask_;
  /// The rank of this peer in a communication group.
  int rank_;
  /// The total size of the communication group.
  int size_;
  /// Rotate all indices so that this index is at the root.
  int root_;
  /// The destination for send and receive is always the same here.
  int dest_;

};

}
} // end of namespace sstmac.

#endif

