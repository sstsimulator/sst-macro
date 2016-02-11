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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_ITERATOR_TOPOLOGYITERATOR_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_ITERATOR_TOPOLOGYITERATOR_H_INCLUDED

#include <string>

namespace sstmac {
namespace sw {

/**
 * Abstract mechanism for stepping through collective operations,
 * informing the user what send and receive operations are required
 * at each stage in the operation.
 *
 * An iterator is only required to advance one direction (.next()).
 * The same type may provide both forward and backward capability,
 * but as far as the base type is concerned they only run one way.
 */
class topology_iterator  {
 public:

  /// Goodbye.
  virtual ~topology_iterator() throw() {
  }

  virtual std::string
  to_string() const {
    return "topology iterator";
  }

  /// Increment the iterator.
  /// \return this->active() on the newly advanced iterator.
  /// \throw iteratorerror if this->active() is false prior to advancing.
  virtual bool next() = 0;

  /// Test whether this iterator is still active (has a valid neighbor).
  virtual bool active() const = 0;

  /// \return true if we are sending to a peer at this iteration.
  /// Returns false if this->active() is false.
  virtual bool sending() const = 0;

  /// \return true if we are receiving from a peer at this iteration.
  /// Returns false if this->active() is false.
  virtual bool receiving() const = 0;

  /// Return the index of the node we are receiving from.
  /// \throw iteratorerror if this->receiving() returns false.
  virtual int source() const = 0;

  /// Return the index of the node we are sending to.
  /// \throw iteratorerror if this->sending() returns false.
  virtual int dest() const = 0;
};

}
} // end of namespace sstmac.

#endif

