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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIRANKMAP_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIRANKMAP_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

/**
 * Assist in implementing MPI collective algorithms that expect number
 * of active nodes to be an even power-of-two.  Extra nodes are collapsed
 * into their lower-numbered peers, so node 1 goes to node 0 etc.
 *
 * Provides functionality to define a root node.  If the root node
 * is not an active rank, this rank gets remapped to active rank 0.
 */
class mpi_rank_map
{

 public:
  /// Hi there.
  mpi_rank_map(mpi_id root, mpi_comm* comm) :
    log2_(0), extra_(0), realroot_(root), comm_(comm) {
    const int size = comm_->size();
    while((1<<(log2_+1)) <= size) {
      ++log2_;
    }
    extra_ = size - (1 << log2_);
    //std::cerr << "mpirankmap(" << root << ", " << comm << "):  extra_=="
    //          << extra_ << "\n";
  }

  /// Get the log2 size of the system.
  int
  log2() const {
    return log2_;
  }

  /// Get the number of extra nodes.
  int
  extra() const {
    return extra_;
  }

  /// Test whether this node is inactive in the reduce.
  bool
  donor() const {
    return donor(comm_->rank());
  }

  /// Test whether ths node is serving for another node.
  bool
  acceptor() const {
    return acceptor(comm_->rank());
  }

  /// Test whether the given node is inactive in the reduce.
  bool
  donor(mpi_id therank) const {
    return ((therank < mpi_id(2 * extra_)) && (therank.id_ % 2 != 0));
  }

  /// Test whether the given node is serving for another node.
  bool
  acceptor(mpi_id therank) const {
    return ((therank < mpi_id(2 * extra_)) && (therank.id_ % 2 == 0));
  }

  /// What real node rank do we communicate with during a shrink?
  /// \return mpiid(-1) if we are not a donor or acceptor.
  mpi_id
  exchangewith() const {
    if(donor()) {
      return mpi_id(int(comm_->rank()) - 1);
    }
    else if(acceptor()) {
      return mpi_id(int(comm_->rank()) + 1);
    }
    else {
      return mpi_id();
    }
  }

  /// Get the rank of this node.
  mpi_id
  real_rank() const {
    return comm_->rank();
  }

  /// Get the real rank of another node.
  mpi_id
  real_rank(int relative) const {
    if(relative < 0) {
      spkt_throw(sprockit::value_error,
                "mpirankmap::real_rank: negative relative rank.");
    }
    mpi_id retval;
    if(relative < extra_) {
      retval.id_ = relative * 2;
    }
    else {
      retval.id_ = relative + extra_;
    }

    if(retval >= comm_->size()) {
      spkt_throw(sprockit::value_error,
                "mpirankmap::real_rank: relative rank out of range.");
    }

    return retval;
  }

  /// Get the effective (reduced) rank of this node.
  int effective_rank() const {
    return effective_rank(comm_->rank());
  }

  /// Get the effective (reduced) rank of the given node.
  int effective_rank(mpi_id rank) const {
    int retval = -1;
    if(! donor(rank)) {
      if(int(rank)< 2 * extra_) {
        retval = rank.id_ / 2;
      }
      else {
        retval = rank.id_ - extra_;
      }
    }
    return retval;
  }

 protected:
  /// Log2 of the power-of-two size of the system.
  int log2_;
  /// The extra nodes (total size is (1<<log2_) + extra_).
  int extra_;
  /// The root rank.
  mpi_id realroot_;
  /// Our communicator
  mpi_comm* comm_;

};

}
} // end of namespace sstmac

#endif

