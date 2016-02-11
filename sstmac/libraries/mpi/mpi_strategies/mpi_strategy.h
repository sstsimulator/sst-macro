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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPISTRATEGY_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPISTRATEGY_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_strategies.h>

namespace sstmac {
namespace sw {

/**
 * A full ensemble of strategies for an MPI communication environment.
 * All the individual strategies have (hopefully reasonable) defaults.
 *
 * All operations are preseumed to be non-blocking.  The choice of whether
 * (or when) to block is left to the caller.
 */
class mpi_strategy  {
 private:
  /// The strategy objects for non-blocking sends.
  mpi_send_strategy* send_, *bsend_, *ssend_, *rsend_;
  /// The strategy object for blocking receives.
  mpi_recv_strategy* recv_;
  /// The strategy object for broadcasts.
  mpi_bcast_strategy* bcast_;
  /// The strategy object for reduce operations.
  mpi_reduce_strategy* reduce_;
  /// The strategy object for allreduce.  Should this be fused with reduce?
  mpi_allreduce_strategy* allreduce_;
  /// The strategy object for gather operations.
  mpi_gather_strategy* gather_;
  /// The strategy object for gatherv operations.
  mpi_gatherv_strategy* gatherv_;
  /// The strategy object for allgather operations.
  mpi_allgather_strategy* allgather_;
  /// The strategy object for allgatherv operations.
  mpi_allgatherv_strategy* allgatherv_;
  /// The strategy object for alltoall operations.
  mpi_alltoall_strategy* alltoall_;
  /// The strategy object for alltoallv operations.
  mpi_alltoallv_strategy* alltoallv_;
  /// The strategy object for scatter operations.
  mpi_scatter_strategy* scatter_;
  /// The strategy object for scatterv operations.
  mpi_scatterv_strategy* scatterv_;
  /// The strategy object for fused reduce_scatter operations.
  mpi_reduce_scatter_strategy* reduce_scatter_;
  // The strategy object for synchronization barriers.
  mpi_barrier_strategy* barrier_;
  /// The strategy object for scan operations.
  mpi_scan_strategy* scan_;

 public:
  /// Default construction.  Builds default implementations for
  /// all the individual strategy types.
  mpi_strategy();

  virtual std::string
  to_string() const {
    return "mpi strategy";
  }

  virtual ~mpi_strategy();

  /// The strategy objects for sends.
  /// The set_* methods throw mpistrategy_error if the strategy is NULL.
  mpi_send_strategy* send() {
    return send_;
  }

  const mpi_send_strategy*
  send() const {
    return send_;
  }

  void set_send(mpi_send_strategy* st);

  mpi_send_strategy* bsend() {
    return bsend_;
  }

  const mpi_send_strategy*
  bsend() const {
    return bsend_;
  }

  void set_bsend(mpi_send_strategy*);

  mpi_send_strategy*
  ssend() {
    return ssend_;
  }

  const mpi_send_strategy*
  ssend() const {
    return ssend_;
  }

  void set_ssend(mpi_send_strategy* st);

  mpi_send_strategy*
  rsend() {
    return rsend_;
  }

  const mpi_send_strategy*
  rsend() const {
    return rsend_;
  }

  void set_rsend(mpi_send_strategy* st);

  /// The strategy object for receives.
  mpi_recv_strategy*
  recv() {
    return recv_;
  }

  const mpi_recv_strategy*
  recv() const {
    return recv_;
  }

  void set_recv(mpi_recv_strategy* st);

  /// The strategy object for broadcasts.
  mpi_bcast_strategy*
  bcast() {
    return bcast_;
  }

  const mpi_bcast_strategy*
  bcast() const {
    return bcast_;
  }

  void set_bcast(mpi_bcast_strategy* st);

  /// The strategy object for reduce operations.
  mpi_reduce_strategy*
  reduce() {
    return reduce_;
  }

  const mpi_reduce_strategy*
  reduce() const {
    return reduce_;
  }

  void set_reduce(mpi_reduce_strategy* st);

  /// The strategy object for allreduce.
  mpi_allreduce_strategy*
  allreduce() {
    return allreduce_;
  }

  const mpi_allreduce_strategy*
  allreduce() const {
    return allreduce_;
  }

  void set_allreduce(mpi_allreduce_strategy* st);

  /// The strategy object for gather operations.
  mpi_gather_strategy*
  gather() {
    return gather_;
  }

  const mpi_gather_strategy*
  gather() const {
    return gather_;
  }

  void set_gather(mpi_gather_strategy* st);

  /// The strategy object for gatherv operations.
  mpi_gatherv_strategy*
  gatherv() {
    return gatherv_;
  }

  const mpi_gatherv_strategy*
  gatherv() const {
    return gatherv_;
  }

  void set_gatherv(mpi_gatherv_strategy* st);

  /// The strategy object for allgather operations.
  mpi_allgather_strategy*
  allgather() {
    return allgather_;
  }

  const mpi_allgather_strategy*
  allgather() const {
    return allgather_;
  }

  void set_allgather(mpi_allgather_strategy* st);

  /// The strategy object for allgatherv operations.
  mpi_allgatherv_strategy*
  allgatherv() {
    return allgatherv_;
  }

  const mpi_allgatherv_strategy*
  allgatherv() const {
    return allgatherv_;
  }

  void set_allgatherv(mpi_allgatherv_strategy* st);

  /// The strategy object for alltoall operations.
  mpi_alltoall_strategy*
  alltoall() {
    return alltoall_;
  }

  const mpi_alltoall_strategy*
  alltoall() const {
    return alltoall_;
  }

  void set_alltoall(mpi_alltoall_strategy* st);

  /// The strategy object for alltoallv operations.
  mpi_alltoallv_strategy*
  alltoallv() {
    return alltoallv_;
  }

  const mpi_alltoallv_strategy*
  alltoallv() const {
    return alltoallv_;
  }

  void set_alltoallv(mpi_alltoallv_strategy* st);

  /// The strategy object for scatter operations.
  mpi_scatter_strategy*
  scatter() {
    return scatter_;
  }

  const mpi_scatter_strategy*
  scatter() const {
    return scatter_;
  }

  void set_scatter(mpi_scatter_strategy* st);

  /// The strategy object for scatterv operations.
  mpi_scatterv_strategy* scatterv() {
    return scatterv_;
  }

  const mpi_scatterv_strategy*
  scatterv() const {
    return scatterv_;
  }

  void set_scatterv(mpi_scatterv_strategy* st);

  /// The strategy object for fused reduce_scatter operations.
  mpi_reduce_scatter_strategy*
  reduce_scatter() {
    return reduce_scatter_;
  }

  const mpi_reduce_scatter_strategy*
  reduce_scatter() const {
    return reduce_scatter_;
  }

  void set_reduce_scatter(mpi_reduce_scatter_strategy* st);

  /// The strategy object for synchronization barriers.
  mpi_barrier_strategy*
  barrier() {
    return barrier_;
  }

  const mpi_barrier_strategy*
  barrier() const {
    return barrier_;
  }

  void set_barrier(mpi_barrier_strategy* st);

  /// Thestrategy object for scan operations.
  mpi_scan_strategy*
  scan() {
    return scan_;
  }

  const mpi_scan_strategy*
  scan() const {
    return scan_;
  }

  void set_scan(mpi_scan_strategy* st);
};
}
} // end of namespace sstmac.

#endif

