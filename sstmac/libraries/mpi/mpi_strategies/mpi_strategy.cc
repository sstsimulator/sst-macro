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

#include <sstmac/libraries/mpi/mpi_strategies/mpi_strategy.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_strategies.h>
#include <sprockit/errors.h>


namespace sstmac {
namespace sw {

// Private test for whether a shared* is null.
template<typename T>
void nonnull(T* t, const char *caller)
{
  if (t == 0) {
    spkt_throw_printf(sprockit::null_error, "%s: null strategy object", caller);
  }
}

//
// Default construction.  Builds default implementations for
// all the individual strategy types.
//
mpi_strategy::mpi_strategy() :
  bcast_(new mpi_core_bcast),
  reduce_(new mpi_core_reduce),
  allreduce_(0), //must be configured by input file
  gather_(new mpi_core_gather),
  gatherv_(new mpi_core_gatherv),
  allgather_(0), //must be configured by input file
  allgatherv_(new mpi_core_allgatherv),
  alltoall_(new mpi_core_alltoall),
  alltoallv_(new mpi_core_alltoallv),
  scatter_(new mpi_core_scatter),
  scatterv_(new mpi_core_scatterv),
  reduce_scatter_(new mpi_core_reduce_scatter),
  barrier_(new mpi_core_barrier(mpi_core_barrier::HFM)),
  scan_(new mpi_core_scan)
{
  // Make the sends and receives shared by default.
  send_ = new mpi_core_send;
  bsend_ = new mpi_core_send;
  rsend_ = new mpi_core_rsend;
  ssend_ = new mpi_core_ssend;
  recv_ = new mpi_core_recv;
}

mpi_strategy::~mpi_strategy()
{
  delete bcast_;
  delete reduce_;
  delete allreduce_;
  delete gather_;
  delete gatherv_;
  delete allgather_;
  delete allgatherv_;
  delete alltoall_;
  delete alltoallv_;
  delete scatter_;
  delete scatterv_;
  delete reduce_scatter_;
  delete barrier_;
  delete scan_;
  delete send_;
  delete bsend_;
  delete rsend_;
  delete ssend_;
  delete recv_;
}

// Assign a non-null strategy to send.
void mpi_strategy::set_send(mpi_send_strategy* st)
{
  nonnull(st, "mpistrategy::set_send");
  delete send_;
  send_ = st;
}

/// Assign a non-null strategy to bsend.
void mpi_strategy::set_bsend(mpi_send_strategy* st)
{
  nonnull(st, "mpistrategy::set_bsend");
  delete bsend_;
  bsend_ = st;
}

/// Assign a non-null strategy to ssend.
void mpi_strategy::set_ssend(mpi_send_strategy* st)
{
  nonnull(st, "mpistrategy::set_ssend");
  delete ssend_;
  ssend_ = st;
}

/// Assign a non-null strategy to rsend.
void mpi_strategy::set_rsend(mpi_send_strategy* st)
{
  nonnull(st, "mpistrategy::set_rsend");
  delete rsend_;
  rsend_ = st;
}

/// Assign a non-null strategy to recv.
void mpi_strategy::set_recv(mpi_recv_strategy* st)
{
  nonnull(st, "mpistrategy::set_recv");
  delete recv_;
  recv_ = st;
}

// Assign a non-null strategy to bcast.
void mpi_strategy::set_bcast(mpi_bcast_strategy* st)
{
  nonnull(st, "mpistrategy::set_bcast");
  delete bcast_;
  bcast_ = st;
}

// Assign a non-null strategy to reduce.
void mpi_strategy::set_reduce(mpi_reduce_strategy* st)
{
  nonnull(st, "mpistrategy::set_reduce");
  delete reduce_;
  reduce_ = st;
}

// Assign a non-null strategy to allreduce.
void mpi_strategy::set_allreduce(mpi_allreduce_strategy* st)
{
  nonnull(st, "mpistrategy::set_allreduce");
  if (allreduce_) delete allreduce_;
  allreduce_ = st;
}

// Assign a non-null strategy to gather.
void mpi_strategy::set_gather(mpi_gather_strategy* st)
{
  nonnull(st, "mpistrategy::set_gather");
  delete gather_;
  gather_ = st;
}

// Assign a non-null strategy to gatherv.
void mpi_strategy::set_gatherv(mpi_gatherv_strategy* st)
{
  nonnull(st, "mpistrategy::set_gatherv");
  delete gatherv_;
  gatherv_ = st;
}

// Assign a non-null strategy to allgather.
void mpi_strategy::set_allgather(mpi_allgather_strategy* st)
{
  nonnull(st, "mpistrategy::set_allgather");
  if (allgather_) delete allgather_;
  allgather_ = st;
}

// Assign a non-null strategy to allgatherv.
void mpi_strategy::set_allgatherv(mpi_allgatherv_strategy* st)
{
  nonnull(st, "mpistrategy::set_allgatherv");
  delete allgatherv_;
  allgatherv_ = st;
}

// Assign a non-null strategy to alltoall.
void mpi_strategy::set_alltoall(mpi_alltoall_strategy* st)
{
  nonnull(st, "mpistrategy::set_alltoall");
  delete alltoall_;
  alltoall_ = st;
}

// Assign a non-null strategy to alltoallv.
void mpi_strategy::set_alltoallv(mpi_alltoallv_strategy* st)
{
  nonnull(st, "mpistrategy::set_alltoallv");
  delete alltoallv_;
  alltoallv_ = st;
}

// Assign a non-null strategy to scatter.
void mpi_strategy::set_scatter(mpi_scatter_strategy* st)
{
  nonnull(st, "mpistrategy::set_scatter");
  scatter_ = st;
}

// Assign a non-null strategy to scatterv.
void mpi_strategy::set_scatterv(mpi_scatterv_strategy* st)
{
  nonnull(st, "mpistrategy::set_scatterv");
  delete scatterv_;
  scatterv_ = st;
}

// Assign a non-null strategy to reduce_scatter.
void mpi_strategy::set_reduce_scatter(mpi_reduce_scatter_strategy*
                                     st)
{
  nonnull(st, "mpistrategy::set_reduce_scatter");
  delete reduce_scatter_;
  reduce_scatter_ = st;
}

// Assign a non-null strategy to synchronization barriers.
void mpi_strategy::set_barrier(mpi_barrier_strategy* st)
{
  nonnull(st, "mpistrategy::set_barrier");
  delete barrier_;
  barrier_ = st;
}

// Assign a non-null strategy to scan operations.
void mpi_strategy::set_scan(mpi_scan_strategy* st)
{
  nonnull(st, "mpistrategy::set_scan");
  delete scan_;
  scan_ = st;
}

}
} // end of namespace sstmac

