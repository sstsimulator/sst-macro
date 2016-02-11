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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPISTRATEGIES_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPISTRATEGIES_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_id.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_op.h>

#include <sstmac/libraries/mpi/mpi_message.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_fwd.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_fwd.h>
#include <sstmac/libraries/mpi/mpi_request_fwd.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_fwd.h>

#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/messages/payload.h>
#include <stdint.h>

namespace sstmac {
namespace sw {

// Forward declaration of important types:
class mpi_strategy_base  {
 public:
  virtual std::string
  to_string() const {
    return "mpistrategy";
  }

  virtual ~mpi_strategy_base(){}
};

//
/// Base strategy for sending messages.
//
class mpi_send_strategy : public mpi_strategy_base
{
 public:

  /// Virtual destructor is critical here.
  virtual ~mpi_send_strategy() throw() {}

  /// Post a send request to the given node and associate the
  /// completion of this request with the given request handle.
  /// All sends are non-blocking; if return receipts are required the
  /// completion of the request must be associated with the arrival
  /// of the return receipt.
  ///
  /// Like all strategy methods, this method must be reentrant.
  virtual void
  execute(
    mpi_request* thekey,
    mpi_queue* queue,
    int count, mpi_type_id type, mpi_id dest,
    mpi_tag tag, mpi_comm* comm,
    const payload::const_ptr& content,
    operating_system* os) const = 0;
};

//
/// Base strategy for receiving messages.
//
class mpi_recv_strategy  : public mpi_strategy_base
{
 public:
  /// Must be virtual
  virtual ~mpi_recv_strategy() throw() {}

  /// Post a receive request from the given node to be matched by the
  /// given request handle.  All receives are non-blocking; it is up
  /// to the strategy to handle return receipts or other synchronization.
  virtual void
  execute(
    mpi_request* thekey,
    mpi_queue* queue,
    int count, mpi_type_id type, mpi_id source,
    mpi_tag tag, mpi_comm* comm,
    operating_system* os) const = 0;
};

//
/// Base strategy for broadcast operations.
//
class mpi_bcast_strategy  : public mpi_strategy_base
{
 public:
  /// Must be virtual
  virtual ~mpi_bcast_strategy() throw() {}

  /// Broadcast from root to all the mpiids in the given list.
  virtual mpi_collective*
  execute(
    mpi_request* thekey,
    mpi_queue* queue,
    int count, mpi_type_id type, mpi_id root,
    mpi_tag tag, mpi_comm* comm,
    const payload::const_ptr& content,
    operating_system* os) const = 0;
};

//
/// Base strategy for reduce operations.
//
class mpi_reduce_strategy : public mpi_strategy_base
{
 public:
  /// Must be virtual.
  virtual ~mpi_reduce_strategy() throw() {}

  /// Reduce between root mpiid and all other participants.
  /// Associates the completion of the reduction operation with the
  /// given request handle.
  virtual mpi_collective*
  execute(
    mpi_request* thekey,
    mpi_queue* queue,
    int count, mpi_type_id type, mpi_op* op,
    mpi_id root, mpi_tag tag, mpi_comm* comm,
    const payload::const_ptr& content,
    operating_system* os) const = 0;
};

//
/// Base strategy for allreduce operations.
//
class mpi_allreduce_strategy :
    public mpi_strategy_base,
    public sprockit::factory_type
{
 public:
  /// Must be virtual.
  virtual ~mpi_allreduce_strategy() throw() {}

  /// Reduce between all nodes.
  /// Associates the completion of the reduction operation with the
  /// given request handle.
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue,
          int count, mpi_type_id type, mpi_op* op,
          mpi_tag tag, mpi_comm* comm,
          const payload::const_ptr& content,
          operating_system* os) const = 0;
};
DeclareFactory(mpi_allreduce_strategy);

//
// Base strategy for gather operations.
//
class mpi_gather_strategy  : public mpi_strategy_base
{
 public:
  /// Must be virtual.
  virtual ~mpi_gather_strategy() throw() {}

  /// Perform a gather operation
  virtual mpi_collective*
  execute(
    mpi_request* thekey,
    mpi_queue* queue,
    int sendcount, mpi_type_id sendtype,
    int recvcount, mpi_type_id recvtype,
    mpi_id root, mpi_tag tag, mpi_comm* comm,
    const payload::const_ptr& content,
    operating_system* os) const = 0;
};

//
/// Base strategy for gatherv operations.
//
class mpi_gatherv_strategy : public mpi_strategy_base
{
 public:
  /// Must be virtual.
  virtual ~mpi_gatherv_strategy() throw() {}

  /// Gather data to root node from all other participants.
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue,
          int sendcount, mpi_type_id sendtype,
          const std::vector<int> &recvcounts, mpi_type_id recvtype,
          mpi_id root, mpi_tag tag, mpi_comm* comm,
          const payload::const_ptr& content, operating_system* os) const = 0;
};

//
// Base strategy for allgather operations.
//
class mpi_allgather_strategy :
    public mpi_strategy_base,
    public sprockit::factory_type
{
 public:
  /// Must be virtual.
  virtual ~mpi_allgather_strategy() throw() {}

  /// Gather data to all nodes from all participants.
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue,
          int sendcount, mpi_type_id sendtype,
          int recvcount, mpi_type_id recvtype,
          mpi_tag tag, mpi_comm* comm,
          const payload::const_ptr& content,
          operating_system* os) const = 0;
};
DeclareFactory(mpi_allgather_strategy);

//
// Base strategy for allgatherv operations.
//
class mpi_allgatherv_strategy : public mpi_strategy_base
{
 public:
  /// Must be virtual.
  virtual ~mpi_allgatherv_strategy() throw() {}

  /// Gather data to all nodes from all participants.
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue,
          int sendcount, mpi_type_id sendtype,
          const std::vector<int> &recvcount, mpi_type_id recvtype,
          mpi_tag tag, mpi_comm* comm,
          const payload::const_ptr& content,
          operating_system* os) const = 0;
};

//
// Base strategy for all-to-all communication with fixed data lengths.
//
class mpi_alltoall_strategy : public mpi_strategy_base
{
 public:
  /// Must be virtual.
  virtual ~mpi_alltoall_strategy() throw() {}

  /// Gather data from all nodes to all nodes.
  virtual mpi_collective*
  execute(
    mpi_request* thekey,
    mpi_queue* queue,
    int sendcnt, mpi_type_id sendtype,
    int recvcnt, mpi_type_id recvtype,
    mpi_tag tag, mpi_comm* comm,
    const std::vector<payload::const_ptr >& content,
    operating_system* os) const = 0;
};

//
// Base strategy for all-to-all communication with variable data lengths.
//
class mpi_alltoallv_strategy  : public mpi_strategy_base
{
 public:
  /// Must be virtual.
  virtual ~mpi_alltoallv_strategy() throw() {}

  /// Gather data from all nodes to all nodes.
  virtual mpi_collective*
  execute(
    mpi_request* thekey,
    mpi_queue* queue,
    const std::vector<int> &sendcnts, mpi_type_id sendtype,
    const std::vector<int> &recvcnts, mpi_type_id recvtype,
    mpi_tag tag, mpi_comm* comm,
    const std::vector<payload::const_ptr >& content,
    operating_system* os) const = 0;
};

//
// Base strategy for scatter operations.
//
class mpi_scatter_strategy  : public mpi_strategy_base
{
 public:
  /// Must be virtual.
  virtual ~mpi_scatter_strategy() throw() {}

  /// Perform a scatter operation
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue,
          int sendcount, mpi_type_id sendtype,
          int recvcount, mpi_type_id recvtype,
          mpi_id root, mpi_tag tag,
          mpi_comm* comm,
          const std::vector<payload::const_ptr > &content,
          operating_system* os) const = 0;
};

//
// Base strategy for scatterv operations.
//
class mpi_scatterv_strategy : public mpi_strategy_base
{
 public:
  /// Must be virtual.
  virtual ~mpi_scatterv_strategy() throw() {}

  /// Scatter data from root node to all other participants.
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue,
          const std::vector<int> &sendcounts, mpi_type_id sendtype,
          int recvcount, mpi_type_id recvtype,
          mpi_id root, mpi_tag tag, mpi_comm* comm,
          const std::vector<payload::const_ptr>& cc,
          operating_system* os) const = 0;
};

//
// Base strategy for fused reduce-scatter operations.
//
class mpi_reduce_scatter_strategy : public mpi_strategy_base
{
 public:
  /// Must be virtual.
  virtual ~mpi_reduce_scatter_strategy() throw() {}

  /// Perform a fused reduce-scatter operation
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue,
          const std::vector<int> &recvcnts, mpi_type_id type,
          mpi_op* op, mpi_tag tag, mpi_comm* comm,
          const payload::const_ptr& content, operating_system* os) const = 0;
};

class mpi_barrier_strategy  : public mpi_strategy_base
{
 public:
  /// Must be virtual.
  virtual ~mpi_barrier_strategy() throw() {}

  /// Synchronize nodes.
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue,
          mpi_tag tag, mpi_comm* comm,
          operating_system* os) const = 0;
};

//
// Base strategy for scan operations.
//
class mpi_scan_strategy  : public mpi_strategy_base
{
 public:
  /// Must be virtual.
  virtual ~mpi_scan_strategy() throw() {}

  /// execute a kernel to perform a scan operation
  virtual mpi_collective*
  execute(mpi_request* thekey,
          mpi_queue* queue,
          int count, mpi_type_id type, mpi_op* op,
          mpi_tag tag, mpi_comm* comm,
          const payload::const_ptr& content,
          operating_system* os) const = 0;
};

}
} // end of namespace sstmac.

#endif

