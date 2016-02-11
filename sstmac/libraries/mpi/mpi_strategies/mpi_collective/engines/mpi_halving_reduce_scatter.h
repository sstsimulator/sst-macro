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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIHALVINGREDUCESCATTER_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIHALVINGREDUCESCATTER_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_op.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>
#include <deque>

namespace sstmac {
namespace sw {

/**
 * Perform an MPI_Reduce_scatter using recursive halving.
 *
 * If the number of nodes is a power of two, the basic communication is
 * <pre>
 * def partner(rank, pivot):
 *     """Given a rank and a pivot point, return communication partner"""
 *     if rank >= (pivot<<1):
 *         raise Exception("Relative rank must not be more than pivot<<1")
 *     if rank < pivot: pal = rank + pivot
 *     else: pal = rank - pivot
 *     return pal
 *
 * def basic_redscat(rank, size):
 *     """Reduce-scatter for power-of-two nodes."""
 *     log2 = math.log(size, 2)
 *     if(log2 != int(log2)): raise Exception("Size must be a power-of-two")
 *     pivot = size
 *     while pivot > 1:
 *         cap = pivot
 *         pivot >>= 1
 *         # we do the caps based on relative rank
 *         relrank = rank % cap
 *         shift = rank - relrank
 *         pal = partner(relrank, pivot) + shift
 *         # Figure out what we're sending
 *         lowframe = [shift, pivot+shift-1]
 *         highframe = [pivot + shift, cap+shift-1]
 *         if highframe[0] >= size: highframe[0] = size-1
 *         if relrank < pivot:
 *             recvframe = lowframe
 *             sendframe = highframe
 *         else:
 *             recvframe = highframe
 *             sendframe = lowframe
 *         print "rank",rank,"sending elements for ranks",sendframe,
 *         print "and receiving ranks",recvframe,"to/from rank",pal
 *     # ! while pivot > 1
 * </pre>
 *
 * When the number of nodes is not a power of two, even- and odd-numbered
 * nodes are collapsed to reduce the number of active nodes.
 */


class mpi_halving_reduce_scatter : public mpi_collective
{

 public:
  std::string
  to_string() const {
    return sprockit::printf("mpihalvingreducescatter(tag=%d)", int(tag_));
  }

  /// Hello.
  mpi_halving_reduce_scatter(mpi_request* thekey,
                             mpi_queue* queue,
                             const std::vector<int> &recvcnts,
                             mpi_type_id type,
                             mpi_op* op,
                             mpi_tag tag,
                             mpi_comm* comm,
                             const payload::const_ptr& content,
                             event_handler* completion);

  /// Goodbye.
  virtual ~mpi_halving_reduce_scatter() throw();

  /// Get busy child.
  virtual void start();

 protected:
  /// Callback method to indicate that a send operation has completed.
  virtual void
  send_complete(const mpi_message::ptr& msg);

  /// Callback method to indicate that a receive operation has completed.
  virtual void
  recv_complete(const mpi_message::ptr& msg);

  /// A private method to perform data exchanges.
  /// Pops the front element of commstep to decide on the next partner.
  /// Omits sends or receives if sendbytes or recvbytes are < 0.
  void sendrecv();

 protected:
  /// Private nested type to monitor where the next bundle of communications
  /// will go.
  struct commstep {
    explicit commstep(mpi_id p, int sb, int rb) :
      peer(p), sendbytes(sb), recvbytes(rb) {
    }
    mpi_id peer;
    int sendbytes;
    int recvbytes;
  };

  /// The list of receive counts for each node.
  std::vector<int> recvcnts_;
  /// The type we are communicating.
  mpi_type_id type_;
  /// Our operation (not used right now).
  mpi_op op_;

  mpi_collective_payload::const_ptr content_;
  /// A list of the nodes we will be communicating with.
  std::deque<commstep> peers_;
  /// The number of pending sends (generally zero or one).
  int pending_sends_;
  /// The number of pending receives (generally zero or one).
  int pending_recvs_;

};

}
} // end of namespace sstmac

#endif

