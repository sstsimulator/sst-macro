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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIBTREESCATTER_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIBTREESCATTER_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sstmac/libraries/mpi/mpi_status.h>

namespace sstmac {
namespace sw {

/**
 * Perform an MPI scatter using a btree.
 *
 * Unfortunately, it is not reasonable to use the same engine for
 * scatter and scatterv, since the MPI standard has some very annoying
 * stipulations regarding which parameters are valid on client nodes
 * for scatterv (see more detailed comments in mpidirectscatterv).
 *
 * The algorithm for the scatter is demonstrated in the following python code:
 * <pre>
 * def btreescatter(absolute_rank, size, root):
 *     def relrank(therank):
 *         "Given an absolute rank, find the relative rank."
 *         return (therank - root + size) % size
 *     def absrank(therank):
 *         "Given a relative rank, find the absolute rank."
 *         return (therank + root) % size
 *     # Go.
 *     relative_rank = relrank(absolute_rank)
 *     if relative_rank == 0:
 *         keymask = 1 << int(math.log(size-1, 2)) + 1
 *     else:
 *         keymask = 1
 *         while not relative_rank & keymask:
 *             keymask <<= 1
 *     # Receive if needed
 *     endrecv = size
 *     if relative_rank != 0:
 *         # Receiver
 *         source = relative_rank - keymask
 *         startrecv = relative_rank
 *         endrecv = relative_rank + keymask
 *         if endrecv > size: endrecv = size
 *         print absolute_rank,"receiving data for ranks", absrank(startrecv),
 *         print "through",absrank(endrecv-1), "from rank",absrank(source)
 *     # (Re-)distribute the data:
 *     localmask = keymask >> 1
 *     endsend = endrecv
 *     while localmask > 0:
 *         dest = relative_rank + localmask
 *         if dest >= size:
 *             localmask >>= 1
 *             continue
 *         print absolute_rank, "sending data for ranks", absrank(dest),
 *         print "through", absrank(endsend-1), "to rank", absrank(dest)
 *         endsend = dest
 *         localmask >>= 1
 * </pre>
 */
class mpi_btree_scatter : public mpi_collective
{

 public:
  virtual std::string
  to_string() const {
    return sprockit::printf("mpibtreescatter(tag=%d)", int(tag_));
  }

  /// Hi.
  mpi_btree_scatter(mpi_request* thekey,
                    mpi_queue* queue,
                    int sendcount, mpi_type_id sendtype,
                    int recvcount, mpi_type_id recvtype,
                    mpi_id root, mpi_tag tag, mpi_comm* comm,
                    const std::vector<payload::const_ptr > &cc,
                    event_handler* completion);
  /// Goodbye.
  virtual
  ~mpi_btree_scatter() throw ();

  /// Start this kernel.
  virtual void
  start();

 protected:
  /// Callback method to indicate that a send operation has completed.
  virtual void
  send_complete(mpi_message* msg);

  /// Callback method to indicate that a receive operation has completed.
  virtual void
  recv_complete(mpi_message* msg);

  /// Given an absolute rank, return the relative rank.
  inline int
  relrank(mpi_id therank) const;

  /// Given a relative rank, return the absolute rank.
  inline mpi_id
  absrank(int therank) const;

  /// Fire off a receive.  Can be called by all nodes (including root).
  void
  init_recv();

  /// Fire off all the sends concurrently.  This is called by copy_done.
  void
  init_sends();

 protected:
  /// The number of elements of type sendtype sent to each node.
  int sendcount_;
  /// The send type
  mpi_type_id sendtype_;
  /// The number of elements of type recvtype received by each node.
  int recvcount_;
  /// The receive type
  mpi_type_id recvtype_;
  /// The root node.
  mpi_id root_;
  /// Our accumulated data (we use this to 'cheat' and get data out faster).
  mpi_collective_payload::const_ptr content_;

  /// The number of pending sends.
  int pending_sends_;
  /// The number of pending receives.
  int pending_recvs_;



};

}
} // end of namespace sstmac

#endif

