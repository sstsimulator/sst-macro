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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIBTREEGATHER_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_STRATEGIES_MPI_COLLECTIVE_ENGINES_MPIBTREEGATHER_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sstmac/libraries/mpi/mpi_status.h>

namespace sstmac {
namespace sw {

/**
 * Perform an MPI gather using a btree.
 *
 * Unfortunately, it is not reasonable to use the same engine for
 * gather and gatherv, since the MPI standard has some very annoying
 * stipulations regarding which parameters are valid on client nodes
 * for scatterv (see more detailed comments in mpibtreescatterv).
 *
 * The algorithm for the scatter is demonstrated in the following python code:
 * <pre>
 * def btreegather(absolute_rank, size, root):
 *     def relrank(arank):
 *         return (arank - root + size) % size
 *     def arank(relrank):
 *         return (relrank + root) % size
 *     # Go.
 *     relative_rank = relrank(absolute_rank)
 *     if relative_rank == 0:
 *         keybit = 1 << int(math.log(size-1, 2)) + 1
 *     else:
 *         keybit = 1
 *         while not relative_rank & keybit:
 *             keybit <<= 1
 *     # Receive if needed
 *     localsrc = 1
 *     endrecv = -1
 *     while localsrc < keybit:
 *         source = relative_rank + localsrc
 *         endrecv = source + localsrc - 1
 *         if endrecv >= size:
 *             endrecv = size-1
 *         startrecv = source
 *         print absolute_rank,"receiving data for rank",arank(startrecv),
 *         print "through",arank(endrecv),"from rank",arank(source)
 *         localsrc <<= 1
 *     # (Re-)send data
 *     if relative_rank != 0:
 *         localdest = relative_rank - keybit
 *         startsend = relative_rank
 *         endsend = endrecv
 *         print absolute_rank,"sending data for rank",arank(startsend),
 *         print "through",arank(endsend),"to rank",arank(localdest)
 * </pre>
 */
class mpi_btree_gather : public mpi_collective
{

 public:
  std::string
  to_string() const {
    return sprockit::printf("mpibtree(tag=%d)", int(tag_));
  }

  /// Hi.
  mpi_btree_gather(mpi_request* thekey,
                   mpi_queue* queue,
                   int sendcount, mpi_type_id sendtype,
                   int recvcount, mpi_type_id recvtype,
                   mpi_id root, mpi_tag tag,
                   mpi_comm* comm,
                   const payload::const_ptr& content,
                   event_handler* completion);

  /// Goodbye.
  virtual ~mpi_btree_gather() throw();

  /// Start this kernel.
  virtual void start();

 protected:
  /// Callback method to indicate that a send operation has completed.
  virtual void
  send_complete(mpi_message* msg);

  /// Callback method to indicate that a receive operation has completed.
  virtual void
  recv_complete(mpi_message* msg);

  /// Given an absolute rank, return the relative rank.
  inline int relrank(mpi_id therank) const;

  /// Given a relative rank, return the absolute rank.
  inline mpi_id absrank(int therank) const;

  /// Fire off any and all receives concurrently.
  void init_recvs();

  /// Fire off the send operation (if any).  Can be called from root.
  void init_send();

 protected:
  /**
    * Information about a partner for an mpi recv operation.
    * Trying to get stronger typing than is afforded by std::pair.
    */
  struct mpisource {
    mpisource(mpi_id dsource = mpi_id(-1), int ccount = -1) :
      source(dsource), count(ccount) {
    }

    mpi_id source;
    int   count;
  };

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
  /// The tag associated with our communications.
  mpi_tag tag_;

  mpi_collective_payload::ptr content_;

  /// The number of pending sends.
  int pending_sends_;
  /// The number of pending receives.
  int pending_recvs_;



};

}
} // end of namespace sstmac

#endif

