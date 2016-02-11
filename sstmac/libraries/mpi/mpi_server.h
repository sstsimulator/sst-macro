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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPISERVER_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPISERVER_H_INCLUDED

#include <sstmac/software/process/software_id.h>
#include <sstmac/software/libraries/service.h>
#include <sstmac/libraries/mpi/mpi_message.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_recv_request.h>

#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_scheduler.h>

#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_fwd.h>

namespace sstmac {
namespace sw {

/**
 * A server that acts as a single entry/exit point for
 * mpi messages for a node.
 * this can have multiple tasks (ranks) attached to it.
 */
class mpi_server :
  public service,
  public event_subscheduler
{
  friend class mpi_queue;

 public:
  virtual std::string
  to_string() const {
    return lib_name();
  }

  mpi_server(int appnum);

  /// Goodbye.
  virtual ~mpi_server() throw ();

  virtual void start();

  virtual void init_os(operating_system *os);

  void buffered_send(const mpi_message::ptr& msg);

  void buffered_recv(const mpi_message::ptr& msg,
                     mpi_queue_recv_request* req);

  /// Define a task for this server.  Each task can only be defined once
  virtual void
  add_task(const software_id &tid,
           mpi_queue* peer,
           int rank);

  /// Test whether the given task is defined on this node.
  virtual bool
  has_task(const software_id &tid) const;

  void
  unregister(const software_id& sid, mpi_queue* queue);

  void
  send(const mpi_message::ptr& payload);

  virtual void
  incoming_message(const sst_message::ptr& msg);

 protected:
  void print_queues();

  virtual void
  handle(const sst_message::ptr &msg);

  virtual void
  handle_recv(const mpi_message::ptr& msg);

  virtual void
  handle_send(const mpi_message::ptr& msg);

  void
  start_send(const sst_message::ptr& msg);

  virtual void
  handle_internode_send(const mpi_message::ptr& msg);

  virtual void
  handle_intranode_send(const mpi_message::ptr& msg);

 protected:
  std::set<software_id> deleted_;

  int appnum_;

  typedef std::map<software_id, mpi_queue*> queue_t;
  queue_t queue_;

 private:
  void send_to_queue_payload(mpi_queue* queue, const mpi_message::ptr& msg);
  void send_to_queue_ack(mpi_queue* queue, const mpi_message::ptr& msg);

};

}
} // end of namespace sstmac.

#endif

