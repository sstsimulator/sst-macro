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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_USERTHREAD_MPIQUEUE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_MPI_MPI_QUEUE_USERTHREAD_MPIQUEUE_H_INCLUDED

#include <sumi-mpi/mpi_queue/mpi_queue.h>

namespace sstmac {
namespace sumi {

class user_thread_mpi_queue :
  public mpi_queue
{

 public:
  virtual std::string
  to_string() const {
    return "user thread mpi_queue";
  }

  virtual ~user_thread_mpi_queue() throw();

  virtual timestamp
  progress_loop(mpi_request* req);

  virtual void
  start_progress_loop(const std::vector<mpi_request*>& req);

  virtual void
  start_progress_loop(const std::vector<mpi_request*>& req,
                     timestamp timeout);

  virtual void
  finish_progress_loop(const std::vector<mpi_request*>& req);

  virtual void
  incoming_message(const mpi_message::ptr& message);

  virtual void
  buffered_send(const mpi_message::ptr& msg);

  virtual void
  buffered_recv(const mpi_message::ptr& msg,
                mpi_queue_recv_request* req);

  virtual void
  buffer_unexpected(const mpi_message::ptr& msg);

  virtual void
  post_rdma(const mpi_message::ptr& msg);

  virtual void
  post_header(const mpi_message::ptr& msg);

  virtual bool
  is_service_thread() const {
    return false;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

 protected:
  bool
  at_least_one_complete(const std::vector<mpi_request*>& req);

  virtual void
  do_send(const mpi_message::ptr& mess);

  virtual void
  do_recv(mpi_queue_recv_request* req);

  void
  clear_pending();

 protected:
  bool is_polling_;

  sw::key* blocking_key_;

  mpi_message::ptr incoming_msg_;

  std::list<mpi_message::ptr> pending_msgs_;

  timestamp post_rdma_delay_;

  timestamp post_header_delay_;

  timestamp poll_delay_;


};


}
} // end of namespace sstmac


#endif

