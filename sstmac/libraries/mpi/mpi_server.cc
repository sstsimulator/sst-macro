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

#include <sstmac/software/process/operating_system.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_message.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_debug.h>

#include <sstmac/common/event_callback.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/runtime.h>

#include <sprockit/errors.h>
#include <sprockit/output.h>

#define _debug_printf(format_str, ...) \
  debug_printf(sprockit::dbg::mpi_server, \
   "MPI Server on node %d: " format_str, \
   int(os_->my_addr()), __VA_ARGS__)

#define _debug_print(format_str) \
  _debug_printf(format_str "%s", "")

namespace sstmac {
namespace sw {

mpi_server::mpi_server(int appnum)
{
  libname_ = sprockit::printf("mpiserver-%d", appnum);
}

mpi_server::~mpi_server() throw ()
{
}

void
mpi_server::start()
{
  service::start();
}

void
mpi_server::incoming_message(const sst_message::ptr& msg)
{
  _debug_printf("handling message %s",
    msg->to_string().c_str());
  mpi_message::ptr mpi = ptr_safe_cast(mpi_message, msg);
  handle_recv(mpi);
}

//
// Define a task for this server.  Each task can only be defined once
//
void
mpi_server::add_task(const software_id &sid,
                     mpi_queue* peer,
                     int rank)
{
  if (peer == 0) {
    spkt_throw(sprockit::null_error,
              "mpiserver::add_task: null peer pointer");
  }

  queue_t::iterator it = queue_.find(sid);
  if (it != queue_.end()) {
    spkt_throw(sprockit::illformed_error,
              "mpiserver::add_task: attempt to redefine peer");
  }
  queue_[sid] = peer;
}

void
mpi_server::unregister(const software_id& sid, mpi_queue *queue)
{
  int oldsize = queue_.size();
  queue_.erase(sid);
  int newsize = queue_.size();
  if (oldsize == newsize){
    abort();
  }
  deleted_.insert(sid);
}

void
mpi_server::init_os(operating_system *os)
{
  service::init_os(os);
  init_loc_id(os->event_location());
}

void
mpi_server::print_queues()
{
  queue_t::iterator it, end = queue_.end();
  for (it=queue_.begin(); it != end; ++it){
    std::cout << it->first << std::endl;
  }
}

//
// Test whether the given task is defined on this node.
//
bool
mpi_server::has_task(const software_id &tid) const
{
  return (queue_.find(tid) != queue_.end());
}

//
// Send data.
//
void
mpi_server::send(const mpi_message::ptr& payload)
{
  handle_send(payload);
}

void
mpi_server::buffered_send(const mpi_message::ptr& payload)
{
  spkt_throw(sprockit::unimplemented_error, "mpi_server::buffered_send");
}

void
mpi_server::start_send(const sst_message::ptr& msg)
{
  handle_send(ptr_safe_cast(mpi_message, msg));
}

void
mpi_server::handle_intranode_send(const mpi_message::ptr& payload)
{
  task_id recver = payload->dest_task();
  software_id sid(payload->app(), recver);
  queue_t::iterator it = queue_.find(sid);
  if (it != queue_.end()) {
    //push a nic ack to the source also
    if (payload->needs_ack()) {
      mpi_message::ptr nicack = ptr_safe_cast(mpi_message, payload->clone_injection_ack());

      task_id sender = payload->source_task();
      software_id sid2(payload->app(), sender);
      queue_t::iterator it2 = queue_.find(sid2);
      if (it2 != queue_.end()) {
        send_to_queue_ack(it2->second, nicack);
      }
    }

    //schedule the recv
    mpi_queue* queue = it->second;
    send_to_queue_payload(queue, payload);

    // Transfer between processes on local node.
    _debug_print("copy to local task");
  }
  else {
    send_delayed_self_message(1e-3, payload);
  }
}

void
mpi_server::handle(const sst_message::ptr &msg)
{
  //should only ever be used for delayed messages while queue initializes
  handle_intranode_send(ptr_safe_cast(mpi_message, msg));
}

void
mpi_server::handle_internode_send(const mpi_message::ptr& payload)
{
  // Network transfer
  _debug_print("executing os kernel to send remote");
  library::os_->execute_kernel(ami::COMM_SEND, payload);
}

void
mpi_server::handle_send(const mpi_message::ptr& payload)
{
  _debug_printf("sending message %s", payload->to_string().c_str());
  if (payload->intranode()) {
    handle_intranode_send(payload);
  }
  else {
    handle_internode_send(payload);
  }
}

void
mpi_server::buffered_recv(const mpi_message::ptr& msg,
                          mpi_queue_recv_request*req)
{
  spkt_throw(sprockit::unimplemented_error,
             "mpi_server::buffered_recv");
}

//
// Complete a receive.
void
mpi_server::handle_recv(const mpi_message::ptr& mess)
{
  _debug_printf("handling recv %s", mess->to_string().c_str());


  int num_queues = queue_.size();

  if (mess == 0) {
    spkt_throw_printf(sprockit::null_error,
        "mpiserver::incoming_message: null message");
  }

  task_id recver = mess->dest_task();
  software_id sid(mess->app(), recver);

  _debug_print("checking for receiver");

  queue_t::iterator it = queue_.find(sid);

  if (it == queue_.end()) {
    if (deleted_.find(sid) == deleted_.end()){
      cerrn << "mpiserver[" << os_->my_addr()
              << "]::incoming_message:  "
              << "For app " << mess->app() << ","
              << " failed to find receiver " << mess->dest_task()
              << " or sender " << mess->source_task()
              << " in the task queues."
              << " My tasks are: [ ";
      for (queue_t::iterator a = queue_.begin(); a != queue_.end(); ++a) {
        cerrn << a->first << ", ";
      }
      cerrn << "\n" << mess->to_string() << "\n";
      cerrn << "]\n";
      spkt_throw_printf(sprockit::illformed_error,
           "mpiserver::incoming_message: message %p for task not registered with this server on server %p, os %p",
           mess.get(), this, os_);
    }
  }
  else {
    _debug_printf("delivering to queue %s", it->first.to_string().c_str());
    it->second->incoming_message(mess);
  }
}

void
mpi_server::send_to_queue_ack(mpi_queue* queue, const mpi_message::ptr& ack)
{
  queue->handle_nic_ack(ack);
}

void
mpi_server::send_to_queue_payload(mpi_queue* queue, const mpi_message::ptr& payload)
{
  event* ev = new_event(os_->event_location(), queue, &mpi_queue::incoming_message, payload);
  SCHEDULE_NOW(ev);
}

}
} // end of namespace sstmac

