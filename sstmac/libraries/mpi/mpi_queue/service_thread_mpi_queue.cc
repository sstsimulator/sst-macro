#include <sstmac/libraries/mpi/mpi_queue/service_thread_mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/libraries/mpi/mpi_implementation/mpi_implementation.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/libraries/unblock_handler.h>

namespace sstmac {
namespace sw {

//SpktRegister("service", mpi_queue, service_thread_mpi_queue,
//"Run MPI as a separate service thread distinct from the application.");


service_thread_mpi_queue::~service_thread_mpi_queue() throw()
{
}

timestamp
service_thread_mpi_queue::progress_loop(mpi_request* req)
{
  //only block if the machine model supports nic send acks
  if (req && !req->is_complete()) {
    return os_->block(req->get_key());
  }

  return os_->now();
}

void
service_thread_mpi_queue::start_progress_loop(const std::vector<mpi_request*>&
    req)
{
  for (uint i = 0; i < req.size(); i++) {
    if(req[i]) {
      os_->add_blocker(req[i]->get_key());
    }
  }

  key* thekey = key::construct(mpi_api::default_key_category);
  os_->block(thekey);
  delete thekey;
}

void
service_thread_mpi_queue::start_progress_loop(
  const std::vector<mpi_request*>& reqs,
  timestamp timeout)
{
  if (timeout.ticks() == 0){
    start_progress_loop(reqs);
    return;
  }

  for (uint i = 0; i < reqs.size(); i++) {
    if(reqs[i]) {
      os_->add_blocker(reqs[i]->get_key());
    }
  }

  timestamp done = os_->now() + timeout;

  key* thekey = key::construct(mpi_api::default_key_category);
  event* awake = new_event(os_->event_location(),
                  this, &service_thread_mpi_queue::check_timeout,
                  done, thekey);
  server_->schedule(done, awake);
  os_->block(thekey);
  delete thekey;
}

void
service_thread_mpi_queue::finish_progress_loop(const std::vector<mpi_request*>& reqs)
{
  //cleanup
  for (uint i = 0; i < reqs.size(); i++) {
    if (reqs[i]) {
      os_->remove_blocker(reqs[i]->get_key());
    }
  }
}

void
service_thread_mpi_queue::check_timeout(const timestamp &t,
                                       key*thekey)
{
  if (os_->now() >= t) {
    //we have timed out
    os_->unblock(thekey);
  }
  else {
    //do nothing
  }
}

void
service_thread_mpi_queue::buffer_unexpected(mpi_message* msg)
{
}

void
service_thread_mpi_queue::incoming_message(mpi_message* message)
{
  mpi_queue::incoming_message(message);
}

event_handler*
service_thread_mpi_queue::progress_done_handler(operating_system* os,
    mpi_request* req)
{
  return unblock_handler::construct(os, req->get_key());
}

void
service_thread_mpi_queue::post_header(mpi_message* msg)
{
  //I have no way to model delay ... just send it
  server_->send(msg);
}

void
service_thread_mpi_queue::post_rdma(mpi_message* msg)
{
  server_->send(msg);
}

void
service_thread_mpi_queue::do_send(mpi_message*mess)
{
  event* ev = new_event(os_->event_location(), this, &mpi_queue::start_send, mess);
  server_->schedule_now(ev);
}

void
service_thread_mpi_queue::do_recv(mpi_queue_recv_request*req)
{
  event* ev = new_event(os_->event_location(), this, &mpi_queue::start_recv, req);
  server_->schedule_now(ev);
}

void
service_thread_mpi_queue::buffered_recv(mpi_message* msg,
                                       mpi_queue_recv_request*req)
{
  server_->buffered_recv(msg, req);
}

void
service_thread_mpi_queue::buffered_send(mpi_message* msg)
{
  server_->buffered_send(msg);
}

}
} // end of namespace sstmac


