#include <sstmac/libraries/mpi/mpi_queue/user_thread_mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_server.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/libraries/mpi/mpi_implementation/mpi_implementation.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/common/sstmac_env.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

SpktRegister("user", mpi_queue, user_thread_mpi_queue,
            "Run MPI as a library within the application.");


void
user_thread_mpi_queue::init_factory_params(sprockit::sim_parameters *params)
{
  mpi_queue::init_factory_params(params);
  /** sstkeyword { gui=1us; docstring=Time it takes at software level to post RDMA transaction.; } */
  post_rdma_delay_ = params->get_optional_time_param("post_rdma_delay", 0);
  /** sstkeyword { gui=0.5us;
      docstring=Time it takes at software level to post a short header using
      low-latency short-message mailboxes.;
  } */
  post_header_delay_ = params->get_optional_time_param("post_header_delay", 0);
  /** sstkeyword {
      docstring=Time it takes at software level to poll for an incoming message.;
  } */
  poll_delay_ = params->get_optional_time_param("poll_delay", 0);
}

user_thread_mpi_queue::~user_thread_mpi_queue() throw()
{
}

void user_thread_mpi_queue::clear_pending()
{
  if (pending_msgs_.empty()) {
    return;
  }

  mpi_queue_debug("clear pending messages");

  std::list<mpi_message*>::iterator it;
  for (it=pending_msgs_.begin(); it != pending_msgs_.end(); ++it) {
    mpi_message* mess = *it;
    mpi_queue_debug("handle pending message %s", mess->to_string().c_str());
    if (poll_delay_.ticks_int64()) {
      user_lib_time_->compute(poll_delay_);
    }
    mpi_queue::incoming_message(mess);
  }
  pending_msgs_.clear();
}

timestamp
user_thread_mpi_queue::progress_loop(mpi_request* req)
{
  clear_pending();

  if (!req || req->is_complete()) {
    return os_->now();
  }

  mpi_queue_debug("entering progress loop");

  SSTMACBacktrace("MPI Queue Poll");
  blocking_key_ = req->get_key();
  while (!req->is_complete()) {
    mpi_queue_debug("blocking in progress loop on thread %ld", os_->current_threadid());
    is_polling_ = true;
    os_->block(blocking_key_);
    is_polling_ = false;
    mpi_queue_debug("continuing progress loop on incoming msg %s", incoming_msg_->to_string().c_str());
    if (poll_delay_.ticks_int64()) {
      user_lib_time_->compute(poll_delay_);
    }
    mpi_queue::incoming_message(incoming_msg_);
    clear_pending();
  }
  mpi_queue_debug("finishing progress loop");

  blocking_key_ = 0;
  incoming_msg_ = 0;

  return os_->now();
}

bool
user_thread_mpi_queue::at_least_one_complete(const std::vector<mpi_request*>&
    req)
{
  mpi_queue_debug("checking if any of %d requests is done", (int)req.size());
  for (int i=0; i < (int) req.size(); ++i) {
    if (req[i] && req[i]->is_complete()) {
      mpi_queue_debug("request is done");
      os_->remove_blocker(req[i]->get_key());
      return true;
    }
  }
  return false;
}

event_handler*
user_thread_mpi_queue::progress_done_handler(operating_system* os,
    mpi_request* req)
{
  return new do_nothing_handler(os->event_location());
}

void
user_thread_mpi_queue::start_progress_loop(const std::vector<mpi_request*>& req)
{
  clear_pending();

  mpi_queue_debug("starting progress loop");

  blocking_key_ = key::construct(mpi_api::default_key_category);
  while (!at_least_one_complete(req)) {
    mpi_queue_debug("blocking on progress loop");
    is_polling_ = true;
    os_->block(blocking_key_);
    mpi_queue_debug("continuing progress loop on incoming msg %s", incoming_msg_->to_string().c_str());
    is_polling_ = false;
    clear_pending();
    mpi_queue::incoming_message(incoming_msg_);
  }
  mpi_queue_debug("finishing progress loop");
  delete blocking_key_;
  incoming_msg_ = 0;
}

void
user_thread_mpi_queue::start_progress_loop(const std::vector<mpi_request*>& req,
    timestamp timeout)
{
  start_progress_loop(req);
}

void
user_thread_mpi_queue::finish_progress_loop(const std::vector<mpi_request*>&
    req)
{
  //for (uint i = 0; i < req.size(); i++) {
  //  if (req[i]) {
  //    os_->remove_blocker(req[i]->get_key());
  //  }
  //}
}

void
user_thread_mpi_queue::do_send(mpi_message*mess)
{
#if SSTMAC_SANITY_CHECK
  if (os_->current_threadid() == thread::main_thread) {
    spkt_throw_printf(sprockit::illformed_error,
                     "user_thread_mpi_queue::do_send: on main thread");
  }
#endif
  mess->protocol()->send_header(this, mess);
}

void
user_thread_mpi_queue::do_recv(mpi_queue_recv_request*req)
{
#if SSTMAC_SANITY_CHECK
  if (os_->current_threadid() == thread::main_thread) {
    spkt_throw_printf(sprockit::illformed_error,
                     "user_thread_mpi_queue::do_recv: on main thread");
  }
#endif
  mpi_queue::start_recv(req);
}

void
user_thread_mpi_queue::buffer_unexpected(mpi_message* msg)
{
  SSTMACBacktrace("MPI Queue Buffer Unexpected Message");
  if (os_->current_threadid() == thread::main_thread) {
    spkt_throw_printf(sprockit::illformed_error, "buffering on main thread");
  }
  user_lib_mem_->copy(msg->payload_bytes());
}

void
user_thread_mpi_queue::incoming_message(mpi_message* message)
{
  if (os_->current_threadid() != -1) {
    spkt_throw_printf(sprockit::illformed_error,
       "user_thread_mpi_queue::incoming message not on main thread");
  }
  if (is_polling_) {
    //the thread is ready to receive
    incoming_msg_ = message;
    mpi_queue_debug("unblocking");
    if (!blocking_key_->still_blocked()) {
      spkt_throw_printf(sprockit::illformed_error,
                       "user_thread_mpi_queue::incoming_message: key is no longer blocked");
    }

    os_->unblock(blocking_key_);
  }
  else {
    mpi_queue_debug("incoming message %s, add to pending since not in progress loop", message->to_string().c_str());

    //this is not polling in a progress loop
    pending_msgs_.push_back(message);
  }

}

void
user_thread_mpi_queue::buffered_recv(mpi_message* msg,
                                    mpi_queue_recv_request* req)
{
  SSTMACBacktrace("MPI_buffered_recv");
  user_lib_mem_->copy(msg->payload_bytes());
  req->handle(msg);
}

void
user_thread_mpi_queue::buffered_send(mpi_message* msg)
{
  SSTMACBacktrace("MPI_buffered_send");
  // we may be sending a header or a payload
  // regardless, we need to copy the actual payload
  user_lib_mem_->copy(msg->payload_bytes());
  post_header(msg);
}

void
user_thread_mpi_queue::post_header(mpi_message* msg)
{
  SSTMACBacktrace("MPI Queue Post Header");
  if (post_header_delay_.ticks_int64()) {
    user_lib_time_->compute(post_header_delay_);
  }
  server_->send(msg);
}

void
user_thread_mpi_queue::post_rdma(mpi_message* msg)
{
  SSTMACBacktrace("MPI Queue Post RDMA Request");
  if (post_rdma_delay_.ticks_int64()) {
    user_lib_time_->compute(post_rdma_delay_);
  }
  server_->send(msg);
}


}
}

