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

#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sumi-mpi/mpi_message.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>

namespace sumi {


//
// Hello.
//
mpi_queue_recv_request::mpi_queue_recv_request(
  mpi_request* key,
  mpi_queue* queue, int count, MPI_Datatype type,
  int source, int tag, MPI_Comm comm) :
  queue_(queue), source_(source), tag_(tag), comm_(comm),
  seqnum_(seqnum_unassigned), count_(count), type_(type), key_(key),
  buffer_(0)
{
}

//
// Goodbye.
//
mpi_queue_recv_request::~mpi_queue_recv_request()
{
}

bool
mpi_queue_recv_request::open_source() const
{
  return source_ == MPI_ANY_SOURCE;
}

template <class T>
class delete_this {
 public:
  delete_this() : t_(0) {}
  ~delete_this(){ if (t_) delete t_; }
  void operator=(T* t){
    t_ = t;
  }
 private:
  T* t_;
};

void
mpi_queue_recv_request::finish_message(void* buffer, const mpi_message::ptr& mess)
{
  mess->protocol()->finish_recv_payload(queue_, mess, this);
  if (buffer_){ //do some copying
#if SSTMAC_SANITY_CHECK
    if (!buffer){
      spkt_throw_printf(sprockit::value_error,
        "Send sent null buffer, but recv posted real buffer"); 
    }
#endif
    ::memcpy(buffer_, buffer, mess->payload_bytes());
  }
  queue_->finalize_recv(mess);
  if (key_){
    key_->complete(mess);
  }
}

//
// We be done.
//
void
mpi_queue_recv_request::handle(const mpi_message::ptr& mess)
{
#if SSTMAC_SANITY_CHECK
  if (mess->count() < 0) {
    spkt_throw(sprockit::value_error,
        "mpi_queue_recv_request::handle: message count < 0");
  }
#endif

  delete_this<mpi_queue_recv_request> delme;
  switch (mess->content_type())
  {
  case mpi_message::header:
    mess->protocol()->finish_recv_header(queue_, mess, this);
    break;
  case mpi_message::eager_payload:
    finish_message(mess->eager_buffer(), mess);
    delme = this;
  case mpi_message::data: {
    finish_message(mess->local_buffer(), mess);
    delme = this;
    break;
  }
  default:
    spkt_throw_printf(sprockit::value_error,
        "mpi_queue_recv_request::handle: cannot handle %s",
        mpi_message::str(mess->content_type()));
  }
}

bool
mpi_queue_recv_request::is_cancelled() const {
  return key_ && key_->is_cancelled();
}

bool
mpi_queue_recv_request::matches(const mpi_message::ptr& msg)
{
  bool count_equals = true; //count_ == msg->count();
  //JJW
  //this should be a comparison of comm IDs only
  //apparently you can free the communicator and then still complete ops on it
  bool comm_equals = comm_ == msg->comm();
  bool seq_equals = ((seqnum_ == seqnum_unassigned) ? true : seqnum_ ==
                     msg->seqnum());
  bool src_equals = source_ == msg->src_rank() || source_ == MPI_ANY_SOURCE;
  bool tag_equals = tag_ == msg->tag() || tag_ == MPI_ANY_TAG;
  return comm_equals && seq_equals && src_equals && tag_equals && count_equals;
}

}

