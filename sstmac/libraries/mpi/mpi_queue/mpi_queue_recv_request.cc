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

#include <sstmac/libraries/mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_message.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

// Pointer types.


//
// Hello.
//
mpi_queue_recv_request::mpi_queue_recv_request(mpi_request* key,
                                      mpi_queue* queue, int count, mpi_type_id type,
                                      mpi_id source, mpi_tag tag, mpi_comm* comm,
                                      mpi_message::category cat, event_handler* completion) :
  queue_(queue), source_(source), tag_(tag), comm_(comm->id()), cat_(cat),
  seqnum_(seqnum_unassigned), count_(count), type_(type), key_(key),
  completion_(completion), buffer_(0)
{
}

//
// Goodbye.
//
mpi_queue_recv_request::~mpi_queue_recv_request()
{
  delete completion_;
}

bool
mpi_queue_recv_request::open_source() const
{
  return source_ == mpi::any_source;
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

//
// We be done.
//
void
mpi_queue_recv_request::handle(mpi_message* mess)
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
  case mpi_message::data: {
    queue_->log_completion(mess);
    mess->protocol()->finish_recv_payload(queue_, mess, this);
    if (buffer_){ //do some copying
      ::memcpy(buffer_, mess->buffer(), mess->payload_bytes());
    }
    queue_->finalize_recv(mess);
    if (key_){
      key_->complete(mess);
      key_->status().set_recv_type(type_);
    }
    completion_->handle(mess);
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
mpi_queue_recv_request::matches(mpi_message* msg)
{
  bool count_equals = true; //count_ == msg->count();
  bool cat_equals = cat_ == msg->cat();
  //JJW
  //this should be a comparison of comm IDs only
  //apparently you can free the communicator and then still complete ops on it
  bool comm_equals = comm_ == msg->commid();
  bool seq_equals = ((seqnum_ == seqnum_unassigned) ? true : seqnum_ ==
                     msg->seqnum());
  bool src_equals = source_ == msg->source() || source_ == mpi::any_source;
  bool tag_equals = tag_ == msg->tag() || tag_ == mpi::any_tag;
  return cat_equals && comm_equals && seq_equals && src_equals && tag_equals && count_equals;
}

}
} // end of namespace sstmac

