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

#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_sender.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_recver.h>
#include <sstmac/libraries/mpi/mpi_payload.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>

#include <sprockit/errors.h>
#include <sprockit/debug.h>
#include <sstmac/common/messages/value_payload.h>

namespace sstmac {
namespace sw {

//
// Hi.
//
mpi_collective::mpi_collective(mpi_request* the_key,
                               mpi_queue* queue, mpi_tag tag,
                               mpi_comm* comm, event_handler* completion) :
  comm_(comm),
  tag_(tag),
  completion_(completion),
  queue_(queue),
  req_(the_key),
  status_(new mpi_status)
{
  sender_ = new sender(this);
  recver_ = new recver(this);
}

//
// Request a send.
//
void
mpi_collective::start_send(int count, mpi_type_id type,
                           mpi_tag tag, mpi_id dest, const payload::const_ptr& content)
{
  mpi_coll_debug("MPI Collective",
    "starting send to %d, count=%d", int(dest), count);

  //I will never block on this - not needed
  mpi_request* req = 0;
  event_handler* sender_ = new sender(this);
  mpi_queue::sendinfo sinfo;
  queue_->send(req, count, type, dest, tag, comm_, sinfo,
               mpi_message::collective, sender_, content);
}

void
mpi_collective::start_send(int count, mpi_id dest)
{
  start_send(count, send_type_, tag_, dest);
}

void
mpi_collective::start_send(int count, mpi_id dest, void *buffer)
{
  mpi_coll_debug("MPI Collective",
    "starting send to %d, count=%d on tag=%d", 
    int(dest), count, int(tag_));

  //I will never block on this - not needed
  mpi_request* req = 0;
  event_handler* sender_ = new sender(this);
  mpi_queue::sendinfo sinfo;
  queue_->send(req, count, send_type_, dest, tag_, comm_, sinfo,
               mpi_message::collective, sender_, buffer);
}

//
// Request a recv.
//
void
mpi_collective::start_recv(int count, mpi_type_id type,
                           mpi_tag tag, mpi_id source, void* buffer)
{
  mpi_coll_debug("MPI Collective",
    "starting recv from %d, count=%d on tag=%d",
    int(source), count, int(tag_));

  //I will never block on this - not needed
  mpi_request* req = 0;

  event_handler* recver_ = new recver(this);
  queue_->recv(req, count, type, source, tag, comm_,
               mpi_message::collective, recver_, buffer);
}

void
mpi_collective::start_recv(int count, mpi_id src, void* buffer)
{
  start_recv(count, recv_type_, tag_, src, buffer);
}

void
mpi_collective::start_recv(int count, mpi_id src)
{
  start_recv(count, recv_type_, tag_, src);
}

//
// Goodbye.
//
mpi_collective::~mpi_collective() throw ()
{
  delete status_;
  delete completion_;
  delete sender_;
  delete recver_;
}

//
// Get busy child.
//
void
mpi_collective::start()
{
  mpi_debug(comm_->rank(),
    sprockit::dbg::mpi_collective,
    "starting collective %s",
    to_string().c_str());
}

void
mpi_collective::complete(const payload::const_ptr& content)
{
  mpi_debug(comm_->rank(),
    sprockit::dbg::mpi_collective,
    "completing collective %s",
    to_string().c_str());

  status_->set_content(content);
  mpi_message* fake = new mpi_message(content, mpi_message::fake);

  req_->complete(fake);
  completion_->handle(fake);
  delete fake;
}

payload::const_ptr
mpi_collective::combine_content(const payload::const_ptr& p1,
                                const payload::const_ptr& p2,
                                mpi_op* op,
                                int rankhere, int rankincoming)
{
  payload::const_ptr ret;

  mpi_coll_debug("MPI collective",
    "collective combining content %s and %s",
    p1->to_string().c_str(), p2->to_string().c_str());

  payload::ptr mp1 = const_cast<payload*>(p1.get());
  payload::ptr mp2 = const_cast<payload*>(p2.get());
  mp1->recover(queue_->api());
  mp2->recover(queue_->api());

  if (op->label == "MPI_SUM") {
    ret = p1->add(p2);
  }
  else if (op->label == "MPI_PROD") {
    ret = p1->prod(p2);
  }
  else if (op->label == "MPI_MIN") {
    ret = p1->min(p2);
  }
  else if (op->label == "MPI_MAX") {
    ret = p1->max(p2);
  }
  else if (op->label == "MPI_MINLOC") {
    mpi_payload::const_ptr pay1 = ptr_safe_cast(const mpi_payload, p1);
    ret = pay1->minloc(p2);
  }
  else if (op->label == "MPI_MAXLOC") {
    mpi_payload::const_ptr pay1 = ptr_safe_cast(const mpi_payload, p1);
    ret = pay1->maxloc(p2);
  }
  else if (op->label == "MPI_LOR") {
    ret = p1->logical_or(p2);
  }
  else if (op->label == "MPI_LXOR") {
    ret = p1->logical_xor(p2);
  }
  else if (op->label == "MPI_LAND") {
    ret = p1->logical_and(p2);
  }
  else if (op->label == "MPI_BAND") {
    ret = p1->bitwise_and(p2);
  }
  else if (op->label == "MPI_BXOR") {
    ret = p1->bitwise_xor(p2);
  }
  else if (op->label == "MPI_BOR") {
    ret = p1->bitwise_or(p2);
  }
  else if (op->label == "user") {
    payload::const_ptr temp1;
    payload::const_ptr temp2;

    //need to respect commutative property
    if (op->commute_ || rankhere > rankincoming) {
      temp1 = p1;
      temp2 = p2;
    }
    else {
      temp1 = p2;
      temp2 = p1;
    }

    mpi_payload::const_ptr cast = ptr_safe_cast(const mpi_payload, temp1,
        "mpicollective::combine_content user op");

    ret = cast->user_op(temp2, op);
  }
  else {
    spkt_throw_printf(sprockit::value_error, "mpicollective: mpiop %s not supported yet",
                     op->label.c_str());
  }

  mpi_coll_debug("MPI collective", "collective got combined result %s",
    ret->to_string().c_str());

  return ret;
}

}
} // end of namespace sstmac

