/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <mpi/mpi_transport.h>
#include <sprockit/sim_parameters.h>
#include <sys/time.h>

#define mpi_debug_out(...) \
  debug_printf(sprockit::dbg::mpi, "Rank %d: %s at t=%8.4e", rank(), sprockit::printf(__VA_ARGS__).c_str(), wall_time())

DeclareDebugSlot(mpi);
RegisterDebugSlot(mpi);

namespace sumi {

#define enumcase(x) case x: return #x


PendingMPI::PendingMPI() :
  req(0),
  id(-1)
{
  clear();
}

const char*
PendingMPI::tostr(type_t ty)
{
  switch(ty)
  {
  enumcase(SmsgSend);
  enumcase(SmsgRecv);
  enumcase(RecvGetReq);
  enumcase(SendGetReq);
  enumcase(RecvPutReq);
  enumcase(SendPutReq);
  enumcase(SendPutAck);
  enumcase(RDMAGetRecv);
  enumcase(RDMAGetSend);
  enumcase(RDMAPutRecv);
  enumcase(RDMAPutSend);
  enumcase(SendPingRequest);
  enumcase(SendPingResponse);
  enumcase(Null);
  }
}

void
PendingMPI::clear()
{
  size = 0;
  rdma_tag = 0;
  sender = 0;
  recver = 0;
  rdma_tag = 0;
  rdma_req = 0;
  send_buf = 0;
  recv_buf = 0;
  msg = 0;
  smsg_tag = 0;
  //smsg_send_buf = 0;
  //smsg_recv_buf = 0;
  type = Null;
}

const char*
mpi_transport::tostr(ping_status_t stat)
{
  switch(stat)
  {
  enumcase(i_am_alive);
  enumcase(i_am_dead);
  }
}

const char*
mpi_transport::tostr(tag_t tag)
{
  switch(tag)
  {
   enumcase(mpi_rdma_get_tag );
   enumcase(mpi_rdma_put_tag);
   enumcase(smsg_send_tag);
   enumcase(ping_request_tag);
   enumcase(ping_response_tag);
   enumcase(transaction_ack);
   default:
     break;
  }
  if (tag > rdma_put_payload_tag){
    return "rdma put payload";
  } else {
    return "rdma get payload";
  }
}

mpi_transport::mpi_transport() :
 max_num_requests_(1000),
 poll_burst_size_(5),
 ping_status_(i_am_alive)
{
}

mpi_transport::~mpi_transport()
{
}



void
mpi_transport::rdma_get_ack(const message::ptr& msg)
{
  msg->set_payload_type(message::rdma_get_ack);
  completion_queue_.push_back(msg);
}

void
mpi_transport::do_send_terminate(int dst)
{
  lock();
  mpi_debug_out("send terminate request to %d", dst);
  MPI_Send(&ping_status_, 1, MPI_INT, dst, terminate_tag, MPI_COMM_WORLD);
  unlock();
}

void
mpi_transport::do_send_ping_request(int dst)
{
  lock();
  PendingMPI* pending = allocate_pending();
  pending->type = PendingMPI::SendPingRequest;
  mpi_debug_out("send ping request to %d", dst);
  MPI_Isend(&ping_status_, 1, MPI_INT, dst, ping_request_tag, MPI_COMM_WORLD, pending->req);
  add_pending(pending);
  unlock();
}

void
mpi_transport::recv_terminate(int src)
{
  int status;
  lock();
  MPI_Recv(&status, 1, MPI_INT, src, terminate_tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  unlock();

  message::ptr unblock_msg = new message;
  unblock_msg->set_class_type(message::terminate);
  handle(unblock_msg);
}

void
mpi_transport::recv_ping_response(int src)
{
  int status;
  lock();
  MPI_Recv(&status, 1, MPI_INT, src, ping_response_tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  unlock();

  mpi_debug_out("recv ping response %s from %d", tostr((ping_status_t)status), src);

  message::ptr ping_msg = new message;
  ping_msg->set_sender(src);
  ping_msg->set_recver(rank_);
  ping_msg->set_class_type(message::ping);
  if (status == i_am_dead){
    //oh no! he done died!
    ping_msg->set_payload_type(message::rdma_get_nack);
  } else {
    ping_msg->set_payload_type(message::rdma_get);
  }
  handle(ping_msg);
}

void
mpi_transport::recv_ping_request(int src)
{
  int ignore;
  lock();
  MPI_Recv(&ignore, 1, MPI_INT, src, ping_request_tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  mpi_debug_out("recv ping request from %d", src);
  PendingMPI* pending = allocate_pending();
  pending->type = PendingMPI::SendPingResponse;
  MPI_Isend(&ping_status_, 1, MPI_INT, src, ping_response_tag, MPI_COMM_WORLD, pending->req);
  add_pending(pending);
  unlock();
}

void
mpi_transport::init()
{
  int argc = 0;
  char* argv_arr[1];
  char** argv = (char**) argv_arr;
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank_);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc_);
  MPI_Barrier(MPI_COMM_WORLD);

  active_msg_transport::init();

  requests_ = new MPI_Request[max_num_requests_];
  pending_ = new PendingMPI[max_num_requests_];


  for (int i=0; i < max_num_requests_; ++i){
      pending_[i].req = &requests_[i];
      pending_[i].id = i;
      free_pending(&pending_[i]);
  }
}

void
mpi_transport::clear_pending(PendingMPI* pending)
{
  pending->clear();
}

void
mpi_transport::free_pending(PendingMPI *pending)
{
  clear_pending(pending);
  pending_pool_.push_back(pending);
}

PendingMPI*
mpi_transport::allocate_pending()
{
  if (pending_pool_.empty()){
    spkt_throw(sprockit::value_error,
      "too many pending mpi requests allocated");
  }
  PendingMPI* ret = pending_pool_.back();
  pending_pool_.pop_back();
  return ret;
}

void
mpi_transport::transport_smsg_send(int dst, int tag, PendingMPI::type_t ty,
  const message::ptr& msg, void* extra_md, int md_size)
{
  CHECK_IF_I_AM_DEAD(return);

  PendingMPI* pending = allocate_pending();
  char* send_buffer = allocate_smsg_buffer();
  char* ser_buffer = send_buffer;
  if (extra_md){
    ::memcpy(send_buffer, extra_md, md_size);
    ser_buffer += md_size;
  }
  sprockit::serializer ser;
  ser.start_packing(ser_buffer, smsg_buffer_size_);
  ser & msg;

  mpi_debug_out("transport send %s to %d on tag %s of type %s of size %d, extra %d",
    msg->to_string().c_str(), dst, tostr((tag_t)tag), PendingMPI::tostr(ty),
    ser.packer().size(), md_size);

  int total_size = ser.packer().size() + md_size;

  pending->type = ty;
  pending->send_buf = send_buffer;
  pending->smsg_tag = tag;
  MPI_Isend(send_buffer, total_size, MPI_BYTE, dst, tag, MPI_COMM_WORLD, pending->req);
  add_pending(pending);

}

char*
mpi_transport::allocate_smsg_buffer()
{
  try {
    return active_msg_transport::allocate_smsg_buffer();
  } catch (const std::exception& e) {
    std::cerr << sprockit::printf("There are %ld pending MPI on %d original buffers\n",
      pending_mpi_.size(), 1000);
    throw e;
  }
}

void
mpi_transport::go_die()
{
  ping_status_ = i_am_dead;
}

void
mpi_transport::go_revive()
{
  ping_status_ = i_am_alive;
}

void
mpi_transport::do_smsg_send(int dst, const message::ptr &msg)
{
  mpi_debug_out("smsg send %s to %d", msg->to_string().c_str(), dst);
  lock();
  transport_smsg_send(dst, smsg_send_tag, PendingMPI::SmsgSend, msg);
  unlock();
}

void
mpi_transport::do_rdma_get(int src, const message::ptr &msg)
{
  long bytes = msg->byte_length();
  void* sender_buf = msg->remote_buffer();
  void* recver_buf = msg->local_buffer();
  lock();

  PendingMPI* pending_recv = allocate_pending();

  int rdma_tag = pending_recv->id + rdma_get_payload_tag;
  mpi_debug_out("rdma get %s from %d on rdma tag %d for local buffer %p, remote buffer %p",
    msg->to_string().c_str(), src, rdma_tag, recver_buf, sender_buf);
  pending_recv->type = PendingMPI::RDMAGetRecv;
  pending_recv->msg = msg;
  pending_recv->sender = src;
  pending_recv->recver = rank_;
  pending_recv->rdma_tag = rdma_tag;
  pending_recv->send_buf = sender_buf;
  pending_recv->recv_buf = recver_buf;
  pending_recv->size = bytes;

  //when the "RDMA get" request is received on the other end,
  //it will match this receive
  MPI_Irecv(pending_recv->recv_buf,
            bytes,
            MPI_BYTE, src,
            pending_recv->rdma_tag, MPI_COMM_WORLD,
            pending_recv->req);

  transport_smsg_send(src, mpi_rdma_get_tag, PendingMPI::SendGetReq, msg, &rdma_tag, sizeof(int));

  add_pending(pending_recv);
  unlock();
}

void
mpi_transport::send_transaction_ack(int dst, const message::ptr& msg)
{
  CHECK_IF_I_AM_DEAD(return);

  mpi_debug_out("send transaction ack to %d for tid %d",
    dst, msg->transaction_id());

  if (msg->transaction_id() < 0){
    spkt_throw(sprockit::value_error,
      "cannot ack message without transaction ID assignment");
  }

  lock();
  PendingMPI* pending = allocate_pending();
  pending->type = PendingMPI::SendPutAck;
  MPI_Isend(&pending->rdma_tag, 1, MPI_INT, dst, transaction_ack, MPI_COMM_WORLD, pending->req);
  add_pending(pending);
  unlock();
}

void
mpi_transport::recv_transaction_ack(int src)
{
  lock();
  int tid;
  MPI_Recv(&tid, 1, MPI_INT, src, transaction_ack, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  unlock();
  mpi_debug_out("recv transaction ack from %d for tid %d",
    src, tid);
  finish_transaction(tid);
}

void
mpi_transport::do_rdma_put(int dst, const message::ptr &msg)
{
  CHECK_IF_I_AM_DEAD(return);

  long bytes = msg->byte_length();
  void* sender_buf = msg->local_buffer();
  void* recver_buf = msg->remote_buffer();
  lock();

  PendingMPI* pending_put = allocate_pending();

  int rdma_tag = msg->transaction_id() + rdma_put_payload_tag;
  mpi_debug_out("rdma put %s to %d on rdma tag %d for local buffer %p, remote buffer %p",
    msg->to_string().c_str(), dst, rdma_tag, sender_buf, recver_buf);

  transport_smsg_send(dst, mpi_rdma_put_tag, PendingMPI::SendPutReq, msg, &rdma_tag, sizeof(int));

  pending_put->type = PendingMPI::RDMAPutSend;
  pending_put->sender = rank_;
  pending_put->recver = dst;
  pending_put->rdma_tag = rdma_tag;
  pending_put->send_buf = sender_buf;
  pending_put->recv_buf = recver_buf;
  pending_put->size = bytes;
  pending_put->msg = msg;

  //when the "RDMA put" request is received on the other end,
  //it will match this send
  MPI_Isend(pending_put->send_buf,
            bytes,
            MPI_BYTE, dst,
            pending_put->rdma_tag,
            MPI_COMM_WORLD,
            pending_put->req);

  add_pending(pending_put);
  unlock();
}

void
mpi_transport::do_nvram_get(int src, const message::ptr &msg)
{
  spkt_throw(sprockit::unimplemented_error,
    "mpi_transpot: cannot do nvram get");
}

void
mpi_transport::new_incoming_msg(int src, int tag, int size)
{
  if (tag >= rdma_get_payload_tag)
    return;

  mpi_debug_out("new incoming message on tag %s of size %d from src %d",
    tostr((tag_t)tag), size, src);

  switch (tag)
  {
  case transaction_ack:
    recv_transaction_ack(src);
    break;
  case mpi_rdma_put_tag:
    recv_rdma_req(src, size, tag, PendingMPI::RecvPutReq);
    break;
  case mpi_rdma_get_tag:
    recv_rdma_req(src, size, tag, PendingMPI::RecvGetReq);
    break;
  case ping_request_tag:
    recv_ping_request(src);
    break;
  case ping_response_tag:
    recv_ping_response(src);
    break;
  case terminate_tag:
    recv_terminate(src);
    break;
  case smsg_send_tag:
    recv_smsg(src, tag, size);
    break;
  default: //data payload
  {
  }
  }

}

void
mpi_transport::add_pending(PendingMPI *pending)
{
  if (pending->type == PendingMPI::Null){
    spkt_throw(sprockit::value_error,
      "got pending MPI with null type");
  }
  pending_mpi_.push_back(pending);
}

void
mpi_transport::poll()
{
  MPI_Status status;
  int flag = 0;
  lock();
  int err = MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);
  unlock();
  if (!flag)
    return;

  int count;
  MPI_Get_count(&status, MPI_BYTE, &count);
  new_incoming_msg(status.MPI_SOURCE, status.MPI_TAG, count);
}

void
mpi_transport::recv_smsg(int src, int tag, int size)
{
  //allocate the tuple buffer
  lock();
  char* recv_buffer = allocate_smsg_buffer();
  PendingMPI* pending = allocate_pending();
  if (size > smsg_buffer_size_){
    spkt_throw(sprockit::value_error,
      "incoming smsg on tag %d of size %d exceeds max buffer size %d",
      tag, size, smsg_buffer_size_);
  }
  mpi_debug_out("receiving smsg from %d on tag %s of size %d", src, tostr((tag_t)tag), size);
  MPI_Irecv(recv_buffer, size, MPI_BYTE, src, tag, MPI_COMM_WORLD, pending->req);
  pending->type = PendingMPI::SmsgRecv;
  pending->sender = src;
  pending->recver = rank_;
  pending->size = size;
  pending->smsg_tag = tag;
  pending->recv_buf = recv_buffer;
  add_pending(pending);
  unlock();
}

message::ptr
mpi_transport::deserialize_smsg(PendingMPI* pending, void* extra_md, int md_size)
{
  sprockit::serializer ser;
  char* ser_buffer = (char*) pending->recv_buf;
  if (extra_md){
    ::memcpy(extra_md, ser_buffer, md_size);
    ser_buffer += md_size;
  }
  ser.start_unpacking(ser_buffer, pending->size);
  message::ptr msg;
  ser & msg;
  lock();
  free_smsg_buffer(pending->recv_buf);
  unlock();
  return msg;
}

void
mpi_transport::process_smsg(PendingMPI *pending)
{
  CHECK_IF_I_AM_DEAD(return);
  mpi_debug_out("processing smsg from %d of size %d",
    pending->sender, pending->size);
  handle(deserialize_smsg(pending));
}

void
mpi_transport::poll_burst()
{
  for (int i=0; i < poll_burst_size_; ++i){
      poll();
  }
  //check_pings();
  maybe_do_heartbeat();
}

void
mpi_transport::block_inner_loop()
{
  poll_burst();
  wait_on_pending();
  renew_pings();
}

void
mpi_transport::wait_on_pending()
{
  lock();

  std::list<PendingMPI*> pending_done;

  std::list<PendingMPI*>::iterator tmp,
          it = pending_mpi_.begin(),
          end = pending_mpi_.end();

  while (it != end){
    tmp = it++;
    PendingMPI* pending = *tmp;
    int done;
    MPI_Test(pending->req, &done, MPI_STATUS_IGNORE);
    if (done){
      pending_done.push_back(pending);
      pending_mpi_.erase(tmp);
    }
  }
  unlock();

  end = pending_done.end();
  for (it=pending_done.begin(); it != end; ++it)
  {
    PendingMPI* pending = *it;
    mpi_debug_out("Finishing up pending %s", PendingMPI::tostr(pending->type));
    switch(pending->type)
    {
    case PendingMPI::RDMAGetRecv:
    {
      if (pending->msg->needs_recv_ack()){
        pending->msg->set_payload_type(message::rdma_get);
        handle(pending->msg);
      }
      break;
    }
    case PendingMPI::RDMAPutRecv:
    {
      mpi_debug_out("received RDMA put, need ack? %d",
        pending->msg->needs_recv_ack());
      if (pending->msg->needs_recv_ack()){
        handle(pending->msg);
      }
      break;
    }
    case PendingMPI::RecvPutReq:
      process_rdma_put_req(pending);
      break;
    case PendingMPI::RecvGetReq:
      process_rdma_get_req(pending);
      break;
    case PendingMPI::SmsgRecv:
      process_smsg(pending);
      break;
    case PendingMPI::SendGetReq:
    case PendingMPI::SendPutReq:
    case PendingMPI::SmsgSend:
      free_smsg_buffer(pending->send_buf);
      break;
    case PendingMPI::Null:
      spkt_throw(sprockit::value_error,
        "cannot receive pending mpi request with value null");
      break;
    case PendingMPI::RDMAPutSend:
      if (pending->msg->needs_send_ack()){
        message::ptr cln = pending->msg->clone_msg();
        cln->set_payload_type(message::rdma_put_ack);
        handle(cln);
      }
      break;
    case PendingMPI::RDMAGetSend:
      if (pending->msg->needs_send_ack()){
        pending->msg->set_payload_type(message::rdma_get_ack);
        handle(pending->msg); //pass the ack on up
      }
      break;
    case PendingMPI::SendPingResponse:
    case PendingMPI::SendPingRequest:
    case PendingMPI::SendPutAck:
      break;
    }
    lock();
    free_pending(pending);
    unlock();
  }
}

void
mpi_transport::finalize()
{
  transport::finalize();
  MPI_Barrier(MPI_COMM_WORLD);
  MPI_Finalize();
}

void
mpi_transport::recv_rdma_req(int src, int size, int tag, PendingMPI::type_t ty)
{
  lock();
  char* recv_buffer = allocate_smsg_buffer();
  PendingMPI* pending_recv = allocate_pending();
  MPI_Irecv(recv_buffer, size, MPI_BYTE, src,
            tag, MPI_COMM_WORLD, pending_recv->req);
  pending_recv->type = ty;
  pending_recv->smsg_tag = tag;
  pending_recv->size = sizeof(PendingMPI);
  pending_recv->recv_buf = recv_buffer;
  pending_recv->sender = src;
  pending_recv->recver = rank_;
  add_pending(pending_recv);
  unlock();
}

void
mpi_transport::process_rdma_put_req(PendingMPI* pending)
{
  int rdma_tag;
  message::ptr msg = deserialize_smsg(pending, &rdma_tag, sizeof(int));

  mpi_debug_out("process rdma put on tag %d from %d",
    rdma_tag, pending->sender);

  //this msg is from the perspective of the request issuer
  void* recver_buf = msg->remote_buffer();
  void* sender_buf = msg->local_buffer();
  long bytes = msg->byte_length();

  lock();
  mpi_debug_out("received rdma put req of size %d from %d"
      " for remote buffer %p, local buffer %p on tag %d",
      bytes,
      pending->sender,
      sender_buf,
      recver_buf,
      rdma_tag);


  PendingMPI* pending_recv = allocate_pending();
  MPI_Irecv(recver_buf,
            bytes,
            MPI_BYTE,
            pending->sender,
            rdma_tag,
            MPI_COMM_WORLD,
            pending_recv->req);

  pending_recv->type = PendingMPI::RDMAPutRecv;
  pending_recv->send_buf = sender_buf;
  pending_recv->recv_buf = recver_buf;
  pending_recv->rdma_tag = rdma_tag;
  pending_recv->recver = pending->recver;
  pending_recv->sender = pending->sender;
  pending_recv->msg = msg;

  add_pending(pending_recv);

  unlock();
}

void
mpi_transport::process_rdma_get_req(PendingMPI* pending)
{
  CHECK_IF_I_AM_DEAD(return);

  int rdma_tag;
  message::ptr msg = deserialize_smsg(pending, &rdma_tag, sizeof(int));

  mpi_debug_out("process rdma get req tag %d from %d",
    rdma_tag, pending->sender);

  //this msg is from the perspective of the request issuer
  void* recver_buf = msg->local_buffer();
  void* sender_buf = msg->remote_buffer();
  long bytes = msg->byte_length();

  lock();
  mpi_debug_out("received rdma get req of size %d from %d"
      " for remote buffer %p, local buffer %p on tag %d",
      bytes,
      pending->recver,
      recver_buf,
      sender_buf,
      rdma_tag);


  PendingMPI* pending_send = allocate_pending();
  MPI_Isend(sender_buf,
            bytes,
            MPI_BYTE,
            pending->sender, //return to the sender of the req
            rdma_tag,
            MPI_COMM_WORLD,
            pending_send->req);

  pending_send->type = PendingMPI::RDMAGetSend;
  pending_send->send_buf = sender_buf;
  pending_send->recv_buf = recver_buf;
  pending_send->rdma_tag = rdma_tag;
  pending_send->recver = pending->sender; //message flipped
  pending_send->sender = pending->recver;
  pending_send->msg = msg;

  add_pending(pending_send);
  unlock();
}


}