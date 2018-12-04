/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sumi-mpi/mpi_queue/mpi_queue_recv_request.h>
#include <sumi-mpi/mpi_queue/mpi_queue_probe_request.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>
#include <sprockit/statics.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/util.h>
#include <stdint.h>

RegisterNamespaces("traffic_matrix", "num_messages");
RegisterKeywords(
{ "smp_single_copy_size", "the minimum size of message for single-copy protocol" },
{ "max_eager_msg_size", "the maximum size for using eager pt2pt protocol" },
{ "max_vshort_msg_size", "the maximum size for mailbox protocol" },
);

DeclareDebugSlot(mpi_all_sends);
RegisterDebugSlot(mpi_all_sends);

using namespace std::placeholders;

namespace sumi {

static sprockit::need_delete_statics<mpi_queue> del_statics;

bool
mpi_queue::sortbyseqnum::operator()(mpi_message* a,
                                    mpi_message*b) const
{
  return (a->seqnum() < b->seqnum());
}

mpi_queue::mpi_queue(sprockit::sim_parameters* params,
                     int task_id,
                     mpi_api* api, collective_engine* engine) :
  taskid_(task_id),
  api_(api),
  queue_(api->os())
{
  max_vshort_msg_size_ = params->get_optional_byte_length_param("max_vshort_msg_size", 512);
  max_eager_msg_size_ = params->get_optional_byte_length_param("max_eager_msg_size", 8192);

  protocols_.resize(mpi_protocol::NUM_PROTOCOLS);
  protocols_[mpi_protocol::EAGER0] = new eager0(params, this);
  protocols_[mpi_protocol::EAGER1] = new eager1(params, this);
  protocols_[mpi_protocol::RENDEZVOUS_GET] = new rendezvous_get(params, this);

  pt2pt_cq_ = api_->allocate_cq_id();
  coll_cq_ = api_->allocate_cq_id();

  api_->allocate_cq(pt2pt_cq_, std::bind(&progress_queue::incoming, &queue_, pt2pt_cq_, _1));
  api_->allocate_cq(coll_cq_, std::bind(&progress_queue::incoming, &queue_, coll_cq_, _1));
}

struct init_struct {
  int pt2pt_cq_id;
  int coll_cq_id;
  bool all_equal;
};

void
mpi_queue::init()
{
  auto init_fxn = [](void* out, const void* src, int nelems){
    auto* outs= (init_struct*)out;
    auto* srcs = (init_struct*)src;
    for (int i=0; i < nelems; ++i){
      auto& out = outs[i];
      auto& src = srcs[i];
      out.all_equal = src.all_equal
          && out.pt2pt_cq_id == src.pt2pt_cq_id
          && out.coll_cq_id == src.coll_cq_id;
    }
  };
  init_struct init;
  init.coll_cq_id = coll_cq_;
  init.pt2pt_cq_id = pt2pt_cq_;
  init.all_equal = true;
  auto cmsg = api_->engine()->allreduce(&init, &init, 1, sizeof(init_struct), 0, init_fxn,
                            message::default_cq);\
  if (cmsg == nullptr){
    cmsg = api_->engine()->block_until_next(message::default_cq);
  }

  if (cmsg->tag() != 0){
    spkt_abort_printf("got bad collective done message in mpi_queue::init");
  }
  if (!init.all_equal){
    spkt_abort_printf("MPI_Init: procs did not agree on completion queue ids");
  }
}

void
mpi_queue::delete_statics()
{
}

double
mpi_queue::now() const {
  return api_->now().sec();
}

mpi_queue::~mpi_queue() throw ()
{
}

void
mpi_queue::send(mpi_request *key, int count, MPI_Datatype type,
  int dest, int tag, mpi_comm *comm, void *buffer)
{
  mpi_type* typeobj = api_->type_from_id(type);
  if (typeobj->packed_size() < 0){
    spkt_throw_printf(sprockit::value_error,
      "MPI_Datatype %s has negative size %ld",
      api_->type_str(type).c_str(), typeobj->packed_size());
  }
  uint64_t bytes = count * uint64_t(typeobj->packed_size());

  int prot_id = mpi_protocol::RENDEZVOUS_GET;
  if (bytes <= max_vshort_msg_size_) {
    prot_id = mpi_protocol::EAGER0;
  } else if (bytes <= max_eager_msg_size_) {
    prot_id = mpi_protocol::EAGER1;
  }

  mpi_protocol* prot = protocols_[prot_id];

  mpi_queue_debug("starting send count=%d, type=%s, dest=%d, tag=%d, comm=%s, prot=%s",
    count, api_->type_str(type).c_str(), int(dest),
    int(tag), api_->comm_str(comm).c_str(),
    prot->to_string().c_str());

  task_id dst_tid = comm->peer_task(dest);
  prot->start(buffer, comm->rank(), dest, dst_tid, count, typeobj,
              tag, comm->id(), next_outbound_[dst_tid]++, key);

#if !SSTMAC_ALLOW_LARGE_PAYLOADS
  if (isNonNullBuffer(buffer) && mess->byte_length() > 64){
    spkt_abort_printf("mpi queue sending large message with real payload:\n%s",
      mess->to_string().c_str());
  }
#endif
}

mpi_message*
mpi_queue::find_matching_recv(mpi_queue_recv_request* req)
{
  for (auto it = need_recv_match_.begin(); it != need_recv_match_.end(); ++it) {
    mpi_message* mess = *it;
    if (req->matches(mess)) {
      mpi_queue_debug("matched recv tag=%s,src=%s on comm=%s to send %s",
        api_->tag_str(req->tag_).c_str(), 
        api_->src_str(req->source_).c_str(),
        api_->comm_str(req->comm_).c_str(),
        mess->to_string().c_str());

      need_recv_match_.erase(it);
      return mess;
    }
  }
  mpi_queue_debug("could not match recv tag=%s, src=%s to any of %d sends on comm=%s",
    api_->tag_str(req->tag_).c_str(), 
    api_->src_str(req->source_).c_str(),
    need_recv_match_.size(),
    api_->comm_str(req->comm_).c_str());

  need_send_match_.push_back(req);
  return nullptr;
}

//
// Receive data.
//
void
mpi_queue::recv(mpi_request* key, int count,
                MPI_Datatype type,
                int source, int tag,
                mpi_comm* comm,
                void* buffer)
{
  mpi_queue_debug("starting recv count=%d, type=%s, src=%s, tag=%s, comm=%s, buffer=%p",
        count, api_->type_str(type).c_str(), api_->src_str(source).c_str(),
        api_->tag_str(tag).c_str(), api_->comm_str(comm).c_str(), buffer);

#if !SSTMAC_ALLOW_LARGE_PAYLOADS
  if (isNonNullBuffer(buffer) && count > 16){
    spkt_abort_printf("mpi queue recving large message with real payload");
  }
#endif

  mpi_queue_recv_request* req = new mpi_queue_recv_request(key, this,
                            count, type, source, tag, comm->id(), buffer);
  mpi_message* mess = find_matching_recv(req);
  if (mess) {
    auto* protocol = protocols_[mess->protocol()];
    protocol->incoming(mess, req);
  }
}

void
mpi_queue::finalize_recv(mpi_message* msg,
                         mpi_queue_recv_request* req)
{
  req->key_->complete(msg);
  if (req->recv_buffer_ != req->final_buffer_){
    req->type_->unpack_recv(req->recv_buffer_, req->final_buffer_, msg->count());
    delete[] req->recv_buffer_;
  }
  delete req;
}

//
// Ask for a notification when a message with the given signature arrives.
//
void
mpi_queue::probe(mpi_request* key, mpi_comm* comm,
                 int source, int tag)
{
  mpi_queue_debug("starting probe src=%s, tag=%s, comm=%s",
    api_->src_str(source).c_str(), api_->tag_str(tag).c_str(),
    api_->comm_str(comm).c_str());

  mpi_queue_probe_request* req = new mpi_queue_probe_request(key, comm->id(), source, tag);
  // Figure out whether we already have a matching message.
  for (auto it = need_recv_match_.begin(); it != need_recv_match_.end(); ++it) {
    mpi_message* mess = *it;
    if (req->matches(mess)){
      // We're good to go.
      req->complete(mess);
      return;
    }
  }
  // If we get here, we still need to wait for the message.
  probelist_.push_back(req);
}

//
// Immediate-mode probe for a message with the given signature.
//
bool
mpi_queue::iprobe(mpi_comm* comm,
                  int source,
                  int tag,
                  MPI_Status* stat)
{
  mpi_queue_debug("starting immediate probe src=%s, tag=%s, comm=%s",
    api_->src_str(source).c_str(), api_->tag_str(tag).c_str(),
    api_->comm_str(comm).c_str());

  mpi_queue_probe_request req(NULL, comm->id(), source, tag);
  for (auto it = need_recv_match_.begin(); it != need_recv_match_.end(); ++it) {
    mpi_message* mess = *it;
    if (req.matches(mess)) {
      // This is it
      if (stat != MPI_STATUS_IGNORE) mess->build_status(stat);
      return true;
    }
  }
  return false;
}

mpi_queue_recv_request*
mpi_queue::find_matching_recv(mpi_message* message)
{
  auto end = need_send_match_.end();
  for (auto it = need_send_match_.begin(); it != end;) {
    auto* req = *it;
    auto tmp = it++;
    if (req->is_cancelled()) {
      need_send_match_.erase(tmp);
    } else if (req->matches(message)) {
      need_send_match_.erase(tmp);
      return req;
    }
  }
  need_recv_match_.push_back(message);
  return nullptr;
}

void
mpi_queue::incoming_collective_message(sumi::message* m)
{
  collective_work_message* msg = safe_cast(collective_work_message, m);
  collective_done_message* cmsg = api_->engine()->incoming(msg);
  if (cmsg){
    mpi_comm* comm = safe_cast(mpi_comm, cmsg->dom());
    mpi_request* req = comm->get_request(cmsg->tag());
    collective_op_base* op = req->collective_data();
    api_->finish_collective(op);
    req->complete();
    delete op;
    delete cmsg;
  }
}

//
// Handle a new incoming message.  Can be either an eager send (with data)
// or a new handshake request
//
void
mpi_queue::incoming_pt2pt_message(message* m)
{
  mpi_queue_debug("incoming new message %s", m->to_string().c_str());

  mpi_message* message = safe_cast(mpi_message, m);

  if (message->stage() > 0 || message->is_nic_ack()){
    //already matched - pass that on through
    handle_pt2pt_message(message);
    return;
  }

  task_id tid(message->sender());
  auto& next_inbound = next_inbound_[tid];
  if (message->seqnum() == next_inbound){
    mpi_queue_debug("seqnum for task %d matched expected seqnum %d and advanced to next seqnum",
        int(tid), int(next_inbound));


    handle_pt2pt_message(message);
    ++next_inbound;

    // Handle any messages that have been freed by the arrival of this one
    if (!held_[tid].empty()) {
      hold_list_t::iterator it = held_[tid].begin(), end = held_[tid].end();
      while (it != end) {
        mpi_message* mess = *it;
        mpi_queue_debug("handling out-of-order message for task %d, seqnum %d",
            int(tid), mess->seqnum());
        if (mess->seqnum() <= next_inbound_[tid]) {
          //it = held_[tid].erase(it);
          it++;
          handle_pt2pt_message(mess);
          ++next_inbound;
        } else {
          break;
        }
      }
      // Now erase all the completed messages from the held queue.
      held_[tid].erase(held_[tid].begin(), it);
    }
  } else if (message->seqnum() < next_inbound){
    spkt_abort_printf("message sequence went backwards on %s from %d",
                      message->to_string().c_str(), next_inbound);
  } else {
    mpi_queue_debug("message arrived out-of-order with seqnum %d, didn't match expected %d for task %d",
        message->seqnum(), int(next_inbound), int(tid));
    held_[tid].insert(message);
  }
}

void
mpi_queue::notify_probes(mpi_message* message)
{
  auto pit = probelist_.begin();
  auto pend = probelist_.end();
  while (pit != pend) {
    auto tmp = pit++;
    mpi_queue_probe_request* preq = *tmp;
    if (preq->matches(message)){
      probelist_.erase(tmp);
      preq->complete(message);
      delete preq;
    }
  }
}

void
mpi_queue::handle_pt2pt_message(mpi_message* message)
{
  mpi_protocol* protocol = protocols_[message->protocol()];
  protocol->incoming(message);
}

void
mpi_queue::incoming_message(sumi::message* msg)
{
  if (msg->cq_id() == pt2pt_cq_){
    incoming_pt2pt_message(msg);
  } else if (msg->cq_id() == coll_cq_){
    incoming_collective_message(msg);
  } else {
    spkt_abort_printf("Got bad completion queue %d for %s",
                      msg->cq_id(), msg->to_string().c_str());
  }
}

void
mpi_queue::nonblocking_progress()
{
  sumi::message* msg = api_->poll(false); //do not block
  while (msg){
    incoming_message(msg);
    msg = api_->poll(false);
  }
}

sstmac::timestamp
mpi_queue::progress_loop(mpi_request* req)
{
  if (!req || req->is_complete()) {
    return api_->now();
  }

  mpi_queue_debug("entering progress loop");

  //SSTMACBacktrace(MPIQueuePoll);
  sstmac::timestamp wait_start = api_->now();
  while (!req->is_complete()) {
    mpi_queue_debug("blocking on progress loop");
    sumi::message* msg = queue_.find_any();
    if (!msg){
      spkt_abort_printf("polling returned null message");
    }
    incoming_message(msg);
#if SSTMAC_COMM_SYNC_STATS
    if (req->is_complete()){
      api_->collect_sync_delays(wait_start.sec(), msg);
    }
#endif
  }

  mpi_queue_debug("finishing progress loop");
  return api_->now();
}

bool
mpi_queue::at_least_one_complete(const std::vector<mpi_request*>& req)
{
  mpi_queue_debug("checking if any of %d requests is done", (int)req.size());
  for (int i=0; i < (int) req.size(); ++i) {
    if (req[i] && req[i]->is_complete()) {
      mpi_queue_debug("request is done");
      return true;
    }
  }
  return false;
}

void
mpi_queue::start_progress_loop(const std::vector<mpi_request*>& reqs)
{
  mpi_queue_debug("starting progress loop");
  while (!at_least_one_complete(reqs)) {
    mpi_queue_debug("blocking on progress loop");
    sumi::message* msg = queue_.find_any();
    if (!msg){
      spkt_abort_printf("polling returned null message");
    }
    incoming_message(msg);
  }
  mpi_queue_debug("finishing progress loop");
}

void
mpi_queue::forward_progress(double timeout)
{
  mpi_queue_debug("starting forward progress with timeout=%f", timeout);
  sumi::message* msg = api_->poll(true, timeout); //block until timeout
  if (msg) incoming_message(msg);
}

void
mpi_queue::start_progress_loop(
  const std::vector<mpi_request*>& req,
  sstmac::timestamp timeout)
{
  start_progress_loop(req);
}

void
mpi_queue::finish_progress_loop(const std::vector<mpi_request*>& req)
{
}

void
mpi_queue::memcopy(uint64_t bytes)
{
  api_->memcopy(bytes);
}

void
mpi_queue::buffer_unexpected(mpi_message* msg)
{
  SSTMACBacktrace(MPIQueueBufferUnexpectedMessage);
  api_->memcopy(msg->payload_bytes());
}

}
