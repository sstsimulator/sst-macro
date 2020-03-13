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

#include <sumi/collective_actor.h>
#include <sumi/transport.h>
#include <sumi/communicator.h>
#include <sprockit/output.h>
#include <cstring>
#include <utility>

/*
#undef debug_printf
#define debug_printf(flags, ...) \
  if (tag_ == 221) std::cout << sprockit::spkt_printf(__VA_ARGS__) << std::endl
*/

RegisterDebugSlot(sumi_collective_buffer);

namespace sumi {

reduce_fxn Slicer::null_reduce_fxn = [](void*,const void*,int){};

using namespace sprockit::dbg;

std::string
Action::toString() const
{
  return sprockit::sprintf("action %s r=%d,p=%d,o=%d,n=%d",
     Action::tostr(type),
     round, partner, offset, nelems);
}

void
debug_print(const char* info, const std::string& rank_str,
            int partner, int round, int offset, int nelems,
            int type_size, const void* buffer)
{
  if (!buffer) return;

  std::cout << sprockit::sprintf("Rank %s, partner %d, round=%d, %s %p [%2d:%2d] = { ",
      rank_str.c_str(), partner, round, info, buffer, offset, offset + nelems);
  char* tmp = (char*) buffer;
  for (int i=0; i < nelems; ++i, tmp += type_size){
    int elem;
    if (type_size == sizeof(int)){
      int* elemPtr = (int*) tmp;
      elem = *elemPtr;
    } else if (type_size == sizeof(double)){
      double* elemPtr = (double*) tmp;
      elem = *elemPtr;
    } else {
      int* elemPtr = (int*) tmp;
      elem = *elemPtr;
    }

    std::cout << sprockit::sprintf("%2d ", elem);
  }
  std::cout << " }\n";
}

CollectiveActor::CollectiveActor(CollectiveEngine* engine, int tag, int cq_id, Communicator* comm) :
  my_api_(engine->tport()),
  engine_(engine),
  dom_me_(comm->myCommRank()),
  dom_nproc_(comm->nproc()),
  tag_(tag),
  comm_(comm),
  cq_id_(cq_id),
  complete_(false)
{
}

CollectiveActor::~CollectiveActor()
{
#ifdef FEATURE_TAG_SUMI_RESILIENCE
  delete timeout_;
#endif
}

std::string
CollectiveActor::rankStr(int dom_rank) const
{
  int global_rank =  comm_->commToGlobalRank(dom_rank);
  return sprockit::sprintf("%d=%d", global_rank, dom_rank);
}

std::string
CollectiveActor::rankStr() const
{
  return sprockit::sprintf("%d=%d", my_api_->rank(), dom_me_);
}

int
CollectiveActor::globalRank(int dom_rank) const
{
  return comm_->commToGlobalRank(dom_rank);
}

int
CollectiveActor::domToGlobalDst(int dom_dst)
{
  int global_physical_dst =  comm_->commToGlobalRank(dom_dst);
  debug_printf(sumi_collective,
    "Rank %s sending message to %s on tag=%d, domain=%d, physical=%d",
    rankStr().c_str(),
    rankStr(dom_dst).c_str(),
    tag_, dom_dst, global_physical_dst);
  return global_physical_dst;
}

void
DagCollectiveActor::start()
{
#if SSTMAC_COMM_DELAY_STATS
  my_api_->startCollectiveMessageLog();
#endif
  while (!initial_actions_.empty()){
    auto iter = initial_actions_.begin();
    Action* ac = *iter;
    initial_actions_.erase(iter);
    startAction(ac);
  }
}

void
DagCollectiveActor::startAction(Action* ac)
{
  ac->start = my_api_->now();
  debug_printf(sumi_collective,
   "Rank %s starting action %s to partner %s on round %d offset %d tag %d -> id = %u: %d pending send headers, %d pending recv headers",
    rankStr().c_str(), Action::tostr(ac->type),
    rankStr(ac->partner).c_str(),
    ac->round, ac->offset, tag_, ac->id,
    pending_send_headers_.size(),
    pending_recv_headers_.size());
  switch (ac->type){
    case Action::send:
      startSend(ac);
      break;
    case Action::unroll:
      break;
    case Action::shuffle:
      do_sumi_debug_print("recv buf before shuffle",
        rankStr().c_str(), ac->partner,
        ac->round,
        0, ac->nelems, type_size_,
        recv_buffer_.ptr);
      do_sumi_debug_print("result buf before shuffle",
        rankStr().c_str(), ac->partner,
        ac->round,
        0, nelems_*dense_nproc_, type_size_,
        result_buffer_.ptr);
      startShuffle(ac);
      do_sumi_debug_print("result buf after shuffle",
        rankStr().c_str(), ac->partner,
        ac->round,
        0, nelems_*dense_nproc_, type_size_,
        result_buffer_.ptr);
      do_sumi_debug_print("send buf after shuffle",
        rankStr().c_str(), ac->partner,
        ac->round,
        0, ac->nelems, type_size_,
        send_buffer_.ptr);
      clearAction(ac);
      break;
    case Action::recv:
      startRecv(ac);
      break;
   case Action::join:
     clearAction(ac); //does nothing, immediately clear
     break;
   case Action::resolve:
    //nothing to do, utility only
    break;
  }
}

void
DagCollectiveActor::startShuffle(Action * /*ac*/)
{
  spkt_throw_printf(sprockit::UnimplementedError,
    "collective %s does not shuffle data - invalid DAG",
    Collective::tostr(type_));
}

void
DagCollectiveActor::clearDependencies(Action* ac)
{
  std::multimap<uint32_t, Action*>::iterator it = pending_comms_.find(ac->id);
  std::list<Action*> pending_actions;
  while (it != pending_comms_.end()){
    Action* pending = it->second;
    pending_actions.push_back(pending);
    pending_comms_.erase(it);

    pending->join_counter--;
    debug_printf(sumi_collective,
      "Rank %s satisfying dependency to join counter %d for action %s to partner %s on round %d"
      " with action %u tag=%d",
      rankStr().c_str(), pending->join_counter,
      Action::tostr(pending->type),
      rankStr(pending->partner).c_str(),
      pending->round,ac->id,tag_);

    if (ac->type == Action::resolve){
      pending->phys_partner = ac->phys_partner;
    }

    if (pending->join_counter == 0){
      startAction(pending);
    }

    it = pending_comms_.find(ac->id);
  }
}

void
DagCollectiveActor::clearAction(Action* ac)
{
  active_comms_.erase(ac->id);
  checkCollectiveDone();
  clearDependencies(ac);
  completed_actions_.push_back(ac);
}

void
DagCollectiveActor::commActionDone(Action* ac)
{
  debug_printf(sumi_collective,
    "Rank %s finishing comm action %s to partner %s on round %d -> id %u tag=%d",
    rankStr().c_str(), Action::tostr(ac->type),
    rankStr(ac->partner).c_str(),
    ac->round, ac->id,
    tag_);

  clearAction(ac);
}

void
DagCollectiveActor::sendEagerMessage(Action* ac)
{
  uint64_t num_bytes;
  void* buf = getSendBuffer(ac, num_bytes);
  /*auto* msg =*/ my_api_->smsgSend<CollectiveWorkMessage>(ac->phys_partner, num_bytes, buf,
                                              cq_id_, cq_id_, Message::collective, engine_->smsgQos(),
                                              type_, dom_me_, ac->partner,
                                              tag_, ac->round, ac->nelems, type_size_,
                                              nullptr, CollectiveWorkMessage::eager);

  debug_printf(sumi_collective | sprockit::dbg::sumi,
   "Rank %s, collective %s(%p) sending eager message to %d on tag=%d offset=%d",
   rankStr().c_str(), toString().c_str(), this,
   ac->partner, tag_, ac->offset);
}

void
DagCollectiveActor::sendRdmaPutHeader(Action* ac)
{
  void* buf = getRecvbuffer(ac);
  my_api_->smsgSend<CollectiveWorkMessage>(ac->phys_partner, 0, buf,
                                           Message::no_ack, cq_id_, Message::collective, engine_->rdmaHeaderQos(),
                                           type_, dom_me_, ac->partner,
                                           tag_, ac->round,
                                           ac->nelems, type_size_, buf, CollectiveWorkMessage::put);

  debug_printf(sumi_collective | sprockit::dbg::sumi,
   "Rank %s, collective %s(%p) sending put header to %s on round=%d tag=%d offset=%d",
   rankStr().c_str(), toString().c_str(), this,
   rankStr(ac->partner).c_str(),
   ac->round, tag_, ac->offset);
}

void
DagCollectiveActor::sendRdmaGetHeader(Action* ac)
{
  uint64_t num_bytes;
  void* buf = getSendBuffer(ac, num_bytes);
  my_api_->smsgSend<CollectiveWorkMessage>(ac->phys_partner, 64, //use platform-independent size
                        buf, Message::no_ack, cq_id_, Message::collective, engine_->rdmaHeaderQos(),
                        type_, dom_me_, ac->partner,
                        tag_, ac->round,
                        ac->nelems, type_size_, buf, CollectiveWorkMessage::get); //do not ack the send

  debug_printf(sumi_collective | sprockit::dbg::sumi,
   "Rank %s, collective %s(%p) sending RDMA get header to %d on tag=%d offset=%d",
   rankStr().c_str(), toString().c_str(), this,
   ac->partner, tag_, ac->offset);
}


void
DagCollectiveActor::addAction(Action* ac)
{
  addDependency(0, ac);
}
void
DagCollectiveActor::addDependencyToMap(uint32_t id, Action* ac)
{
  //in case this accidentally got added to initial set
  //make sure it gets removed
  initial_actions_.erase(ac);
  debug_printf(sumi_collective | sumi_collective_init,
   "Rank %s, collective %s adding dependency %u to %s tag=%d",
   rankStr().c_str(), Collective::tostr(type_),
   id, ac->toString().c_str(), tag_);
  pending_comms_.insert(std::make_pair(id, ac));
  ac->join_counter++;
}

void
DagCollectiveActor::addCommDependency(Action* precursor, Action *ac)
{
  int physical_rank =  comm_->commToGlobalRank(ac->partner);

  if (physical_rank == Communicator::unresolved_rank){
    //uh oh - need to wait on this
    uint32_t resolve_id = Action::messageId(Action::resolve, 0, ac->partner);
     comm_->registerRankCallback(this);
    addDependencyToMap(resolve_id, ac);
    if (precursor) addDependencyToMap(precursor->id, ac);
  } else {
    ac->phys_partner = physical_rank;
    if (precursor){
      addDependencyToMap(precursor->id, ac);
    } else if (ac->join_counter == 0){
      debug_printf(sumi_collective | sumi_collective_init,
       "Rank %s, collective %s adding initial %s on tag=%d",
       rankStr().c_str(), Collective::tostr(type_),
       ac->toString().c_str(), tag_);
      initial_actions_.insert(ac);
    } else {
      //no new dependency, but not an initial action
    }
  }
}

void
DagCollectiveActor::rankResolved(int global_rank, int comm_rank)
{
  Action ac(Action::resolve, 0, comm_rank);
  ac.phys_partner = global_rank;
  clearDependencies(&ac);
}

void
DagCollectiveActor::addDependency(Action* precursor, Action *ac)
{
  switch (ac->type){
    case Action::send:
    case Action::recv:
      addCommDependency(precursor, ac);
      break;
    default:
      if (precursor){
        addDependencyToMap(precursor->id, ac);
      } else if (ac->join_counter == 0){
        initial_actions_.insert(ac);
      } else {
        //no new dependency, but not an initial action
      }
    break;
  }
}

DagCollectiveActor::~DagCollectiveActor()
{
  std::list<Action*>::iterator it, end = completed_actions_.end();
  for (it=completed_actions_.begin(); it != end; ++it){
    Action* ac = *it;
    delete ac;
  }
  completed_actions_.clear();
  if (slicer_) delete slicer_;
}

void
DagCollectiveActor::checkCollectiveDone()
{
  debug_printf(sumi_collective,
      "Rank %s has %d active comms, %d pending comms, %d initial comms",
      rankStr().c_str(), active_comms_.size(), pending_comms_.size(), initial_actions_.size());
  if (active_comms_.empty() && pending_comms_.empty() && initial_actions_.empty()){
    finalize();
    putDoneNotification();
  }
}

void
DagCollectiveActor::startSend(Action* ac)
{
  active_comms_[ac->id] = ac;
  reputPending(ac->id, pending_send_headers_);
  doSend(ac);
}

void
DagCollectiveActor::doSend(Action* ac)
{
  protocol_t pr = protocolForAction(ac);
  switch(pr){
    case eager_protocol:
      sendEagerMessage(ac);
      break;
    case get_protocol:
      sendRdmaGetHeader(ac);
      break;
    case put_protocol:
      break; //do nothing for put
  }
}

DagCollectiveActor::protocol_t
DagCollectiveActor::protocolForAction(Action* ac) const
{
  uint64_t byte_length = ac->nelems*type_size_;
  if (engine_->useEagerProtocol(byte_length)){
    return eager_protocol;
  } else if (engine_->useGetProtocol()){
    return get_protocol;
  } else {
    return put_protocol;
  }
}

void
DagCollectiveActor::startRecv(Action* ac)
{
  doRecv(ac);
  reputPending(ac->id, pending_recv_headers_);
}

void
DagCollectiveActor::doRecv(Action* ac)
{
  active_comms_[ac->id] = ac;
  uint64_t byte_length = ac->nelems*type_size_;
  if (engine_->useEagerProtocol(byte_length) || engine_->useGetProtocol()){
    //I need to wait for the sender to contact me
  } else {
    //put protocol, I need to tell the sender where to put it
    sendRdmaPutHeader(ac);
  }
}

void
DagCollectiveActor::deadlockCheck() const
{
  std::cout << sprockit::sprintf("  deadlocked actor %d of %d on tag %d",
    dom_me_, dom_nproc_, tag_) << std::endl;

  for (Action* ac : completed_actions_){
    std::cout << sprockit::sprintf("    Rank %s: completed action %s partner %d round %d",
                      rankStr().c_str(), Action::tostr(ac->type), ac->partner, ac->round) << std::endl;
  }

  for (auto& pair : active_comms_){
    Action* ac = pair.second;
    std::cout << sprockit::sprintf("    Rank %s: active %s",
                    rankStr().c_str(), ac->toString().c_str()) << std::endl;
  }

  for (auto& pair  : pending_comms_){
    uint32_t id = pair.first;
    Action::type_t ty;
    int r, p;
    Action::details(id, ty, r, p);
    auto range = pending_comms_.equal_range(id);
    if (range.first != range.second){
      std::cout << sprockit::sprintf("    Rank %s: waiting on action %s partner %d round %d",
                      rankStr().c_str(), Action::tostr(ty), p, r) << std::endl;
    }

    for (auto rit=range.first; rit != range.second; ++rit){
      Action* ac = rit->second;
      std::cout << sprockit::sprintf("      Rank %s: pending %s partner %d round %d join counter %d",
                    rankStr().c_str(), Action::tostr(ac->type), ac->partner, ac->round, ac->join_counter)
                << std::endl;
    }
  }
}

void
DagCollectiveActor::reputPending(uint32_t id, pending_msg_map& pending)
{
  std::list<CollectiveWorkMessage*> tmp;

  {pending_msg_map::iterator it = pending.find(id);
  while (it != pending.end()){
    CollectiveWorkMessage* msg = it->second;
    tmp.push_back(msg);
    pending.erase(it);
    it = pending.find(id);
  }}

  {std::list<CollectiveWorkMessage*>::iterator it, end = tmp.end();
  for (it=tmp.begin(); it != end; ++it){
    CollectiveWorkMessage* msg = *it;
    recv(msg);
  }}
}

void
DagCollectiveActor::erasePending(uint32_t id, pending_msg_map& pending)
{
  pending_msg_map::iterator it = pending.find(id);
  while (it != pending.end()){
    pending.erase(it);
    it = pending.find(id);
  }
}

Action*
DagCollectiveActor::commActionDone(Action::type_t ty, int round, int partner)
{
  uint32_t id = Action::messageId(ty, round, partner);

  active_map::iterator it = active_comms_.find(id);
  if (it == active_comms_.end()){
    for (it=active_comms_.begin(); it != active_comms_.end(); ++it){
      std::cerr << "Have action id " << it->first
        << " to partner " << it->second->partner << std::endl;
    }
    spkt_abort_printf("Rank %d=%d invalid action %s for round %d, partner %d",
     my_api_->rank(), dom_me_, Action::tostr(ty), round, partner);
  }
  Action* ac = it->second;
  commActionDone(ac);
  return ac;
}

void
DagCollectiveActor::dataSent(CollectiveWorkMessage* msg)
{
  Action* ac = commActionDone(Action::send, msg->round(), msg->domRecver());
  my_api_->logMessageDelay(msg, ac->nelems * type_size_, 1,
                           msg->recvSyncDelay(), my_api_->activeDelay(ac->start));
}

void
DagCollectiveActor::dataRecved(Action* ac_, CollectiveWorkMessage* msg, void *recvd_buffer)
{
  sstmac::TimeDelta sync_delay;
  if (msg->timeStarted() > ac_->start){
    sync_delay = msg->timeStarted() - ac_->start;
  }
  my_api_->logMessageDelay(msg, ac_->nelems * type_size_, 1,
                           sync_delay, my_api_->activeDelay(ac_->start));
  RecvAction* ac = static_cast<RecvAction*>(ac_);
  //we are allowed to have a null buffer
  //this just walks through the communication pattern
  //without actually passing around large payloads or doing memcpy's
  //if we end up here, we have a real buffer
  if (recv_buffer_){
    int my_comm_rank =  comm_->myCommRank();
    int sender_comm_rank = msg->domSender();
    if (my_comm_rank == sender_comm_rank){
      do_sumi_debug_print("ignoring", rankStr().c_str(), msg->domSender(),
        ac->round, 0, nelems_, type_size_, recv_buffer_);
    } else {

      /**
      do_sumi_debug_print("currently",
       rank_str().c_str(), ac->partner,
       ac->round, ac->offset, ac->nelems, type_size_,
       dst_buffer);

      do_sumi_debug_print("receiving",
        rank_str().c_str(), ac->partner,
        ac->round,
        ac->offset, ac->nelems, type_size_,
        recvd_buffer);
        */

      RecvAction::recv_type_t recv_ty = RecvAction::recv_type(
            msg->protocol() == CollectiveWorkMessage::eager, ac->buf_type);

      //printf("%d %d -> %d\n",
      //       msg->payload_type() == message::eager_payload,
      //       ac->buf_type, recv_ty);

      switch(recv_ty){
       //eager and rdvz reduce are the same
       case RecvAction::eager_reduce:
       case RecvAction::rdvz_reduce:
        my_api_->memcopy(ac->nelems*type_size_);
        slicer_->unpackReduce(recvd_buffer, result_buffer_, ac->offset, ac->nelems);
        break;
       case RecvAction::eager_in_place:
       case RecvAction::eager_unpack_temp_buf:
       case RecvAction::rdvz_unpack_temp_buf:
        my_api_->memcopy(ac->nelems*type_size_);
        slicer_->unpackRecvBuf(recvd_buffer, result_buffer_, ac->offset, ac->nelems);
        break;
       case RecvAction::eager_packed_temp_buf:
        //I am copying from packed to packed
        my_api_->memcopy(ac->nelems*type_size_);
        slicer_->memcpyPackedBufs(recv_buffer_, recvd_buffer, ac->nelems);
        break;
       case RecvAction::rdvz_packed_temp_buf:
       case RecvAction::rdvz_in_place:
        //there is nothing to do - the data has already landed in palce
        break;
      }

      /*
      do_sumi_debug_print("now", rank_str().c_str(),
        ac->partner, ac->round,
        0, ac->offset + ac->nelems, type_size_,
        buffer_to_use);
      */
    }
  }
  commActionDone(ac);
}

void
DagCollectiveActor::dataRecved(
  CollectiveWorkMessage* msg,
  void* recvd_buffer)
{
  debug_printf(sumi_collective | sprockit::dbg::sumi,
    "Rank %s collective %s(%p) finished recving for round=%d tag=%d buffer=%p msg=%p",
    rankStr().c_str(), toString().c_str(),
    this, msg->round(), tag_,
    (void*) recv_buffer_, msg);

  uint32_t id = Action::messageId(Action::recv, msg->round(), msg->domSender());
  Action* ac = active_comms_[id];
  if (ac == nullptr){
    spkt_throw_printf(sprockit::ValueError,
      "on %d, received data for unknown receive %u from %d on round %d\n%s",
      dom_me_, id, msg->domSender(), msg->round(),
      msg->toString().c_str());
  }

  dataRecved(ac, msg, recvd_buffer);
}

void*
DagCollectiveActor::getRecvbuffer(Action* ac_)
{
  RecvAction* ac = static_cast<RecvAction*>(ac_);
  void* recv_buf = ac->buf_type != RecvAction::in_place
                ? recv_buffer_ : result_buffer_;
  if (result_buffer_ && recv_buf == nullptr){
    sprockit::abort("working with real payload, but somehow getting a null buffer");
  }
  return sumi::Message::offset_ptr(recv_buf,ac->offset*type_size_);
}

void*
DagCollectiveActor::getSendBuffer(Action* ac_, uint64_t& nbytes)
{
  SendAction* ac = static_cast<SendAction*>(ac_);
  nbytes = ac->nelems * type_size_;
  switch(ac->buf_type){
    case SendAction::in_place:
      if (slicer_->contiguous()){
        return sumi::Message::offset_ptr(result_buffer_, ac->offset*slicer_->elementPackedSize());
      } else {
        nbytes = slicer_->packsendBuf(send_buffer_, result_buffer_,
                               ac->offset, ac->nelems);
        return send_buffer_;
      }
      break;
    case SendAction::temp_send:
      return sumi::Message::offset_ptr(send_buffer_, ac->offset*type_size_);
      break;
    case SendAction::prev_recv:
      return sumi::Message::offset_ptr(recv_buffer_, ac->offset*type_size_);
      break;
  }
}

void
DagCollectiveActor::nextRoundReadyToPut(
  Action* ac, CollectiveWorkMessage* header)
{
  debug_printf(sumi_collective | sprockit::dbg::sumi,
    "Rank %s, collective %s ready to put for round=%d tag=%d from rank %d",
    rankStr().c_str(), toString().c_str(),
    header, header->round(), tag_,
    header->domSender());

  header->reverse();

  uint64_t size; void* buf = getSendBuffer(ac, size);
  my_api_->rdmaPutResponse(header, size, buf, header->partnerBuffer(),
                            cq_id_, cq_id_, engine_->rdmaGetQos());

  debug_printf(sumi_collective | sprockit::dbg::sumi,
    "Rank %s, collective %s(%p) starting put %d elems at offset %d to %d for round=%d tag=%d msg %p",
    rankStr().c_str(), toString().c_str(), this,
    ac->nelems, ac->offset,
    header->domSender(),
    ac->round, tag_, header);
}

void
DagCollectiveActor::nextRoundReadyToGet(
  Action* ac,
  CollectiveWorkMessage* header)
{
  debug_printf(sumi_collective | sprockit::dbg::sumi,
    "Rank %s, collective %s received get header %p for round=%d tag=%d from rank %d",
    rankStr().c_str(), toString().c_str(),
    header, header->round(), tag_, header->domSender());

  if (ac->start > header->timeArrived()){
    header->addRecvSyncDelay(ac->start - header->timeArrived());
  }

  sstmac::TimeDelta sync_delay;
  if (header->timeStarted() > ac->start){
    sync_delay = header->timeStarted() - ac->start;
  }
  my_api_->logMessageDelay(header, ac->nelems * type_size_, 0,
                           sync_delay, my_api_->activeDelay(ac->start));

  my_api_->rdmaGetRequestResponse(header, ac->nelems*type_size_,
                                  getRecvbuffer(ac), header->partnerBuffer(),
                                  cq_id_, cq_id_, engine_->rdmaGetQos());

  debug_printf(sumi_collective | sprockit::dbg::sumi,
      "Rank %s, collective %s(%p) starting get %d elems at offset %d from %d for round=%d tag=%d msg %p",
      rankStr().c_str(), toString().c_str(), this,
      ac->nelems, ac->offset,
      header->domSender(), header->round(), tag_, header);

}

void
DagCollectiveActor::incomingHeader(CollectiveWorkMessage* msg)
{
  switch(msg->protocol()){
    case CollectiveWorkMessage::eager: {
      uint32_t mid = Action::messageId(Action::recv, msg->round(), msg->domSender());
      active_map::iterator it = active_comms_.find(mid);
      if (it == active_comms_.end()){
        debug_printf(sumi_collective,
           "Rank %s not yet ready for recv message from %s on round %d tag %d",
           rankStr().c_str(), rankStr(msg->domSender()).c_str(), msg->round(), msg->tag());
        pending_recv_headers_.insert(std::make_pair(mid, msg));
      } else {
        //data recved will clear the actions
#if SSTMAC_SANITY_CHECK
        if (recv_buffer_ && !msg->smsgBuffer() && msg->byteLength()){
          spkt_abort_printf("Flow %llu: no SMSG buffer: %s", msg->flowId(), msg->toString().c_str());
        }
#endif
        dataRecved(msg, msg->smsgBuffer());
        delete msg;
      }
      break;
    }
    case CollectiveWorkMessage::get: {
      uint32_t mid = Action::messageId(Action::recv, msg->round(), msg->domSender());
      auto it = active_comms_.find(mid);
      if (it == active_comms_.end()){
        debug_printf(sumi_collective,
           "Rank %s not yet ready for recv message from %s on round %d",
           rankStr().c_str(), rankStr(msg->domSender()).c_str(), msg->round());
        pending_recv_headers_.insert(std::make_pair(mid, msg));
      } else {
        Action* ac = it->second;
        nextRoundReadyToGet(ac, msg);
      }
      break;
    }
    case CollectiveWorkMessage::put: {
      uint32_t mid = Action::messageId(Action::send, msg->round(), msg->domSender());
      auto it = active_comms_.find(mid);
      if (it == active_comms_.end()){
        pending_send_headers_.insert(std::make_pair(mid, msg));
      } else {
        Action* ac = it->second;
        nextRoundReadyToPut(ac, msg);
      }
      break;
    }
    default:
      spkt_abort_printf("message %s has bad protocol %d", msg->toString().c_str(), msg->protocol());
  }
}

CollectiveDoneMessage*
DagCollectiveActor::doneMsg() const
{
  auto msg = new CollectiveDoneMessage(tag_, type_, comm_, cq_id_);
  msg->set_comm_rank(comm_->myCommRank());
  msg->set_result(result_buffer_);
#ifdef FEATURE_TAG_SUMI_RESILIENCE
  auto end = failed_ranks_.start_iteration();
  for (auto it = failed_ranks_.begin(); it != end; ++it){
    msg->append_failed(global_rank(*it));
  }
  failed_ranks_.end_iteration();
#endif
  return msg;
}

void
DagCollectiveActor::putDoneNotification()
{
  if (complete_){
    return; //no longer treat as error
    //beause of self messages, you could end up calling this twice
  }
  complete_ = true;

  debug_printf(sumi_collective,
    "Rank %s putting done notification on tag=%d ",
    rankStr().c_str(), tag_);

  finalizeBuffers();
  engine_->notifyCollectiveDone(dom_me_, type_, tag_);
}

void
DagCollectiveActor::recv(CollectiveWorkMessage* msg)
{
  debug_printf(sumi_collective | sprockit::dbg::sumi,
    "Rank %s on incoming message with protocol %s type %s from %d on round=%d tag=%d ",
    rankStr().c_str(),
    CollectiveWorkMessage::tostr(msg->protocol()),
    msg->sstmac::hw::NetworkMessage::typeStr(),
    msg->domSender(), msg->round(), tag_);

  auto ty = msg->sstmac::hw::NetworkMessage::type();
  switch (ty)
  {
    case Message::rdma_get_payload:
      //the recv buffer was the "local" buffer in the RDMA get
      //I got it from someone locally
      dataRecved(msg, msg->localBuffer());
      delete msg;
      break;
    case Message::rdma_put_payload:
      //the recv buffer the "remote" buffer in the RDMA put
      //some put into me remotely
      dataRecved(msg, msg->remoteBuffer());
      delete msg;
      break;
    case Message::payload_sent_ack:
    case Message::rdma_get_sent_ack:
    case Message::rdma_put_sent_ack:
      dataSent(msg);
      delete msg;
      break;
    case Message::rdma_get_nack:
      //partner is the sender - I tried and RDMA get but they were dead
      spkt_abort_printf("do not currently support NACK");
      break;
    case Message::payload:
      incomingHeader(msg);
      break;
    default:
      spkt_abort_printf("virtual_dag_collective_actor::recv: invalid message type %s",
        sstmac::hw::NetworkMessage::tostr(ty));
  }
}

void
RecursiveDoubling::computeTree(int nproc, int &log2nproc, int &midpoint, int &pow2nproc)
{
  pow2nproc = 1;
  log2nproc = 0;
  while (pow2nproc < nproc)
  {
    ++log2nproc;
    pow2nproc *= 2;
  }
  midpoint = pow2nproc / 2;
}

int
VirtualRankMap::realToVirtual(int rank, int* ret) const
{
  int num_actors_two_roles = virtual_nproc_ - nproc_;
  if (rank >= num_actors_two_roles){
    ret[0] = num_actors_two_roles + rank;
    return 1;
  } else {
    ret[0] = 2*rank;
    ret[1] = ret[0] + 1;
    return 2;
  }
}

int
VirtualRankMap::virtualToReal(int virtual_rank) const
{
  //these are the guys who have to do two roles because
  //we don't have an exact power of two
  int num_actors_two_roles = virtual_nproc_ - nproc_;
  if (virtual_rank >= 2*num_actors_two_roles){
    int real_rank = virtual_rank - num_actors_two_roles;
    return real_rank;
  } else {
    int real_rank = virtual_rank / 2;
    return real_rank;
  }
}


void*
DagCollectiveActor::messageBuffer(void* buffer, int offset)
{
  if (isNonNull(buffer)){
    int total_stride = type_size_ * offset;
    char* tmp = ((char*)buffer) + total_stride;
    return tmp;
  }
  //nope, no buffer
  return 0;
}

#ifdef FEATURE_TAG_SUMI_RESILIENCE
void
dag_collective_actor::dense_partner_ping_failed(int dense_rank)
{
  debug_printf(sumi_collective,
     "Rank %s on collective %s - partner %s returned failed ping",
      rank_str().c_str(), collective::tostr(type_),
      rank_str(dense_rank).c_str());
  failed_ranks_.insert(dense_rank);
  fail_actions(dense_rank);
}

void
collective_actor::validate_pings_cleared()
{
  int size = ping_refcounts_.size();
  if (size){
    spkt_throw_printf(sprockit::illformed_error,
        "dag_collective_actor::rank %d still has %d outstanding pings\n",
        my_api_->rank(), ping_refcounts_.size());
  }
}

void
collective_actor::partner_ping_failed(int global_rank)
{
  //map this to virtual rank
  int comm_rank =  cfg_.dom->global_to_comm_rank(global_rank);
  int dense_rank = rank_map_.dense_rank(comm_rank);
  dense_partner_ping_failed(dense_rank);
}

void
collective_actor::cancel_ping(int dense_rank)
{
  if (!fault_aware_)
    return;

  int cm_rank = comm_rank(dense_rank);
  if (cm_rank ==  cfg_.dom->my_comm_rank())  //no need to ping self
    return;

  std::map<int,int>::iterator it = ping_refcounts_.find(cm_rank);
  if (it == ping_refcounts_.end())
    spkt_throw_printf(sprockit::illformed_error,
        "dag_collective_actor trying to cancel non-existent ping");

  int& refcount = it->second;
  --refcount;
  if (refcount == 0){
    int global_phys_rank =  cfg_.dom->comm_to_global_rank(cm_rank);
    debug_printf(sumi_collective | sumi_ping,
      "Rank %s collective %s(%p) erase ping for partner %d:%d:%d on tag=%d ",
      rank_str().c_str(), toString().c_str(), this,
      dense_rank, cm_rank, global_phys_rank, tag_);
      ping_refcounts_.erase(it);
      stop_check_neighbor(global_phys_rank);
  } else {
    debug_printf(sumi_collective | sumi_ping,
        "Rank %s collective %s(%p) decrement ping refcount to %d for partner %d:%d on tag=%d ",
    rank_str().c_str(), toString().c_str(), this,
    refcount,
    dense_rank, cm_rank, tag_);
  }
}

bool
collective_actor::ping_rank(int comm_rank, int dense_rank)
{
  int& refcount = ping_refcounts_[comm_rank];
  if (refcount != 0){
     debug_printf(sumi_collective | sumi_ping,
         "Rank %s collective %s(%p) already pinging %d with refcount=%d on tag=%d ",
         rank_str().c_str(), toString().c_str(), this,
         comm_rank, refcount, tag_);
    //we be pinging in the rain, just pinging in the rain
    ++refcount;
    return false; //all is well, we think - we have a pending ping
  } else {
    int global_phys_rank =  cfg_.dom->comm_to_global_rank(comm_rank);
    debug_printf(sumi_collective | sumi_ping,
      "Rank %s collective %s(%p) begin pinging %d:%d on tag=%d ",
      rank_str().c_str(), toString().c_str(), this,
      comm_rank, global_phys_rank, tag_);

    //we don't know anything - do a more extensive check
    bool is_dead = check_neighbor(global_phys_rank);
    if (is_dead){
      debug_printf(sumi_collective | sumi_ping,
        "Rank %s collective %s(%p) sees that %d:%d is apparently dead on tag=%d ",
        rank_str().c_str(), toString().c_str(), this,
        comm_rank, global_phys_rank, tag_);
      failed_ranks_.insert(dense_rank);
      ping_refcounts_.erase(comm_rank);
      return true;
    } else {
      debug_printf(sumi_collective | sumi_ping,
        "Rank %s collective %s(%p) has started new ping to %s on tag=%d ",
        rank_str().c_str(), toString().c_str(), this,
        rank_str(dense_rank).c_str(),
        tag_);
      ++refcount;
      return false; //nope, all good
    }
  }
}

bool
collective_actor::do_ping_neighbor(int dense_rank)
{
  int cm_rank = comm_rank(dense_rank);
  if (cm_rank ==  cfg_.dom->my_comm_rank()){
    //no reason to ping self
    return false;
  }
  return ping_rank(cm_rank, dense_rank);
}

bool
collective_actor::ping_neighbor(int dense_rank)
{
  if (!fault_aware_){
    return false; //not failed
  } else if (is_failed(dense_rank)){
    //this guy is failed - no reason to communicate
    return true;
  } else {
    return do_ping_neighbor(dense_rank);
  }
}

std::string
collective_actor::failed_proc_string() const
{
  std::stringstream sstr;
  sstr << "{";
  auto end = failed_ranks_.start_iteration();
  for (auto it = failed_ranks_.begin(); it != end; ++it){
    sstr << " " << *it;
  }
  failed_ranks_.end_iteration();
  sstr << " }";
  return sstr.str();
}

bool
collective_actor::check_neighbor(int global_phys_rank)
{
  return my_api_->start_watching(global_phys_rank, timeout_);
}

void
collective_actor::stop_check_neighbor(int global_phys_rank)
{
  my_api_->stop_watching(global_phys_rank, timeout_);
}
#endif

}
