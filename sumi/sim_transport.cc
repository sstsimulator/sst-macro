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

#include <cstring>
#include <sumi/transport.h>
#include <sumi/allreduce.h>
#include <sumi/reduce_scatter.h>
#include <sumi/reduce.h>
#include <sumi/allgather.h>
#include <sumi/allgatherv.h>
#include <sumi/alltoall.h>
#include <sumi/alltoallv.h>
#include <sumi/communicator.h>
#include <sumi/bcast.h>
#include <sumi/gather.h>
#include <sumi/scatter.h>
#include <sumi/gatherv.h>
#include <sumi/scatterv.h>
#include <sumi/scan.h>
#include <sprockit/stl_string.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/software/api/api.h>

RegisterKeywords(
{ "lazy_watch", "whether failure notifications can be receive without active pinging" },
{ "eager_cutoff", "what message size in bytes to switch from eager to rendezvous" },
{ "use_put_protocol", "whether to use a put or get protocol for pt2pt sends" },
{ "algorithm", "the specific algorithm to use for a given collecitve" },
{ "comm_sync_stats", "whether to track synchronization stats for communication" },
{ "smp_single_copy_size", "the minimum size of message for single-copy protocol" },
{ "max_eager_msg_size", "the maximum size for using eager pt2pt protocol" },
{ "max_vshort_msg_size", "the maximum size for mailbox protocol" },
{ "post_rdma_delay", "the time it takes to post an RDMA operation" },
{ "post_header_delay", "the time it takes to send an eager message" },
{ "poll_delay", "the time it takes to poll for an incoming message" },
{ "rdma_pin_latency", "the latency for each RDMA pin information" },
{ "rdma_page_delay", "the per-page delay for RDMA pinning" },
);

#include <sstmac/common/sstmac_config.h>
#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/event.h>
#endif
#include <sumi/sim_transport.h>
#include <sumi/message.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/runtime.h>
//#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/output.h>

using namespace sprockit::dbg;
using sstmac::TimeDelta;

RegisterDebugSlot(sumi);

namespace sumi {

const int options::initial_context = -2;

class SumiServer :
  public sstmac::sw::Service
{

 public:
  SumiServer(SimTransport* tport)
    : Service(tport->serverLibname(),
       sstmac::sw::SoftwareId(-1, -1), //belongs to no application
       tport->parent()->os())
  {
  }

  void registerProc(int rank, SimTransport* proc){
    int app_id = proc->sid().app_;
    debug_printf(sprockit::dbg::sumi,
                 "SumiServer registering rank %d for app %d",
                 rank, app_id);
    SimTransport*& slot = procs_[app_id][rank];
    if (slot){
      spkt_abort_printf("SumiServer: already registered rank %d for app %d on node %d",
                        rank, app_id, os_->addr());
    }
    slot = proc;

    auto iter = pending_.begin();
    auto end = pending_.end();
    while (iter != end){
      auto tmp = iter++;
      Message* msg = *tmp;
      if (msg->targetRank() == rank && msg->aid() == proc->sid().app_){
        pending_.erase(tmp);
        proc->incomingMessage(msg);
      }
    }
  }

  bool unregisterProc(int rank, Transport* proc){
    int app_id = proc->sid().app_;
    auto iter = procs_.find(app_id);
    auto& subMap = iter->second;
    subMap.erase(rank);
    if (subMap.empty()){
      procs_.erase(iter);
    }
    return procs_.empty();
  }

  void incomingEvent(sstmac::Event* ev) override {
    sstmac::sw::Service::incomingEvent(ev);
  }

  void incomingRequest(sstmac::Request *req) override {
    Message* smsg = safe_cast(Message, req);
    debug_printf(sprockit::dbg::sumi,
                 "SumiServer %d: incoming %s",
                 os_->addr(), smsg->toString().c_str());
    SimTransport* tport = procs_[smsg->aid()][smsg->targetRank()];
    if (!tport){
      debug_printf(sprockit::dbg::sumi,
                  "SumiServer %d: message pending to app %d, target %d",
                  os_->addr(), smsg->aid(), smsg->targetRank());
      pending_.push_back(smsg);
    } else {
      tport->incomingMessage(smsg);
    }
  }

  const std::map<int,SimTransport*>& getProcs(int aid) const {
    auto iter = procs_.find(aid);
    if (iter == procs_.end()){
      spkt_abort_printf("SumiServer got bad app id %d", aid);
    }
    return iter->second;
  }

 private:
  std::map<int, std::map<int, SimTransport*>> procs_;
  std::list<Message*> pending_;

};


Transport* Transport::get()
{
  return sstmac::sw::OperatingSystem::currentThread()->getApi<sumi::SimTransport>("sumi");
}

Transport::~Transport()
{
  if (engine_) delete engine_;
}

SimTransport::SimTransport(SST::Params& params, sstmac::sw::App* parent, SST::Component* comp) :
  //the name of the transport itself should be mapped to a unique name
  API(params, parent, comp),
  Transport("sumi", parent->sid(), parent->os()->addr()),
  //the server is what takes on the specified libname
  spy_bytes_(nullptr),
  completion_queues_(1),
  default_progress_queue_(parent->os()),
  nic_ioctl_(parent->os()->nicDataIoctl()),
  parent_app_(parent),
  qos_analysis_(nullptr)
{
  completion_queues_[0] = std::bind(&DefaultProgressQueue::incoming,
                                    &default_progress_queue_, 0, std::placeholders::_1);
  null_completion_notify_ = std::bind(&SimTransport::drop, this, std::placeholders::_1);
  rank_ = sid().task_;
  auto* server_lib = parent_->os()->lib(server_libname_);
  SumiServer* server;
  // only do one server per app per node
  if (server_lib == nullptr) {
    server = new SumiServer(this);
    server->start();
  } else {
    server = safe_cast(SumiServer, server_lib);
  }

  post_rdma_delay_ = TimeDelta(params.find<SST::UnitAlgebra>("post_rdma_delay", "0s").getValue().toDouble());
  post_header_delay_ = TimeDelta(params.find<SST::UnitAlgebra>("post_header_delay", "0s").getValue().toDouble());
  poll_delay_ = TimeDelta(params.find<SST::UnitAlgebra>("poll_delay", "0s").getValue().toDouble());

  rdma_pin_latency_ = TimeDelta(params.find<SST::UnitAlgebra>("rdma_pin_latency", "0s").getValue().toDouble());
  rdma_page_delay_ = TimeDelta(params.find<SST::UnitAlgebra>("rdma_page_delay", "0s").getValue().toDouble());
  pin_delay_ = rdma_pin_latency_.ticks() || rdma_page_delay_.ticks();
  page_size_ = params.find<SST::UnitAlgebra>("rdma_page_size", "4096").getRoundedValue();

  rank_mapper_ = sstmac::sw::TaskMapping::globalMapping(sid().app_);
  nproc_ = rank_mapper_->nproc();

  auto qos_params = params.find_scoped_params("qos");
  auto qos_name = qos_params.find<std::string>("name", "null");
  qos_analysis_ = sprockit::create<QoSAnalysis>("macro", qos_name, qos_params);

  server->registerProc(rank_, this);

#if !SSTMAC_INTEGRATED_SST_CORE
  std::string subname = sprockit::printf("app%d.rank%d", parent->aid(), parent->tid());
  auto* spy = comp->registerMultiStatistic<int,int,uint64_t>(params, "spy_bytes", subname);
  spy_bytes_ = dynamic_cast<sstmac::StatSpyplot<int,int,uint64_t>*>(spy);
#endif

  if (!engine_) engine_ = new CollectiveEngine(params, this);

  smp_optimize_ = params.find<bool>("smp_optimize", false);
}

void
SimTransport::allocateCq(int id, std::function<void(Message*)>&& f)
{
  completion_queues_[id] = std::move(f);
  auto iter = held_.find(id);
  if (iter != held_.end()){
    auto& list = iter->second;
    for (Message* m : list){
      f(m);
    }
    held_.erase(iter);
  }
}

void
SimTransport::init()
{
  if (smp_optimize_){
    engine_->barrier(-1, Message::default_cq);
    engine_->blockUntilNext(Message::default_cq);

    SumiServer* server = safe_cast(SumiServer, parent_->os()->lib(server_libname_));
    auto& map = server->getProcs(sid().app_);
    if (map.size() > 1){ //enable smp optimizations
      for (auto& pair : map){
        smp_neighbors_.insert(pair.first);
      }
    }
  }
}

void
SimTransport::finish()
{
  //this should really loop through and kill off all the pings
  //so none of them execute
}

SimTransport::~SimTransport()
{
  SumiServer* server = safe_cast(SumiServer, parent_->os()->lib(server_libname_));
  bool del = server->unregisterProc(rank_, this);
  if (del) delete server;

  //if (engine_) delete engine_;

  //if (spy_bytes_) delete spy_bytes_;
  //if (spy_num_messages_) delete spy_num_messages_;
}

void
SimTransport::pinRdma(uint64_t bytes)
{
  int num_pages = bytes / page_size_;
  if (bytes % page_size_) ++num_pages;
  sstmac::TimeDelta pin_delay = rdma_pin_latency_ + num_pages*rdma_page_delay_;
  compute(pin_delay);
}

void
SimTransport::memcopy(uint64_t bytes)
{
  parent_->computeBlockMemcpy(bytes);
}

void
SimTransport::incomingEvent(sstmac::Event *ev)
{
  spkt_abort_printf("sumi_transport::incoming_event: should not directly handle events");
}

int*
SimTransport::nidlist() const
{
  //just cast an int* - it's fine
  //the types are the same size and the bits can be
  //interpreted correctly
  return (int*) rank_mapper_->rankToNode().data();
}

void
SimTransport::compute(sstmac::TimeDelta t)
{
  parent_->compute(t);
}


void
SimTransport::send(Message* m)
{
  int qos = qos_analysis_->selectQoS(m);
  m->setQoS(qos);
  m->setTimeSent(parent_app_->now());

  if (spy_bytes_){
    switch(m->sstmac::hw::NetworkMessage::type()){
    case sstmac::hw::NetworkMessage::payload:
      spy_bytes_->addData(m->sender(), m->recver(), m->byteLength());
      break;
    case sstmac::hw::NetworkMessage::rdma_get_request:
    case sstmac::hw::NetworkMessage::rdma_put_payload:
      spy_bytes_->addData(m->sender(), m->recver(), m->payloadBytes());
      break;
    default:
      break;
    }
  }

  switch(m->sstmac::hw::NetworkMessage::type()){
    case sstmac::hw::NetworkMessage::payload:
      if (m->recver() == rank_){
        //deliver to self
        debug_printf(sprockit::dbg::sumi,
          "Rank %d SUMI sending self message", rank_);
        if (m->needsRecvAck()){
          completion_queues_[m->recvCQ()](m);
        }
        if (m->needsSendAck()){
          auto* ack = m->cloneInjectionAck();
          completion_queues_[m->sendCQ()](static_cast<Message*>(ack));
        }
      } else {
        if (post_header_delay_.ticks()) {
          parent_->compute(post_header_delay_);
        }
        nic_ioctl_(m);
      }
      break;
    case sstmac::hw::NetworkMessage::rdma_get_request:
    case sstmac::hw::NetworkMessage::rdma_put_payload:
      if (post_rdma_delay_.ticks()) {
        parent_->compute(post_rdma_delay_);
      }
      nic_ioctl_(m);
      break;
    default:
      spkt_abort_printf("attempting to initiate send with invalid type %d",
                        m->type())
  }
}

void
SimTransport::smsgSendResponse(Message* m, uint64_t size, void* buffer, int local_cq, int remote_cq)
{
  //reverse both hardware and software info
  m->sstmac::hw::NetworkMessage::reverse();
  m->reverse();
  m->setupSmsg(buffer, size);
  m->setSendCq(local_cq);
  m->setRecvCQ(remote_cq);
  m->sstmac::hw::NetworkMessage::setType(Message::payload);
  send(m);
}

void
SimTransport::rdmaGetRequestResponse(Message* m, uint64_t size,
                                     void* local_buffer, void* remote_buffer,
                                     int local_cq, int remote_cq)
{
  //do not reverse send/recver - this is hardware reverse, not software reverse
  m->sstmac::hw::NetworkMessage::reverse();
  m->setupRdmaGet(local_buffer, remote_buffer, size);
  m->setSendCq(remote_cq);
  m->setRecvCQ(local_cq);
  m->sstmac::hw::NetworkMessage::setType(Message::rdma_get_request);
  send(m);
}

void
SimTransport::rdmaGetResponse(Message* m, uint64_t size, int local_cq, int remote_cq)
{
  smsgSendResponse(m, size, nullptr, local_cq, remote_cq);
}

void
SimTransport::rdmaPutResponse(Message* m, uint64_t payload_bytes,
                 void* loc_buffer, void* remote_buffer, int local_cq, int remote_cq)
{
  m->reverse();
  m->sstmac::hw::NetworkMessage::reverse();
  m->setupRdmaPut(loc_buffer, remote_buffer, payload_bytes);
  m->setSendCq(local_cq);
  m->setRecvCQ(remote_cq);
  m->sstmac::hw::NetworkMessage::setType(Message::rdma_put_payload);
  send(m);
}

uint64_t
SimTransport::allocateFlowId()
{
  return parent_->os()->node()->allocateUniqueId();
}

void
SimTransport::incomingMessage(Message *msg)
{
#if SSTMAC_COMM_DELAY_STATS
  if (msg){
    msg->setTimeArrived(parent_app_->now());
  }
#endif
  msg->writeSyncValue();
  int cq = msg->isNicAck() ? msg->sendCQ() : msg->recvCQ();
  if (cq != Message::no_ack){
    if (cq >= completion_queues_.size()){
      debug_printf(sprockit::dbg::sumi, "No CQ yet for %s", msg->toString().c_str());
      held_[cq].push_back(msg);
    } else {
      debug_printf(sprockit::dbg::sumi, "CQ %d handle %s", cq, msg->toString().c_str());
      completion_queues_[cq](msg);
    }
  } else {
    debug_printf(sprockit::dbg::sumi, "Dropping message without CQ: %s", msg->toString().c_str());
    null_completion_notify_(msg);
    delete msg;
  }
}

sstmac::Timestamp
SimTransport::now() const
{
  return parent_app_->now();
}

CollectiveEngine::CollectiveEngine(SST::Params& params, Transport *tport) :
  system_collective_tag_(-1), //negative tags reserved for special system work
  eager_cutoff_(512),
  use_put_protocol_(false),
  global_domain_(nullptr),
  tport_(tport)
{
  global_domain_ = new GlobalCommunicator(tport);
  eager_cutoff_ = params.find<int>("eager_cutoff", 512);
  use_put_protocol_ = params.find<bool>("use_put_protocol", false);
  alltoall_type_ = params.find<std::string>("alltoall", "bruck");
  allgather_type_ = params.find<std::string>("allgather", "bruck");
}

CollectiveEngine::~CollectiveEngine()
{
  if (global_domain_) delete global_domain_;
}

void
CollectiveEngine::notifyCollectiveDone(int rank, Collective::type_t ty, int tag)
{
  Collective* coll = collectives_[ty][tag];
  if (!coll){
    spkt_throw_printf(sprockit::ValueError,
      "transport::notify_collective_done: invalid collective of type %s, tag %d",
       Collective::tostr(ty), tag);
  }
  finishCollective(coll, rank, ty, tag);
}

void
CollectiveEngine::initSmp(const std::set<int>& neighbors)
{
}

void
CollectiveEngine::deadlockCheck()
{
  collective_map::iterator it, end = collectives_.end();
  for (it=collectives_.begin(); it != end; ++it){
    tag_to_collective_map& next = it->second;
    tag_to_collective_map::iterator cit, cend = next.end();
    for (cit=next.begin(); cit != cend; ++cit){
      Collective* coll = cit->second;
      if (!coll->complete()){
        coll->deadlockCheck();
      }
    }
  }
}

CollectiveDoneMessage*
CollectiveEngine::skipCollective(Collective::type_t ty,
  int cq_id, Communicator* comm,
  void* dst, void *src,
  int nelems, int type_size,
  int tag)
{
  if (!comm) comm = global_domain_;
  if (comm->nproc() == 1){
    if (dst && src && (dst != src)){
      ::memcpy(dst, src, nelems*type_size);
    }
    return new CollectiveDoneMessage(tag, ty, comm, cq_id);
  } else {
    return nullptr;
  }
}

CollectiveDoneMessage*
CollectiveEngine::allreduce(void* dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
                            int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::allreduce, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;

  Collective* coll = nullptr;
  if (comm->smpComm()){
    //tags are restricted to 28 bits - the front 4 bits are mine for various internal operations
    int intra_reduce_tag = 1<<28 | tag;
    auto* intra_reduce = new WilkeHalvingAllreduce(this, dst, src, nelems,
                                   type_size, intra_reduce_tag, fxn, cq_id, comm->smpComm());

    int root = comm->smpComm()->commToGlobalRank(0);
    Collective* prev;
    if (comm->myCommRank() == root){
      if (!comm->ownerComm()){
        spkt_abort_printf("Bad owner comm configuration - rank 0 in SMP comm should 'own' node");
      }
      //I am the owner!
      int inter_reduce_tag = 2<<28 | tag;
      auto* inter_reduce = new WilkeHalvingAllreduce(this, dst, dst, nelems,
                                     type_size, inter_reduce_tag, fxn, cq_id, comm->ownerComm());


      intra_reduce->setSubsequent(inter_reduce);
      prev = inter_reduce;
    } else {
      prev = intra_reduce;
    }
    auto* intra_bcast = new BinaryTreeBcastCollective(this, root, dst, nelems, type_size, tag,
                                                      cq_id, comm->smpComm());
    prev->setSubsequent(intra_bcast);
    //this should report back as done on the original communicator!
    coll = new DoNothingCollective(this, tag, cq_id, comm);
    intra_bcast->setSubsequent(coll);
  } else {
    coll = new WilkeHalvingAllreduce(this, dst, src, nelems, type_size, tag, fxn, cq_id, comm);
  }

  return startCollective(coll);
}

sumi::CollectiveDoneMessage*
CollectiveEngine::reduceScatter(void* dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
                                  int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::reduce_scatter, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  DagCollective* coll = new HalvingReduceScatter(this, dst, src, nelems, type_size, tag, fxn, cq_id, comm);
  return startCollective(coll);
}

sumi::CollectiveDoneMessage*
CollectiveEngine::scan(void* dst, void* src, int nelems, int type_size, int tag, reduce_fxn fxn,
                        int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::scan, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  DagCollective* coll = new SimultaneousBtreeScan(this, dst, src, nelems, type_size, tag, fxn, cq_id, comm);
  return startCollective(coll);
}


CollectiveDoneMessage*
CollectiveEngine::reduce(int root, void* dst, void *src, int nelems, int type_size, int tag, reduce_fxn fxn,
                          int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::reduce, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  DagCollective* coll = new WilkeHalvingReduce(this, root, dst, src, nelems, type_size, tag, fxn, cq_id, comm);
  return startCollective(coll);
}

CollectiveDoneMessage*
CollectiveEngine::bcast(int root, void *buf, int nelems, int type_size, int tag,
                         int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::bcast, cq_id, comm, buf, buf, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  DagCollective* coll = new BinaryTreeBcastCollective(this, root, buf, nelems, type_size, tag, cq_id, comm);
  return startCollective(coll);
}

CollectiveDoneMessage*
CollectiveEngine::gatherv(int root, void *dst, void *src,
                   int sendcnt, int *recv_counts,
                   int type_size, int tag, int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::gatherv, cq_id, comm, dst, src, sendcnt, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  DagCollective* coll = new BtreeGatherv(this, root, dst, src, sendcnt, recv_counts, type_size, tag, cq_id, comm);
  sprockit::abort("gatherv");
  return startCollective(coll);
}

CollectiveDoneMessage*
CollectiveEngine::gather(int root, void *dst, void *src, int nelems, int type_size, int tag,
                          int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::gather, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  DagCollective* coll = new BtreeGather(this, root, dst, src, nelems, type_size, tag, cq_id, comm);
  return startCollective(coll);
}

CollectiveDoneMessage*
CollectiveEngine::scatter(int root, void *dst, void *src, int nelems, int type_size, int tag,
                           int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::scatter, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  DagCollective* coll = new BtreeScatter(this, root, dst, src, nelems, type_size, tag, cq_id, comm);
  return startCollective(coll);
}

CollectiveDoneMessage*
CollectiveEngine::scatterv(int root, void *dst, void *src, int* send_counts, int recvcnt, int type_size, int tag,
                            int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::scatterv, cq_id, comm, dst, src, recvcnt, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  DagCollective* coll = new BtreeScatterv(this, root, dst, src, send_counts, recvcnt, type_size, tag, cq_id, comm);
  sprockit::abort("scatterv");
  return startCollective(coll);
}

CollectiveDoneMessage*
CollectiveEngine::alltoall(void *dst, void *src, int nelems, int type_size, int tag,
                            int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::alltoall, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;

  auto* fact = AllToAllCollective::getBuilderLibrary("macro");
  auto* builder = fact->getBuilder(alltoall_type_);
  if (!builder){
    spkt_abort_printf("invalid alltoall type requested: %s", allgather_type_.c_str());
  }

  if (comm->smpComm() && comm->smpBalanced()){
    int smpSize = comm->smpComm()->nproc();
    void* intraDst = dst ? new char[nelems*type_size*smpSize] : nullptr;
    int intra_tag = 1<<28 | tag;

    BtreeGather* intra = new BtreeGather(this, 0, intraDst, src, smpSize*nelems,
                                         type_size, intra_tag, cq_id, comm->smpComm());
    DagCollective* prev;
    if (comm->ownerComm()){
      int inter_tag = 2<<28 | tag;
      AllToAllCollective* inter = builder->create(this, dst, intraDst, smpSize*nelems,
                                                  type_size, inter_tag, cq_id, comm->ownerComm());
      intra->setSubsequent(inter);
      prev = inter;
    } else {
      prev = intra;
    }
    int bcast_tag = 3<<28 | tag;
    auto* bcast = new BinaryTreeBcastCollective(this, 0, dst, comm->nproc()*nelems,
                                                type_size, bcast_tag, cq_id, comm->smpComm());
    prev->setSubsequent(bcast);
    auto* final = new DoNothingCollective(this, tag, cq_id, comm);
    bcast->setSubsequent(final);
    return startCollective(intra);
  } else {
    AllToAllCollective* coll = builder->create(this, dst, src, nelems, type_size, tag, cq_id, comm);
    return startCollective(coll);
  }
}

CollectiveDoneMessage*
CollectiveEngine::alltoallv(void *dst, void *src, int* send_counts, int* recv_counts, int type_size, int tag,
                             int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::alltoallv, cq_id, comm, dst, src, send_counts[0], type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  DagCollective* coll = new DirectAlltoallvCollective(this, dst, src, send_counts, recv_counts, type_size, tag, cq_id, comm);
  return startCollective(coll);
}

CollectiveDoneMessage*
CollectiveEngine::allgather(void *dst, void *src, int nelems, int type_size, int tag,
                             int cq_id, Communicator* comm)
{
 auto* msg = skipCollective(Collective::allgather, cq_id, comm, dst, src, nelems, type_size, tag);
 if (msg) return msg;

  if (!comm) comm = global_domain_;

  auto* fact = AllgatherCollective::getBuilderLibrary("macro");
  if (!fact){
    spkt_abort_printf("No allgather collective algorithms registered!");
  }

  auto* builder = fact->getBuilder(allgather_type_);
  if (!builder){
    spkt_abort_printf("invalid allgather type requested: %s", allgather_type_.c_str());
  }

  if (comm->smpComm() && comm->smpBalanced()){
    int smpSize = comm->smpComm()->nproc();
    void* intraDst = dst ? new char[nelems*type_size*smpSize] : nullptr;

    int intra_tag = 1<<28 | tag;



    AllgatherCollective* intra = builder->create(this, intraDst, src, nelems,
                                                 type_size, intra_tag, cq_id, comm->smpComm());

    DagCollective* prev;
    if (comm->ownerComm()){
      int inter_tag = 2<<28 | tag;

      AllgatherCollective* inter = builder->create(this, dst, intraDst, smpSize*nelems, type_size,
                                                   inter_tag, cq_id, comm->ownerComm());
      intra->setSubsequent(inter);
      prev = inter;
    } else {
      prev = intra;
    }
    int bcast_tag = 3<<28 | tag;
    auto* bcast = new BinaryTreeBcastCollective(this, 0, dst, comm->nproc()*nelems,
                                                type_size, bcast_tag, cq_id, comm->smpComm());
    prev->setSubsequent(bcast);
    auto* final = new DoNothingCollective(this, tag, cq_id, comm);
    bcast->setSubsequent(final);
    return startCollective(intra);
  } else {
    AllgatherCollective* coll = builder->create(this, dst, src, nelems, type_size, tag, cq_id, comm);
    return startCollective(coll);
  }
}

CollectiveDoneMessage*
CollectiveEngine::allgatherv(void *dst, void *src, int* recv_counts, int type_size, int tag,
                              int cq_id, Communicator* comm)
{
  //if the allgatherv is skipped, we have a single recv count
  int nelems = *recv_counts;
  auto* msg = skipCollective(Collective::allgatherv, cq_id, comm, dst, src, nelems, type_size, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  DagCollective* coll = new BruckAllgathervCollective(this, dst, src, recv_counts, type_size, tag, cq_id, comm);
  return startCollective(coll);
}

CollectiveDoneMessage*
CollectiveEngine::barrier(int tag, int cq_id, Communicator* comm)
{
  auto* msg = skipCollective(Collective::barrier, cq_id, comm, 0, 0, 0, 0, tag);
  if (msg) return msg;

  if (!comm) comm = global_domain_;
  DagCollective* coll = new BruckBarrierCollective(this, nullptr, nullptr, tag, cq_id, comm);
  return startCollective(coll);
}

CollectiveDoneMessage*
CollectiveEngine::deliverPending(Collective* coll, int tag, Collective::type_t ty)
{
  std::list<CollectiveWorkMessage*> pending = pending_collective_msgs_[ty][tag];
  pending_collective_msgs_[ty].erase(tag);
  CollectiveDoneMessage* dmsg = nullptr;
  for (auto* msg : pending){
    dmsg = coll->recv(msg);
  }
  return dmsg;
}

void
CollectiveEngine::validateCollective(Collective::type_t ty, int tag)
{
  tag_to_collective_map::iterator it = collectives_[ty].find(tag);
  if (it == collectives_[ty].end()){
    return; // all good
  }

  Collective* coll = it->second;
  if (!coll){
   spkt_throw_printf(sprockit::IllformedError,
    "sumi_api::validate_collective: lingering null collective of type %s with tag %d",
    Collective::tostr(ty), tag);
  }

  if (coll->persistent() && coll->complete()){
    return; // all good
  }

  spkt_throw_printf(sprockit::IllformedError,
    "sumi_api::validate_collective: cannot overwrite collective of type %s with tag %d",
    Collective::tostr(ty), tag);
}

CollectiveDoneMessage*
CollectiveEngine::startCollective(Collective* coll)
{
  if (coll->type() == Collective::donothing){
    todel_.push_back(coll);
    return new CollectiveDoneMessage(coll->tag(), coll->type(), coll->comm(), coll->cqId());
  }

  coll->initActors();
  int tag = coll->tag();
  Collective::type_t ty = coll->type();
  Collective*& map_entry = collectives_[ty][tag];
  Collective* active = nullptr;
  CollectiveDoneMessage* dmsg=nullptr;
  if (map_entry){
    active = map_entry;
    coll->start();
    dmsg = active->addActors(coll);
    delete coll;
  } else {
    map_entry = active = coll;
    coll->start();
    dmsg = deliverPending(coll, tag, ty);
  }

  while (dmsg && active->hasSubsequent()){
    delete dmsg;
    active = active->popSubsequent();
    dmsg = startCollective(active);
  }

  return dmsg;
}

void
CollectiveEngine::finishCollective(Collective* coll, int rank, Collective::type_t ty, int tag)
{
  bool deliver_cq_msg; bool delete_collective;
  coll->actorDone(rank, deliver_cq_msg, delete_collective);
  debug_printf(sprockit::dbg::sumi,
    "Rank %d finishing collective of type %s tag %d - deliver=%d",
    tport_->rank(), Collective::tostr(ty), tag, deliver_cq_msg);

  if (!deliver_cq_msg)
    return;

  coll->complete();
  if (delete_collective && !coll->persistent()){ //otherwise collective must exist FOREVER
    collectives_[ty].erase(tag);
    todel_.push_back(coll);
  }

  pending_collective_msgs_[ty].erase(tag);
  debug_printf(sprockit::dbg::sumi,
    "Rank %d finished collective of type %s tag %d",
    tport_->rank(), Collective::tostr(ty), tag);
}

void
CollectiveEngine::waitBarrier(int tag)
{
  if (tport_->nproc() == 1) return;
  barrier(tag, Message::default_cq);
  auto* dmsg = blockUntilNext(Message::default_cq);
}

void
CollectiveEngine::cleanUp()
{
  for (Collective* coll : todel_){
    delete coll;
  }
  todel_.clear();
}

CollectiveDoneMessage*
CollectiveEngine::incoming(Message* msg)
{
  cleanUp();

  CollectiveWorkMessage* cmsg = dynamic_cast<CollectiveWorkMessage*>(msg);
  if (cmsg->sendCQ() == -1 && cmsg->recvCQ() == -1){
    spkt_abort_printf("both CQs are invalid for %s", msg->toString().c_str())
  }
  int tag = cmsg->tag();
  Collective::type_t ty = cmsg->type();
  tag_to_collective_map::iterator it = collectives_[ty].find(tag);
  if (it == collectives_[ty].end()){
    debug_printf(sprockit::dbg::sumi_collective,
      "Rank %d, queuing %p %s from %d on tag %d for type %s",
      tport_->rank(), msg,
      Message::tostr(msg->classType()),
      msg->sender(),
      tag, Collective::tostr(ty));
      //message for collective we haven't started yet
      pending_collective_msgs_[ty][tag].push_back(cmsg);
      return nullptr;
  } else {
    Collective* coll = it->second;
    auto* dmsg = coll->recv(cmsg);
    while (dmsg && coll->hasSubsequent()){
      delete dmsg;
      coll = coll->popSubsequent();
      dmsg = startCollective(coll);
    }
    return dmsg;
  }
}

CollectiveDoneMessage*
CollectiveEngine::blockUntilNext(int cq_id)
{
  CollectiveDoneMessage* dmsg = nullptr;
  while (dmsg == nullptr){
    debug_printf(sprockit::dbg::sumi_collective,
      "Rank %d, blocking collective until next message arrives on CQ %d", tport_->rank(), cq_id);
    auto* msg = tport_->blockingPoll(cq_id);
    debug_printf(sprockit::dbg::sumi_collective,
      "Rank %d, unblocking collective on CQ %d", tport_->rank(), cq_id);
    dmsg = incoming(msg);
  }
  debug_printf(sprockit::dbg::sumi_collective,
    "Rank %d, exiting collective progress on CQ %d", tport_->rank(), cq_id);
  return dmsg;
}

class NullQoSAnalysis : public QoSAnalysis
{
 public:
  SST_ELI_REGISTER_DERIVED(
    QoSAnalysis,
    NullQoSAnalysis,
    "macro",
    "null",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "perform a QoS analyis based on pattern")

  NullQoSAnalysis(SST::Params& params) :
    QoSAnalysis(params)
  {

  }

  int selectQoS(Message *m) override {
    return 0;
  }

  void logDelay(sstmac::TimeDelta delay, Message *m) override {

  }

};

class PatternQoSAnalysis : public QoSAnalysis
{
 public:
  SST_ELI_REGISTER_DERIVED(
    QoSAnalysis,
    PatternQoSAnalysis,
    "macro",
    "pattern",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "perform a QoS analyis based on pattern")

  PatternQoSAnalysis(SST::Params& params) :
    QoSAnalysis(params)
  {
    rtLatency_ = TimeDelta(params.find<SST::UnitAlgebra>("rt_latency").getValue().toDouble());
    eagerLatency_ = TimeDelta(params.find<SST::UnitAlgebra>("eager_latency").getValue().toDouble());
    rdmaLatency_ = TimeDelta(params.find<SST::UnitAlgebra>("rdma_latency").getValue().toDouble());
    byteDelay_ = TimeDelta(params.find<SST::UnitAlgebra>("bandwidth").getValue().inverse().toDouble());
    rdmaCutoff_ = params.find<SST::UnitAlgebra>("rdma_cutoff").getRoundedValue();
  }

  int selectQoS(Message *m) override {
    return 0;
  }

  void logDelay(sstmac::TimeDelta delay, Message *m) override {
    sstmac::TimeDelta acceptable_delay = allowedDelay_ + m->byteLength() * byteDelay_;
    if (m->byteLength() > rdmaCutoff_){
      acceptable_delay += rdmaLatency_ + 2*eagerLatency_ + 3*rtLatency_;
    } else {
      acceptable_delay += eagerLatency_ + rtLatency_;
    }

    printf("Message %12lu: %2u->%2u %8llu %10.4e %10.4e\n",
       m->hash(), m->sender(), m->recver(), m->byteLength(),
       delay.sec(), acceptable_delay.sec());

  }

 private:
  uint32_t rdmaCutoff_;
  sstmac::TimeDelta eagerLatency_;
  sstmac::TimeDelta rdmaLatency_;
  sstmac::TimeDelta byteDelay_;
  sstmac::TimeDelta rtLatency_;
  sstmac::TimeDelta allowedDelay_;
};



}
