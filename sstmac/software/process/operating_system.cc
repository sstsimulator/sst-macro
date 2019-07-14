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

#include <stdlib.h>
#include <sstream>
#include <csignal>
#include <sstmac/hardware/common/flow.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/software/launch/launch_request.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/logp/logp_switch.h>
#include <sstmac/software/libraries/service.h>
#include <sstmac/software/launch/app_launcher.h>
#include <sstmac/software/process/graphviz.h>
#include <sstmac/software/process/ftq.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/time.h>
#include <sstmac/software/process/progress_queue.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/compute_scheduler.h>
#include <sstmac/software/process/thread_info.h>
#include <sstmac/software/launch/app_launcher.h>
#include <sstmac/software/libraries/unblock_event.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/common/thread_lock.h>

#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/processor/processor.h>
#include <sstmac/hardware/network/network_message.h>

#include <sprockit/errors.h>
#include <sprockit/statics.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sstmac/software/api/api.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

RegisterDebugSlot(os,
    "print debug output related to operating system operators - the majority of this debug info will be related to thread context switching");

MakeDebugSlot(dropped_events)

#define os_debug(...) \
  debug_printf(sprockit::dbg::os, "OS on Node %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())

RegisterNamespaces("callGraph", "ftq");
RegisterKeywords(
{ "stack_size", "the size of stack to allocate to each user-space thread" },
{ "stack_chunk_size", "the block size to allocate in the memory pool when more stacks are needed" },
{ "ftq", "DEPRECATED: sets the fileroot of the FTQ statistic" },
{ "ftq_epoch", "DEPRECATED: sets the time epoch size for the FTQ statistic" },
{ "callGraph", "DEPRECATED: sets the fileroot of the call graph statistic" },
{ "compute_scheduler", "the type of compute scheduler or assigning cores to computation" },
{ "context", "the user-space thread context library" },
);

#include <sstmac/software/process/gdb.h>

void sst_gdb_swap(){
}

//we have to have a globally visible (to C code) stack-size variable
extern int sstmac_global_stacksize;

namespace sstmac {
namespace sw {

class DeleteThreadEvent :
  public ExecutionEvent
{
 public:
  DeleteThreadEvent(Thread* thr) :
    thr_(thr)
  {
  }

  void execute() override {
    delete thr_;
  }

 protected:
  Thread* thr_;
};

struct NullImplicitState : public OperatingSystem::ImplicitState
{
  SST_ELI_REGISTER_DERIVED(
    OperatingSystem::ImplicitState,
    NullImplicitState,
    "macro",
    "null",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "an implicit that holds nothing")

  NullImplicitState(SST::Params& params) :
    OperatingSystem::ImplicitState(params){}

  void setState(int type, int value) override {}
  void unsetState(int type) override {}
};

struct NullRegression : public OperatingSystem::ThreadSafeTimerModel<double>
{
  /**
  SST_ELI_REGISTER_DERIVED(
    OperatingSystem::RegressionModel,
    NullRegression,
    "macro",
    "null",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "a regression model that does nothing")
  */

  using Parent = OperatingSystem::ThreadSafeTimerModel<double>;

  NullRegression(SST::BaseComponent* comp, const std::string& key,
                 const std::string& subName, SST::Params& params)
    : Parent(params, comp, key, ""), computed_(false) {}

  double compute(int n_params, const double params[],
                 OperatingSystem::ImplicitState* state) override {
    if (!computed_){
      computed_ = true;
      computeMean();
    }
    return mean_;
  }

  int startCollection() override {
    return Parent::start();
  }

  void finishCollection(int thr_tag, int n_params, const double params[],
                         OperatingSystem::ImplicitState* state) override {
    Parent::finish(thr_tag);
  }

  /**
  void finalize(sstmac::TimeDelta t) override {
    compute_mean();
    std::string file = key() + ".memo";
    std::ofstream ofs(file);
    ofs << key()
        << "\n" << mean_;
    ofs.close();
  }
  */

 private:
  void computeMean(){
    double total = 0;
    for (double d : samples_){
      total += d;
    }
    mean_ = total / samples_.size();
  }

  double mean_;
  bool computed_;

};

struct LinearRegression : public OperatingSystem::ThreadSafeTimerModel<std::pair<double,double>>
{
  /**
  SST_ELI_REGISTER_DERIVED(
    OperatingSystem::RegressionModel,
    LinearRegression,
    "macro",
    "linear",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "a simple linear regression model")
  */

  using parent = OperatingSystem::ThreadSafeTimerModel<std::pair<double,double>>;
  LinearRegression(SST::BaseComponent* comp, const std::string& key,
                   const std::string& subName, SST::Params& params)
    : parent(params, comp, key, ""), computed_(false) {}

  double compute(int n_params, const double params[],
                 OperatingSystem::ImplicitState* state) override {
    if (n_params != 1){
      spkt_abort_printf("linear regression can only take one parameter - got %d", n_params);
    }
    if (!computed_){
      computeRegression();
      computed_ = true;
    }
    double val = m_*params[0] + b_;
    //std::cout << "Computed " << key() << "->f(" << params[0] << ") = " << val << std::endl;
    return val;
  }

  int startCollection() override {
    return parent::start();
  }

  void finishCollection(int thr_tag, int n_params, const double params[],
                        OperatingSystem::ImplicitState* state) override {
    if (n_params != 1){
      spkt_abort_printf("linear regression can only take one parameter - got %d", n_params);
    }
    parent::finish(thr_tag, params[0]);
  }

  /**
  void finalize(sstmac::TimeDelta t) override {
    computeRegression();
    std::string file = key() + ".memo";
    std::ofstream ofs(file);
    ofs << key()
        << "\n" << m_
        << "\n" << b_;
    ofs.close();
  }
  */

 private:
  void computeRegression(){
    double meanX = 0;
    double meanY = 0;
    for (auto& pair : samples_){
      meanX += pair.second;
      meanY += pair.second;
    }
    meanX /= samples_.size();
    meanY /= samples_.size();

    double Cov = 0;
    double Var = 0;
    for (auto& pair : samples_){
      double dx = pair.first - meanX;
      double dy = pair.second - meanY;
      Var += dx*dx;
      Cov += dx*dy;
    }

    b_ = Cov / Var;
    m_ = meanY - b_*meanX;
    //std::cout << "Computed " << key() << "-> m=" << m_ << " b=" << b_ << std::endl;
  }

  double m_;
  double b_;
  bool computed_;
};

static sprockit::NeedDeletestatics<OperatingSystem> del_statics;

#if SSTMAC_USE_MULTITHREAD
std::vector<OperatingSystem*> OperatingSystem::active_os_;
#else
OperatingSystem* OperatingSystem::active_os_ = nullptr;
#endif

bool OperatingSystem::hold_for_gdb_ = false;
ThreadContext* OperatingSystem::gdb_context_ = nullptr;
ThreadContext* OperatingSystem::gdb_original_context_ = nullptr;
ThreadContext* OperatingSystem::gdb_des_context_ = nullptr;
std::unordered_map<uint32_t,Thread*> OperatingSystem::all_threads_;
bool OperatingSystem::gdb_active_ = false;
std::map<std::string,std::unique_ptr<OperatingSystem::RegressionModel>> OperatingSystem::memoize_models_;
std::unique_ptr<std::map<std::string,std::string>> OperatingSystem::memoize_init_ = nullptr;

OperatingSystem::OperatingSystem(SST::Component* parent, SST::Params& params) :
  node_(safe_cast(hw::Node,parent)),
  active_thread_(nullptr),
  des_context_(nullptr),
  compute_sched_(nullptr),
  SubComponent("os", parent),
  params_(params)
{
  my_addr_ = node_ ? node_->addr() : 0;

  //assume macro for now
  compute_sched_ = sprockit::create<ComputeScheduler>(
    "macro", params.find<std::string>("compute_scheduler", "simple"),
    params, this, node_ ? node_->proc()->ncores() : 1, node_ ? node_->nsocket() : 1);

  StackAlloc::init(params);

  rebuildMemoizations();

  SST::Params env_params = params.find_scoped_params("env");
  std::set<std::string> keys = env_params.getKeys();
  for (auto& key : keys){
    env_[key] = env_params.find<std::string>(key);
  }

  //sprockit::thread_stack_size<int>() = sw::StackAlloc::stacksize();
}

std::string
OperatingSystem::hostname() const
{
  return node_->hostname();
}

void
OperatingSystem::rebuildMemoizations()
{
  if (!memoize_init_) return;

  for (auto& pair : *memoize_init_){
    auto iter = memoize_models_.find(pair.first);
#if !SSTMAC_INTEGRATED_SST_CORE
    if (iter == memoize_models_.end()){
      SST::Params memo_params = params_.find_scoped_params(pair.first);
      memo_params.insert("fileroot", pair.first);
      auto* model = sprockit::create<RegressionModel>(
        "macro", pair.second, node(), pair.first, "", memo_params);
      memoize_models_[pair.first] = std::unique_ptr<RegressionModel>(model);
      //EventManager::global->registerStat(model, nullptr);
    }
#else
    spkt_abort_printf("do not yet support memoization in integrated core - failed memoizing %s",
                      pair.first.c_str());
#endif
  }
}

void
OperatingSystem::addMemoization(const std::string& name, const std::string& model)
{
  if (!memoize_init_){
    memoize_init_ = std::unique_ptr<std::map<std::string,std::string>>(new std::map<std::string,std::string>);
  }
  (*memoize_init_)[name] = model;

}

void
OperatingSystem::allocateCore(Thread *thr)
{
  os_debug("attempting to reserve core for thread %d", thr->threadId());
  compute_sched_->reserveCores(1, thr);
  os_debug("successfully reserved core for thread %d");
}

void
OperatingSystem::deallocateCore(Thread *thr)
{
  compute_sched_->releaseCores(1, thr);
}

void
OperatingSystem::initThreads(int nthread)
{
#if SSTMAC_USE_MULTITHREAD
  if (active_os_.size() == 0){
    active_os_.resize(nthread);
  }
#endif
}

OperatingSystem::~OperatingSystem()
{
  for (auto& pair : pending_library_request_){
    std::string name = pair.first;
    for (Request* req : pair.second){
      cerrn << "Valid libraries on OS " << addr() << ":\n";
      for  (auto& pair : libs_){
        cerrn << pair.first << std::endl;
      }
      spkt_abort_printf("OperatingSystem:: never registered library %s on os %d for event %s",
                     name.c_str(), int(addr()),
                     sprockit::toString(req).c_str());
    }
  }

  if (des_context_) {
    des_context_->destroyContext();
    delete des_context_;
  }
  if (compute_sched_) delete compute_sched_;

  //these are owned now by the stats system - don't delete here
  //if (callGraph_) delete callGraph_;
  //if (ftq_trace_) delete ftq_trace_;
}

void
OperatingSystem::initThreading(SST::Params& params)
{
  if (des_context_){
    return; //already done
  }

#if SSTMAC_USE_MULTITHREAD
  static thread_lock lock;
  lock.lock();
  if (active_os_.size() == 0){
    active_os_.resize(nthread());
  }
  lock.unlock();
#endif

  des_context_ = sprockit::create<ThreadContext>(
     "macro", params.find<std::string>("context", ThreadContext::defaultThreading()));

  des_context_->initContext();

  active_thread_ = nullptr;
}

void
OperatingSystem::localShutdown()
{
}

void
OperatingSystem::deleteStatics()
{
}

void
OperatingSystem::sleep(TimeDelta t)
{
  CallGraphAppend(sleep);
  FTQScope scope(active_thread_, FTQTag::sleep);

  sw::UnblockEvent* ev = new sw::UnblockEvent(this, active_thread_);
  sendDelayedExecutionEvent(t, ev);
  int ncores = active_thread_->numActiveCcores();
  //when sleeping, release all cores
  compute_sched_->releaseCores(ncores, active_thread_);
  block();
  compute_sched_->reserveCores(ncores, active_thread_);
}

void
OperatingSystem::sleepUntil(Timestamp t)
{
  Timestamp now_ = now();
  if (t > now_){
    FTQScope scope(active_thread_, FTQTag::sleep);
    sw::UnblockEvent* ev = new sw::UnblockEvent(this, active_thread_);
    sendExecutionEvent(t, ev);
    int ncores = active_thread_->numActiveCcores();
    //when sleeping, release all cores
    compute_sched_->releaseCores(ncores, active_thread_);
    block();
    compute_sched_->reserveCores(ncores, active_thread_);
  }
}

void
OperatingSystem::compute(TimeDelta t)
{
  // guard the ftq tag in this function
  const auto& cur_tag = active_thread_->tag();
  FTQScope scope(active_thread_, FTQTag::compute);

  sw::UnblockEvent* ev = new sw::UnblockEvent(this, active_thread_);
  sendDelayedExecutionEvent(t, ev);
  block();
}

std::function<void(hw::NetworkMessage*)>
OperatingSystem::nicDataIoctl()
{
  return node_->nic()->dataIoctl();
}

std::function<void(hw::NetworkMessage*)>
OperatingSystem::nicCtrlIoctl()
{
  return node_->nic()->ctrlIoctl();
}

void
OperatingSystem::execute(ami::COMP_FUNC func, Event *data, int nthr)
{
  int owned_ncores = active_thread_->numActiveCcores();
  if (owned_ncores < nthr){
    compute_sched_->reserveCores(nthr-owned_ncores, active_thread_);
  }

  //initiate the hardware events
  Callback* cb = newCallback(this, &OperatingSystem::unblock, active_thread_);

  switch (func) {
    case sstmac::ami::COMP_INSTR:
      node_->proc()->compute(data, cb);
      break;
    case sstmac::ami::COMP_TIME: {
      sw::TimedComputeEvent* ev = safe_cast(sw::TimedComputeEvent, data);
      sendDelayedExecutionEvent(ev->data(), cb);
      break;
    }
    default:
      spkt_throw_printf(sprockit::SpktError,
            "simplenode: cannot process kernel %s",
            ami::tostr(func));
  }

  block();

  if (owned_ncores < nthr){
    compute_sched_->releaseCores(nthr-owned_ncores,active_thread_);
  }
}

void
OperatingSystem::decrementAppRefcount()
{
  node_->decrementAppRefcount();
}

void
OperatingSystem::incrementAppRefcount()
{
  node_->incrementAppRefcount();
}

void
OperatingSystem::simulationDone()
{
}

Library*
OperatingSystem::currentLibrary(const std::string &name)
{
  return currentOs()->lib(name);
}

void
OperatingSystem::switchToThread(Thread* tothread)
{
  if (active_thread_ != nullptr){ //not an error
    //but this must be thrown over to the DES context to actually execute
    //we cannot context switch directly from subthread to subthread
    sendExecutionEventNow(newCallback(this, &OperatingSystem::switchToThread, tothread));
    return;
  }

  os_debug("switching to thread %d", tothread->threadId());
  active_thread_ = tothread;
  activeOs() = this;
  tothread->context()->resumeContext(des_context_);

  os_debug("switched back from thread %d to main thread", tothread->threadId());

  /** back to main thread */
  active_thread_ = nullptr;
}

void
OperatingSystem::printLibs(std::ostream &os) const
{
  os << "available libraries: \n";
  for (auto& pair : libs_){
    os << pair.first << "\n";
  }
}

int
OperatingSystem::startMemoize(const char *token, const char* model_name)
{
  auto iter = memoize_models_.find(token);
  if (iter == memoize_models_.end()){
    spkt_abort_printf("memoization %s for model %s was not registered - likely a compiler wrapper error",
                      token, model_name);
  }
  return iter->second->startCollection();
}

static thread_local std::unique_ptr<sstmac::sw::OperatingSystem::ImplicitState> implicit_memo_state_;

OperatingSystem::ImplicitState*
OperatingSystem::getImplicitState()
{
  if (!implicit_memo_state_){
    implicit_memo_state_ = std::unique_ptr<sstmac::sw::OperatingSystem::ImplicitState>(
          sprockit::create<sstmac::sw::OperatingSystem::ImplicitState>(
        "macro", params_.find<std::string>("implicit_state", "null"), params_));
  }
  return implicit_memo_state_.get();
}

void
OperatingSystem::stopMemoize(int thr_tag, const char *token, int n_params, double params[])
{
  auto iter = memoize_models_.find(token);
  if (iter == memoize_models_.end()){
    spkt_abort_printf("memoization %s was not registered - likely a compiler wrapper error",
                      token);
  }

  uintptr_t localStorage = get_sstmac_tls();
  auto* states = (ImplicitState*)(localStorage + SSTMAC_TLS_IMPLICIT_STATE);
  iter->second->finishCollection(thr_tag, n_params, params, states);
}

void
OperatingSystem::computeMemoize(const char *token, int n_params, double params[])
{
  auto iter = memoize_models_.find(token);
  if (iter == memoize_models_.end()){
    spkt_abort_printf("memoization %s was not registered - likely a compiler wrapper error",
                      token);
  }

  uintptr_t localStorage = get_sstmac_tls();
  auto* states = (ImplicitState*)(localStorage + SSTMAC_TLS_IMPLICIT_STATE);
  double time = iter->second->compute(n_params, params, states);
  currentOs()->compute(TimeDelta(time));
}

void
OperatingSystem::reassignCores(Thread *thr)
{
  int ncores = thr->numActiveCcores();
  //this is not equivalent to a no-op
  //I could release cores - then based on changes
  //to the cpumask, reserve different cores
  compute_sched_->releaseCores(ncores, thr);
  compute_sched_->reserveCores(ncores, thr);
}

void
OperatingSystem::block()
{
  Timestamp before = now();
  //back to main DES thread
  ThreadContext* old_context = active_thread_->context();
  if (old_context == des_context_){
    spkt_abort_printf("blocking main DES thread on node %d", my_addr_);
  }
  Thread* old_thread = active_thread_;
  //reset the time flag
  active_thread_->setTimedOut(false);
  os_debug("pausing context on thread %d", active_thread_->threadId());
  active_thread_ = nullptr;
  old_context->pauseContext(des_context_);

  while(hold_for_gdb_){
    sst_gdb_swap();  //do nothing - this is only for gdb purposes
  }

  //restore state to indicate this thread and this OS are active again
  activeOs() = this;
  os_debug("resuming context on thread %d", active_thread_->threadId());
  active_thread_ = old_thread;
  active_thread_->incrementBlockCounter();

   //collect any statistics associated with the elapsed time
  Timestamp after = now();
  TimeDelta elapsed = after - before;

  if (elapsed.ticks()){
    active_thread_->collectStats(before, elapsed);
  }
}

void
OperatingSystem::blockTimeout(TimeDelta delay)
{
  sendDelayedExecutionEvent(delay, new TimeoutEvent(this, active_thread_));
  block();
}

void
OperatingSystem::unblock(Thread* thr)
{
  if (thr->isCanceled()){
    //just straight up delete the thread
    //it shall be neither seen nor heard
    delete thr;
  } else {
    switchToThread(thr);
  }
}

void
OperatingSystem::joinThread(Thread* t)
{
  if (t->getState() != Thread::DONE) {
    //key* k = key::construct();
    os_debug("joining thread %ld - thread not done so blocking on thread %p",
        t->threadId(), active_thread_);
    t->joiners_.push(active_thread_);
    int ncores = active_thread_->numActiveCcores();
    //when joining - release all cores
    compute_sched_->releaseCores(ncores, active_thread_);
    block();
    compute_sched_->reserveCores(ncores, active_thread_);
  } else {
    os_debug("joining completed thread %ld", t->threadId());
  }
  delete t;
}

void
OperatingSystem::scheduleThreadDeletion(Thread* thr)
{
  //JJW 11/6/2014 This here is weird
  //The thread has run out of work and is terminating
  //However, because of weird thread swapping the DES thread
  //might still operate on the thread... we need to delay the delete
  //until the DES thread has completely finished processing its current event
  sendExecutionEventNow(new DeleteThreadEvent(thr));
}

void
OperatingSystem::completeActiveThread()
{
  if (gdb_active_){
    all_threads_.erase(active_thread_->tid());
  }
  compute_sched_->releaseCores(1, active_thread_);
  Thread* thr_todelete = active_thread_;

  //if any threads waiting on the join, unblock them
  os_debug("completing thread %ld", thr_todelete->threadId());
  while (!thr_todelete->joiners_.empty()) {
    Thread* blocker = thr_todelete->joiners_.front();
    os_debug("thread %ld is unblocking joiner %p",
        thr_todelete->threadId(), blocker);
    unblock(blocker);
    //to_awake_.push(thr_todelete->joiners_.front());
    thr_todelete->joiners_.pop();
  }
  active_thread_ = nullptr;

  os_debug("completing context for %ld", thr_todelete->threadId());
  thr_todelete->context()->completeContext(des_context_);
}

void
OperatingSystem::registerLib(Library* lib)
{
#if SSTMAC_SANITY_CHECK
  if (lib->libName() == "") {
    sprockit::abort("OperatingSystem: trying to register a lib with no name");
  }
#endif
  os_debug("registering lib %s:%p", lib->libName().c_str(), lib);
  int& refcount = lib_refcounts_[lib];
  ++refcount;
  libs_[lib->libName()] = lib;
  debug_printf(sprockit::dbg::dropped_events,
               "OS %d should no longer drop events for %s",
               addr(), lib->libName().c_str());

  auto iter = pending_library_request_.find(lib->libName());
  if (iter != pending_library_request_.end()){
    const std::list<Request*> reqs = iter->second;
    for (Request* req : reqs){
      os_debug("delivering delayed event to lib %s: %s",
               lib->libName().c_str(), sprockit::toString(req).c_str());
      sendExecutionEventNow(newCallback(lib, &Library::incomingRequest, req));
    }
    pending_library_request_.erase(iter);
  }
}

void
OperatingSystem::unregisterLib(Library* lib)
{
  os_debug("unregistering lib %s", lib->libName().c_str());
  int& refcount = lib_refcounts_[lib];
  if (refcount == 1){
    lib_refcounts_.erase(lib);
    debug_printf(sprockit::dbg::dropped_events,
                 "OS %d will now drop events for %s",
                 addr(), lib->libName().c_str());
    libs_.erase(lib->libName());
    //delete lib;
  } else {
    --refcount;
  }
}

void
OperatingSystem::killNode()
{
}

Library*
OperatingSystem::lib(const std::string& name) const
{
  auto it = libs_.find(name);
  if (it == libs_.end()) {
    return nullptr;
  } else {
    return it->second;
  }
}

void
OperatingSystem::outcastAppStart(int my_rank, int aid, const std::string& app_ns,
                                 TaskMapping::ptr mapping,
                                 const SST::Params& app_params,
                                 bool include_root)
{
  int num_ranks = mapping->numRanks();
  //job launcher needs to add this - might need it later
  OutcastIterator iter(my_rank, num_ranks);
  int ranks[64];
  int num_to_send = 0;
  if (include_root){
    //only send to root
    num_to_send = 1;
    ranks[0] = my_rank;
  } else {
    num_to_send = iter.forward_to(ranks);
  }

  for (int r=0; r < num_to_send; ++r){
    int dst_rank = ranks[r];
    int dst_nid = mapping->rankToNode(dst_rank);
    sw::StartAppRequest* lev = new StartAppRequest(node_->allocateUniqueId(),
                                     aid, app_ns, mapping, dst_rank, dst_nid,
                                     addr(), app_params);
    os_debug("outcast to %d: %s", dst_rank, lev->toString().c_str());
    node_->nic()->sendManagerMsg(lev);
  }
}

ThreadContext*
OperatingSystem::activeContext()
{
  if (active_thread_){
    return active_thread_->context();
  } else {
    return des_context_;
  }
}

void
OperatingSystem::gdbSwitchToThread(uint32_t id)
{
  Thread* thr = all_threads_[id];
  if (thr){
    ThreadContext* from_context = nullptr;
    if (gdb_context_){
      from_context = gdb_context_;
    } else {
      OperatingSystem* os = staticOsThreadContext();
      if (os->activeThread()){
        from_context = os->activeThread()->context();
      } else {
        from_context = os->des_context_;
      }
    }

    if (!gdb_original_context_){
      gdb_original_context_ = from_context;
    }

    hold_for_gdb_ = true;
    from_context->jumpContext(thr->context());
  } else {
    std::cerr << "Invalid rank " << id << std::endl;
  }
}

void
OperatingSystem::gdbReset()
{
  hold_for_gdb_ = false;
  if (!gdb_original_context_){
    std::cerr << "Cannot reset GDB/LLDB - never selected thread" << std::endl;
    return;
  }

  ThreadContext* orig = gdb_original_context_;
  ThreadContext* current = gdb_context_;
  gdb_original_context_ = nullptr;
  gdb_context_ = nullptr;

  current->jumpContext(orig);
}

void
OperatingSystem::startThread(Thread* t)
{
  if (active_thread_){
    //crap - can't do this on this thread - need to do on DES thread
    sendExecutionEventNow(newCallback(this, &OperatingSystem::startThread, t));
  } else {
    active_thread_ = t;
    activeOs() = this;
    App* parent = t->parentApp();
    void* stack = StackAlloc::alloc();
    t->initThread(
      parent->params(),
      threadId(),
      des_context_,
      stack,
      StackAlloc::stacksize(),
      parent->globalsStorage(),
      parent->newTlsStorage());
  }

  if (gdb_active_){
    static thread_lock all_threads_lock;
    all_threads_lock.lock();
    auto tid = t->tid();
    Thread*& thrInMap = all_threads_[tid];
    if (thrInMap){
      spkt_abort_printf("error: thread %d already exists for app %d",
                        t->tid(), thrInMap->aid());
    }
    thrInMap = t;
    all_threads_lock.unlock();
  }
}

void
OperatingSystem::startApp(App* theapp, const std::string& unique_name)
{
  os_debug("starting app %d:%d on thread %d",
    int(theapp->tid()), int(theapp->aid()), threadId());
  //this should be called from the actual thread running it
  initThreading(params_);
  startThread(theapp);
}

bool
OperatingSystem::handleLibraryRequest(const std::string& name, Request* req)
{
  auto it = libs_.find(name);
  bool found = it != libs_.end();
  if (found){
    Library* lib = it->second;
    os_debug("delivering message to lib %s:%p: %s",
        name.c_str(), lib, sprockit::toString(req).c_str());
    lib->incomingRequest(req);
  } else {
    os_debug("unable to deliver message to lib %s: %s",
        name.c_str(), sprockit::toString(req).c_str());
  }
  return found;
}

#if 0
void
OperatingSystem::handleEvent(Event* ev)
{  
  //this better be an incoming event to a library, probably from off node
  Flow* libmsg = test_cast(Flow, ev);
  if (!libmsg) {
    spkt_throw_printf(sprockit::illformed_error,
      "OperatingSystem::handle_event: got event %s instead of library event",
      sprockit::toString(ev).c_str());
  }

  bool found = handleLibraryEvent(libmsg->libname(), ev);
  if (!found){
    os_debug("delaying event to lib %s: %s",
             libmsg->libname().c_str(), libmsg->toString().c_str());
    pending_library_events_[libmsg->libname()].push_back(ev);
  }
}
#endif

void
OperatingSystem::handleRequest(Request* req)
{
  //this better be an incoming event to a library, probably from off node
  Flow* libmsg = test_cast(Flow, req);
  if (!libmsg) {
    spkt_throw_printf(sprockit::IllformedError,
      "OperatingSystem::handle_event: got event %s instead of library event",
      sprockit::toString(req).c_str());
  }

  bool found = handleLibraryRequest(libmsg->libname(), req);
  if (!found){
    os_debug("delaying event to lib %s: %s",
             libmsg->libname().c_str(), libmsg->toString().c_str());
    pending_library_request_[libmsg->libname()].push_back(req);
  }
}

}
} //end of namespace sstmac
