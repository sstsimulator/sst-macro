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
#include <sprockit/delete.h>
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

struct null_ImplicitState : public OperatingSystem::ImplicitState
{
  FactoryRegister("null", OperatingSystem::ImplicitState, null_ImplicitState)

  null_ImplicitState(sprockit::sim_parameters* params) :
    OperatingSystem::ImplicitState(params){}

  void set_state(int type, int value) override {}
  void unset_state(int type) override {}
};

struct null_regression : public OperatingSystem::ThreadSafeTimerModel<double>
{
  FactoryRegister("null", OperatingSystem::RegressionModel, null_regression)

  using parent = OperatingSystem::ThreadSafeTimerModel<double>;

  null_regression(sprockit::sim_parameters* params,
                  const std::string& key)
    : parent(params, key), computed_(false) {}

  double compute(int n_params, const double params[],
                 OperatingSystem::ImplicitState* state) override {
    if (!computed_){
      computed_ = true;
      compute_mean();
    }
    return mean_;
  }

  int StartCollection() override {
    return parent::start();
  }

  void finishCollection(int thr_tag, int n_params, const double params[],
                         OperatingSystem::ImplicitState* state) override {
    parent::finish(thr_tag);
  }

  void finalize(sstmac::Timestamp t) override {
    compute_mean();
    std::string file = key() + ".memo";
    std::ofstream ofs(file);
    ofs << key()
        << "\n" << mean_;
    ofs.close();
  }

  void dumpGlobalData() override {}

  void dumpLocalData() override {}

  std::string toString() const override {
    return "mean";
  }

  void clear() override {}

  void reduce(StatCollector* coll) override {}

  void globalReduce(sstmac::ParallelRuntime* rt) override {}

  StatCollector* doClone(sprockit::sim_parameters* params) const override {
    return new null_regression(params, key());
  }

 private:
  void compute_mean(){
    double total = 0;
    for (double d : samples_){
      total += d;
    }
    mean_ = total / samples_.size();
  }

  double mean_;
  bool computed_;

};

struct linear_regression : public OperatingSystem::ThreadSafeTimerModel<std::pair<double,double>>
{
  FactoryRegister("linear", OperatingSystem::RegressionModel, linear_regression)
  using parent = OperatingSystem::ThreadSafeTimerModel<std::pair<double,double>>;
  linear_regression(sprockit::sim_parameters* params,
                    const std::string& key)
    : parent(params, key), computed_(false) {}

  double compute(int n_params, const double params[],
                 OperatingSystem::ImplicitState* state) override {
    if (n_params != 1){
      spkt_abort_printf("linear regression can only take one parameter - got %d", n_params);
    }
    if (!computed_){
      compute_regression();
      computed_ = true;
    }
    double val = m_*params[0] + b_;
    //std::cout << "Computed " << key() << "->f(" << params[0] << ") = " << val << std::endl;
    return val;
  }

  int StartCollection() override {
    return parent::start();
  }

  void dumpGlobalData() override {}

  void dumpLocalData() override {}

  std::string toString() const override {
    return "linear";
  }

  void clear() override {}

  void reduce(StatCollector* coll) override {}

  void globalReduce(sstmac::ParallelRuntime* rt) override {}

  StatCollector* doClone(sprockit::sim_parameters* params) const override {
    return new null_regression(params, key());
  }

  void finishCollection(int thr_tag, int n_params, const double params[],
                         OperatingSystem::ImplicitState* state) override {
    if (n_params != 1){
      spkt_abort_printf("linear regression can only take one parameter - got %d", n_params);
    }
    parent::finish(thr_tag, params[0]);
  }

  void finalize(sstmac::Timestamp t) override {
    compute_regression();
    std::string file = key() + ".memo";
    std::ofstream ofs(file);
    ofs << key()
        << "\n" << m_
        << "\n" << b_;
    ofs.close();
  }

 private:
  void compute_regression(){
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

static sprockit::need_deleteStatics<OperatingSystem> del_statics;

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
std::map<std::string,OperatingSystem::RegressionModel*> OperatingSystem::memoize_models_;
std::map<std::string,std::string>* OperatingSystem::memoize_init_ = nullptr;

OperatingSystem::OperatingSystem(sprockit::sim_parameters* params, hw::Node* parent) :
#if SSTMAC_INTEGRATED_SST_CORE
  nthread_(1),
  thread_id_(0),
#else
  nthread_(parent->nthread()),
  thread_id_(parent->threadId()),
#endif
  node_(parent),
  active_thread_(nullptr),
  callGraph_(nullptr),
  callGraph_active_(true), //on by default
  des_context_(nullptr),
  ftq_trace_(nullptr),
  compute_sched_(nullptr),
  SubComponent(parent),
  params_(params)
{
  my_addr_ = node_ ? node_->addr() : 0;

  compute_sched_ = ComputeScheduler::factory::get_optional_param(
                     "compute_scheduler", "simple", params, this,
                      node_ ? node_->proc()->ncores() : 1,
                      node_ ? node_->nsocket() : 1);

#if SSTMAC_HAVE_GRAPHVIZ
  stat_descr_t stat_descr;
  stat_descr.dump_all = false;
  stat_descr.unique_tag = &cg_tag;
  callGraph_ = optionalStats<graph_viz>(parent,
          params, "callGraph", "callGraph", &stat_descr);
#else
  if (params->has_namespace("callGraph")){
    spkt_abort_printf("cannot activate call graph collection - need to configure with --enable-graphviz");
  }
#endif

  ftq_trace_ = optionalStats<FTQCalendar>(parent, params, "ftq", "ftq");

  StackAlloc::init(params);

  rebuildMemoizations();

  sprockit::sim_parameters* env_params = params->get_optional_namespace("env");
  for (auto iter=env_params->begin(); iter != env_params->end(); ++iter){
    env_[iter->first] = iter->second.value;
  }
}

void
OperatingSystem::rebuildMemoizations()
{
  if (!memoize_init_) return;

  for (auto& pair : *memoize_init_){
    auto iter = memoize_models_.find(pair.first);
#if !SSTMAC_INTEGRATED_SST_CORE
    if (iter == memoize_models_.end()){
      sprockit::sim_parameters* memo_params = params_->get_optional_namespace(pair.first);
      memo_params->add_param_override("fileroot", pair.first);
      auto* model = RegressionModel::factory::get_value(pair.second, memo_params, pair.first);
      memoize_models_[pair.first] = model;
      EventManager::global->registerStat(model, nullptr);
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
    memoize_init_ = new std::map<std::string,std::string>;
  }
  (*memoize_init_)[name] = model;

}

void
OperatingSystem::allocateCore(Thread *thr)
{
  compute_sched_->reserveCores(1, thr);
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
  if (!pending_library_events_.empty()){
    auto& pair = *pending_library_events_.begin();
    const std::string& name = pair.first;
    Event* ev = pair.second.front();
    cerrn << "Valid libraries on OS " << addr() << ":\n";
    for  (auto& pair : libs_){
      cerrn << pair.first << std::endl;
    }
    spkt_abort_printf("OperatingSystem::handle_event: never registered library %s on os %d for event %s",
                   name.c_str(), int(addr()),
                   sprockit::toString(ev).c_str());
  }

  if (des_context_) {
    des_context_->destroyContext();
    delete des_context_;
  }
  if (compute_sched_) delete compute_sched_;

#if SSTMAC_HAVE_GRAPHVIZ
  if (callGraph_) delete callGraph_;
#endif

  //if (ftq_trace_) delete ftq_trace_;
}

void
OperatingSystem::initThreading(sprockit::sim_parameters* params)
{
  if (des_context_){
    return; //already done
  }

#if SSTMAC_USE_MULTITHREAD
  static thread_lock lock;
  sprockit::thread_stack_size<int>() = sw::stack_alloc::stacksize();
  lock.lock();
  if (active_os_.size() == 0){
    active_os_.resize(nthread());
  }
  lock.unlock();
#endif

  des_context_ = ThreadContext::factory::get_optional_param(
        "context", ThreadContext::defaultThreading(), params);

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
OperatingSystem::sleep(Timestamp t)
{
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
OperatingSystem::compute(Timestamp t)
{
  // guard the ftq tag in this function
  const auto& cur_tag = active_thread_->tag();
  FTQScope scope(active_thread_,
      cur_tag.id() == FTQTag::null.id() ? FTQTag::compute:cur_tag);

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
      spkt_throw_printf(sprockit::spkt_error,
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

  active_thread_ = tothread;
  activeOs() = this;
  tothread->context()->resumeContext(des_context_);

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

  RegressionModel* model = iter->second;
  return model->StartCollection();
}

static thread_local sstmac::sw::OperatingSystem::ImplicitState* implicit_memo_state_ = nullptr;

OperatingSystem::ImplicitState*
OperatingSystem::getImplicitState()
{
  if (!implicit_memo_state_){
      implicit_memo_state_ = sstmac::sw::OperatingSystem::ImplicitState::factory
                    ::get_optional_param("ImplicitState", "null", params_);
  }
  return implicit_memo_state_;
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
  RegressionModel* model = iter->second;
  model->finishCollection(thr_tag, n_params, params, states);
}

void
OperatingSystem::computeMemoize(const char *token, int n_params, double params[])
{
  auto iter = memoize_models_.find(token);
  if (iter == memoize_models_.end()){
    spkt_abort_printf("memoization %s was not registered - likely a compiler wrapper error",
                      token);
  }

  RegressionModel* model = iter->second;

  uintptr_t localStorage = get_sstmac_tls();
  auto* states = (ImplicitState*)(localStorage + SSTMAC_TLS_IMPLICIT_STATE);
  double time = model->compute(n_params, params, states);
  currentOs()->compute(Timestamp(time));
}

void
OperatingSystem::reassign_cores(Thread *thr)
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
  active_thread_ = nullptr;
  old_context->pauseContext(des_context_);

  while(hold_for_gdb_){
    sst_gdb_swap();  //do nothing - this is only for gdb purposes
  }

  //restore state to indicate this thread and this OS are active again
  activeOs() = this;
  active_thread_ = old_thread;
  active_thread_->incrementBlockCounter();

   //collect any statistics associated with the elapsed time
  Timestamp after = now();
  Timestamp elapsed = after - before;

  if (callGraph_ && callGraph_active_) {
    callGraph_->countTrace(elapsed.ticks(), active_thread_);
  }

  if (ftq_trace_){
    FTQTag tag = active_thread_->tag();
    ftq_trace_->collect(tag.id(),
      active_thread_->aid(), active_thread_->tid(),
      before.ticks(), elapsed.ticks());
    active_thread_->setTag(FTQTag::null);
  }
}

void
OperatingSystem::blockTimeout(Timestamp delay)
{
  sendDelayedExecutionEvent(delay, new TimeoutEvent(this, active_thread_));
  block();
}

Timestamp
OperatingSystem::unblock(Thread* thr)
{
  if (thr->isCanceled()){
    //just straight up delete the thread
    //it shall be neither seen nor heard
    delete thr;
  } else {
    switchToThread(thr);
  }

  return now();
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

  auto iter = pending_library_events_.find(lib->libName());
  if (iter != pending_library_events_.end()){
    const std::list<Event*> events = iter->second;
    for (Event* ev : events){
      sendExecutionEventNow(newCallback(componentId(), lib, &Library::incomingEvent, ev));
    }
    pending_library_events_.erase(iter);
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
                                  TaskMapping::ptr mapping,  sprockit::sim_parameters* app_params,
                                  bool include_root)
{
  int num_ranks = mapping->numRanks();
  //job launcher needs to add this - might need it later
  outcast_iterator iter(my_rank, num_ranks);
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
    sw::StartAppEvent* lev = new StartAppEvent(node_->allocateUniqueId(),
                                     aid, app_ns, mapping, dst_rank, dst_nid,
                                     addr(), app_params);
    os_debug("outcast to %d: %s", dst_rank, lev->toString().c_str());
    node_->nic()->logPLink()->send(lev);
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
  if (params_->has_param("context")){
    theapp->params()->add_param("context", params_->get_param("context"));
  }
  if (ftq_trace_){
    ftq_trace_->registerApp(theapp->aid(), sprockit::printf("app%d", int(theapp->aid())));
  }

  startThread(theapp);
}

bool
OperatingSystem::handleLibraryEvent(const std::string& name, Event* ev)
{
  auto it = libs_.find(name);
  bool found = it != libs_.end();
  if (found){
    Library* lib = it->second;
    os_debug("delivering message to lib %s:%p\n%s",
        name.c_str(), lib, sprockit::toString(ev).c_str());
    lib->incomingEvent(ev);
  } else {
    os_debug("unable to deliver message to lib %s\n%s",
        name.c_str(), sprockit::toString(ev).c_str());
  }
  return found;
}

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
    pending_library_events_[libmsg->libname()].push_back(ev);
  }
}

}
} //end of namespace sstmac
