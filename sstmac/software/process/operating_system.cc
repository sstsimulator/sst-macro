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

#include <stdlib.h>
#include <sstream>

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/messages/library_message.h>

#if SSTMAC_HAVE_GNU_PTH
#include <sstmac/software/threading/threading_pth.h>
#endif
#if SSTMAC_HAVE_PTHREAD
#include <sstmac/software/threading/threading_pthread.h>
#endif
#include <sstmac/software/libraries/service.h>
#include <sstmac/software/launch/app_launcher.h>
#include <sstmac/software/process/graphviz.h>
#include <sstmac/software/process/ftq.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/compute_scheduler.h>
#include <sstmac/software/process/thread_info.h>
#include <sstmac/software/launch/app_launcher.h>
#include <sstmac/software/libraries/unblock_event.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/common/thread_lock.h>

#if SSTMAC_HAVE_UCONTEXT
#include <sstmac/software/threading/threading_ucontext.h>
#endif

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

RegisterNamespaces("call_graph", "ftq");
RegisterKeywords(
"stack_size",
"stack_chunk_size",
"ftq",
"ftq_trace",
"ftq_epoch",
"call_graph",
"event_trace",
"stack_protect",
"event_trace_start",
"event_trace_stop",
"compute_scheduler",
);

//we have to have a globally visible (to C code) stack-size variable
extern int sstmac_global_stacksize;

namespace sstmac {
namespace sw {

class delete_thread_event :
  public event_queue_entry
{
 public:
  delete_thread_event(thread* thr) :
    thr_(thr),
    event_queue_entry(thr->os()->component_id(), thr->os()->component_id())
  {
  }

  void execute(){
    delete thr_;
  }

 protected:
  thread* thr_;
};

static sprockit::need_delete_statics<operating_system> del_statics;

#if SSTMAC_USE_MULTITHREAD
std::vector<operating_system*> operating_system::active_os_;
#else
operating_system* operating_system::active_os_ = nullptr;
#endif

operating_system::operating_system(sprockit::sim_parameters* params, hw::node* parent) :
  my_addr_(parent->addr()),
  node_(parent),
  call_graph_(nullptr),
  call_graph_active_(true), //on by default
  des_context_(nullptr),
  ftq_trace_(nullptr),
  compute_sched_(nullptr),
  event_subcomponent(parent),
  params_(params),
  nthread_(parent->nthread()),
  thread_id_(parent->thread())
{
  compute_sched_ = compute_scheduler::factory::get_optional_param(
                     "compute_scheduler", "simple", params, this);

#if SSTMAC_HAVE_GRAPHVIZ
  stat_descr_t stat_descr;
  stat_descr.dump_all = false;
  call_graph_ = optional_stats<graph_viz>(parent,
          params, "call_graph", "call_graph", &stat_descr);
#else
  if (params->has_namespace("call_graph")){
    spkt_abort_printf("cannot activate call graph collection - need to configure with --enable-graphviz");
  }
#endif

  ftq_trace_ = optional_stats<ftq_calendar>(parent, params, "ftq", "ftq");

  stack_alloc::init(params);

  compute_sched_->configure(node_->proc()->ncores(), node_->nsocket());
}

void
operating_system::init_threads(int nthread)
{
#if SSTMAC_USE_MULTITHREAD
  if (active_os_.size() == 0){
    active_os_.resize(nthread);
  }
#endif
}

operating_system::~operating_system()
{
  if (!pending_library_events_.empty()){
    auto& pair = *pending_library_events_.begin();
    const std::string& name = pair.first;
    event* ev = pair.second.front();
    cerrn << "Valid libraries on OS " << addr() << ":\n";
    for  (auto& pair : libs_){
      cerrn << pair.first << std::endl;
    }
    spkt_abort_printf("operating_system::handle_event: never registered library %s on os %d for event %s",
                   name.c_str(), int(addr()),
                   sprockit::to_string(ev).c_str());
  }

  if (des_context_) {
    des_context_->destroy_context();
    delete des_context_;
  }
  if (compute_sched_) delete compute_sched_;

#if SSTMAC_HAVE_GRAPHVIZ
  if (call_graph_) delete call_graph_;
#endif

  //if (ftq_trace_) delete ftq_trace_;
}

void
operating_system::init_threading(sprockit::sim_parameters* params)
{
  if (des_context_){
    return; //already done
  }

  static thread_lock lock;
  sprockit::thread_stack_size<int>() = sw::stack_alloc::stacksize();
  lock.lock();
  if (active_os_.size() == 0){
    active_os_.resize(nthread());
  }
  lock.unlock();

  //make sure to stash the thread ID in some thread-local storage
  void* stack = thread_info::get_current_stack();
  thread_info::set_thread_id(stack, threadId());

  des_context_ = threading_interface::factory::get_optional_param(
        "context", threading_interface::default_threading(), params);

  des_context_->init_context();

  active_thread_ = nullptr;
}

void
operating_system::local_shutdown()
{
}

void
operating_system::delete_statics()
{
}

void
operating_system::sleep(timestamp t)
{
  //sw::key* k = sw::key::construct();
  sw::unblock_event* ev = new sw::unblock_event(this, active_thread_);
  send_delayed_self_event_queue(t, ev);
  block();
  //delete k;
}

void
operating_system::sleep_until(timestamp t)
{
  timestamp now_ = now();
  if (t > now_){
    //sw::key* k = sw::key::construct();
    sw::unblock_event* ev = new sw::unblock_event(this, active_thread_);
    send_self_event_queue(t, ev);
    block();
    //delete k;
  }
}

void
operating_system::compute(timestamp t)
{
  //first thing's first - make sure I have a core to execute on
  //this will block if the thread has no core to run on
  compute_sched_->reserve_core(active_thread_);
  sleep(t);
  compute_sched_->release_core(active_thread_);
}


void
operating_system::async_kernel(ami::SERVICE_FUNC func,
                               event *data)
{
  node_->execute(func, data);
}

void
operating_system::execute_kernel(ami::COMP_FUNC func, event *data,
                                 callback* cb)
{
  spkt_abort_printf("operating_system::execute_kernel(%s): not implemented",
                    ami::tostr(func));
}

void
operating_system::execute(ami::COMP_FUNC func, event* data,
                          key_traits::category cat)
{
  //this will block if the thread has no core to run on
  compute_sched_->reserve_core(active_thread_);
  //initiate the hardware events
  //key* k = new key(cat);
  callback* cb = new_callback(this, &operating_system::unblock, active_thread_);
  node_->execute(func, data, cb);
  block();
  compute_sched_->release_core(active_thread_);
  //delete k;
  //callbacks deleted by core
}

void
operating_system::execute_kernel(ami::COMM_FUNC func,
                                 message* data)
{
  switch(func){
  case sstmac::ami::COMM_SEND: {
    hw::network_message* netmsg = safe_cast(hw::network_message, data);
    netmsg->set_fromaddr(my_addr_);
    node_->send_to_nic(netmsg);
    break;
  }
  case sstmac::ami::COMM_PMI_SEND: {
    hw::network_message* netmsg = safe_cast(hw::network_message, data);
    netmsg->set_fromaddr(my_addr_);
    node_->get_nic()->send_to_logp_switch(netmsg);
    break;
  }
  }
}

void
operating_system::decrement_app_refcount()
{
  node_->decrement_app_refcount();
}

void
operating_system::increment_app_refcount()
{
  node_->increment_app_refcount();
}

void
operating_system::simulation_done()
{
}

library*
operating_system::current_library(const std::string &name)
{
  return current_os()->lib(name);
}

void
operating_system::switch_to_thread(thread* tothread)
{
  if (active_thread_ != nullptr){ //not an error
    //but this must be thrown over to the DES context to actually execute
    //we cannot context switch directly from subthread to subthread
    send_now_self_event_queue(new_callback(this, &operating_system::switch_to_thread, tothread));
    return;
  }

  active_thread_ = tothread;
  active_os() = this;
  tothread->context()->resume_context(des_context_);

  /** back to main thread */
  active_thread_ = nullptr;
}

void
operating_system::print_libs(std::ostream &os) const
{
  os << "available libraries: \n";
  for (auto& pair : libs_){
    os << pair.first << "\n";
  }
}

timestamp
operating_system::block()
{
  int64_t before_ticks = now().ticks_int64();
  //back to main DES thread
  threading_interface* old_context = active_thread_->context();
  if (old_context == des_context_){
    spkt_abort_printf("blocking main DES thread on node %d", my_addr_);
  }
  thread* old_thread = active_thread_;
  active_thread_ = nullptr;
  old_context->pause_context(des_context_);
  active_os() = this;


  //I am reactivated !!!!
  active_thread_ = old_thread;
  int64_t after_ticks = now().ticks_int64();
  int64_t delta_ticks = after_ticks - before_ticks;

  if (call_graph_ && call_graph_active_) {
    call_graph_->count_trace(delta_ticks, active_thread_);
  }

  if (ftq_trace_) {
    sprockit::abort("FTQ not yet supported");
    //ftq_trace_->collect(req->event_typeid(),
    //  active_thread_->aid(), active_thread_->tid(),
    //  before_ticks, delta_ticks);
  }

  return now();
}

void
operating_system::schedule_timeout(timestamp delay, thread* thr)
{
  send_delayed_self_event_queue(delay, new timeout_event(this, thr));
}

timestamp
operating_system::unblock(thread* thr)
{
  if (thr->is_canceled()){
    //just straight up delete the thread
    //it shall be neither seen nor heard
    delete thr;
  } else {
    switch_to_thread(thr);
  }

  return now();
}

void
operating_system::join_thread(thread* t)
{
  if (t->get_state() != thread::DONE) {
    //key* k = key::construct();
    os_debug("joining thread %ld - thread not done so blocking on thread %p",
        t->thread_id(), active_thread_);
    t->joiners_.push(active_thread_);
    block();
  }
  else {
    os_debug("joining completed thread %ld", t->thread_id());
  }
}

void
operating_system::complete_active_thread()
{
  thread* thr_todelete = active_thread_;

  //if any threads waiting on the join, unblock them
  os_debug("completing thread %ld", thr_todelete->thread_id());
  while (!thr_todelete->joiners_.empty()) {
    thread* blocker = thr_todelete->joiners_.front();
    os_debug("thread %ld is unblocking joiner %p",
        thr_todelete->thread_id(), blocker);
    unblock(blocker);
    //to_awake_.push(thr_todelete->joiners_.front());
    thr_todelete->joiners_.pop();
  }
  active_thread_ = nullptr;

  //JJW 11/6/2014 This here is weird
  //The thread has run out of work and is terminating
  //However, because of weird thread swapping the DES thread
  //might still operate on the thread... we need to delay the delete
  //until the DES thread has completely finished processing its current event
  send_now_self_event_queue(new delete_thread_event(thr_todelete));

  os_debug("completing context for %ld", thr_todelete->thread_id());
  thr_todelete->context()->complete_context(des_context_);
}

void
operating_system::register_lib(library* lib)
{
#if SSTMAC_SANITY_CHECK
  if (lib->lib_name() == "") {
    sprockit::abort("operating_system: trying to register a lib with no name");
  }
#endif
  os_debug("registering lib %s:%p", lib->lib_name().c_str(), lib);
  int& refcount = lib_refcounts_[lib];
  ++refcount;
  libs_[lib->lib_name()] = lib;
  debug_printf(sprockit::dbg::dropped_events,
               "OS %d should no longer drop events for %s",
               addr(), lib->lib_name().c_str());

  auto iter = pending_library_events_.find(lib->lib_name());
  if (iter != pending_library_events_.end()){
    const std::list<event*> events = iter->second;
    for (event* ev : events){
      send_now_self_event_queue(new_callback(component_id(), lib, &library::incoming_event, ev));
    }
    pending_library_events_.erase(iter);
  }
}

void
operating_system::unregister_lib(library* lib)
{
  os_debug("unregistering lib %s", lib->lib_name().c_str());
  int& refcount = lib_refcounts_[lib];
  if (refcount == 1){
    lib_refcounts_.erase(lib);
    debug_printf(sprockit::dbg::dropped_events,
                 "OS %d will now drop events for %s",
                 addr(), lib->lib_name().c_str());
    libs_.erase(lib->lib_name());
    //delete lib;
  } else {
    --refcount;
  }
}

void
operating_system::kill_node()
{
  node_->fail_stop();
}

library*
operating_system::lib(const std::string& name) const
{
  auto it = libs_.find(name);
  if (it == libs_.end()) {
    return nullptr;
  }
  else {
    return it->second;
  }
}

void
operating_system::start_thread(thread* t)
{
#if SSTMAC_HAVE_GRAPHVIZ
  {
    void** stack = call_graph_->allocate_trace();
    t->set_backtrace(stack);
  }
#endif
  if (active_thread_){
    //crap - can't do this on this thread - need to do on DES thread
    send_now_self_event_queue(new_callback(this, &operating_system::start_thread, t));
  } else {
    active_thread_ = t;
    active_os() = this;
    app* parent = t->parent_app();
    void* stack = stack_alloc::alloc();
    t->init_thread(
      parent->params(),
      threadId(),
      des_context_,
      stack,
      stack_alloc::stacksize(),
      parent->globals_storage());
  }
}

void
operating_system::start_app(app* theapp, const std::string& unique_name)
{
  os_debug("starting app %d:%d on thread %d",
    int(theapp->tid()), int(theapp->aid()), thread_id());
  //this should be called from the actual thread running it
#if SSTMAC_HAVE_GRAPHVIZ
  {
    void** stack = call_graph_->allocate_trace();
    theapp->set_backtrace(stack);
  }
#endif
  init_threading(params_);
  if (params_->has_param("context")){
    theapp->params()->add_param("context", params_->get_param("context"));
  }
  if (ftq_trace_){
    ftq_trace_->register_app(theapp->aid(), sprockit::printf("app%d", int(theapp->aid())));
  }

  start_thread(theapp);
}

bool
operating_system::handle_library_event(const std::string& name, event* ev)
{
  auto it = libs_.find(name);
  bool found = it != libs_.end();
  if (found){
    library* lib = it->second;
    os_debug("delivering message to lib %s:%p\n%s",
        name.c_str(), lib, sprockit::to_string(ev).c_str());
    lib->incoming_event(ev);
  }
  return found;
}

void
operating_system::handle_event(event* ev)
{  
  //this better be an incoming event to a library, probably from off node
  library_interface* libmsg = test_cast(library_interface, ev);
  if (!libmsg) {
    spkt_throw_printf(sprockit::illformed_error,
      "operating_system::handle_event: got event %s instead of library event",
      sprockit::to_string(ev).c_str());
  }

  bool found = handle_library_event(libmsg->lib_name(), ev);
  if (!found){
    pending_library_events_[libmsg->lib_name()].push_back(ev);
  }
}

}
} //end of namespace sstmac
