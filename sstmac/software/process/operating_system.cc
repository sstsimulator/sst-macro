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

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/messages/library_message.h>
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

RegisterNamespaces("call_graph", "ftq");
RegisterKeywords(
{ "stack_size", "the size of stack to allocate to each user-space thread" },
{ "stack_chunk_size", "the block size to allocate in the memory pool when more stacks are needed" },
{ "ftq", "DEPRECATED: sets the fileroot of the FTQ statistic" },
{ "ftq_epoch", "DEPRECATED: sets the time epoch size for the FTQ statistic" },
{ "call_graph", "DEPRECATED: sets the fileroot of the call graph statistic" },
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
static stats_unique_tag cg_tag;

#if SSTMAC_USE_MULTITHREAD
std::vector<operating_system*> operating_system::active_os_;
#else
operating_system* operating_system::active_os_ = nullptr;
#endif

bool operating_system::hold_for_gdb_ = false;
thread_context* operating_system::gdb_context_ = nullptr;
thread_context* operating_system::gdb_original_context_ = nullptr;
thread_context* operating_system::gdb_des_context_ = nullptr;
std::unordered_map<uint32_t,thread*> operating_system::all_threads_;
bool operating_system::gdb_active_ = false;

operating_system::operating_system(sprockit::sim_parameters* params, hw::node* parent) :
#if SSTMAC_INTEGRATED_SST_CORE
  nthread_(1),
  thread_id_(0),
#else
  nthread_(parent->nthread()),
  thread_id_(parent->thread()),
#endif
  node_(parent),
  active_thread_(nullptr),
  call_graph_(nullptr),
  call_graph_active_(true), //on by default
  des_context_(nullptr),
  ftq_trace_(nullptr),
  compute_sched_(nullptr),
  event_subcomponent(parent),
  params_(params)
{
  my_addr_ = node_ ? node_->addr() : 0;

  compute_sched_ = compute_scheduler::factory::get_optional_param(
                     "compute_scheduler", "simple", params, this);

#if SSTMAC_HAVE_GRAPHVIZ
  stat_descr_t stat_descr;
  stat_descr.dump_all = false;
  stat_descr.unique_tag = &cg_tag;
  call_graph_ = optional_stats<graph_viz>(parent,
          params, "call_graph", "call_graph", &stat_descr);
#else
  if (params->has_namespace("call_graph")){
    spkt_abort_printf("cannot activate call graph collection - need to configure with --enable-graphviz");
  }
#endif

  ftq_trace_ = optional_stats<ftq_calendar>(parent, params, "ftq", "ftq");

  stack_alloc::init(params);

  if (node_) {
    compute_sched_->configure(node_->proc()->ncores(), node_->nsocket());
  } else {
    compute_sched_->configure(1, 1);
  }
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

#if SSTMAC_USE_MULTITHREAD
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
#endif

  des_context_ = thread_context::factory::get_optional_param(
        "context", thread_context::default_threading(), params);

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
  sw::unblock_event* ev = new sw::unblock_event(this, active_thread_);
  send_delayed_self_event_queue(t, ev);
  block();
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
  // guard the ftq tag in this function
  const auto& cur_tag = active_thread_->tag();
  ftq_scope scope(active_thread_,
      cur_tag.id() == ftq_tag::null.id()?ftq_tag::compute:cur_tag);

  //Make sure I have a core to execute on
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
operating_system::execute(ami::COMP_FUNC func, event *data, int nthr)
{
  int old_ncores = active_thread_->num_active_cores();
  active_thread_->set_num_active_cores(nthr);

  //this will block if the thread has no core to run on
  compute_sched_->reserve_core(active_thread_);
  //initiate the hardware events
  callback* cb = new_callback(this, &operating_system::unblock, active_thread_);

  switch (func) {
    case sstmac::ami::COMP_INSTR:
      node_->proc()->compute(data, cb);
      break;
    case sstmac::ami::COMP_TIME: {
      sw::timed_compute_event* ev = safe_cast(sw::timed_compute_event, data);
      send_delayed_self_event_queue(ev->data(), cb);
      break;
    }
    default:
      spkt_throw_printf(sprockit::spkt_error,
            "simplenode: cannot process kernel %s",
            ami::tostr(func));
  }

  block();
  compute_sched_->release_core(active_thread_);
  active_thread_->set_num_active_cores(old_ncores);
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

void
operating_system::block()
{
  timestamp before = now();
  //back to main DES thread
  thread_context* old_context = active_thread_->context();
  if (old_context == des_context_){
    spkt_abort_printf("blocking main DES thread on node %d", my_addr_);
  }
  thread* old_thread = active_thread_;
  //reset the time flag
  active_thread_->set_timed_out(false);
  active_thread_ = nullptr;
  old_context->pause_context(des_context_);

  while(hold_for_gdb_){
    sst_gdb_swap();  //do nothing - this is only for gdb purposes
  }

  //restore state to indicate this thread and this OS are active again
  active_os() = this;
  active_thread_ = old_thread;
  active_thread_->increment_block_counter();

   //collect any statistics associated with the elapsed time
  timestamp after = now();
  timestamp elapsed = after - before;

  if (call_graph_ && call_graph_active_) {
    call_graph_->count_trace(elapsed.ticks(), active_thread_);
  }

  if (ftq_trace_){
    ftq_tag tag = active_thread_->tag();
    ftq_trace_->collect(tag.id(),
      active_thread_->aid(), active_thread_->tid(),
      before.ticks(), elapsed.ticks());
    active_thread_->set_tag(ftq_tag::null);
  }
}

void
operating_system::block_timeout(timestamp delay)
{
  send_delayed_self_event_queue(delay, new timeout_event(this, active_thread_));
  block();
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
  } else {
    os_debug("joining completed thread %ld", t->thread_id());
  }
  delete t;
}

void
operating_system::schedule_thread_deletion(thread* thr)
{
  //JJW 11/6/2014 This here is weird
  //The thread has run out of work and is terminating
  //However, because of weird thread swapping the DES thread
  //might still operate on the thread... we need to delay the delete
  //until the DES thread has completely finished processing its current event
  send_now_self_event_queue(new delete_thread_event(thr));
}

void
operating_system::complete_active_thread()
{
  if (gdb_active_){
    all_threads_.erase(active_thread_->tid());
  }
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
operating_system::outcast_app_start(int my_rank, int aid, const std::string& app_ns,
                                  task_mapping::ptr mapping,  sprockit::sim_parameters* app_params,
                                  bool include_root)
{
  int num_ranks = mapping->num_ranks();
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
    int dst_nid = mapping->rank_to_node(dst_rank);
    sw::start_app_event* lev = new start_app_event(aid, app_ns,
                                     mapping, dst_rank, dst_nid,
                                     addr(), app_params);
    node_->get_nic()->send_to_logp_switch(lev);
  }
}

thread_context*
operating_system::active_context()
{
  if (active_thread_){
    return active_thread_->context();
  } else {
    return des_context_;
  }
}

void
operating_system::gdb_switch_to_thread(uint32_t id)
{
  thread* thr = all_threads_[id];
  if (thr){
    thread_context* from_context = nullptr;
    if (gdb_context_){
      from_context = gdb_context_;
    } else {
      operating_system* os = static_os_thread_context();
      if (os->active_thread()){
        from_context = os->active_thread()->context();
      } else {
        from_context = os->des_context_;
      }
    }

    if (!gdb_original_context_){
      gdb_original_context_ = from_context;
    }

    hold_for_gdb_ = true;
    from_context->jump_context(thr->context());
  } else {
    std::cerr << "Invalid rank " << id << std::endl;
  }
}

void
operating_system::gdb_reset()
{
  hold_for_gdb_ = false;
  if (!gdb_original_context_){
    std::cerr << "Cannot reset GDB/LLDB - never selected thread" << std::endl;
    return;
  }

  thread_context* orig = gdb_original_context_;
  thread_context* current = gdb_context_;
  gdb_original_context_ = nullptr;
  gdb_context_ = nullptr;

  current->jump_context(orig);
}

void
operating_system::start_thread(thread* t)
{
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
      parent->globals_storage(),
      parent->new_tls_storage());
  }

  if (gdb_active_){
    static thread_lock all_threads_lock;
    all_threads_lock.lock();
    auto tid = t->tid();
    thread*& thrInMap = all_threads_[tid];
    if (thrInMap){
      spkt_abort_printf("error: thread %d already exists for app %d",
                        t->tid(), thrInMap->aid());
    }
    thrInMap = t;
    all_threads_lock.unlock();
  }
}

void
operating_system::start_app(app* theapp, const std::string& unique_name)
{
  os_debug("starting app %d:%d on thread %d",
    int(theapp->tid()), int(theapp->aid()), thread_id());
  //this should be called from the actual thread running it
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
