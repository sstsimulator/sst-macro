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

#ifndef SSTMAC_SOFTWARE_PROCESS_operatingsystem_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_operatingsystem_H_INCLUDED


#include <sstmac/software/threading/threading_interface.h>
#include <sstmac/software/threading/stack_alloc.h>
#include <sstmac/software/ami/ami.h>
#include <sstmac/software/libraries/library.h>
#include <sstmac/software/process/thread_fwd.h>
#include <sstmac/software/process/app_fwd.h>

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/stats/event_trace.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/thread_info.h>


#include <sstmac/software/libraries/service_fwd.h>
#include <sstmac/software/process/ftq_fwd.h>
#include <sstmac/software/process/graphviz_fwd.h>
#include <sstmac/software/launch/app_launch_fwd.h>
#include <sstmac/software/process/compute_scheduler_fwd.h>

#include <sstmac/hardware/node/node_fwd.h>

#include <sprockit/unordered.h>
#include <sprockit/debug.h>
#include <stack>
#include <queue>

DeclareDebugSlot(os);

namespace sstmac {
namespace sw {

class operating_system :
  public event_subscheduler
{
  friend class service;
  friend class thread;

 public:
  struct os_thread_context {
    thread* current_thread;
    std::list<thread*> to_delete;
    operating_system* current_os;
    stack_alloc stackalloc;
    app_id current_aid;
    task_id current_tid;
  };

  virtual ~operating_system();

  static operating_system*
  construct(sprockit::sim_parameters* params);

  static inline os_thread_context&
  static_os_thread_context() {
    if (cxa_finalizing_){
      abort();
    }
  #if SSTMAC_USE_MULTITHREAD
    int thr = thread_info::current_physical_thread_id();
    return os_thread_contexts_[thr];
  #else
    return os_thread_context_;
  #endif
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  finalize_init();

  static void
  delete_statics();

  static void
  switch_to_context(int aid, int tid);

  static operating_system*
  current_os();

  static app_id
  current_aid();

  static task_id
  current_tid();

  static library*
  current_library(const std::string& name);

  static node_id
  current_node_id();

  static node_id
  remote_node(int tid);

  void
  execute_kernel(ami::COMP_FUNC func, event* data);

  void
  execute_kernel(ami::COMM_FUNC func, message* data);

  bool
  kernel_supported(ami::COMP_FUNC) const;

  bool
  kernel_supported(ami::COMM_FUNC) const;

  static void
  stack_check();
  
  timestamp
  block(key* req);

  timestamp
  unblock(key* req);

  void
  start_thread(thread* t);

  void
  join_thread(thread* t);

  void
  complete_thread(bool succ);

  void
  register_lib(void* owner, library* lib);

  void
  unregister_all_libs(void* owner);

  library*
  lib(const std::string& name) const;
  
  void
  set_ncores(int ncores, int nsocket);

  void
  set_event_parent(event_scheduler* man);

  node_id my_addr() const;

  long
  task_threadid(const task_id& id) const;

  bool
  is_task_here(const task_id &id) const;

  void
  add_application(app* a);

  void
  add_task(const task_id& tid);

  void
  start_app(app* a);

  void
  handle_event(event* ev);

  std::list<app*>
  app_ptrs(app_id aid);

  app*
  app_ptr(software_id sid);

  thread_data_t
  current_context() const {
    return threadstack_.top();
  }

  void
  print_libs(std::ostream& os = std::cout) const;

  long
  current_threadid() const;

  void
  set_node(sstmac::hw::node* n){
    node_ = n;
  }

  hw::node*
  node() const {
    return node_;
  }

  node_id
  addr() const {
    return my_addr_;
  }

  void
  set_addr(node_id addr) {
    my_addr_ = addr;
    init_loc_id(event_loc_id(addr));
  }

  void
  schedule_unblock_now(key* k);

  void
  schedule_timeout(timestamp delay, key* k);

  void
  add_blocker(key* req);

  void
  remove_blocker(key* req);

  void
  free_thread_stack(void* stack);

  static size_t
  stacksize(){
    return stacksize_;
  }

  static thread*
  current_thread();

  static void
  simulation_done();

  sprockit::sim_parameters*
  params() const {
    return params_;
  }

  void
  sleep(timestamp t);

  void kill_node();

 private:
  operating_system();

  void
  add_thread(thread* t);

  void
  switch_to_thread(thread_data_t tothread);

  void
  init_threading();

  void
  init_startup_libs();

  os_thread_context&
  current_os_thread_context();

  void
  register_lib(library* lib);

  void
  unregister_lib(library* lib);
  
 private:
  hw::node* node_;
  spkt_unordered_map<std::string, library*> libs_;
  spkt_unordered_map<library*, int> lib_refcounts_;
  spkt_unordered_map<void*, std::list<library*> > libs_by_owner_;
  spkt_unordered_set<std::string> deleted_libs_;

  node_id my_addr_;

  std::list<thread*> threads_;

  std::vector<std::string> startup_libs_;

  std::stack<thread_data_t> threadstack_;

  int current_thread_id_;

  int next_msg_id_;

  spkt_unordered_map<task_id, long> task_to_thread_;

  std::queue<thread*> to_awake_; // from thread join

  /// The caller context (main DES thread).  We go back
  /// to this context on every context switch.
  threading_interface *des_context_;

  sprockit::sim_parameters* params_;

  compute_scheduler* compute_sched_;

  static graph_viz* call_graph_;

  ftq_calendar* ftq_trace_;

  event_trace* event_trace_;

#if SSTMAC_USE_MULTITHREAD
  static std::vector<operating_system::os_thread_context> os_thread_contexts_;
#else
  static operating_system::os_thread_context os_thread_context_;
#endif


#if SSTMAC_SANITY_CHECK
  std::set<key*> valid_keys_;
#endif

 private:
  static size_t stacksize_;
  static bool cxa_finalizing_;
  static os_thread_context cxa_finalize_context_;

};

}
} //end of namespace sstmac
#endif

