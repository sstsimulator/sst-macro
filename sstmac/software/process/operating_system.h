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
#include <sstmac/software/api/api_fwd.h>

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/stats/event_trace.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/thread_info.h>

#include <sstmac/software/libraries/service_fwd.h>
#include <sstmac/software/process/ftq_fwd.h>
#include <sstmac/software/process/graphviz_fwd.h>
#include <sstmac/software/process/compute_scheduler_fwd.h>

#include <sstmac/common/messages/sst_message_fwd.h>

#include <sstmac/hardware/node/node_fwd.h>

#include <sprockit/unordered.h>
#include <sprockit/debug.h>
#include <stack>
#include <queue>

DeclareDebugSlot(os);

namespace sstmac {
namespace sw {

class operating_system :
  public event_subcomponent
{
  friend class service;
  friend class thread;

 public:
  operating_system(sprockit::sim_parameters* params, hw::node* parent);

  struct os_thread_context {
    thread* current_thread;
    std::list<thread*> to_delete;
    operating_system* current_os;
    stack_alloc stackalloc;
    app_id current_aid;
    task_id current_tid;
  };

  virtual ~operating_system();

  std::string
  to_string() const {
    return "operating system";
  }

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

  /**
   * @brief execute Execute a compute function.
   * This function MUST begin on a user-space thread
   * since it may block and context switch until completion.
   * To invoke compute operations for the main DES thread,
   * use execute_kernel
   * @param func  The function to perform
   * @param data  Event carrying all the data describing the compute
   * @param cat   An optional category labeling the type of
   *              operation
   */
  void
  execute(ami::COMP_FUNC, event* data,
          key::category cat = key::general);

  /**
   * @brief execute Execute a communication function.
   * This function MUST begin on a user-space thread
   * since it may block and context switch until completion.
   * To invoke compute operations for the main DES thread,
   * use execute_kernel
   * @param data  Event carrying all the data describing the compute
   */
  void
  execute(ami::COMM_FUNC func, message* data){
    return execute_kernel(func, data);
  }

  /**
   * @brief execute Execute a compute function.
   * This function takes place in "kernel" land
   * and will never block and context switch.
   * This function can therefore run on the main DES thread
   * @param func  The function to perform
   * @param data  Event carrying all the data describing the compute
   * @return A return code specifying success or failure
   */
  void execute_kernel(ami::COMM_FUNC func, message* data);

  /**
   * @brief execute Execute a communication function.
   * This function takes place in "kernel" land
   * and will never block and context switch.
   * This function can therefore run on the main DES thread
   * @param func  The function to perform
   * @param data  Event carrying all the data describing the compute
   * @param cb    The callback to invoke when the kernel is complete
   */
  void
  execute_kernel(ami::COMP_FUNC func,
                 event* data,
                 callback* cb);
  /**
   * @brief execute Enqueue an operation to perform
   * This function takes place in "kernel" land
   * and will never block and context switch.
   * This function can therefore run on the main DES thread.
   * The function must run asynchronously and immediately return
   * with no virtual time advancing.
   * @param func  The function to perform
   * @param data  Event carrying all the data describing the compute
   */
  void
  async_kernel(ami::SERVICE_FUNC func,
               event* data);

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

  library*
  lib(const std::string& name) const;

  void
  add_application(app* a);

  void
  start_app(app* a, const std::string& unique_name);

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

  static void
  shutdown() {
    current_os()->local_shutdown();
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
  start_api_call();

  void
  schedule_timeout(timestamp delay, key* k);

  void
  free_thread_stack(void* stack);

  static size_t
  stacksize(){
    return stacksize_;
  }

  static thread*
  current_thread();

  graph_viz*
  call_graph() const {
    return call_graph_;
  }

  static void
  simulation_done();

  sprockit::sim_parameters*
  params() const {
    return params_;
  }

  /**
   * @brief sleep Sleep for a specified delay. Sleeps do not require
   *        core reservation, unlike #compute. Sleeps always begin immediately.
   * @param sleep_delay The length of time to sleep (delta T)
   */
  void sleep(timestamp sleep_delay);

  /**
   * @brief sleep_until Sleep until a specified time. If that time has already been reached
   *          return immediately. Otherwise block until the time arrives.
   * @param t The time to sleep until
   */
  void sleep_until(timestamp t);

  /**
   * @brief compute Compute for a specified time period. This requires
   *        a core to be reserved. If no cores are available,
   *        block until a core becomes available.
   * @param t The length of time to compute (delta T)
   */
  void compute(timestamp t);

  void kill_node();

  void decrement_app_refcount();

  void increment_app_refcount();

 private:
  void
  add_thread(thread* t);

  void
  switch_to_thread(thread_data_t tothread);

  void
  init_threading();

  os_thread_context&
  current_os_thread_context();


  friend class library;

  void
  register_lib(library* lib);

  void
  unregister_lib(library* lib);

  void
  local_shutdown();

  bool handle_library_event(const std::string& name, event* ev);
  
 private:
  hw::node* node_;
  spkt_unordered_map<std::string, library*> libs_;
  spkt_unordered_map<library*, int> lib_refcounts_;
  spkt_unordered_map<void*, std::list<library*> > libs_by_owner_;
  std::map<std::string, std::list<event*>> pending_library_events_;

  node_id my_addr_;

  std::list<thread*> threads_;

  std::list<api*> services_;

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

  graph_viz* call_graph_;

  ftq_calendar* ftq_trace_;

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

