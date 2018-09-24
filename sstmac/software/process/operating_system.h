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

#ifndef SSTMAC_SOFTWARE_PROCESS_operatingsystem_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_operatingsystem_H_INCLUDED


#include <sstmac/software/threading/threading_interface.h>
#include <sstmac/software/threading/stack_alloc.h>
#include <sstmac/software/ami/ami.h>
#include <sstmac/software/libraries/library.h>
#include <sstmac/software/process/thread_fwd.h>
#include <sstmac/software/process/app_fwd.h>
#include <sstmac/software/process/thread_info.h>
#include <sstmac/software/api/api_fwd.h>
#include <sstmac/software/launch/job_launcher_fwd.h>

#include <sstmac/common/messages/sst_message_fwd.h>
#include <sstmac/common/event_scheduler.h>

#include <sstmac/software/libraries/service_fwd.h>
#include <sstmac/software/process/ftq_fwd.h>
#include <sstmac/software/process/graphviz_fwd.h>
#include <sstmac/software/process/compute_scheduler_fwd.h>
#include <sstmac/software/process/global.h>
#include <sstmac/common/messages/sst_message_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <unordered_map>
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

  virtual ~operating_system();

  std::string to_string() const {
    return "operating system";
  }

  static inline operating_system*& static_os_thread_context(){
  #if SSTMAC_USE_MULTITHREAD
    int thr = thread_info::current_physical_thread_id();
    return active_os_[thr];
  #else
    return active_os_;
  #endif
  }

  inline operating_system*& active_os() {
#if SSTMAC_USE_MULTITHREAD
  return active_os_[thread_id_];
#else
  return active_os_;
#endif
  }

  thread* active_thread() const {
    return active_thread_;
  }

  int thread_id() const {
    return thread_id_;
  }

  int nthread() const {
    return nthread_;
  }

  static void delete_statics();

  static operating_system* current_os(){
    return static_os_thread_context();
  }

  static library* current_library(const std::string& name);

  static node_id current_node_id() {
    return static_os_thread_context()->addr();
  }

  static void set_gdb_hold(bool flag){
    hold_for_gdb_ = flag;
  }

  /**
   * @brief execute Execute a compute function.
   * This function MUST begin on a user-space thread
   * since it may block and context switch until completion.
   * To invoke compute operations for the main DES thread,
   * use execute_kernel
   * @param func  The function to perform
   * @param data  Event carrying all the data describing the compute
   * @param nthr  The number of threads that need to execute
   */
  void execute(ami::COMP_FUNC, event* data, int nthr = 1);

  /**
   * @brief execute Execute a communication function.
   * This function MUST begin on a user-space thread
   * since it may block and context switch until completion.
   * To invoke compute operations for the main DES thread,
   * use execute_kernel
   * @param data  Event carrying all the data describing the compute
   */
  void execute(ami::COMM_FUNC func, message* data){
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
   * @brief execute Enqueue an operation to perform
   * This function takes place in "kernel" land
   * and will never block and context switch.
   * This function can therefore run on the main DES thread.
   * The function must run asynchronously and immediately return
   * with no virtual time advancing.
   * @param func  The function to perform
   * @param data  Event carrying all the data describing the compute
   */
  void async_kernel(ami::SERVICE_FUNC func, event* data);
  
  /**
   * @brief block Block the currently running thread context.
   * This must be called from an application thread, NOT the DES thread
   * @param req [in-out] A key that will store blocking data that will be needed
   *                     for later unblocking
   * @return
   */
  void block();

  void block_timeout(timestamp delay);

  /**
   * @brief unblock Unblock the thread context associated with the key
   *        This must be called from the DES thread, NOT an application thread
   * @param req [in] The key storing blocking data that was previously passed into
   *                  a block(req) call
   * @return
   */
  timestamp unblock(thread* thr);

  void outcast_app_start(int my_rank, int aid, const std::string& app_ns,
                      task_mapping_ptr mapping, sprockit::sim_parameters* app_params,
                      bool include_root = false);

  /**
   * @brief start_thread Start a thread object and schedule the context switch to it
   * @param t The thread to start
   */
  void start_thread(thread* t);

  /**
   * @brief join_thread If this thread created a subthread, join with the given subthread.
   *                    This should be called from the application thread that spawned the subthread,
   *                    NOT the DES thread
   * @param subthread The spawned subthread to join
   */
  void join_thread(thread* subthread);

  /**
   * @brief complete_active_thread Must be called from a currently running application thread, not the DES thread.
   *                               This returns from the currently running context, closes it out,
   *                               and schedules resources associated with it (stack, etc) to be cleaned up.
   */
  void complete_active_thread();

  void schedule_thread_deletion(thread* thr);

  /**
   * @brief start_app
   * Similar to start_thread, but performs special operations associated
   *  with a full application rather than just a subthread.
   * @param a The application to start
   * @param unique_name A known name for identifying the process across multiple nodes
   */
  void start_app(app* a, const std::string& unique_name);

  static size_t stacksize(){
    return sstmac_global_stacksize;
  }

  static thread* current_thread(){
    return static_os_thread_context()->active_thread();
  }

  void handle_event(event* ev);

  static void shutdown() {
    current_os()->local_shutdown();
  }

  library* lib(const std::string& name) const;

  void print_libs(std::ostream& os = std::cout) const;

  hw::node* node() const {
    return node_;
  }

  node_id addr() const {
    return my_addr_;
  }

  graph_viz* call_graph() const {
    return call_graph_;
  }

  static void simulation_done();

  sprockit::sim_parameters* params() const {
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

  static void init_threads(int nthread);

  void kill_node();

  void decrement_app_refcount();

  void increment_app_refcount();

  void set_call_graph_active(bool flag){
    call_graph_active_ = flag;
  }

  bool call_graph_active() const {
    return call_graph_active_;
  }

  static void gdb_switch_to_thread(uint32_t thr_id);

  static void gdb_set_active(int flag){
    gdb_active_ = flag;
  }

  static void gdb_reset();

 private:
  thread_context* active_context();

  void switch_to_thread(thread* tothread);

  void init_threading(sprockit::sim_parameters* params);

  friend class library;

  void register_lib(library* lib);

  void unregister_lib(library* lib);

  void local_shutdown();

  bool handle_library_event(const std::string& name, event* ev);

 private:
  int thread_id_;
  int nthread_;
  hw::node* node_;
  std::unordered_map<std::string, library*> libs_;
  std::unordered_map<library*, int> lib_refcounts_;
  std::map<std::string, std::list<event*>> pending_library_events_;

  thread* active_thread_;

  node_id my_addr_;

  /// The caller context (main DES thread).  We go back
  /// to this context on every context switch.
  thread_context *des_context_;

  sprockit::sim_parameters* params_;

  compute_scheduler* compute_sched_;

  graph_viz* call_graph_;

  ftq_calendar* ftq_trace_;

  bool call_graph_active_;

  static std::unordered_map<uint32_t, thread*> all_threads_;
  static bool hold_for_gdb_;
  static thread_context* gdb_context_;
  static thread_context* gdb_original_context_;
  static thread_context* gdb_des_context_;
  static bool gdb_active_;

#if SSTMAC_USE_MULTITHREAD
  static std::vector<operating_system*> active_os_;
#else
  static operating_system* active_os_;
#endif

};

}
} //end of namespace sstmac
#endif
