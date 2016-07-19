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

#ifndef SSTMAC_SOFTWARE_PROCESS_THREAD_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_THREAD_H_INCLUDED

#include <sstmac/common/node_address.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/software/process/process_context.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/software/process/api.h>
#include <sprockit/errors.h>

#include <sstmac/software/launch/app_launch_fwd.h>
#include <sstmac/software/process/key_fwd.h>
#include <sstmac/software/process/app_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/process/thread_fwd.h>
#include <sstmac/software/libraries/library_fwd.h>
#include <sstmac/software/libraries/compute/lib_sleep_fwd.h>
#include <sstmac/software/api/api_fwd.h>
#include <sstmac/software/threading/threading_interface_fwd.h>

#include <queue>
#include <map>
#include <utility>
#include <list>

#include <unistd.h>

namespace sstmac {
namespace sw {


class thread
{
 public:
  friend class operating_system;
  friend class app;
  friend class delete_thread_event;

  /// Help resolve deadlock situations.
  enum state {
    PENDING=0,
    INITIALIZED=1,
    ACTIVE=2,
    SUSPENDED=3,
    BLOCKED=4,
    CANCELED=5,
    DONE=6
  };

  virtual std::string
  to_string() const {
    return "thread";
  }

  void
  set_api(thread* thr) {
    thr->apis_ = apis_;
  }

  virtual api*
  build_api(int aid, const std::string& name);

  virtual void
  init_os(operating_system* os);

  template <class T>
  void
  add_api(T* api) {
    if (API<T>::id == API<T>::null_id) {
      spkt_throw_printf(sprockit::illformed_error, "API %s is not initialized."
                       "Make sure API is statically initialized correctly"
                       " using ImplementAPI macro.",
                       API<T>::name);
    }
    apis_[API<T>::id] = api;
  }

  static thread*
  current();

  template <class T>
  T*
  get_api() {
    if (API<T>::id == API<T>::null_id) {
      spkt_throw_printf(sprockit::illformed_error, "API %s is not initialized."
                       "Make sure API is statically initialized correctly"
                       " using ImplementAPI macro.",
                       API<T>::name);
    }

    api* api = apis_[API<T>::id];
    if (!api) {
      api = build_api(API<T>::id, API<T>::name);
      apis_[API<T>::id] = api;
    }

    T* casted = dynamic_cast<T*>(api);
    if (!casted) {
      spkt_throw_printf(sprockit::value_error, "Failed to cast API of type %s",
                       API<T>::name);
    }
    return casted;
  }

  virtual app*
  parent_app() const {
    return parent_app_;
  }

  virtual void
  clear_subthread_from_parent_app();

  static const int no_core_affinity = -1;
  static const int no_socket_affinity = -1;
  static const int main_thread = -1;
  static const int nic_thread = -2;
  static const int rdma_thread = -3;
  static const app_id main_thread_aid;
  static const task_id main_thread_tid;


 public:
  virtual
  ~thread();

  /// Get current thread state.
  state
  get_state() const {
    return state_;
  }

  app_id aid() const {
    return aid_;
  }

  task_id tid() const {
    return tid_;
  }

  void
  spawn(thread* thr);

  long
  init_id();

  long
  thread_id() const {
    return thread_id_;
  }

  /**
   * This thread is not currently active - blocked on something
   * However, some kill event happened and I never want to see
   * this thread again.  Make sure the thread doesn't unblock
   * and clean up all resources associated with the thread
   */
  void
  cancel(){
    state_ = CANCELED;
  }

  bool
  is_canceled() const {
    return state_ == CANCELED;
  }

  virtual void
  kill();

  operating_system*
  os() const {
    return os_;
  }

  app_launch*
  env() const;

  void*
  stack() const {
    return stack_;
  }

  size_t
  stacksize() const {
    return stacksize_;
  }

  void**
  backtrace() const {
    return backtrace_;
  }

  int
  last_backtrace_nfxn() const {
    return last_bt_collect_nfxn_;
  }

  int
  backtrace_nfxn() const {
    return bt_nfxn_;
  }

  void
  append_backtrace(void* fxn);

  void
  pop_backtrace();

  void
  set_backtrace(void** bt) {
    backtrace_ = bt;
  }

  void collect_backtrace(int nfxn);

  void
  init_thread(int phyiscal_thread_id,
    threading_interface* tocopy, void *stack, int stacksize,
    operating_system* os, threading_interface *yield_to);

  /// Derived types need to override this method.
  virtual void
  run() = 0;

  /// A convenience request to start a new thread.
  /// The current thread has to be initialized for this to work.
  void
  start_thread(thread* thr);

  void
  join();

  process_context
  get_process_context() const {
    return p_txt_;
  }

  /**
   * @brief key used 
   * @return 
   */
  key*
  schedule_key() {
    return schedule_key_;
  }

  /// Test whether the current task has been initialized (activated)
  /// by a scheduler.
  bool
  is_initialized() const {
    return state_ >= INITIALIZED;
  }

  void
  register_lib(library* lib);

  void
  set_affinity(int core){
    zero_affinity();
    add_affinity(core);
  }
  
  void
  add_affinity(int core){
    cpumask_ = cpumask_ | (1<<core);
  }
  
  void
  zero_affinity(){
    cpumask_ = 0;
  }
  
  void
  set_cpumask(uint64_t cpumask){
    cpumask_ = cpumask;
  }
  
  uint64_t
  cpumask() const {
    return cpumask_;
  }
  
  int
  active_core() const {
    return active_core_;
  }
  
  void
  set_active_core(int core) {
    active_core_ = core;
  }
  
  typedef spkt_unordered_map<long, thread*> pthread_map_t;
  void
  set_pthread_map(pthread_map_t* threadmap){
    pthread_map_ = threadmap;
  }

  void*
  get_tls_value(long thekey) const;

  void
  set_tls_value(long thekey, void* ptr);

 protected:
  thread();

  void
  unregister_all_libs();

  timestamp
  now();

  node_id
  physical_address();

  void
  set_sid(const software_id& sid);
  
 private:
  /// Run routine that defines the initial context for this task.
  /// This routine calls the virtual thread::run method.
  static void
  run_routine(void* threadptr);

  /**
   * This should only ever be invoked by the delete thread event.
   * This ensures that the thread is completely done being operated on
   * It is now safe to free all resources (thread-local vars, etc)
   */
  void cleanup();

 protected:
  spkt_unordered_map<int, api*> apis_;

  /// Monitor state for deadlock detection.
  state state_;

  /// Each thread can only run under one OS/scheduler.
  operating_system* os_;

  std::queue<key*> joiners_;

  app* parent_app_; // who created this one. null if launch/os.

  process_context p_txt_;

 private:
  bool isInit;

  void** backtrace_;

  int bt_nfxn_;

  std::map<long, void*> tls_values_;

  pthread_map_t* pthread_map_;

  int last_bt_collect_nfxn_;

  /// The stack given to this thread.
  void *stack_;
  /// The stacksize.
  size_t stacksize_;

  app_id aid_;

  task_id tid_;
  
  long thread_id_;


  threading_interface* context_;

  /// This key gets used by the compute scheduler to delay this thread
  /// 
  key* schedule_key_;

  std::list<library*> pending_libs_;
  
  uint64_t cpumask_;
  
  int active_core_;

};

}
} // end of namespace sstmac
#endif

