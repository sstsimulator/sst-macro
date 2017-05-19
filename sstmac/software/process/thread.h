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

#ifndef SSTMAC_SOFTWARE_PROCESS_THREAD_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_THREAD_H_INCLUDED

#include <sstmac/common/node_address.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/software/process/process_context.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/software/api/api.h>
#include <sprockit/errors.h>

#include <sstmac/software/process/key_fwd.h>
#include <sstmac/software/process/app_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/process/thread_fwd.h>
#include <sstmac/software/libraries/library_fwd.h>
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

  static thread*
  current();

  template <class T>
  T*
  get_api() {
    api* a = _get_api(T::factory_string());
    T* casted = dynamic_cast<T*>(a);
    if (!casted) {
      spkt_throw_printf(sprockit::value_error,
               "Failed to cast API to correct type for %s",
                T::factory_string());
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
    return sid_.app_;
  }

  task_id tid() const {
    return sid_.task_;
  }

  software_id
  sid() const {
    return sid_;
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

  device_id
  event_location() const;

  void collect_backtrace(int nfxn);

  void
  init_thread(int phyiscal_thread_id,
    threading_interface* tocopy, void *stack, int stacksize,
    threading_interface *yield_to, void* globals_storage);

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

  void*
  get_tls_value(long thekey) const;

  void
  set_tls_value(long thekey, void* ptr);

  timestamp
  now();

 protected:
  thread(sprockit::sim_parameters* params, software_id sid, operating_system* os);

  friend api* static_get_api(const char *name);

  virtual api*
  _get_api(const char* name);

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
  /// Monitor state for deadlock detection.
  state state_;

  /// Each thread can only run under one OS/scheduler.
  operating_system* os_;

  std::queue<key*> joiners_;

  app* parent_app_; // who created this one. null if launch/os.

  process_context p_txt_;

  software_id sid_;

 private:
  bool isInit;

  void** backtrace_;

  int bt_nfxn_;

  std::map<long, void*> tls_values_;

  int last_bt_collect_nfxn_;

  /// The stack given to this thread.
  void* stack_;
  /// The stacksize.
  size_t stacksize_;
  
  long thread_id_;

  threading_interface* context_;

  /// This key gets used by the compute scheduler to delay this thread
  /// 
  key* schedule_key_;
  
  uint64_t cpumask_;
  
  int active_core_;

};

}
} // end of namespace sstmac
#endif