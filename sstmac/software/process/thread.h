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

#ifndef SSTMAC_SOFTWARE_PROCESS_THREAD_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_THREAD_H_INCLUDED

#include <sstmac/common/node_address.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/software/process/process_context.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/software/api/api.h>
#include <sprockit/errors.h>

#include <sstmac/software/process/graphviz.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/app_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/process/thread_fwd.h>
#include <sstmac/software/process/host_timer.h>
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

/**
 * @brief The thread class
 * Encapsulates all the state associated with a simulated thread within SST/macro
 * Not to be confused with thread_context, which just manages the details
 * of context-switching between user space threads.
 */
class thread
{
 public:
  class kill_exception : public std::exception {};
  class clean_exit_exception: public std::exception {};

  friend class operating_system;
  friend class app;
  friend class delete_thread_event;
  friend class ftq_scope;

  enum detach_t {
    JOINABLE=0,
    DETACHED=1
  };

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

  static thread* current();

  template <class T>
  T* get_api() {
    api* a = _get_api(T::factory_string());
    T* casted = dynamic_cast<T*>(a);
    if (!casted) {
      spkt_abort_printf("Failed to cast API to correct type for %s: got %s",
                T::factory_string(), typeid(a).name());
    }
    return casted;
  }

  virtual app* parent_app() const {
    return parent_app_;
  }

  static constexpr int no_core_affinity = -1;
  static constexpr int no_socket_affinity = -1;
  static constexpr uint32_t main_thread = -1;
  static constexpr uint32_t nic_thread = -2;
  static constexpr uint32_t rdma_thread = -3;
  static constexpr app_id main_thread_aid = -1;
  static constexpr task_id main_thread_tid = -1;
  static constexpr int use_omp_num_threads = -1;

 public:
  virtual ~thread();

  detach_t detach_state() const {
    return detach_state_;
  }

  void set_detach_state(detach_t d) {
    detach_state_= d;
  }

  state get_state() const {
    return state_;
  }

  app_id aid() const {
    return sid_.app_;
  }

  task_id tid() const {
    return sid_.task_;
  }

  software_id sid() const {
    return sid_;
  }

  thread_context* context() const {
    return context_;
  }

  void spawn(thread* thr);

  uint32_t init_id();

  uint32_t thread_id() const {
    return thread_id_;
  }

  /**
   * This thread is not currently active - blocked on something
   * However, some kill event happened and I never want to see
   * this thread again.  Make sure the thread doesn't unblock
   * and clean up all resources associated with the thread
   */
  void cancel(){
    state_ = CANCELED;
  }

  bool is_canceled() const {
    return state_ == CANCELED;
  }

  void set_pthread_concurrency(int lvl){
    pthread_concurrency_ = lvl;
  }

  int pthread_concurrency() const {
    return pthread_concurrency_;
  }

  /**
   * This can get called by anyone to have a thread exit, including during normal app termination
   * This must be called while running on this thread's context, NOT the DES thread or any other thread
   */
  void kill(int code = 1) {
    if (code == 0){
      throw clean_exit_exception();
    } else {
      throw kill_exception();
    }
  }

  operating_system* os() const {
    return os_;
  }

#if SSTMAC_HAVE_GRAPHVIZ
  const int* backtrace() const {
    return backtrace_;
  }
#endif


  int last_backtrace_nfxn() const {
    return last_bt_collect_nfxn_;
  }

  int backtrace_nfxn() const {
    return bt_nfxn_;
  }

  bool timed_out() const {
    return timed_out_;
  }

  void set_timed_out(bool flag){
    timed_out_ = flag;
  }

  uint64_t block_counter() const {
    return block_counter_;
  }

  void increment_block_counter() {
    ++block_counter_;
  }

  void append_backtrace(int fxnId);

  void pop_backtrace();

  uint32_t component_id() const;

  void collect_backtrace(int nfxn);

  void init_thread(sprockit::sim_parameters* params, int phyiscal_thread_id,
    thread_context* tocopy, void *stack, int stacksize,
    void* globals_storage, void* tls_storage);

  virtual void run() = 0;

  /** A convenience request to start a new thread.
  *  The current thread has to be initialized for this to work.
  */
  void start_thread(thread* thr);

  void set_thread_id(int thr);

  void join();

  process_context get_process_context() const {
    return p_txt_;
  }

  bool is_initialized() const {
    return state_ >= INITIALIZED;
  }

  void set_affinity(int core){
    zero_affinity();
    add_affinity(core);
  }
  
  void add_affinity(int core){
    cpumask_ = cpumask_ | (1<<core);
  }
  
  void zero_affinity(){
    cpumask_ = 0;
  }

  void set_cpumask(uint64_t cpumask){
    cpumask_ = cpumask;
  }
  
  uint64_t cpumask() const {
    return cpumask_;
  }
  
  uint64_t active_core_mask() const {
    return active_core_mask_;
  }

  void set_active_core_mask(uint64_t mask){
    active_core_mask_ = mask;
  }

  int num_active_cores() const {
    return num_active_cores_;
  }

  void set_num_active_cores(int ncores) {
    num_active_cores_ = ncores;
  }

  void compute_detailed(uint64_t flops, uint64_t intops,
                        uint64_t bytes, int nthread=use_omp_num_threads);

  int omp_get_thread_num() const {
    auto& active = omp_contexts_.back();
    return active.id;
  }

  int omp_get_num_threads() const {
    auto& active = omp_contexts_.back();
    return active.num_threads;
  }

  int omp_get_max_threads() const {
    auto& active = omp_contexts_.back();
    return active.max_num_subthreads;
  }

  int omp_get_ancestor_thread_num() const {
    auto& active = omp_contexts_.back();
    return active.parent_id;
  }

  void omp_set_num_threads(int thr) {
    auto& active = omp_contexts_.back();
    active.requested_num_subthreads = thr;
  }

  int omp_get_level() const {
    auto& active = omp_contexts_.back();
    return active.level;
  }

  int omp_in_parallel() {
    auto& active = omp_contexts_.back();
    bool parallel = active.level > 0;
    return parallel ? 1 : 0;
  }

  void* get_tls_value(long thekey) const;

  void set_tls_value(long thekey, void* ptr);

  timestamp now();

  void start_api_call();

  void end_api_call();

  void set_tag(ftq_tag t){
    if (!protect_tag)
        ftag_ = t;
  }

  ftq_tag tag() const {
    return ftag_;
  }

  void spawn_omp_parallel();

 protected:
  thread(sprockit::sim_parameters* params, software_id sid, operating_system* os);

  friend api* static_get_api(const char *name);

  virtual api* _get_api(const char* name);

 private:
  struct omp_context {
    omp_context* parent;
    int level;
    int id;
    int parent_id;
    int num_threads;
    int requested_num_subthreads;
    int max_num_subthreads;
    std::vector<thread*> subthreads;
    omp_context() :
      parent(nullptr), id(0), parent_id(-1),
      num_threads(1), max_num_subthreads(1)
    {}
  };

  /// Run routine that defines the initial context for this task.
  /// This routine calls the virtual thread::run method.
  static void run_routine(void* threadptr);

  void set_omp_parent_context(int id, const omp_context& parent);

  /**
   * This should only ever be invoked by the delete thread event.
   * This ensures that the thread is completely done being operated on
   * It is now safe to free all resources (thread-local vars, etc)
   */
  virtual void cleanup();

 protected:
  state state_;

  operating_system* os_;

  std::queue<thread*> joiners_;

  app* parent_app_; // who created this one. null if launch/os.

  process_context p_txt_;

  ftq_tag ftag_;

  bool protect_tag;

  software_id sid_;

  HostTimer* host_timer_;

 private:
  bool isInit;

#if SSTMAC_HAVE_GRAPHVIZ
  graphviz_trace backtrace_; //each function is labeled by unique integer
#endif

  int bt_nfxn_;

  bool timed_out_;

  std::map<long, void*> tls_values_;

  int last_bt_collect_nfxn_;

  void* stack_;

  char* tls_storage_;
  
  uint32_t thread_id_;

  thread_context* context_;
  
  uint64_t cpumask_;
  
  uint64_t active_core_mask_;

  int num_active_cores_;

  uint64_t block_counter_;

  int pthread_concurrency_;

  detach_t detach_state_;

  std::list<omp_context> omp_contexts_;

};

}
} // end of namespace sstmac
#endif
