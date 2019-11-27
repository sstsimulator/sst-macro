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
#include <sstmac/hardware/common/flow_fwd.h>
#include <sstmac/hardware/network/network_message_fwd.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/stats/stat_collector.h>

#include <sstmac/software/libraries/service_fwd.h>
#include <sstmac/software/process/compute_scheduler_fwd.h>
#include <sstmac/software/process/global.h>
#include <sstmac/hardware/common/flow_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <unordered_map>
#include <sprockit/debug.h>
#include <sprockit/sim_parameters.h>
#include <stack>
#include <queue>
#include <functional>

#include <sstmac/software/process/ipc_tunnel.h>

DeclareDebugSlot(os);

extern "C" double sstmacWallTime();

namespace sstmac {
namespace sw {

class OperatingSystem : public SubComponent
{
  friend class Service;
  friend class Thread;

 public:
#if SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_REGISTER_SUBCOMPONENT_API(sstmac::sw::OperatingSystem,
                                    hw::Node*)

  SST_ELI_REGISTER_SUBCOMPONENT_DERIVED(
    OperatingSystem,
    "macro",
    "os",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "the operating system for a node",
    sstmac::sw::OperatingSystem
  )
#endif
  OperatingSystem(uint32_t id, SST::Params& params, hw::Node* parent);

  virtual ~OperatingSystem();

  std::string toString() const override {
    return "operating system";
  }

  static inline OperatingSystem*& staticOsThreadContext(){
  #if SSTMAC_USE_MULTITHREAD
    int thr = ThreadInfo::currentPhysicalThreadId();
    return active_os_[thr];
  #else
    return active_os_;
  #endif
  }

  inline OperatingSystem*& activeOs() {
#if SSTMAC_USE_MULTITHREAD
  return active_os_[threadId()];
#else
  return active_os_;
#endif
  }

  Thread* activeThread() const {
    return active_thread_;
  }

  ShadowPuppetSync* syncTunnel() const {
    return sync_tunnel_->get();
  }

  void reassignCores(Thread* thr);

  static void deleteStatics();

  static OperatingSystem* currentOs(){
    return staticOsThreadContext();
  }

  static Library* currentLibrary(const std::string& name);

  static NodeId currentNodeId() {
    return staticOsThreadContext()->addr();
  }

  static void setGdbHold(bool flag){
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
  void execute(ami::COMP_FUNC, Event* data, int nthr = 1);

  std::function<void(hw::NetworkMessage*)> nicDataIoctl();

  std::function<void(hw::NetworkMessage*)> nicCtrlIoctl();
  
  /**
   * @brief block Block the currently running thread context.
   * This must be called from an application thread, NOT the DES thread
   * @param req [in-out] A key that will store blocking data that will be needed
   *                     for later unblocking
   * @return
   */
  void block();

  void blockTimeout(TimeDelta delay);

  /**
   * @brief unblock Unblock the thread context associated with the key
   *        This must be called from the DES thread, NOT an application thread
   * @param req [in] The key storing blocking data that was previously passed into
   *                  a block(req) call
   * @return
   */
  void unblock(Thread* thr);

  void outcastAppStart(int my_rank, int aid, const std::string& app_ns,
                      task_mapping_ptr mapping,
                      const SST::Params& app_params,
                      bool include_root = false);

  /**
   * @brief start_thread Start a thread object and schedule the context switch to it
   * @param t The thread to start
   */
  void startThread(Thread* t);

  /**
   * @brief join_thread If this thread created a subthread, join with the given subthread.
   *                    This should be called from the application thread that spawned the subthread,
   *                    NOT the DES thread
   * @param subthread The spawned subthread to join
   */
  void joinThread(Thread* subthread);

  /**
   * @brief complete_active_thread Must be called from a currently running application thread, not the DES thread.
   *                               This returns from the currently running context, closes it out,
   *                               and schedules resources associated with it (stack, etc) to be cleaned up.
   */
  void completeActiveThread();

  void scheduleThreadDeletion(Thread* thr);

  /**
   * @brief start_app
   * Similar to start_thread, but performs special operations associated
   *  with a full application rather than just a subthread.
   * @param a The application to start
   * @param unique_name A known name for identifying the process across multiple nodes
   */
  void startApp(App* a, const std::string& unique_name);

  static size_t stacksize(){
    return sstmac_global_stacksize;
  }

  static Thread* currentThread(){
    return staticOsThreadContext()->activeThread();
  }

  void handleRequest(Request* req);

  static void shutdown() {
    currentOs()->localShutdown();
  }

  Library* lib(const std::string& name) const;

  void printLibs(std::ostream& os = std::cout) const;

  hw::Node* node() const {
    return node_;
  }

  NodeId addr() const {
    return my_addr_;
  }

  static void simulationDone();

  SST::Params& params() {
    return params_;
  }

  std::string hostname() const;

  std::map<std::string,std::string>::const_iterator env_begin() const {
    return env_.begin();
  }

  std::map<std::string,std::string>::const_iterator env_end() const {
    return env_.end();
  }

  /**
   * @brief sleep Sleep for a specified delay. Sleeps do not require
   *        core reservation, unlike #compute. Sleeps always begin immediately.
   * @param sleep_delay The length of time to sleep (delta T)
   */
  void sleep(TimeDelta sleep_delay);

  /**
   * @brief sleep_until Sleep until a specified time. If that time has already been reached
   *          return immediately. Otherwise block until the time arrives.
   * @param t The time to sleep until
   */
  void sleepUntil(Timestamp t);

  /**
   * @brief compute Compute for a specified time period. This requires
   *        a core to be reserved. If no cores are available,
   *        block until a core becomes available.
   * @param t The length of time to compute (delta T)
   */
  void compute(TimeDelta t);

  static void initThreads(int nthread);

  void killNode();

  void decrementAppRefcount();

  void incrementAppRefcount();

  void unblockBlockedThread();

  void setIpcName(const std::string& name);

  static void gdbSwitchToThread(uint32_t thr_id);

  static void gdbSetActive(int flag){
    gdb_active_ = flag;
  }

  static void gdbReset();

 private:
  ThreadContext* activeContext();

  void switchToThread(Thread* tothread);

  void initThreading(SST::Params& params);

  friend class Library;

  void registerLib(Library* lib);

  void unregisterLib(Library* lib);

  void localShutdown();

  bool handleLibraryRequest(const std::string& name, Request* req);

  struct CoreAllocateGuard {
    CoreAllocateGuard(OperatingSystem* os, Thread* thr) :
      os_(os),
      thr_(thr)
    {
      os->allocateCore(thr);
    }

    ~CoreAllocateGuard(){
      os_->deallocateCore(thr_);
    }

    OperatingSystem* os_;
    Thread* thr_;
  };


 private:
  friend struct CoreAllocateGuard;
  void allocateCore(Thread* thr);
  void deallocateCore(Thread* thr);


  hw::Node* node_;
  std::unordered_map<std::string, Library*> libs_;
  std::unordered_map<Library*, int> lib_refcounts_;
  std::map<std::string, std::list<Request*>> pending_library_request_;
  std::map<std::string, std::string> env_;

  Thread* active_thread_;
  Thread* blocked_thread_;

  NodeId my_addr_;

  /// The caller context (main DES thread).  We go back
  /// to this context on every context switch.
  ThreadContext *des_context_;

  SST::Params params_;

  ComputeScheduler* compute_sched_;

  static std::unordered_map<uint32_t, Thread*> all_threads_;
  static bool hold_for_gdb_;
  static ThreadContext* gdb_context_;
  static ThreadContext* gdb_original_context_;
  static ThreadContext* gdb_des_context_;
  static bool gdb_active_;

#if SSTMAC_USE_MULTITHREAD
  static std::vector<OperatingSystem*> active_os_;
#else
  static OperatingSystem* active_os_;
#endif

  IPCTunnel<ShadowPuppetSync>* sync_tunnel_;

};

}
} //end of namespace sstmac
#endif
