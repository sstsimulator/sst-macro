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

#ifndef SSTMAC_COMMON_EVENTMANAGER_H_INCLUDED
#define SSTMAC_COMMON_EVENTMANAGER_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_location.h>

#include <sstmac/common/messages/sst_message_fwd.h>
#include <sstmac/common/event_handler_fwd.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/stats/stat_collector.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/backends/common/sim_partition_fwd.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <sstmac/common/event_scheduler_fwd.h>
#include <sstmac/backends/native/manager_fwd.h>

#include <vector>


DeclareDebugSlot(event_manager);

namespace sstmac {

#if SSTMAC_INTEGRATED_SST_CORE
#else
/**
 * Base type for implementations of an engine that
 * is able to schedule events and advance simulation time
 * in the right order.
 */

class event_manager
{
  DeclareFactory(event_manager, parallel_runtime*)

  FactoryRegister("map", event_manager, event_manager,
      "Implements a basic event manager running in serial")

  friend class event_component;
  friend class event_subcomponent;
  friend class event_scheduler;
  friend class centralized_link;
  friend class native::manager;

 public:
  event_manager(sprockit::sim_parameters* params, parallel_runtime* rt);

  bool is_complete() {
    return complete_;
  }

  static const timestamp no_events_left_time;

  static event_manager* global;

  virtual ~event_manager(){}

  virtual void run();

  void cancel_all_messages(uint32_t component_id);

  void register_stat(
    stat_collector* stat,
    stat_descr_t* descr);

  stat_collector* register_thread_unique_stat(
    stat_collector* stat,
    stat_descr_t* descr);

  partition* topology_partition() const;

  parallel_runtime* runtime() const {
    return rt_;
  }

  void finish_stats();

  timestamp final_time() const {
    return final_time_;
  }

  bool scheduled() const {
    return scheduled_;
  }

  void set_scheduled(bool flag) {
    scheduled_ = flag;
  }

  /** 
   * @return The MPI rank of this event manager 
   * */
  int me() const {
    return me_;
  }

  int thread() const {
    return thread_;
  }

  void set_thread(int thr) {
    thread_ = thr;
  }

  int nproc() const {
    return nproc_;
  }

  int nworker() const {
    return nproc_ * nthread_;
  }

  int nthread() const {
    return nthread_;
  }

  virtual event_manager* thread_manager(int thr) const {
    return const_cast<event_manager*>(this);
  }

  void ipc_schedule(ipc_event_t* iev);

  void multithread_schedule(int srcThread, event_queue_entry* ev){
    (*outgoing_events_)[srcThread].push_back(ev);
  }

  void schedule(event_queue_entry* ev){
    event_queue_.insert(ev);
  }

  void set_interconnect(hw::interconnect* ic);

  void schedule_stop(timestamp until);

  /**
   * @brief run_events
   * @param event_horizon
   * @return Whether no more events or just hit event horizon
   */
  timestamp run_events(timestamp event_horizon);

  timestamp now() const {
    return now_;
  }

  const timestamp* now_ptr() const {
    return &now_;
  }

  void set_min_ipc_time(timestamp t){
    min_ipc_time_ = std::min(t,min_ipc_time_);
  }

  timestamp pull_min_ipc_time(){
    timestamp ret = min_ipc_time_;
    min_ipc_time_ = no_events_left_time;
    return ret;
  }

  timestamp min_event_time() const {
    return event_queue_.empty()
          ? no_events_left_time
          : (*event_queue_.begin())->time();
  }

 protected:
  void register_pending();

  virtual void finish_stats(stat_collector* main, const std::string& name);

  virtual timestamp receive_incoming_events(timestamp vote) {
    return vote;
  }

  std::vector<std::vector<event_queue_entry*>>* incoming_events_;
  std::vector<std::vector<event_queue_entry*>>* outgoing_events_;

  std::vector<std::vector<event_queue_entry*>> pending_events1_;
  std::vector<std::vector<event_queue_entry*>> pending_events2_;

  void swap_pending_event_queues(){
    std::swap(incoming_events_, outgoing_events_);
  }

 protected:
  bool complete_;
  timestamp final_time_;
  parallel_runtime* rt_;
  hw::interconnect* interconn_;
  bool scheduled_;

  // Dim-1: Num threads
  // Dim-2: List of events
  // Pair: event to schedule, scheduler to deliver to
  std::vector<std::vector<std::pair<event_queue_entry*,event_scheduler*>>> pending_events_;

  int me_;

  int nproc_;

  int nthread_;

  timestamp lookahead_;
  timestamp now_;

  int thread_;

 private:
  struct stats_entry {
    bool reduce_all;
    bool dump_all;
    bool dump_main;
    bool need_delete;
    stat_collector* main_collector;
    std::list<stat_collector*> collectors;
    stats_entry() : main_collector(nullptr), need_delete(false)
    {}
  };

#define MAX_EVENT_MGR_THREADS 128
  std::vector<event_scheduler*> pending_registration_[MAX_EVENT_MGR_THREADS];

 protected:
  timestamp min_ipc_time_;
  int thread_id_;

  struct event_compare {
    bool operator()(event_queue_entry* lhs, event_queue_entry* rhs) {
      bool neq = lhs->time() != rhs->time();
      if (neq) return lhs->time() < rhs->time();

      if (lhs->src_component_id() == rhs->src_component_id()){
        return lhs->seqnum() < rhs->seqnum();
      } else {
        return lhs->src_component_id() < rhs->src_component_id();
      }
    }
  };
  typedef std::set<event_queue_entry*, event_compare> queue_t;
  queue_t event_queue_;


  std::map<std::string, stats_entry> stats_;

};

class null_event_manager : public event_manager
{
 public:
  null_event_manager(sprockit::sim_parameters* params, parallel_runtime* rt) :
    event_manager(params, rt)
  {
  }

  void run() override {}

};
#endif

} // end of namespace sstmac


#endif
