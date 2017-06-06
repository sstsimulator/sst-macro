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
  friend class event_component;
  friend class event_subcomponent;
  friend class event_scheduler;
  friend class native::manager;

 public:
  bool
  is_complete() {
    return complete_;
  }

  static event_manager* global;

  /// Goodbye.
  virtual ~event_manager(){}

  /// Clear all events and set time back to a zero of your choice.
  /// This call shall not be permitted while the event manager is running.
  virtual void
  clear(timestamp zero_time = timestamp(0)) = 0;

  /// Run the eventmanager.
  /// The eventmanager shall return control when no more messages remain.
  virtual void
  run() = 0;

  virtual bool
  empty() const = 0;

  timestamp
  now() const {
    return now_;
  }

  void
  register_stat(
    stat_collector* stat,
    stat_descr_t* descr);

  stat_collector*
  register_thread_unique_stat(
    stat_collector* stat,
    stat_descr_t* descr);

  virtual void
  cancel_all_messages(device_id canceled_loc) = 0;

  partition*
  topology_partition() const;

  parallel_runtime*
  runtime() const {
    return rt_;
  }

  void
  finish_stats();

  void
  stop() {
    stopped_ = true;
  }

  /** 
   * @return The MPI rank of this event manager 
   * */
  int
  me() const {
    return me_;
  }

  /**
   * @return The unique worker id amongst all threads on all ranks
   */
  int
  worker_id() const {
    return me_ * nthread_ + thread_id_;
  }

  int
  nproc() const {
    return nproc_;
  }

  int
  nworker() const {
    return nproc_ * nthread_;
  }


  // ---- These are interface functions for PDES, they should
  // ----   only get called when running in parallel mode
  virtual void
  ipc_schedule(
    timestamp t,
    device_id dst,
    device_id src,
    uint32_t seqnum,
    event* ev);

  virtual void
  multithread_schedule(
    int srcthread,
    int dstthread,
    uint32_t seqnum,
    event_queue_entry* ev);

  virtual int
  lpid() const {
    return -1;
  }

  int
  thread_id() const {
    return thread_id_;
  }

  int
  nthread() const {
    return nthread_;
  }

  virtual event_manager*
  ev_man_for_thread(int thread_id) const;

  virtual void
  set_interconnect(hw::interconnect* interconn){}

  virtual void
  schedule_stop(timestamp until);

 protected:
  event_manager(sprockit::sim_parameters* params, parallel_runtime* rt);

  void
  set_now(const timestamp &ts);

  virtual void
  finish_stats(stat_collector* main, const std::string& name, timestamp end);

 protected:
  bool complete_;
  bool stopped_;
  bool finish_on_stop_;
  parallel_runtime* rt_;
  int thread_id_;

  int me_;

  int nproc_;

  int nthread_;

 private:
  struct stats_entry {
    bool reduce_all;
    bool dump_all;
    bool dump_main;
    stat_collector* main_collector;
    std::list<stat_collector*> collectors;
    stats_entry() : main_collector(nullptr) {}
  };
  std::map<std::string, stats_entry> stats_;

  timestamp now_;

 private:
  virtual void
  schedule(timestamp start_time, uint32_t seqnum, event_queue_entry* event_queue_entry) = 0;

};

class null_event_manager : public event_manager
{
 public:
  null_event_manager(sprockit::sim_parameters* params, parallel_runtime* rt) :
    event_manager(params, rt)
  {
  }

  void schedule(timestamp start_time, uint32_t seqnum, event_queue_entry *event_queue_entry){}
  void cancel_all_messages(device_id canceled_loc){}
  void clear(timestamp time){}
  void run(){}
  bool empty() const {
    return true;
  }

};
#endif

} // end of namespace sstmac


#endif