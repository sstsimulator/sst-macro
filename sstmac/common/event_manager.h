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

class event_manager :
  public sprockit::factory_type
{
  friend class event_scheduler;
  friend class native::manager;
  friend class native::macro_manager;

 public:
  virtual std::string
  to_string() const {
    return "event manager";
  }

  bool
  is_complete() {
    return complete_;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  static event_manager* global;

  /// Goodbye.
  virtual ~event_manager();

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
    bool reduce_all = true,
    bool dump_all = false,
    bool dump_main = true);

  stat_collector*
  register_thread_unique_stat(
    stat_collector* stat,
    bool reduce_all = true,
    bool dump_all = false,
    bool dump_main = true);

  virtual void
  cancel_all_messages(event_loc_id canceled_loc) = 0;

  partition*
  topology_partition() const;

  parallel_runtime*
  runtime() const;

  void
  finish_stats();

  virtual void
  finish_stats(stat_collector* main, const std::string& name, timestamp end);

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
    event_loc_id dst,
    event_loc_id src,
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
  set_interconnect(hw::interconnect* interconn);

  static int
  current_thread_id();

  void
  init_param1(parallel_runtime* rt){
    rt_ = rt;
  }

  virtual void
  schedule_stop(timestamp until);

 protected:
  event_manager();

  void
  set_now(const timestamp &ts);

 protected:
  bool complete_;
  bool stopped_;
  bool finish_on_stop_;
  parallel_runtime* rt_;
  int thread_id_;

  int me_;

  int nproc_;

  int nthread_;

  static std::vector<pthread_t> pthreads_;
  static std::vector<pthread_attr_t> pthread_attrs_;

 private:
  struct stats_entry {
    bool reduce_all;
    bool dump_all;
    bool dump_main;
    stat_collector* main_collector;
    std::list<stat_collector*> collectors;
    stats_entry() : main_collector(0) {}
  };
  std::map<std::string, stats_entry> stats_;

  timestamp now_;

 private:
  virtual void
  schedule(timestamp start_time, uint32_t seqnum, event_queue_entry* event_queue_entry) = 0;

};

DeclareFactory1InitParam(event_manager, parallel_runtime*);
#endif

} // end of namespace sstmac
#endif

