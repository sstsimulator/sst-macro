#ifndef MULTITHREADED_EVENT_CONTAINER_H
#define MULTITHREADED_EVENT_CONTAINER_H

#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE

#include <sstmac/backends/native/clock_cycle_parallel/clock_cycle_event_container.h>
#include <sstmac/backends/native/clock_cycle_parallel/multithreaded_subcontainer.h>
#include <sstmac/backends/native/clock_cycle_parallel/thread_barrier.h>
#include <pthread.h>


DeclareDebugSlot(multithread_event_manager);
DeclareDebugSlot(cpu_affinity);

namespace sstmac {
namespace native {

class thread_event_schedule_map
{
 public:
  std::list<event_queue_entry*>&
  pending_events(int srcthread, int dstthread);

  void
  init(int nthread);

 protected:
  int array_index(int srcthread, int dstthread);

 protected:
  int nthread_;

  std::vector<std::list<event_queue_entry*> > events_;

};


class multithreaded_event_container :
  public clock_cycle_event_map
{
 public:
  multithreaded_event_container(parallel_runtime* rt) :
    clock_cycle_event_map(rt){}

  ~multithreaded_event_container() throw () {}

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  finalize_init();

  virtual void
  run();

  virtual void
  schedule_stop(timestamp until);

  void
  multithread_schedule(
    int srcthread,
    int dstthread,
    uint32_t seqnum,
    event_queue_entry* ev);

  std::list<event_queue_entry*>&
  pending_events(int srcthread, int dstthread) {
    return pending_event_map_.pending_events(srcthread, dstthread);
  }

  virtual void
  set_interconnect(hw::interconnect* interconn);

  virtual void
  receive_incoming_events();

  void
  schedule_incoming(int thread_id, clock_cycle_event_map* mgr);

  void
  send_recv_barrier(int thread_id);

  timestamp
  time_vote_barrier(int thread_id, timestamp min_time);

  virtual timestamp
  vote_next_round(timestamp my_time);

  event_manager*
  ev_man_for_thread(int thread_id) const;

  virtual void
  finish_stats(stat_collector *main, const std::string &name, timestamp end);

 protected:
  struct vote_thread_functor : public thread_barrier_functor {
    virtual int64_t
    execute(int64_t min_time){
      return parent->do_vote(min_time);
    }
    multithreaded_event_container* parent;
  };
  vote_thread_functor vote_functor_;

  struct send_recv_thread_functor : public thread_barrier_functor {
    virtual int64_t
    execute(int64_t){
      parent->clock_cycle_event_map::receive_incoming_events();
      return 0;
    }
    multithreaded_event_container* parent;
  };
  send_recv_thread_functor send_recv_functor_;

  std::vector<multithreaded_subcontainer*> subthreads_;

  thread_barrier send_recv_barrier_;
  thread_barrier vote_barrier_;

  thread_event_schedule_map pending_event_map_;

  std::vector<int> cpu_affinity_;
  int me_;
  int nproc_;

};

}
}


#endif // !SSTMAC_INTEGRATED_SST_CORE

#endif // MULTITHREADED_EVENT_CONTAINER_H
