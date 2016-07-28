#include <sstmac/backends/native/clock_cycle_parallel/multithreaded_subcontainer.h>
#include <sstmac/backends/native/clock_cycle_parallel/multithreaded_event_container.h>
#include <sstmac/common/thread_info.h>
#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE

namespace sstmac {
namespace native {

multithreaded_subcontainer::multithreaded_subcontainer(
  parallel_runtime* rt,
  int thread_id,
  multithreaded_event_container *parent) :
  clock_cycle_event_map(rt),
  parent_(parent)
{
  thread_id_ = thread_id;
  rt_ = parent_->runtime();
  nproc_ = parent_->nproc();
  nthread_ = parent_->nthread();
  me_ = parent_->me();
}

void
multithreaded_subcontainer::receive_incoming_events()
{
  //make sure everyone has arrived and that all messages are received
  parent_->send_recv_barrier(thread_id_);
  parent_->schedule_incoming(thread_id_, this);
}

void
multithreaded_subcontainer::run()
{
  pthread_t self = pthread_self();
  //thread_info::register_kernel_space_virtual_thread(thread_id(), &self, NULL);

  if (!pthread_equal(pthread_self(), pthreads_[thread_id()])){
    std::cerr << "pthreads not equal" << std::endl;
    abort();
  }

  clock_cycle_event_map::run();
}

timestamp
multithreaded_subcontainer::vote_next_round(timestamp my_time)
{
  debug_printf(sprockit::dbg::event_manager | sprockit::dbg::event_manager_time_vote,
    "Rank %d thread barrier to start vote on thread %d, epoch %d\n",
    rt_->me(), thread_id(), epoch_);
  return parent_->time_vote_barrier(thread_id_, my_time);
}

void
multithreaded_subcontainer::multithread_schedule(
    int srcthread,
    int dstthread,
    uint32_t seqnum,
    event_queue_entry* ev)
{
  debug_printf(sprockit::dbg::multithread_event_manager,
    "scheduling events on thread %d from thread %d",
    dstthread, srcthread);
#if SSTMAC_SANITY_CHECK
  if (ev->time() < next_time_horizon_){
    spkt_throw(sprockit::illformed_error,
        "multithread_schedule: scheduling before next time horizon");
  }
#endif
  parent_->multithread_schedule(srcthread, dstthread, seqnum, ev);
}

}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE
