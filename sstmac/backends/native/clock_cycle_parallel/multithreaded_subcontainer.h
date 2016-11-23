#ifndef MULTITHREADED_SUBCONTAINER_H
#define MULTITHREADED_SUBCONTAINER_H

#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE

#include <sstmac/backends/native/clock_cycle_parallel/clock_cycle_event_container.h>
#include <sstmac/backends/native/clock_cycle_parallel/multithreaded_event_container_fwd.h>

namespace sstmac {
namespace native {

class multithreaded_subcontainer :
  public clock_cycle_event_map
{
 public:
  ~multithreaded_subcontainer() throw () {}

  void
  multithread_schedule(
    int srcthread,
    int dstthread,
    uint32_t seqnum,
    event_queue_entry* ev);

  void
  receive_incoming_events();

  timestamp
  vote_next_round(timestamp my_time, vote_type_t ty) override;

  void run();

  multithreaded_subcontainer(
    sprockit::sim_parameters* params,
    parallel_runtime* rt,
    int thread_id,
    multithreaded_event_container* parent);

 protected:
  multithreaded_event_container* parent_;

};

}
}

#endif // !SSTMAC_INTEGRATED_SST_CORE

#endif // MULTITHREADED_SUBCONTAINER_H
