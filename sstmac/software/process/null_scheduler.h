#ifndef NULLSCHEDULER_H
#define NULLSCHEDULER_H

#include <sstmac/hardware/processor/compute_scheduler.h>

namespace sstmac {
namespace hw {

class null_scheduler :
  public compute_scheduler
{
 public:
  std::string
  to_string() const {
    return "null scheduler";
  }

  virtual compute_scheduler*
  clone(node_id nid, sw::operating_system* os) const;

  void
  handle(sst_message* msg) {}

 protected:
  bool
  find_open_core(const sw::compute_message*&msg, int &core_assignment);

  void
  start_next_suspended(int core);

  void
  add_suspended(const sw::compute_message*&msg, int core);

};


}
}

#endif // NULLSCHEDULER_H

