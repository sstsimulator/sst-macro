#include <sstmac/software/process/pmi.h>
#include <sstmac/software/launch/launcher.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/thread_info.h>
#include <sstmac/common/thread_lock.h>

namespace sstmac {
namespace sw {

process_manager::app_to_proc_to_node_map process_manager::node_map_;
process_manager::app_to_node_to_proc_map process_manager::proc_map_;

process_manager::process_manager(software_id sid, operating_system* os) :
  sid_(sid), my_os_(os)
{
  node_id addr = runtime::node_for_task(sid.app_, sid.task_);
  static thread_lock lock;
  lock.lock();
  node_map_[sid.app_][sid.task_] = addr;
  proc_map_[sid.app_][addr] = sid.task_;
  lock.unlock();
  my_addr_ = addr;
}

process_manager::~process_manager()
{
}

void
process_manager::kill_node()
{
#if !SSTMAC_INTEGRATED_CORE
  my_os_->kill_node();
#else
  spkt_throw(sprockit::unimplemented_error,
    "process_manager::kill_node");
#endif
}

void
process_manager::kill_process()
{
  spkt_throw(sprockit::unimplemented_error,
    "process_manager::kill_process");
}

int
process_manager::get_partner(node_id addr) const
{
  auto it1 = proc_map_.find(sid_.app_);
  if (it1 == proc_map_.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "process_manager::get_partner: "
                     "Could not find app %d. "
                     "Are you sure process_manager::init was called?",
                     int(sid_.app_));
  }

  const node_to_proc_map& node_map = it1->second;
  auto it2 = node_map.find(addr);
  if (it2 == node_map.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "process_manager::get_partner: "
                     "Could not find nodeaddress %ld for app %d",
                     long(addr), int(sid_.app_));
  }

  return it2->second;
}

}
}

