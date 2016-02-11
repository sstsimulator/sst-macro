#include <sstmac/hardware/processor/null_scheduler.h>

namespace sstmac {
namespace hw {

SpktRegister("null", compute_scheduler, null_scheduler,
            "Compute scheduler that performs no work and ignores all computation (i.e. null)");

void
null_scheduler::start_next_suspended(int core)
{
  spkt_throw_printf(sprockit::illformed_error,
      "nullscheduler::start_next_suspended: null scheduler should never be invoked");
}

void
null_scheduler::add_suspended(const sw::compute_message::ptr&msg, int core)
{
  spkt_throw_printf(sprockit::illformed_error,
      "nullscheduler::add_suspended: null scheduler should never be invoked");
}

compute_scheduler*
null_scheduler::clone(node_id nid, sw::operating_system*os) const
{
  null_scheduler* cln = new null_scheduler;
  cln->init_param1(nid);
  cln->init_param2(os);
  clone_into(cln);
  cln->finalize_init();
  return cln;
}

bool
null_scheduler::find_open_core(const sw::compute_message::ptr&msg, int &core_assignment)
{
  spkt_throw_printf(sprockit::illformed_error,
      "nullscheduler::find_open_core: null scheduler should never be invoked");
  return false;
}

}
}

