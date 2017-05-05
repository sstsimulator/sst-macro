#include <sstmac/hardware/common/connection.h>
#include <sprockit/debug.h>

DeclareDebugSlot(timestamp)

namespace sstmac {
namespace hw {

static bool checked_prefix_fxn = false;

class timestamp_prefix_fxn :
  public sprockit::debug_prefix_fxn
{
 public:
  timestamp_prefix_fxn(sstmac::event_scheduler* mgr) : mgr_(mgr){}

  std::string str() {
    double t_ms = mgr_->now().msec();
    return sprockit::printf("T=%12.8e ms:", t_ms);
  }

 private:
  event_scheduler* mgr_;
};

connectable_component::connectable_component(sprockit::sim_parameters* params,
                      uint64_t cid,
                      device_id id,
                      event_manager* mgr)
  : event_component(params, cid, id, mgr)
{
  if (!checked_prefix_fxn){
    if (sprockit::debug::slot_active(sprockit::dbg::timestamp)){
      sprockit::debug_prefix_fxn* fxn = new timestamp_prefix_fxn(this);
      sprockit::debug::prefix_fxn = fxn;
    }
    checked_prefix_fxn = true;
  }
}


}
}
