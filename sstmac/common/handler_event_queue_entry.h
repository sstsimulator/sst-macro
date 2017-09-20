#ifndef sstmac_common_handler_queue_entry_H
#define sstmac_common_handler_queue_entry_H

#include <sstmac/common/sst_event.h>
#include <sprockit/thread_safe_new.h>

namespace sstmac {

class handler_event_queue_entry :
  public event_queue_entry,
  public sprockit::thread_safe_new<handler_event_queue_entry>
{

 public:
  virtual ~handler_event_queue_entry() {}

  handler_event_queue_entry(event* ev,
    event_handler* hand,
    uint32_t src_comp_id) :
    ev_to_deliver_(ev),
    handler_(hand),
    event_queue_entry(hand->component_id(), src_comp_id)
  {
  }

  void execute() override {
    handler_->handle(ev_to_deliver_);
  }

 protected:
  event* ev_to_deliver_;

  event_handler* handler_;

};

}

#endif
