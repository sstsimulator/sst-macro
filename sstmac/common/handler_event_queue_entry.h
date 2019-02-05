#ifndef sstmac_common_handler_queue_entry_H
#define sstmac_common_handler_queue_entry_H

#include <sstmac/common/sst_event.h>
#include <sstmac/common/event_handler.h>
#include <sprockit/thread_safe_new.h>

namespace sstmac {

class HandlerExecutionEvent :
  public ExecutionEvent,
  public sprockit::thread_safe_new<HandlerExecutionEvent>
{
 public:
  using sprockit::thread_safe_new<HandlerExecutionEvent>::operator delete;

  virtual ~HandlerExecutionEvent() {}

  HandlerExecutionEvent(Event* ev, EventHandler* hand) :
    ev_to_deliver_(ev),
    handler_(hand)
  {
  }

  void execute() override {
    handler_->handle(ev_to_deliver_);
  }

 protected:
  Event* ev_to_deliver_;

  EventHandler* handler_;

};

}

#endif
