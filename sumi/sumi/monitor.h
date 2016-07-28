#ifndef sumi_api_MONITOR_H
#define sumi_api_MONITOR_H

#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>
#include <sumi/message.h>
#include <sumi/timeout.h>
#include <sumi/transport_fwd.h>

DeclareDebugSlot(sumi_ping)
DeclareDebugSlot(sumi_failure)

namespace sumi {

class function_set {
 public:
  int
  erase(timeout_function* func);

  void
  append(timeout_function* func){
    listeners_.push_back(func);
  }

  int
  refcount() const {
    return listeners_.size();
  }

  bool
  empty() const {
    return listeners_.empty();
  }

  void
  timeout_all_listeners(int dst);

 protected:
  std::list<timeout_function*> listeners_;
};

class activity_monitor :
  public sprockit::factory_type
{
 public:
  activity_monitor(transport* t) : api_(t){}

  virtual ~activity_monitor(){}

  virtual void
  ping(int dst, timeout_function* func) = 0;

  virtual void
  cancel_ping(int dst, timeout_function* func) = 0;

  virtual void
  message_received(const message::ptr& msg) = 0;

  virtual void
  renew_pings(double wtime) = 0;

  virtual void
  validate_done() = 0;

  virtual void
  validate_all_pings() = 0;

 protected:
  transport* api_;

};

DeclareFactory(activity_monitor, transport*);

}

#endif // MONITOR_H
