#ifndef sumi_api_PING_H
#define sumi_api_PING_H

#include <sumi/monitor.h>
#include <sumi/transport_fwd.h>
#include <sumi/timeout.h>

namespace sumi {

/**
 * @class pinger
 * The pinger object exists as a sort of refcounted class
 * so that you're never sending out more than one ping.
 * Multiple ping requests to the same machine all get
 * funneled into the same ping.
 */
class pinger
{
 public:
  ~pinger();

  pinger(transport* api, int dst, double timeout);

  void
  execute();

  /**
   * Notify the pinger that its ping has safely arrived
   */
  void
  arrived();

  /**
   * Send out the first ping and start things going
   */
  void
  start();

  /**
   * Create the event that waits on a successful ping
   */
  void
  wait();

  /**
   * @brief cancel
   * @param tag
   */
  void
  cancel(timeout_function* func);

  /**
   * The number of independent listeners, i.e.
   * collectives or functions depending on results
   * from this piong
   * @return
   */
  int
  refcount(){
    return functions_.refcount();
  }

  bool
  has_arrived() const {
    return arrived_;
  }

  bool
  has_failed() const {
    return failed_;
  }

  bool
  is_expired(double wtime) const {
    return (start_time_ + timeout_) < wtime;
  }

  double
  start_time() const {
    return start_time_;
  }

  void
  maybe_renew(double wtime);

  /**
   * Attach a new listener (timeout function) to this ping
   * @param func
   */
  void
  attach_listener(timeout_function* func);

 protected:
  void
  timeout_all_listeners();

  void
  schedule_next();

  void
  schedule_next(double wtime);

 protected:
  function_set functions_;

  transport* my_api_;
  int dst_;
  double timeout_;
  double start_time_;
  bool failed_;
  bool arrived_;

};

class ping_monitor :
    public activity_monitor
{
 public:
  ping_monitor(sprockit::sim_parameters* params,
               transport* tport);

  void
  ping(int dst, timeout_function* func);

  void
  renew_pings(double wtime);

  void
  cancel_ping(int dst, timeout_function* func);

  void
  message_received(const message::ptr& msg);

  void
  validate_done();

  void
  validate_all_pings();

 protected:
  /**
   * @brief pingers_
   * Map where key is destination (rank)
   * and value is a ping object.  Multiple
   * #timeout_function objects can be attached to a single pinger.
   * The pinger object exists as a sort of refcounted class
   * so that you're never sending out more than one ping.
   * Multiple ping requests to the same machine all get
   * funneled into the same ping.
   */
  std::map<int, pinger*> pingers_;

  double timeout_;
};


}

#endif // PING_H
