#ifndef sumi_SUMI_TRANSPORT_H
#define sumi_SUMI_TRANSPORT_H

#include <sstmac/libraries/sumi/sumi_api.h>
#include <sstmac/libraries/sumi/message_fwd.h>
#include <sumi/monitor.h>
#include <sumi/timeout.h>
#include <sumi/message_fwd.h>
#include <sumi/collective.h>
#include <sumi/transport.h>
#include <sumi/comm_functions.h>

namespace sumi {

class sumi_transport :
  public sstmac::sumi_api,
  public transport
{
  ImplementAPI(sumi_transport)
 public:  
  sumi_transport(sstmac::sw::software_id sid) :
    sumi_api("sumi", sid)
  {
  }

  virtual void
  init();

  virtual void
  finalize();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  void
  finalize_init();

  virtual ~sumi_transport(){}

  virtual sumi::message_ptr
  handle(sstmac::transport_message* msg);

  /**
   * Block on a collective of a particular type and tag
   * until that collective is complete
   * @param ty
   * @param tag
   * @return
   */
  collective_done_message::ptr
  collective_block(sumi::collective::type_t ty, int tag);

  void
  cq_notify();

  double
  wall_time() const;

  message::ptr
  block_until_message();

  message::ptr
  block_until_message(double timeout);

  void
  ping_timeout(pinger* pnger);

  void
  incoming_event(sstmac::event *ev){
    library::incoming_event(ev);
  }

 protected:
  sumi_transport(const char* name, sstmac::sw::software_id sid);

  void
  do_smsg_send(int dst, const message::ptr &msg);

  void
  do_rdma_put(int dst, const message::ptr& msg);

  void
  do_rdma_get(int src, const message::ptr& msg);

  void
  do_nvram_get(int src, const message::ptr& msg);

  void
  do_send_terminate(int dst);

  void
  do_send_ping_request(int dst);

  void
  delayed_transport_handle(const message::ptr& msg);

  void
  schedule_ping_timeout(pinger* pnger, double to);

  void
  schedule_next_heartbeat();

  void
  go_die();

  void
  go_revive();

};

}

#endif // sumi_SUMI_TRANSPORT_H
