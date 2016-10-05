#ifndef sumi_SUMI_TRANSPORT_H
#define sumi_SUMI_TRANSPORT_H

#include <sstmac/libraries/sumi/message_fwd.h>
#include <sstmac/common/node_address.h>
#include <sumi/monitor.h>
#include <sumi/timeout.h>
#include <sumi/message_fwd.h>
#include <sumi/collective.h>
#include <sumi/transport.h>
#include <sumi/comm_functions.h>
#include <sstmac/libraries/sumi/message_fwd.h>
#include <sstmac/software/process/pmi.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/software/libraries/service.h>
#include <sstmac/software/api/api.h>
#include <sstmac/hardware/network/network_message_fwd.h>
#include <sumi/message_fwd.h>

/**
 * SUMI = Simulator unified messagine interface
 * It is also the name for a solid ink in Japanese -
 * i.e. the substrate for sending messages!
 */

namespace sstmac {

class sumi_queue
{

 public:
  sumi_queue(sstmac::sw::operating_system* os);

  sumi_queue();

  ~sumi_queue();

  transport_message*
  poll_until_message();

  transport_message*
  poll_until_message(timestamp timeout);

  void
  put_message(transport_message* message);

  bool
  blocked() const {
    return !blocked_keys_.empty();
  }

 private:
  std::list<transport_message*> pending_messages_;

  std::list<sstmac::sw::key*> blocked_keys_;

  sstmac::sw::operating_system* os_;

};

class sumi_transport :
  public sstmac::sw::api,
  public sstmac::sw::process_manager,
  public sumi::transport
{
  ImplementAPI(sumi_transport)
 public:  
  sumi_transport(sprockit::sim_parameters* params,
                 sstmac::sw::software_id sid,
                 sstmac::sw::operating_system* os) :
    sumi_transport(params, "sumi", sid, os)
  {
  }

  virtual void
  init();

  virtual void
  finalize();

  virtual ~sumi_transport();

  virtual sumi::message_ptr
  handle(sstmac::transport_message* msg);

  void
  client_server_send(
    const std::string& server_name,
    node_id dest,
    const sumi::message::ptr& msg);

  /**
   * Block on a collective of a particular type and tag
   * until that collective is complete
   * @param ty
   * @param tag
   * @return
   */
  sumi::collective_done_message::ptr
  collective_block(sumi::collective::type_t ty, int tag);

  void
  cq_notify();

  double
  wall_time() const;

  sumi::message::ptr
  block_until_message();

  sumi::message::ptr
  block_until_message(double timeout);

  void
  ping_timeout(sumi::pinger* pnger);

  void
  incoming_event(sstmac::event *ev){
    library::incoming_event(ev);
  }

  void
  send(
    long byte_length,
    const sumi::message_ptr& msg,
    int ty,
    int dst,
    bool needs_ack);

  void incoming_message(transport_message* msg){
    queue_->put_message(msg);
  }

 private:
  bool
  blocked() const {
    return queue_->blocked();
  }

  void
  do_smsg_send(int dst, const sumi::message::ptr &msg);

  void
  do_rdma_put(int dst, const sumi::message::ptr& msg);

  void
  do_rdma_get(int src, const sumi::message::ptr& msg);

  void
  do_nvram_get(int src, const sumi::message::ptr& msg);

  void
  do_send_terminate(int dst);

  void
  do_send_ping_request(int dst);

  void
  delayed_transport_handle(const sumi::message::ptr& msg);

  void
  schedule_ping_timeout(sumi::pinger* pnger, double to);

  void
  schedule_next_heartbeat();

  void
  go_die();

  void
  go_revive();

 protected:
  sumi_transport(sprockit::sim_parameters* params,
                 const char* name,
                 sstmac::sw::software_id sid,
                 sstmac::sw::operating_system* os);

 private:
  std::string server_libname_;

  sstmac::sw::app_launch* rank_mapper_;

  /**
   * @brief queue_
   * Manages incoming/outgoing messages
   */
  sumi_queue* queue_;

  device_id loc_;


};

}

#endif // sumi_SUMI_TRANSPORT_H
