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
  init() override;

  virtual void
  finalize() override;

  virtual ~sumi_transport();

  sumi::message_ptr
  handle(sstmac::transport_message* msg);

  void incoming_event(event *ev) override;

  void
  client_server_send(
    const std::string& server_name,
    int dest_rank,
    node_id dest_node,
    const sumi::message::ptr& msg);

  /**
   * Block on a collective of a particular type and tag
   * until that collective is complete
   * @param ty
   * @param tag
   * @return
   */
  sumi::collective_done_message::ptr
  collective_block(sumi::collective::type_t ty, int tag) override;

  void
  cq_notify() override;

  double
  wall_time() const override;

  sumi::message::ptr
  block_until_message() override;

  sumi::message::ptr
  block_until_message(double timeout) override;

  void
  ping_timeout(sumi::pinger* pnger);

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
  do_smsg_send(int dst, const sumi::message::ptr &msg) override;

  void
  do_rdma_put(int dst, const sumi::message::ptr& msg) override;

  void
  do_rdma_get(int src, const sumi::message::ptr& msg) override;

  void
  do_nvram_get(int src, const sumi::message::ptr& msg) override;

  void
  do_send_terminate(int dst) override;

  void
  do_send_ping_request(int dst) override;

  void
  delayed_transport_handle(const sumi::message::ptr& msg) override;

  void
  schedule_ping_timeout(sumi::pinger* pnger, double to) override;

  void
  schedule_next_heartbeat() override;

  void
  go_die() override;

  void
  go_revive() override;

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
