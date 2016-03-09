#ifndef PACKETFLOW_CREDITOR_H
#define PACKETFLOW_CREDITOR_H

#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/packet_flow/packet_flow_handler.h>
#include <sstmac/hardware/packet_flow/packet_flow_arbitrator.h>

#define packet_flow_debug(...) \
  debug_printf(sprockit::dbg::packet_flow, __VA_ARGS__)

namespace sstmac {
namespace hw {

class packet_flow_MTL
{
 public:
  packet_flow_MTL(int mtu) : mtu_(mtu) {}

  virtual void mtl_send(sst_message* msg) = 0;

  packet_flow_payload*
  next_chunk(long byte_offset, sst_message* parent);

 private:
  int mtu_;

};

class packet_flow_sender :
  public packet_flow_handler
{
 public:
  virtual ~packet_flow_sender() {}

  virtual void
  start(sst_message* msg) = 0;

  void
  set_acker(event_handler* acker) {
    acker_ = acker;
  }

  virtual void
  set_input(int my_inport, int dst_outport,
            event_handler* input) = 0;

  virtual void
  set_output(int my_outport, int dst_inport,
             event_handler* output) = 0;

  virtual void
  init_credits(int port, int num_credits) = 0;

  virtual int
  num_initial_credits() const = 0;

  void
  handle_payload(packet_flow_payload* msg) {
    msg->set_arrival(now().sec());
    do_handle_payload(msg);
  }

  void
  set_event_location(node_id nid) {
    init_loc_id(event_loc_id(nid));
  }

  void
  set_event_location(switch_id sid) {
    init_loc_id(event_loc_id(sid));
  }

  void
  set_congestion_spyplot(stat_spyplot* plot){
    congestion_spyplot_ = plot;
  }

  bool
  accumulate_delay() const {
    return acc_delay_;
  }

  void
  set_accumulate_delay(bool flag) {
    acc_delay_ = flag;
  }

  std::string
  to_string() const;

  virtual std::string
  packet_flow_name() const = 0;

  void
  set_update_vc(bool flag){
    update_vc_ = flag;
  }

 protected:
  packet_flow_sender(
    const timestamp& send_lat,
    const timestamp& credit_lat);

  packet_flow_sender();

  void
  send_credit(const packet_flow_input& src,
    packet_flow_payload* payload,
    timestamp packet_tail_leaves);

  void
  send(packet_flow_bandwidth_arbitrator* arb,
       packet_flow_payload* msg,
       const packet_flow_input& src,
       const packet_flow_output& dest);

  virtual void
  do_handle_payload(packet_flow_payload* msg) = 0;

 protected:
  stat_spyplot* congestion_spyplot_;

  event_handler* acker_;

  timestamp send_lat_;

  timestamp credit_lat_;

  timestamp credit_latency_;

  bool acc_delay_;

  bool update_vc_;

};


}
}

#endif // PACKETFLOW_CREDITOR_H

