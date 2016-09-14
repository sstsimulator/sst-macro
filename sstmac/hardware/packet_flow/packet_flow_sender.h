#ifndef PACKETFLOW_CREDITOR_H
#define PACKETFLOW_CREDITOR_H

#include <sprockit/util.h>
#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/packet_flow/packet_flow_handler.h>
#include <sstmac/hardware/packet_flow/packet_flow_arbitrator.h>
#include <sstmac/hardware/packet_flow/packet_flow_stats.h>

#define packet_flow_debug(...) \
  debug_printf(sprockit::dbg::packet_flow, __VA_ARGS__)

namespace sstmac {
namespace hw {

class packet_flow_sender :
  public packet_flow_handler
{
 public:
  virtual ~packet_flow_sender() {}

  void
  set_acker(event_handler* acker) {
    acker_ = acker;
  }

  virtual void
  set_input(sprockit::sim_parameters* params,
     int my_inport, int dst_outport,
     event_handler* input) = 0;

  virtual void
  set_output(sprockit::sim_parameters* params,
    int my_outport, int dst_inport,
    event_handler* output) = 0;

  void
  handle_payload(packet_flow_payload* pkt) {
    pkt->set_arrival(now());
    do_handle_payload(pkt);
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
  set_stat_collector(packet_sent_stats* c){
    stat_collector_ = c;
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
  packet_flow_sender(sprockit::sim_parameters* params,
                     event_scheduler* parent);

  void
  send_credit(const packet_flow_input& src,
    packet_flow_payload* payload,
    timestamp packet_tail_leaves);

  void
  send(packet_flow_bandwidth_arbitrator* arb,
       packet_flow_payload* pkt,
       const packet_flow_input& src,
       const packet_flow_output& dest);

  virtual void
  do_handle_payload(packet_flow_payload* pkt) = 0;

 protected:
  packet_sent_stats* stat_collector_;

  event_handler* acker_;

  timestamp send_lat_;

  timestamp credit_lat_;

  bool update_vc_;

};

DeclareFactory(packet_flow_sender, event_scheduler*)

}
}

#endif // PACKETFLOW_CREDITOR_H

