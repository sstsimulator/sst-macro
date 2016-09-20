#ifndef PACKETFLOW_CREDITOR_H
#define PACKETFLOW_CREDITOR_H

#include <sprockit/util.h>
#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/packet_flow/packet_flow_arbitrator.h>
#include <sstmac/hardware/packet_flow/packet_flow_stats.h>
#include <sstmac/common/event_scheduler.h>

#define packet_flow_debug(...) \
  debug_printf(sprockit::dbg::packet_flow, __VA_ARGS__)

namespace sstmac {
namespace hw {

struct payload_queue {

  std::list<packet_flow_payload*> queue;

  typedef std::list<packet_flow_payload*>::iterator iterator;

  packet_flow_payload*
  pop(int num_credits);

  packet_flow_payload*
  front();

  void
  push_back(packet_flow_payload* payload);

};

struct packet_flow_input {
  int src_outport;
  event_handler* handler;
  packet_flow_input() :
    src_outport(-1),
    handler(0)
  {
  }
};

struct packet_flow_output {
  int dst_inport;
  event_handler* handler;
  packet_flow_output() :
    dst_inport(-1),
    handler(0)
  {
  }
};

class packet_flow_sender :
  public sprockit::printable,
  public event_subscheduler
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

  virtual void handle_credit(event* ev) = 0;

  virtual void handle_payload(event* ev) = 0;

  void
  set_stat_collector(packet_sent_stats* c){
    stat_collector_ = c;
  }

  virtual std::string
  packet_flow_name() const = 0;

  std::string
  to_string() const override;

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

