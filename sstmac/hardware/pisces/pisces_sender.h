#ifndef PACKETFLOW_CREDITOR_H
#define PACKETFLOW_CREDITOR_H

#include <sprockit/util.h>
#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/pisces/pisces_arbitrator.h>
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sstmac/common/event_scheduler.h>

#define pisces_debug(...) \
  debug_printf(sprockit::dbg::pisces, __VA_ARGS__)

namespace sstmac {
namespace hw {

struct payload_queue {

  std::list<pisces_payload*> queue;

  typedef std::list<pisces_payload*>::iterator iterator;

  pisces_payload*
  pop(int num_credits);

  pisces_payload*
  front();

  void
  push_back(pisces_payload* payload);

};

struct pisces_input {
  int src_outport;
  event_handler* handler;
  pisces_input() :
    src_outport(-1),
    handler(0)
  {
  }
};

struct pisces_output {
  int dst_inport;
  event_handler* handler;
  pisces_output() :
    dst_inport(-1),
    handler(0)
  {
  }
};

class pisces_sender :
  public event_subcomponent
{
 public:
  virtual ~pisces_sender() {}

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

  static void
  configure_credit_port_latency(sprockit::sim_parameters* params);

  static void
  configure_payload_port_latency(sprockit::sim_parameters* params);

  void
  set_stat_collector(packet_stats_callback* c){
    stat_collector_ = c;
  }

  virtual std::string
  pisces_name() const = 0;

  std::string
  to_string() const override;

  void
  set_update_vc(bool flag){
    update_vc_ = flag;
  }

 protected:
  pisces_sender(sprockit::sim_parameters* params,
                     event_scheduler* parent);

  virtual void
  send_credit(const pisces_input& src,
    pisces_payload* payload,
    timestamp packet_tail_leaves);

  void
  send(pisces_bandwidth_arbitrator* arb,
       pisces_payload* pkt,
       const pisces_input& src,
       const pisces_output& dest);

 protected:
  packet_stats_callback* stat_collector_;

  timestamp send_lat_;

  timestamp credit_lat_;

  bool update_vc_;

};

DeclareFactory(pisces_sender, event_component*)

}
}

#endif // PACKETFLOW_CREDITOR_H

