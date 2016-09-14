#ifndef SIMPLE_SWITCH_H
#define SIMPLE_SWITCH_H

#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/messages/sst_message_fwd.h>
#include <sstmac/hardware/nic/nic_fwd.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sprockit/unordered.h>

namespace sstmac {
namespace hw {

class simple_switch :
  public network_switch
{

 public:
  simple_switch(sprockit::sim_parameters* params, uint64_t id, event_manager* mgr);

  int
  queue_length(int port) const override {
    return 0;
  }

  virtual void
  connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    connectable *mod) override;

  virtual void
  connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    connectable *mod) override;

  std::vector<switch_id>
  connected_switches() const override;

  /**
   Cast message and pass to #send
   @param msg Incoming message (should cast to packet_train)
   */
  virtual void
  handle(event* ev) override;

  virtual std::string
  to_string() const override {
    return "simple switch";
  }

  virtual
  ~simple_switch();

  double
  inverse_bw() const {
    return inverse_bw_;
  }

 protected:
  virtual void
  connect_injector(sprockit::sim_parameters* params,
                   int src_outport, int dst_inport, event_handler* nic) override;

  virtual void
  connect_ejector(sprockit::sim_parameters* params,
                  int src_outport, int dst_inport, event_handler* nic) override;

  void
  add_switch(connectable* sw);

 protected:
  node_id my_start_;
  node_id my_end_;

  double inj_bw_inverse_;

  timestamp inj_lat_;

  double inverse_bw_;

  double inv_min_bw_;

  timestamp hop_latency_;

  topology* top_;

  spkt_unordered_map<node_id, network_switch*> neighbors_;

  spkt_unordered_map<node_id, nic*> nics_;


 private:
  void send_to_nic(timestamp delay, node_id dst, message* msg);
  void send_to_switch(timestamp delay, node_id dst, message* msg);

};

}
}

#endif // SIMPLE_SWITCH_H
