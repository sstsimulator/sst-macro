#ifndef PACKETFLOW_SWITCH_H
#define PACKETFLOW_SWITCH_H

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/packet_flow/packet_flow_buffer.h>
#include <sstmac/hardware/packet_flow/packet_flow_crossbar.h>
#include <sstmac/hardware/packet_flow/packet_flow_arbitrator.h>
#include <sstmac/hardware/packet_flow/packet_flow_stats_fwd.h>

namespace sstmac {
namespace hw {

class packet_flow_abstract_switch :
  public network_switch
{
 public:
  packet_sent_stats*
  xbar_stats() const {
    return xbar_stats_;
  }

  packet_sent_stats*
  buf_stats() const {
    return buf_stats_;
  }

 protected:
  packet_flow_abstract_switch(
    sprockit::sim_parameters* params,
    uint64_t id,
    event_manager* mgr);

  virtual ~packet_flow_abstract_switch();

  packet_sent_stats* xbar_stats_;
  packet_sent_stats* buf_stats_;
};

/**
 @class packet_flow_switch
 A switch in the network that arbitrates/routes packet_trains
 to the next link in the network
 */
class packet_flow_switch :
  public packet_flow_abstract_switch
{

 public:
  packet_flow_switch(sprockit::sim_parameters* params, uint64_t id, event_manager* mgr);

  virtual ~packet_flow_switch();

  int
  queue_length(int port) const override;

  virtual void
  connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    connectable* mod) override;

  virtual void
  connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    connectable* mod) override;

  std::vector<switch_id>
  connected_switches() const override;

  /**
   Cast message and pass to #send
   @param msg Incoming message (should cast to packet_train)
   */
  void
  handle(event* ev) override;

  void deadlock_check() override;

  void deadlock_check(event* ev) override;

  /**
   Set the link to use when ejecting packets at their endpoint.  A packet_flow_switch
   can have any number of ejectors, corresponding to the number of nodes
   per switch.
   @param addr The compute node address of the endpoint to eject to
   @param link The link to the compute node for ejection
   */
  void
  add_ejector(node_id addr, event_handler* link);

  virtual std::string
  to_string() const override;

 private:
  std::vector<packet_flow_sender*> out_buffers_;

  packet_flow_crossbar* xbar_;

 private:
  void resize_buffers();

  packet_flow_sender*
  output_buffer(sprockit::sim_parameters* params, int port);

};

DeclareSSTComponent(packet_flow_switch);

}
}

#endif // PACKETFLOW_SWITCH_H

