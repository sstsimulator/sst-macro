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
  public network_switch,
  public packet_flow_component
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  packet_flow_abstract_switch(
    SST::ComponentId_t id,
    SST::Params& params
  ) : network_switch(id, params)
  { }
#endif

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  initialize() = 0;

  timestamp
  hop_latency() const {
    return hop_lat;
  }

  timestamp
  lookahead() const {
    return hop_lat;
  }

  virtual int
  initial_credits() const = 0;

  double
  hop_bandwidth() const {
    return link_bw;
  }

 protected:
  int packet_size_;

  double link_bw;

  double ej_bw;

  int xbar_output_buffer_num_bytes;

  int xbar_input_buffer_num_bytes;

  double xbar_bw;

  int row_buffer_num_bytes;

  timestamp hop_lat;

  packet_flow_bandwidth_arbitrator* link_arbitrator_template;

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
#if !SSTMAC_INTEGRATED_SST_CORE
  packet_flow_switch();
#endif

#if SSTMAC_INTEGRATED_SST_CORE
  packet_flow_switch(
    SST::ComponentId_t id,
    SST::Params& params
  );
#endif

  ~packet_flow_switch();

  virtual void
  initialize();

  packet_flow_crossbar*
  crossbar(config* cfg);

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  int
  queue_length(int port) const;

  int
  initial_credits() const {
    return xbar_input_buffer_num_bytes;
  }

  virtual void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod,
    config* cfg);

  std::vector<switch_id>
  connected_switches() const;

  /**
   Cast message and pass to #send
   @param msg Incoming message (should cast to packet_train)
   */
  void
  handle(event* ev);

  void deadlock_check();

  /**
   Set the link to use when ejecting packets at their endpoint.  A packet_flow_switch
   can have any number of ejectors, corresponding to the number of nodes
   per switch.
   @param addr The compute node address of the endpoint to eject to
   @param link The link to the compute node for ejection
   */
  void
  add_ejector(node_id addr, event_handler* link);

  virtual void
  set_event_manager(event_manager* m);

  void
  set_topology(topology *top);

  virtual std::string
  to_string() const;

 protected:
  virtual void
  connect_injector(int src_outport, int dst_inport, event_handler* nic);

  virtual void
  connect_ejector(int src_outport, int dst_inport, event_handler* nic);

 protected:
  sprockit::sim_parameters* params_;

  std::vector<packet_flow_sender*> out_buffers_;

  packet_flow_crossbar* xbar_;

 private:
  void
  connect_output(
    int src_outport,
    int dst_inport,
    connectable* mod,
    config* cfg);

  void
  connect_input(
    int src_outport,
    int dst_inport,
    connectable* mod,
    config* cfg);

  void resize_buffers();

  packet_flow_sender*
  output_buffer(int port, config* cfg);

};

DeclareIntegratedComponent(packet_flow_switch);

}
}

#endif // PACKETFLOW_SWITCH_H

