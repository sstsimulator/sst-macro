#ifndef PACKETFLOW_SWITCH_H
#define PACKETFLOW_SWITCH_H

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/packet_flow/packet_flow_buffer.h>
#include <sstmac/hardware/packet_flow/packet_flow_crossbar.h>
#include <sstmac/hardware/packet_flow/packet_flow_arbitrator.h>
#include <sstmac/hardware/packet_flow/packet_flow_stats_fwd.h>
#include <sstmac/common/stats/stat_global_int_fwd.h>

namespace sstmac {
namespace hw {

class packet_flow_params  {
 public:
  double link_bw;

  timestamp hop_lat;

  int xbar_output_buffer_num_bytes;

  int xbar_input_buffer_num_bytes;

  double crossbar_bw;

  int row_buffer_num_bytes;

  packet_flow_bandwidth_arbitrator* link_arbitrator_template;

  bool queue_depth_reporting;
  int queue_depth_delta;

  std::string
  to_string() const {
    return "packet_flow parameters";
  }

};

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
    return params_->hop_lat;
  }

  timestamp
  lookahead() const {
    return params_->hop_lat;
  }

  virtual int
  initial_credits() const = 0;

  double
  hop_bandwidth() const {
    return params_->link_bw;
  }

 protected:
#if !SSTMAC_INTEGRATED_SST_CORE
  packet_flow_abstract_switch() :
    params_(0)
  {
  }
#endif

 protected:
  packet_flow_params* params_
#if SSTMAC_HAVE_CXX11
    = nullptr
#endif
  ;
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

  virtual void
  initialize();

  packet_flow_crossbar*
  crossbar();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  int
  queue_length(int port) const;

  int
  initial_credits() const {
    return params_->xbar_input_buffer_num_bytes;
  }

  virtual void
  connect_weighted(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod,
    double weight, int red);

  std::vector<switch_id>
  connected_switches() const;

  /**
   Cast message and pass to #send
   @param msg Incoming message (should cast to packet_train)
   */
  void
  handle(const sst_message::ptr& msg);

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

  virtual
  ~packet_flow_switch();

 protected:
  virtual void
  connect_injector(int src_outport, int dst_inport, event_handler* nic);

  virtual void
  connect_ejector(int src_outport, int dst_inport, event_handler* nic);

 protected:
  stat_spyplot* congestion_spyplot_;
  stat_bytes_sent* bytes_sent_;
  stat_global_int* byte_hops_;

  std::vector<packet_flow_sender*> out_buffers_;

  packet_flow_crossbar* xbar_;

  bool acc_delay_;

 private:
  void
  connect_output(
    int src_outport,
    int dst_inport,
    connectable* mod,
    double weight, int red);

  void
  connect_input(
    int src_outport,
    int dst_inport,
    connectable* mod,
    double weight, int red);

  void resize_buffers();

  packet_flow_sender*
  output_buffer(int port, double bw, int red);

};

DeclareIntegratedComponent(packet_flow_switch);

}
}

#endif // PACKETFLOW_SWITCH_H

