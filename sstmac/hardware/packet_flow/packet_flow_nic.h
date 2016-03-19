#ifndef sstmac_hardware_nic_packet_flow_nic_H
#define sstmac_hardware_nic_packet_flow_nic_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/packet_flow/packet_flow_endpoint.h>
#include <sstmac/hardware/packet_flow/packet_flow_switch.h>
#include <sstmac/common/stats/stat_histogram.h>

namespace sstmac {
namespace hw {

/**
 @class packet_flow_nic
 Network interface compatible with sending packet trains
 */
class packet_flow_nic :
  public nic,
  public packet_flow_component
{

 public:
  packet_flow_nic();

  std::string
  to_string() const {
    return sprockit::printf("packet flow nic(%d)", int(addr()));
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual ~packet_flow_nic() throw ();

  virtual int
  initial_credits() const {
    return buffer_size_;
  }

  virtual void
  set_node(node* parent);

  virtual void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod);

  /**
   Set up the injection/ejection links to the switch the NIC is connected to
   @param sw The switch that injects/ejects
   */
  void
  set_injection_output(int port, connectable* output);

  void
  set_ejection_input(int port, connectable* input);

  virtual void
  set_event_parent(event_scheduler* m);

  virtual void
  finalize_init();

  timestamp
  injection_latency() const {
    return inj_lat_;
  }

 protected:
  virtual void
  recv_packet(event* packet);

  virtual void
  recv_credit(event* credit);

  virtual void
  do_send(network_message* payload);

 protected:
  stat_histogram* congestion_hist_;
  stat_spyplot* congestion_spyplot_;
  bool acc_delay_;

  packet_flow_endpoint* endpoint_;
  double inj_bw_;
  timestamp inj_lat_;
  int buffer_size_;
  packet_flow_buffer* inj_buffer_;
  packet_flow_eject_buffer* ej_buffer_;
  event_handler* inj_handler_;
  int injection_credits_;

};

class packet_flow_netlink :
  public netlink,
  public packet_flow_component
{
 public:
  packet_flow_netlink() :
    block_(0),
    tile_rotater_(0),
    inited_(false)
  {
  }

  std::string
  to_string() const {
    return "packet flow netlink";
  }

  void
  init_factory_params(sprockit::sim_parameters *params);

  void
  connect(int src_outport, int dst_inport, connection_type_t ty, connectable *mod);

  void
  set_event_parent(event_scheduler* m);

  void
  handle(event* ev);

  event_handler*
  ejector() {
    init();
    return block_;
  }

  int
  initial_credits() const {
    return really_big_buffer;
  }

  event_handler*
  injector() {
    init();
    return block_;
  }

 private:
  void init();

 private:
  static const int really_big_buffer;
  packet_flow_crossbar* block_;
  int tile_rotater_;
  bool inited_;
};

}
} // end of namespace sstmac


#endif // packet_flow_nic_H

