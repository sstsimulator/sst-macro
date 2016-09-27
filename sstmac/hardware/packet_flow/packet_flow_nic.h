#ifndef sstmac_hardware_nic_packet_flow_nic_H
#define sstmac_hardware_nic_packet_flow_nic_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/packet_flow/packet_flow_switch.h>
#include <sstmac/hardware/packet_flow/packet_flow_packetizer.h>
#include <sstmac/common/stats/stat_histogram.h>

namespace sstmac {
namespace hw {

/**
 @class packet_flow_nic
 Network interface compatible with sending packet trains
 */
class packet_flow_nic :
  public nic,
  public packetizer_callback
{

 public:
  packet_flow_nic(sprockit::sim_parameters* params, node* parent);

  std::string
  to_string() const override {
    return sprockit::printf("packet flow nic(%d)", int(addr()));
  }

  void init(unsigned int phase) override;

  void setup() override;

  virtual ~packet_flow_nic() throw ();

  void notify(int vn, message* msg) override {
    recv_message(msg);
  }

  virtual void
  connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_handler* mod) override;

  virtual void
  connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_handler* mod) override;

  link_handler*
  ack_handler(int port) const override;

  link_handler*
  payload_handler(int port) const override;

 protected:
  virtual void
  do_send(network_message* payload) override;

 protected:
  packetizer* packetizer_;
#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* payload_handler_;
  link_handler* ack_handler_;
#endif
};

class packet_flow_netlink :
  public netlink
{
 public:
  packet_flow_netlink(sprockit::sim_parameters* params, node* parent);

  virtual ~packet_flow_netlink();

  std::string
  to_string() const override {
    return "packet flow netlink";
  }

  void handle_credit(event* ev);

  void handle_payload(event* ev);

  link_handler*
  payload_handler(int port) const override;

  link_handler*
  ack_handler(int port) const override;

  virtual void
  connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_handler* mod) override;

  virtual void
  connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_handler* mod) override;

  void
  deadlock_check() override;

 private:
  static const int really_big_buffer;
  packet_flow_crossbar* block_;
  int tile_rotater_;
  bool inited_;
#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* payload_handler_;
  link_handler* ack_handler_;
#endif
};

}
} // end of namespace sstmac


#endif // packet_flow_nic_H

