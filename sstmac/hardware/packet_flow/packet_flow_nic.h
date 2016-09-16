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

  virtual ~packet_flow_nic() throw ();

  void handle(event *ev) override;

  void notify(int vn, message* msg) override {
    recv_message(msg);
  }

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

 protected:
  virtual void
  do_send(network_message* payload) override;

 protected:
  packetizer* packetizer_;

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

  void
  deadlock_check() override;

  void
  handle(event* ev) override;

 private:
  static const int really_big_buffer;
  packet_flow_crossbar* block_;
  int tile_rotater_;
  bool inited_;
};

}
} // end of namespace sstmac


#endif // packet_flow_nic_H

