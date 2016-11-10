#ifndef sstmac_hardware_nic_pisces_nic_H
#define sstmac_hardware_nic_pisces_nic_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/pisces/pisces_packetizer.h>
#include <sstmac/common/stats/stat_histogram.h>

namespace sstmac {
namespace hw {

/**
 @class pisces_nic
 Network interface compatible with sending packet trains
 */
class pisces_nic :
  public nic,
  public packetizer_callback
{

 public:
  pisces_nic(sprockit::sim_parameters* params, node* parent);

  std::string
  to_string() const override {
    return sprockit::printf("packet flow nic(%d)", int(addr()));
  }

  void init(unsigned int phase) override;

  void setup() override;

  virtual ~pisces_nic() throw ();

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
  credit_handler(int port) const override;

  link_handler*
  payload_handler(int port) const override;

  void
  deadlock_check() override;

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

class pisces_netlink :
  public netlink
{
 public:
  pisces_netlink(sprockit::sim_parameters* params, node* parent);

  virtual ~pisces_netlink();

  std::string
  to_string() const override {
    return "packet flow netlink";
  }

  void handle_credit(event* ev);

  void handle_payload(event* ev);

  void deadlock_check() override;

  link_handler*
  payload_handler(int port) const override;

  link_handler*
  credit_handler(int port) const override;

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

 private:
  static const int really_big_buffer;
  pisces_crossbar* inj_block_;
  pisces_crossbar* ej_block_;
  int tile_rotater_;
  bool inited_;
#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* payload_handler_;
  link_handler* ack_handler_;
#endif
};

}
} // end of namespace sstmac


#endif // pisces_nic_H

