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
  public packet_flow_component,
  public packetizer_callback
{

 public:
  packet_flow_nic(sprockit::factory_type* interconn) :
    nic(interconn),
    packetizer_(0),
    injection_credits_(0)
  {
  }

  std::string
  to_string() const {
    return sprockit::printf("packet flow nic(%d)", int(addr()));
  }

#if SSTMAC_INTEGRATED_SST_CORE
  virtual void
  init_sst_params(SST::Params &params, SST::Component* parent);
#endif

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual ~packet_flow_nic() throw ();

  void handle(event *ev);

  void notify(int vn, message* msg){
    recv_message(msg);
  }

  virtual void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod,
    config* cfg);

  virtual void
  set_event_parent(event_scheduler* m);

  timestamp
  injection_latency() const {
    return inj_lat_;
  }

  double
  injection_bandwidth() const;

  int
  initial_credits() const {
    return injection_credits_;
  }

 protected:
  virtual void
  do_send(network_message* payload);

 protected:
  packetizer* packetizer_;
  timestamp inj_lat_;
  int injection_credits_;

};

class packet_flow_netlink :
  public netlink,
  public packet_flow_component
{
 public:
  packet_flow_netlink(sprockit::factory_type* interconn) :
    netlink(interconn),
    block_(nullptr),
    tile_rotater_(0),
    inited_(false)
  {
  }

  virtual ~packet_flow_netlink();

  std::string
  to_string() const {
    return "packet flow netlink";
  }

  void
  init_factory_params(sprockit::sim_parameters *params);

  void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable *mod,
    config* cfg);

  void
  deadlock_check();

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

