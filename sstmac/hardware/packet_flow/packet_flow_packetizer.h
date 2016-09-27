#ifndef PACKET_FLOW_PACKETIZER_H
#define PACKET_FLOW_PACKETIZER_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/packet_flow/packet_flow_switch.h>
#include <sstmac/hardware/common/packetizer.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/hardware/packet_flow/packet_allocator_fwd.h>

namespace sstmac {
namespace hw {

/**
 @class packet_flow_nic
 Network interface compatible with sending packet trains
 */
class packet_flow_nic_packetizer :
  public packetizer
{

 public:
  packet_flow_nic_packetizer(sprockit::sim_parameters* params,
                             event_scheduler* parent, packetizer_callback* cb);

  virtual ~packet_flow_nic_packetizer();

  /**
   * We assume the injection buffer has infinite occupancy
   */
  bool
  spaceToSend(int vn, int num_bits) const override;

  void inject(int vn, long bytes, long byte_offset, message *payload) override;

  /**
   Set up the injection/ejection links to the switch the NIC is connected to
   @param sw The switch that injects/ejects
   */
  void
  set_output(sprockit::sim_parameters* params,
             int port, event_handler* output);

  void
  set_input(sprockit::sim_parameters* params,
            int port, event_handler* input);

  void recv_credit(event* credit);

  virtual void recv_packet(event* ev) = 0;

  link_handler*
  new_payload_handler() const override;

  link_handler*
  new_ack_handler() const override;

 protected:
  void
  recv_packet_common(packet_flow_payload* pkt);

 protected:
  packet_flow_injection_buffer* inj_buffer_;
  packet_flow_eject_buffer* ej_buffer_;

  event_handler* payload_handler_;

  recv_cq completion_queue_;

  node_id my_addr_;

  packet_stats_callback* stat_collector_;
  packet_stats_callback* buf_stats_;
  packet_allocator* pkt_allocator_;

};

class packet_flow_cut_through_packetizer : public packet_flow_nic_packetizer
{
 public:
  packet_flow_cut_through_packetizer(sprockit::sim_parameters* params,
                                     event_scheduler* parent,
                                     packetizer_callback* cb) :
    packet_flow_nic_packetizer(params, parent, cb)
  {
  }

  void recv_packet(event* pkt) override;

};

class packet_flow_simple_packetizer : public packet_flow_nic_packetizer
{
 public:
  packet_flow_simple_packetizer(sprockit::sim_parameters* params,
                                     event_scheduler* parent,
                                     packetizer_callback* cb) :
    packet_flow_nic_packetizer(params, parent, cb)
  {
  }

  void recv_packet(event* pkt) override;

};

}
} // end of namespace sstmac

#endif // PACKET_FLOW_PACKETIZER_H
