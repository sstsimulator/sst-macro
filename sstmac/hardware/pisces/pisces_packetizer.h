#ifndef pisces_packetizer_H
#define pisces_packetizer_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/common/packetizer.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/hardware/pisces/packet_allocator_fwd.h>
#include <sstmac/hardware/network/network_message.h>

namespace sstmac {
namespace hw {

/**
 @class pisces_packetizer
 */
class pisces_packetizer :
  public packetizer
{

 public:
  pisces_packetizer(sprockit::sim_parameters* params,
                    event_scheduler* parent);

  virtual ~pisces_packetizer();

  void init(unsigned int phase) override;

  void setup() override;

  bool
  spaceToSend(int vn, int num_bits) override;

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

  /**
   * @brief recv_packet Receive new packet arriving from network.
   *  Assemble packet into corresponding message (flow)
   * @param ev
   */
  virtual void recv_packet(event* ev) = 0;

  link_handler*
  new_payload_handler() const override;

  link_handler*
  new_credit_handler() const override;

  void deadlock_check() override;

 protected:
  void
  recv_packet_common(pisces_payload* pkt);

 private:
  void init(sprockit::sim_parameters* params, event_scheduler* parent);

 protected:
  pisces_injection_buffer* inj_buffer_;
  pisces_eject_buffer* ej_buffer_;

  event_handler* payload_handler_;

  recv_cq completion_queue_;

  node_id my_addr_;

  packet_stats_callback* inj_stats_;
  packet_stats_callback* ej_stats_;
  packet_allocator* pkt_allocator_;

};

/**
 * @brief The pisces_cut_through_packetizer class
 * See #pisces_cut_through_bandwidth_arbitrator.
 */
class pisces_cut_through_packetizer : public pisces_packetizer
{
 public:
  pisces_cut_through_packetizer(sprockit::sim_parameters* params,
                                event_scheduler* parent) :
    pisces_packetizer(params, parent)
  {
  }

  /**
   * @brief recv_packet Invoked when head flit arrives for packet.
   *  Cut-through delays must be added before packet can be processed.
   * @param pkt
   */
  void recv_packet(event* pkt) override;

  std::string
  to_string() const override {
    return "cut through packetizer";
  }

};

/**
 * @brief The pisces_simple_packetizer class
 */
class pisces_simple_packetizer : public pisces_packetizer
{
 public:
  pisces_simple_packetizer(sprockit::sim_parameters* params,
                           event_scheduler* parent) :
    pisces_packetizer(params, parent)
  {
  }

  std::string
  to_string() const override {
    return "simple packetizer";
  }

  /**
   * @brief recv_packet Invoked when tail flit arrives for packet.
   *  No extra delays must be processed for packet
   * @param pkt
   */
  void recv_packet(event* pkt) override;

};

}
} // end of namespace sstmac

#endif // pisces_packetizer_H
