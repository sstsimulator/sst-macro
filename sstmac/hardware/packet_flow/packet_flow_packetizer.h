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

class packet_flow_packetizer :
  public packetizer
{
 public:
  void handle(event *ev);

 protected:
  packet_flow_packetizer(sprockit::sim_parameters* params,
           event_scheduler* parent, packetizer_callback* cb)
    : packetizer(params, parent, cb){}

 private:
  virtual void recv_credit(packet_flow_credit* ev) = 0;

  virtual void recv_packet(packet_flow_payload* ev) = 0;

};

/**
 @class packet_flow_nic
 Network interface compatible with sending packet trains
 */
class packet_flow_nic_packetizer :
  public packet_flow_packetizer
{

 public:
  packet_flow_nic_packetizer(sprockit::sim_parameters* params,
                             event_scheduler* parent, packetizer_callback* cb);

  std::string
  to_string() const {
    return sprockit::printf("packet_flow_nic_packetizer");
  }

  virtual ~packet_flow_nic_packetizer();

  void setNotify(event_handler *handler);

  /**
   * We assume the injection buffer has infinite occupancy
   */
  bool
  spaceToSend(int vn, int num_bits) const;

  void inject(int vn, long bytes, long byte_offset, message *payload);

  /**
   Set up the injection/ejection links to the switch the NIC is connected to
   @param sw The switch that injects/ejects
   */
  void
  set_output(sprockit::sim_parameters* params,
             int port, connectable* output);

  void
  set_input(sprockit::sim_parameters* params,
            int port, connectable* input);

  void
  set_acker(event_handler* handler);

 protected:
  void
  recv_packet_common(packet_flow_payload* pkt);

  virtual void
  recv_credit(packet_flow_credit* credit);

 protected:
  packet_flow_injection_buffer* inj_buffer_;
  packet_flow_eject_buffer* ej_buffer_;

  recv_cq completion_queue_;

  node_id my_addr_;

  packet_sent_stats* stat_collector_;
  packet_sent_stats* buf_stats_;
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

  void recv_packet(packet_flow_payload* pkt);

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

  void recv_packet(packet_flow_payload* pkt);

};

}
} // end of namespace sstmac

#endif // PACKET_FLOW_PACKETIZER_H
