#ifndef pisces_packetizer_H
#define pisces_packetizer_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/common/packetizer.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/hardware/pisces/packet_allocator_fwd.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/interfaces/simpleNetwork.h>
#endif

namespace sstmac {
namespace hw {

/**
 @class pisces_nic
 Network interface compatible with sending packet trains
 */
class pisces_packetizer :
  public packetizer
#if SSTMAC_INTEGRATED_SST_CORE
 ,public SST::Interfaces::SimpleNetwork
#endif
{

 public:
  pisces_packetizer(sprockit::sim_parameters* params,
                    event_scheduler* parent);

  virtual ~pisces_packetizer();

  void init(unsigned int phase) override;

  void setup() override;

  /**
   * We assume the injection buffer has infinite occupancy
   */
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

  virtual void recv_packet(event* ev) = 0;

  link_handler*
  new_payload_handler() const override;

  link_handler*
  new_ack_handler() const override;

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

  packet_stats_callback* stat_collector_;
  packet_stats_callback* buf_stats_;
  packet_allocator* pkt_allocator_;

#if SSTMAC_INTEGRATED_SST_CORE
 public:
  void init_links(sprockit::sim_parameters* parmas);

  bool send(Request *req, int vn) override;

  Request* recv(int vn) override;

  bool requestToReceive(int vn) override;

  bool initialize(const std::string &portName,
                  const SST::UnitAlgebra &link_bw,
                  int vns,
                  const SST::UnitAlgebra &in_buf_size,
                  const SST::UnitAlgebra &out_buf_size) override;

  void setNotifyOnSend(HandlerBase* functor) override {
    send_functor_ = functor;
  }

  void setNotifyOnReceive(HandlerBase* functor) override {
    recv_functor_ = functor;
  }

  bool isNetworkInitialized() const override {
    return initialized_;
  }

  nid_t
  getEndpointID() const override {
    return my_addr_;
  }

  const SST::UnitAlgebra& getLinkBW() const override {
    return sst_link_bw_;
  }

  using SST::Interfaces::SimpleNetwork::Request;

  virtual void
  sendInitData(Request* req) override;

  virtual Request*
  recvInitData() override;

 protected:
  pisces_packetizer(sprockit::sim_parameters *params, SST::Component* comp);

 private:
  class sst_component_wrapper : public event_scheduler
  {
   public:
    sst_component_wrapper(sprockit::sim_parameters* params,
                          event_loc_id loc_id,
                          SST::Component* comp) :
      event_scheduler(params, comp->getId(), loc_id, nullptr, nullptr)
    {
      self_link_ = comp->configureSelfLink("self", time_converter_,
                      new SST::Event::Handler<SSTIntegratedComponent>(this,
                        &SSTIntegratedComponent::handle_self_link));
    }
    void connect_output(sprockit::sim_parameters *params, int src_outport, int dst_inport, event_handler *mod);
    void connect_input(sprockit::sim_parameters *params, int src_outport, int dst_inport, event_handler *mod);
    link_handler* payload_handler(int port) const {
      return nullptr;
    }
    link_handler* ack_handler(int port) const {
      return nullptr;
    }
  };

  sst_component_wrapper* wrapper_;
  SST::UnitAlgebra sst_link_bw_;
  HandlerBase* send_functor_;
  HandlerBase* recv_functor_;
  bool initialized_;

#endif

};

class pisces_cut_through_packetizer : public pisces_packetizer
{
 public:
  pisces_cut_through_packetizer(sprockit::sim_parameters* params,
                                event_scheduler* parent) :
    pisces_packetizer(params, parent)
  {
  }

  void recv_packet(event* pkt) override;

#if SSTMAC_INTEGRATED_SST_CORE
  pisces_cut_through_packetizer(sprockit::sim_parameters *params, SST::Component* comp) :
    pisces_packetizer(params, comp)
  {
  }
#endif

};

class pisces_simple_packetizer : public pisces_packetizer
{
 public:
  pisces_simple_packetizer(sprockit::sim_parameters* params,
                           event_scheduler* parent) :
    pisces_packetizer(params, parent)
  {
  }

  void recv_packet(event* pkt) override;

};

}
} // end of namespace sstmac

#endif // pisces_packetizer_H
