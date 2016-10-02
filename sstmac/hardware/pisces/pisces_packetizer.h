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

  virtual void recv_packet(event* ev) = 0;

  link_handler*
  new_payload_handler() const override;

  link_handler*
  new_credit_handler() const override;

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

  std::string
  to_string() const override {
    return "cut through packetizer";
  }

};

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

  void recv_packet(event* pkt) override;

};

#if SSTMAC_INTEGRATED_SST_CORE
class pisces_simple_network :
  public SST::Interfaces::SimpleNetwork,
  public event_scheduler
{
 public:
  pisces_simple_network(sprockit::sim_parameters *params, SST::Component* comp);

  std::string
  to_string() const override {
    return "PISCES simple network";
  }

  void init_links(sprockit::sim_parameters* parmas);

  /**
   * @brief packet_arrived Callback when packets arrive off the network
   * @param ev
   */
  void packet_arrived(event* ev);

  void init(unsigned int phase) override;

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
   return nid_;
  }

  const SST::UnitAlgebra& getLinkBW() const override {
   return sst_link_bw_;
  }

  bool
  spaceToSend(int vn, int num_bits) override;

  using SST::Interfaces::SimpleNetwork::Request;

  virtual void
  sendInitData(Request* req) override;

  virtual Request*
  recvInitData() override;

 private:
  event_loc_id init_loc(sprockit::sim_parameters* params);

  nid_t nid_;

  std::list<Request*> vn_reqs_[2];

  pisces_injection_buffer* inj_buffer_;
  pisces_eject_buffer* ej_buffer_;
  SST::Link* logp_link_;

  SST::UnitAlgebra sst_link_bw_;
  HandlerBase* send_functor_;
  HandlerBase* recv_functor_;
  bool initialized_;

};
#endif

}
} // end of namespace sstmac

#endif // pisces_packetizer_H
