#ifndef PACKETFLOW_CROSSBAR_H
#define PACKETFLOW_CROSSBAR_H

#include <sstmac/hardware/pisces/pisces_sender.h>
#include <sstmac/hardware/pisces/pisces_stats_fwd.h>
#include <sstmac/common/stats/stat_global_int_fwd.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/keyword_registration.h>

namespace sstmac {
namespace hw {

class pisces_NtoM_queue :
  public pisces_sender
{
 public:
  virtual ~pisces_NtoM_queue();

  pisces_NtoM_queue(sprockit::sim_parameters* params,
                         event_scheduler* parent);

  int
  thread_id() const {
    return event_subcomponent::thread_id();
  }

  void
  handle_payload(event* ev) override;

  void
  handle_credit(event* ev) override;

  event_handler*
  credit_handler();

  event_handler*
  payload_handler();

  void
  set_input(sprockit::sim_parameters* params,
            int my_inport, int src_outport, event_handler* input) override;

  void
  set_output(sprockit::sim_parameters* params,
             int my_outport, int dst_inport, event_handler* output) override;

  virtual void
  start_message(message* msg);

  void
  configure_mod_ports(int mod);

  void
  configure_div_ports(int div, int num_ports);

  void
  configure_offset_ports(int offset, int num_ports);

  void
  configure_basic_ports(int num_ports);

  inline int
  local_port(int port) const {
    int lp;
//    std::cerr << "port: " << port << std::endl;
    if (port_mod_){
//      std::cerr << "yes port_mod_: " << port_mod_ << std::endl;
      lp =  port % port_mod_;
    } else {
//      std::cerr << "no port_div_: " << port_div_ << " port_offset: "
//                << port_offset_ << std::endl;
      lp = port / port_div_ - port_offset_;
    }
    if (lp < 0) {
      spkt_abort_printf("pisces_NtoM_queue: negative local port %d\n", lp);
    }
    return lp;
  }

  inline int
  local_slot(int port, int vc) const {
    return local_port(port) * num_vc_ + vc;
  }

  void
  set_tile_id(std::string id) {
    tile_id_ = id;
  }

  std::string tile_id() const {
    return tile_id_;
  }

  void
  deadlock_check() override;

  void
  deadlock_check(event* ev) override;

 protected:
  typedef spkt_unordered_map<int, pisces_input> input_map;

  typedef std::vector<pisces_output> output_map;
  typedef std::vector<int> credit_map;
  typedef std::vector<payload_queue> queue_map;

  pisces_bandwidth_arbitrator* arb_;

  input_map inputs_;
  output_map outputs_;
  //indexed by slot number = (port,vc)
  credit_map credits_;
  //indexed by slot number = (port,vc)
  queue_map queues_;

  int num_vc_;
  int port_offset_;
  int port_div_;
  int port_mod_;

  event_handler* credit_handler_;
  event_handler* payload_handler_;

  std::map<int, std::set<int> > deadlocked_channels_;

  std::map<int, std::map<int, std::list<pisces_payload*> > > blocked_messages_;

  std::string tile_id_;

 protected:
  void
  send_payload(pisces_payload* pkt);

  void
  build_blocked_messages();

 private:
  inline int& credit(int port, int vc){
    return credits_[local_slot(port, vc)];
  }

  void resize(int num_ports);

  inline payload_queue& queue(int port, int vc){
    return queues_[local_slot(port, vc)];
  }

  std::string
  input_name(pisces_payload* pkt);

  std::string
  output_name(pisces_payload* pkt);

  event_handler*
  output_handler(pisces_payload* pkt);

};

class pisces_demuxer :
  public pisces_NtoM_queue
{
 public:
  pisces_demuxer(sprockit::sim_parameters* params,
                      event_scheduler* parent);

  std::string
  pisces_name() const override {
    return "demuxer" + tile_id();
  }


};


class pisces_muxer :
  public pisces_NtoM_queue
{
 public:
  pisces_muxer(sprockit::sim_parameters* params,
                    event_scheduler* parent);

  std::string
  pisces_name() const override {
    return "muxer" + tile_id();
  }

};

class pisces_crossbar :
  public pisces_NtoM_queue
{
 public:
  pisces_crossbar(sprockit::sim_parameters* params,
                       event_scheduler* parent);

  std::string
  pisces_name() const override {
    return "crossbar" + tile_id();
  }

};


}
}

#endif // PACKETFLOW_CROSSBAR_H

