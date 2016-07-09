#ifndef PACKETFLOW_CROSSBAR_H
#define PACKETFLOW_CROSSBAR_H

#include <sstmac/hardware/packet_flow/packet_flow_sender.h>
#include <sstmac/hardware/packet_flow/packet_flow_stats_fwd.h>
#include <sstmac/common/stats/stat_global_int_fwd.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/keyword_registration.h>

namespace sstmac {
namespace hw {

class packet_flow_NtoM_queue :
  public packet_flow_sender
{
 public:
  virtual ~packet_flow_NtoM_queue();

  packet_flow_NtoM_queue(
    timestamp send_lat,
    timestamp credit_lat,
    double out_bw,
    int num_vc,
    int buffer_size,
    packet_flow_bandwidth_arbitrator* arb);

  packet_flow_NtoM_queue(
    timestamp send_lat,
    timestamp credit_lat,
    int num_vc,
    int buffer_size);

  int
  thread_id() const {
    return event_subscheduler::thread_id();
  }

  virtual void
  do_handle_payload(packet_flow_payload* pkt){
    handle_routed_payload(pkt);
  }

  void
  set_input(int my_inport, int src_outport, event_handler* input);

  void
  set_output(int my_outport, int dst_inport, event_handler* output);

  virtual void
  handle_credit(packet_flow_credit* msg);

  virtual void
  start_message(message* msg);

  void
  init_credits(int port, int num_credits);

  int
  num_initial_credits() const {
    return buffer_size_;
  }

  int
  buffer_size() const {
    return buffer_size_;
  }

  void
  configure_mod_ports(int mod);

  void
  configure_div_ports(int div, int max_port);

  void
  configure_offset_ports(int offset, int max_port);

  void
  configure_basic_ports(int num_ports);

  inline int
  local_port(int port) const {
    if (port_mod_){
      return port % port_mod_;
    } else {
      return port / port_div_ - port_offset_;
    }
  }

  inline int
  local_slot(int port, int vc) const {
    return local_port(port) * num_vc_ + vc;
  }

  packet_flow_bandwidth_arbitrator*&
  port_arbitrator(int port){
    return port_arbitrators_[local_port(port)];
  }

  void
  deadlock_check();

  void
  deadlock_check(event* ev);

 protected:
  struct request {
    int port;
    int num_bytes;
  };

  typedef std::list<request> buffer_request_list;

  typedef spkt_unordered_map<int, buffer_request_list> xbar_request_map;

  void
  handle_routed_payload(packet_flow_payload* pkt);

 protected:
  typedef spkt_unordered_map<int, packet_flow_input> input_map;

  typedef std::vector<packet_flow_output> output_map;
  typedef std::vector<int> credit_map;
  typedef std::vector<payload_queue> queue_map;

  packet_flow_bandwidth_arbitrator* arb_tmpl_;

  std::vector<packet_flow_bandwidth_arbitrator*> port_arbitrators_;

  input_map inputs_;
  //indexed by slot number = (port,vc)
  output_map outputs_;
  //indexed by slot number = (port,vc)
  credit_map credits_;
  //indexed by slot number = (port,vc)
  queue_map queues_;

  int num_vc_;
  int buffer_size_;
  int port_offset_;
  int port_div_;
  int port_mod_;

  double out_bw_;

  std::map<int, std::set<int> > deadlocked_channels_;

  std::map<int, std::map<int, std::list<packet_flow_payload*> > > blocked_messages_;

 protected:
  void
  send_payload(packet_flow_payload* pkt);

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
  input_name(packet_flow_payload* pkt);

  std::string
  output_name(packet_flow_payload* pkt);

  event_handler*
  output_handler(packet_flow_payload* pkt);

};

class packet_flow_demuxer :
  public packet_flow_NtoM_queue
{
 public:
  virtual std::string
  to_string() const {
    return "packet_flow_demuxer";
  }

  packet_flow_demuxer(
    timestamp send_lat,
    timestamp credit_lat,
    int num_vc,
    int buffer_size);

  std::string
  packet_flow_name() const {
    return "demuxer";
  }
};


class packet_flow_muxer :
  public packet_flow_NtoM_queue
{
 public:
  packet_flow_muxer(
    timestamp send_lat,
    timestamp credit_lat,
    double out_bw,
    int num_vc,
    int buffer_size,
    packet_flow_bandwidth_arbitrator* arb);

  std::string
  packet_flow_name() const {
    return "muxer";
  }

};

class packet_flow_crossbar :
  public packet_flow_NtoM_queue
{
 public:
  packet_flow_crossbar(
    timestamp send_lat,
    timestamp credit_lat,
    double out_bw,
    int num_vc,
    int buffer_size,
    packet_flow_bandwidth_arbitrator* arb);

  packet_flow_crossbar(
    timestamp send_lat,
    timestamp credit_lat,
    int num_vc,
    int buffer_size,
    const char* name = 0);

  std::string
  packet_flow_name() const {
    if (name_) return name_;
    else return "crossbar";
  }

 private:
  const char* name_;

};


}
}

#endif // PACKETFLOW_CROSSBAR_H

