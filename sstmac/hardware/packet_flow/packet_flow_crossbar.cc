#include <sstmac/hardware/packet_flow/packet_flow_crossbar.h>
#include <sstmac/hardware/packet_flow/packet_flow_stats.h>
#include <sstmac/hardware/packet_flow/packet_flow_tiled_switch.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/common/stats/stat_global_int.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/runtime.h>
#include <iomanip>
#include <sprockit/util.h>

#define PRINT_FINISH_DETAILS 0

RegisterNamespaces("bytes_sent");

namespace sstmac {
namespace hw {


packet_flow_crossbar::packet_flow_crossbar(
  timestamp send_lat,
  timestamp credit_lat,
  double out_bw,
  int num_vc,
  int buffer_size,
  packet_flow_bandwidth_arbitrator* arb) :
  packet_flow_NtoM_queue(send_lat, credit_lat, out_bw, num_vc, buffer_size, arb),
  name_(0)
{
}

packet_flow_crossbar::packet_flow_crossbar(
  timestamp send_lat,
  timestamp credit_lat,
  int num_vc,
  int buffer_size,
  const char* name) :
  packet_flow_NtoM_queue(send_lat, credit_lat, num_vc, buffer_size),
  name_(name)
{
}

packet_flow_demuxer::packet_flow_demuxer(
  timestamp send_lat,
  timestamp credit_lat,
  int num_vc,
  int buffer_size) :
  packet_flow_NtoM_queue(send_lat, credit_lat, num_vc, buffer_size)
{
}

packet_flow_muxer::packet_flow_muxer(
  timestamp send_lat,
  timestamp credit_lat,
  double out_bw,
  int num_vc,
  int buffer_size,
  packet_flow_bandwidth_arbitrator *arb) :
  packet_flow_NtoM_queue(send_lat, credit_lat, out_bw, num_vc, buffer_size, arb)
{
}

packet_flow_NtoM_queue::packet_flow_NtoM_queue(
  timestamp send_lat,
  timestamp credit_lat,
  int num_vc,
  int buffer_size) :
  num_vc_(num_vc),
  buffer_size_(buffer_size),
  arb_tmpl_(0),
  packet_flow_sender(send_lat, credit_lat)
{
}

packet_flow_NtoM_queue::packet_flow_NtoM_queue(
  timestamp send_lat,
  timestamp credit_lat,
  double out_bw,
  int num_vc,
  int buffer_size,
  packet_flow_bandwidth_arbitrator* arb) :
  packet_flow_sender(send_lat, credit_lat),
  num_vc_(num_vc),
  buffer_size_(buffer_size),
  arb_tmpl_(arb),
  out_bw_(out_bw)
{
}

packet_flow_NtoM_queue::~packet_flow_NtoM_queue()
{
  int narbs = port_arbitrators_.size();
  for (int i=0; i < narbs; ++i){
    packet_flow_bandwidth_arbitrator* arb = port_arbitrators_[i];
    if (arb) delete arb;
  }
  delete arb_tmpl_;
}

void
packet_flow_NtoM_queue::deadlock_check()
{
  for (int i=0; i < queues_.size(); ++i){
    payload_queue& queue = queues_[i];
    packet_flow_payload* pkt = queue.front();
    if (pkt){
      deadlocked_channels_[pkt->next_port()].insert(pkt->next_vc());
      packet_flow_output& poutput = outputs_[local_port(pkt->next_port())];
      event_handler* output = output_handler(pkt);
      pkt->set_inport(poutput.dst_inport);
      int vc = update_vc_ ? pkt->next_vc() : pkt->vc();
      std::cerr << "Starting deadlock check on " << to_string() << " on queue " << i 
        << " going to " << output->to_string() 
        << " outport=" << pkt->next_port()
        << " inport=" << pkt->inport()
        << " vc=" << vc
        << " for message " << pkt->to_string()
        << std::endl;
      output->deadlock_check(pkt);
    }
  }
}

void
packet_flow_NtoM_queue::build_blocked_messages()
{
  //std::cerr << "\tbuild blocked messages on " << to_string() << std::endl;
  for (int i=0; i < queues_.size(); ++i){
    payload_queue& queue = queues_[i];
    packet_flow_payload* pkt = queue.pop(1000000);
    while (pkt){
      blocked_messages_[pkt->inport()][pkt->vc()].push_back(pkt);
      //std::cerr << "\t\t" << "into port=" << msg->inport() << " vc=" << msg->vc()
      //  << " out on port=" << msg->port() << " vc=" << msg->routable_message::vc() << std::endl;
      pkt = queue.pop(10000000);
    }
  }
}

void
packet_flow_NtoM_queue::deadlock_check(event* ev)
{
  if (blocked_messages_.empty()){
    build_blocked_messages();
  }

  packet_flow_payload* payload = safe_cast(packet_flow_payload, ev);
  int outport = payload->next_port();
  int inport = payload->inport();
  int vc = update_vc_ ? payload->next_vc() : payload->vc();
  std::set<int>& deadlocked_vcs = deadlocked_channels_[outport];
  if (deadlocked_vcs.find(vc) != deadlocked_vcs.end()){
    spkt_throw_printf(sprockit::value_error,
      "found deadlock:\n%s", to_string().c_str());
  }

  deadlocked_channels_[outport].insert(vc);

  std::list<packet_flow_payload*>& blocked = blocked_messages_[inport][vc];
  if (blocked.empty()){
    spkt_throw_printf(sprockit::value_error,
      "channel is NOT blocked on deadlock check on outport=%d inport=%d vc=%d",
      outport, inport, vc);
  } else {
    packet_flow_payload* next = blocked.front();
    packet_flow_output& poutput = outputs_[local_port(outport)];
    event_handler* output = output_handler(next);
    next->set_inport(poutput.dst_inport);
    std::cerr << to_string() << " going to " << output->to_string() 
      << " outport=" << next->next_port()
      << " inport=" << next->inport()
      << " vc=" << next->next_vc()
      << " : " << next->to_string()
      << std::endl;
    output->deadlock_check(next);
  }
}

std::string
packet_flow_NtoM_queue::input_name(packet_flow_payload* pkt)
{
  event_handler* handler = inputs_[pkt->inport()].handler;
  return handler->to_string();
}

event_handler*
packet_flow_NtoM_queue::output_handler(packet_flow_payload* pkt)
{
  int loc_port = local_port(pkt->next_port());
  event_handler* handler = outputs_[loc_port].handler;
  packet_flow_tiled_switch* sw = test_cast(packet_flow_tiled_switch, handler);
  if (!handler) {
    for (int i=0; i< outputs_.size(); ++i) {
      std::cerr << "outputs[" << i << "] = " << outputs_[i].handler << "\n";
    }
    spkt_throw_printf(sprockit::value_error,
      "no output handler for port %d:%d",
      pkt->next_port(), loc_port);
  }
  if (sw){
    return sw->demuxer(pkt->next_port());
  } else {
    return handler;
  }
}

std::string
packet_flow_NtoM_queue::output_name(packet_flow_payload* pkt)
{
  return output_handler(pkt)->to_string();
}

void
packet_flow_NtoM_queue::send_payload(packet_flow_payload* pkt)
{
  int loc_port = local_port(pkt->next_port());
  packet_flow_bandwidth_arbitrator* arb = port_arbitrators_[loc_port];
  packet_flow_debug(
    "On %s:%p mapped port:%d vc:%d to local port %d handling {%s} going to %s:%p",
     to_string().c_str(), this,
     pkt->next_port(), pkt->next_vc(),
     loc_port,
     pkt->to_string().c_str(),
     output_name(pkt).c_str(),
     output_handler(pkt));
  send(arb, pkt, inputs_[pkt->inport()], outputs_[loc_port]);
}

void
packet_flow_NtoM_queue::handle_credit(packet_flow_credit* pkt)
{
  int outport = pkt->port();
  int vc = pkt->vc();
  int channel = outport * num_vc_ + vc;

  int& num_credits = credit(outport, vc);

  packet_flow_debug(
    "On %s:%p with %d credits, handling credit {%s} for port:%d vc:%d channel:%d",
     to_string().c_str(), this,
     num_credits,
     pkt->to_string().c_str(),
     outport, vc, channel);

  num_credits += pkt->num_credits();

  packet_flow_payload* payload = queue(outport, vc).pop(num_credits);
  if (payload) {
    num_credits -= payload->num_bytes();
    send_payload(payload);
  }

  delete pkt;
}

void
packet_flow_NtoM_queue::handle_routed_payload(packet_flow_payload* pkt)
{
  int dst_vc = update_vc_ ? pkt->next_vc() : pkt->vc();
  int dst_port = pkt->next_port();

  int& num_credits = credit(dst_port, dst_vc);
   packet_flow_debug(
    "On %s:%p with %d credits, handling {%s} for port:%d vc:%d -> local port %d going to",// %s:%p",
     to_string().c_str(), this,
     num_credits,
     pkt->to_string().c_str(),
     dst_port,
     dst_vc,
     local_port(dst_port));
     //output_name(msg).c_str(), output_handler(msg));

  if (dst_vc < 0 || dst_port < 0){
    spkt_throw_printf(sprockit::value_error,
       "On %s handling {%s}, got negative vc,port %d,%d",
        to_string().c_str(), pkt->to_string().c_str(), dst_port, dst_vc);
  }

  if (num_credits >= pkt->num_bytes()) {
    num_credits -= pkt->num_bytes();
    send_payload(pkt);
  }
  else {
    packet_flow_debug(
      "On %s:%p, pushing back on queue %d=(%d,%d) for nq=%d nvc=%d mapper=(%d,%d,%d)",
      to_string().c_str(), this,
      local_slot(dst_port, dst_vc), dst_port, dst_vc, queues_.size(), num_vc_,
      port_offset_, port_div_, port_mod_);
    queue(dst_port, dst_vc).push_back(pkt);
  }
}

void
packet_flow_NtoM_queue::init_credits(int port, int num_credits)
{
  int num_credits_per_vc = num_credits / num_vc_;
  for (int i=0; i < num_vc_; ++i) {
    debug_printf(sprockit::dbg::packet_flow_config,
      "On %s:%p, initing %d credits on port:%d vc:%d",
      to_string().c_str(), this,
      num_credits_per_vc,
      port, i);
    credit(port, i) = num_credits_per_vc;
  }
}

void
packet_flow_NtoM_queue::resize(int num_ports)
{
  port_arbitrators_.resize(num_ports);
  outputs_.resize(num_ports);
  queues_.resize(num_ports*num_vc_);
  credits_.resize(num_ports*num_vc_);
}

void
packet_flow_NtoM_queue::configure_basic_ports(int num_ports)
{
  port_offset_ = 0;
  port_mod_ = 0;
  port_div_ = 1;
  resize(num_ports);
}

void
packet_flow_NtoM_queue::configure_div_ports(int div, int max_port)
{
  debug_printf(sprockit::dbg::packet_flow_config | sprockit::dbg::packet_flow,
    "On %s configured for div local ports: div=%d,max=%d",
    to_string().c_str(), div, max_port);

  port_offset_ = 0;
  port_mod_ = 0;
  port_div_ = div;
  int my_num_ports = (max_port + 1) / port_div_;
  resize(my_num_ports);
}

void
packet_flow_NtoM_queue::configure_offset_ports(int offset, int max_port)
{
  debug_printf(sprockit::dbg::packet_flow_config | sprockit::dbg::packet_flow,
    "On %s configured for offset local ports: offset=%d,max=%d",
    to_string().c_str(), offset, max_port);

  port_offset_ = offset;
  port_mod_ = 0;
  port_div_ = 1;
  resize(max_port + 1);
}

void
packet_flow_NtoM_queue::configure_mod_ports(int mod)
{
  debug_printf(sprockit::dbg::packet_flow_config | sprockit::dbg::packet_flow,
    "On %s configured for modulo local ports: m=%d",
    to_string().c_str(), mod);

  port_offset_ = 0;
  port_mod_ = mod;
  port_div_ = 1;
  resize(mod);
}

void
packet_flow_NtoM_queue::set_input(int my_inport, int src_outport,
                                event_handler* input)
{
  debug_printf(sprockit::dbg::packet_flow_config | sprockit::dbg::packet_flow,
    "On %s:%d setting input %s:%d",
    to_string().c_str(), my_inport,
    input->to_string().c_str(), src_outport);

  packet_flow_input inp;
  inp.src_outport = src_outport;
  inp.handler = input;
  inputs_[my_inport] = inp;
}

void
packet_flow_NtoM_queue::set_output(int my_outport, int dst_inport,
                                 event_handler* output)
{
  int loc_port = local_port(my_outport);
  debug_printf(sprockit::dbg::packet_flow_config | sprockit::dbg::packet_flow,
    "On %s:%d setting output %s:%d -> local port %d, mapper=(%d,%d,%d) of %d",
    to_string().c_str(), my_outport,
    output->to_string().c_str(), dst_inport,
    loc_port, port_offset_, port_div_, port_mod_,
    outputs_.size());

  packet_flow_output out;
  out.dst_inport = dst_inport;
  out.handler = output;

  outputs_[loc_port] = out;
  //credits_[loc_port] = std::vector<long>(num_vc_, 0L);
  //queues_[loc_port] = std::vector<payload_queue>(num_vc_);
  port_arbitrators_[loc_port] = arb_tmpl_ ? arb_tmpl_->clone(out_bw_) : 0;
}

#if PRINT_FINISH_DETAILS
void
print_msg(const std::string& prefix, switch_id addr, packet_flow_payload* pkt)
{
  structured_topology* top = safe_cast(structured_topology, sstmac_runtime::current_topology());
  coordinates src_coords = top->get_node_coords(msg->fromaddr());
  src_coords.resize(3);
  coordinates dst_coords = top->get_node_coords(msg->toaddr());
  dst_coords.resize(3);
  coordinates my_coords = top->get_switch_coords(addr);
  coutn << prefix << std::setw(12) << src_coords.to_string();
  coutn << "->";
  coutn << std::setw(12) << my_coords.to_string();
  coutn << "->";
  coutn << std::setw(12) << dst_coords.to_string();
  coutn << "  vc=" << msg->rinfo()->vc()
    << " port=" << msg->rinfo()->port() << std::endl;
}
#endif

void
packet_flow_NtoM_queue::start_message(message* msg)
{
  spkt_throw(sprockit::illformed_error,
    "packet_flow_NtoM_queue:: should never start a flow");
}

#if PRINT_FINISH_DETAILS
  structured_topology* top = safe_cast(structured_topology, sstmac_runtime::current_topology());
  coordinates my_coords = top->get_switch_coords(router_->get_addr());
  coutn << "Crossbar " << my_coords.to_string() << "\n";
  { queue_map::iterator it, end = queues_.end();
  for (it = queues_.begin(); it != end; ++it){
    std::vector<payload_queue>& vec = it->second;
    coutn << "\tPort " << it->first << "\n";
    for (int i=0; i < vec.size(); ++i){
        coutn << "\t\tVC " << i << std::endl;
        payload_queue& que = vec[i];
        payload_queue::iterator pit, pend = que.end();
        for (pit = que.begin(); pit != pend; ++pit){
            packet_flow_payload* pkt = *pit;
            print_msg("\t\t\tPending: ", router_->get_addr(), msg);
        }
    }
  } }
  { credit_map::iterator it, end = credits_.end();
  for (it = credits_.begin(); it != end; ++it){
    std::vector<long>& vec = it->second;
    coutn << "\tPort " << it->first << "\n";
    for (int i=0; i < vec.size(); ++i){
      coutn << "\t\tVC " << i
            << " = " << vec[i] << " credits" << std::endl;

    }
  } }
#endif

}
}


