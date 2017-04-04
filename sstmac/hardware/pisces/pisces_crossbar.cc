#include <sstmac/hardware/pisces/pisces_crossbar.h>
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sstmac/hardware/pisces/pisces_tiled_switch.h>
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

pisces_crossbar::pisces_crossbar(
  sprockit::sim_parameters* params,
  event_scheduler* parent) :
  pisces_NtoM_queue(params, parent)
{
}

pisces_demuxer::pisces_demuxer(
  sprockit::sim_parameters* params,
  event_scheduler* parent) :
  pisces_NtoM_queue(params, parent)
{
}

pisces_muxer::pisces_muxer(
  sprockit::sim_parameters* params,
  event_scheduler* parent) :
  pisces_NtoM_queue(params, parent)
{

}

pisces_NtoM_queue::
pisces_NtoM_queue(sprockit::sim_parameters* params,
                       event_scheduler* parent)
  : pisces_sender(params, parent),
    credit_handler_(nullptr),
    payload_handler_(nullptr),
    tile_id_("")
{
  num_vc_ = params->get_int_param("num_vc");
  arb_ = pisces_bandwidth_arbitrator_factory::get_param("arbitrator", params);
}

event_handler*
pisces_NtoM_queue::credit_handler()
{
  if (!credit_handler_){
    credit_handler_ = new_handler(this, &pisces_NtoM_queue::handle_credit);
  }
  return credit_handler_;
}

event_handler*
pisces_NtoM_queue::payload_handler()
{
  if (!payload_handler_){
    payload_handler_ = new_handler(this, &pisces_NtoM_queue::handle_payload);
  }
  return payload_handler_;
}

pisces_NtoM_queue::~pisces_NtoM_queue()
{
  if (arb_) delete arb_;
  if (credit_handler_) delete credit_handler_;
}

void
pisces_NtoM_queue::deadlock_check()
{
  for (int i=0; i < queues_.size(); ++i){
    payload_queue& queue = queues_[i];
    pisces_payload* pkt = queue.front();
    while (pkt){
      deadlocked_channels_[pkt->next_port()].insert(pkt->next_vc());
      pisces_output& poutput = outputs_[local_port(pkt->next_port())];
      event_handler* output = output_handler(pkt);
      if (output){
        pkt->set_inport(poutput.dst_inport);
        int vc = update_vc_ ? pkt->next_vc() : pkt->vc();
        std::cerr << "Starting deadlock check on " << to_string()
                  << " on queue " << i
                  << " going to " << output->to_string()
                  << " outport=" << pkt->next_port()
                  << " inport=" << pkt->inport()
                  << " vc=" << vc
                  << " for message " << pkt->to_string()
                  << std::endl;
        output->deadlock_check(pkt);
      }
      queue.pop(1000000);
      pkt = queue.front();
    }
  }
}

void
pisces_NtoM_queue::build_blocked_messages()
{
  for (int i=0; i < queues_.size(); ++i){
    payload_queue& queue = queues_[i];
    pisces_payload* pkt = queue.pop(1000000);
    while (pkt){
      blocked_messages_[pkt->inport()][pkt->vc()].push_back(pkt);
      pkt = queue.pop(10000000);
    }
  }
}

void
pisces_NtoM_queue::deadlock_check(event* ev)
{
  if (blocked_messages_.empty()){
    build_blocked_messages();
  }

  pisces_payload* payload = safe_cast(pisces_payload, ev);
  int outport = payload->next_port();
  int inport = payload->inport();
  int vc = update_vc_ ? payload->next_vc() : payload->vc();
  std::set<int>& deadlocked_vcs = deadlocked_channels_[outport];
  if (deadlocked_vcs.find(vc) != deadlocked_vcs.end()){
    spkt_throw_printf(sprockit::value_error,
      "found deadlock:\n%s", to_string().c_str());
  }

  deadlocked_channels_[outport].insert(vc);

  std::list<pisces_payload*>& blocked = blocked_messages_[inport][vc];
  if (blocked.empty()){
    spkt_throw_printf(sprockit::value_error,
      "channel is NOT blocked on deadlock check on outport=%d inport=%d vc=%d",
      outport, inport, vc);
  } else {
    pisces_payload* next = blocked.front();
    pisces_output& poutput = outputs_[local_port(outport)];
    event_handler* output = output_handler(next);
    next->set_inport(poutput.dst_inport);
    output->deadlock_check(next);
  }
}

std::string
pisces_NtoM_queue::input_name(pisces_payload* pkt)
{
  event_handler* handler = inputs_[pkt->inport()].handler;
  return handler->to_string();
}

event_handler*
pisces_NtoM_queue::output_handler(pisces_payload* pkt)
{
  int loc_port = local_port(pkt->next_port());
  event_handler* handler = outputs_[loc_port].handler;
  if (!handler)
    return nullptr;

  //pisces_tiled_switch* sw = test_cast(pisces_tiled_switch, handler);
  //if (sw){
  //  return sw->demuxer(pkt->next_port());
  //} else {
    return handler;
  //}
}

std::string
pisces_NtoM_queue::output_name(pisces_payload* pkt)
{
  return output_handler(pkt)->to_string();
}

void
pisces_NtoM_queue::send_payload(pisces_payload* pkt)
{
  auto rpkt = static_cast<pisces_routable_packet*>(pkt);
  pisces_debug(
        "On %s:%p local_port:%d vc:%d sending {%s} going to %s:%p",
        to_string().c_str(), this,
        rpkt->local_outport(), pkt->next_vc(),
        pkt->to_string().c_str(),
        output_name(pkt).c_str(),
        output_handler(pkt));
  send(arb_, pkt, inputs_[pkt->inport()], outputs_[rpkt->local_outport()]);
}

void
pisces_NtoM_queue::handle_credit(event* ev)
{
  pisces_debug(
    "On %s:%p handling credit",
     to_string().c_str(), this);
  pisces_credit* pkt = static_cast<pisces_credit*>(ev);
  int loc_outport = pkt->port();
  int vc = pkt->vc();
  int channel = loc_outport * num_vc_ + vc;

  int& num_credits = credit(loc_outport, vc);

  pisces_debug(
    "On %s:%p with %d credits, handling credit {%s} for local port:%d vc:%d channel:%d",
     to_string().c_str(), this,
     num_credits,
     pkt->to_string().c_str(),
     loc_outport, vc, channel);

  num_credits += pkt->num_credits();

  pisces_payload* payload = queue(loc_outport, vc).pop(num_credits);
  if (payload) {
    num_credits -= payload->num_bytes();
    send_payload(payload);
  }

  delete pkt;
}

void
pisces_NtoM_queue::handle_payload(event* ev)
{
  auto pkt = static_cast<pisces_routable_packet*>(ev);
  pkt->set_arrival(now());

  int dst_vc = update_vc_ ? pkt->next_vc() : pkt->routable::vc();
  int glob_port = pkt->global_outport();
  int loc_port = local_port(glob_port);
  pkt->set_local_outport(loc_port);
  pisces_debug(
   "On %s:%p, handling {%s} for global_port:%d vc:%d local_port:%d",
    to_string().c_str(), this,
    pkt->to_string().c_str(),
    glob_port, dst_vc, loc_port);

  int& num_credits = credit(loc_port, dst_vc);
   pisces_debug(
    "On %s:%p with %d credits, handling {%s} for local port:%d vc:%d",
     to_string().c_str(), this,
     num_credits,
     pkt->to_string().c_str(),
     loc_port, dst_vc);

  if (dst_vc < 0 || loc_port < 0){
    spkt_throw_printf(sprockit::value_error,
       "On %s handling {%s}, got negative vc,local_port %d,%d",
        to_string().c_str(), pkt->to_string().c_str(), loc_port, dst_vc);
  }

  if (num_credits >= pkt->num_bytes()) {
    num_credits -= pkt->num_bytes();
    send_payload(pkt);
  }
  else {
    pisces_debug(
      "On %s:%p, pushing back %s on queue %d=(%d,%d) for nq=%d nvc=%d mapper=(%d,%d,%d)",
      to_string().c_str(), this, pkt->to_string().c_str(),
      slot(loc_port, dst_vc), loc_port, dst_vc, queues_.size(), num_vc_,
      port_offset_, port_div_, port_mod_);
      queue(loc_port, dst_vc).push_back(pkt);
  }
}

void
pisces_NtoM_queue::resize(int num_ports)
{
  outputs_.resize(num_ports);
  queues_.resize(num_ports*num_vc_);
  credits_.resize(num_ports*num_vc_);
}

void
pisces_NtoM_queue::configure_basic_ports(int num_ports)
{
  port_offset_ = 0;
  port_mod_ = 0;
  port_div_ = 1;
  resize(num_ports);
}

void
pisces_NtoM_queue::configure_div_ports(int div, int num_ports)
{
//  debug_printf(sprockit::dbg::pisces_config | sprockit::dbg::pisces,
//    "On %s configured for div local ports: div=%d,max=%d",
//    to_string().c_str(), div, num_ports);

  port_offset_ = 0;
  port_mod_ = 0;
  port_div_ = div;
  resize(num_ports);
}

void
pisces_NtoM_queue::configure_offset_ports(int offset, int num_ports)
{
//  debug_printf(sprockit::dbg::pisces_config | sprockit::dbg::pisces,
//    "On %s configured for offset local ports: offset=%d,max=%d",
//    to_string().c_str(), offset, num_ports);

  port_offset_ = offset;
  port_mod_ = 0;
  port_div_ = 1;
  resize(num_ports);
}

void
pisces_NtoM_queue::configure_mod_ports(int mod)
{
//  debug_printf(sprockit::dbg::pisces_config | sprockit::dbg::pisces,
//    "On %s configured for modulo local ports: m=%d",
//    to_string().c_str(), mod);

  port_offset_ = 0;
  port_mod_ = mod;
  port_div_ = 1;
  resize(mod);
}

void
pisces_NtoM_queue::set_input(
  sprockit::sim_parameters* port_params,
  int my_inport, int src_outport,
  event_handler* input)
{
  debug_printf(sprockit::dbg::pisces_config | sprockit::dbg::pisces,
    "On %s:%d setting input %s:%d",
    to_string().c_str(), my_inport,
    input->to_string().c_str(), src_outport);

//  std::cerr << "inputs size1: " << inputs_.size() << std::endl;

//  std::cerr << "my_inport: " << my_inport << "\n";
  pisces_input inp;
  inp.src_outport = src_outport;
  inp.handler = input;
//  std::cerr << "inputs size2: " << inputs_.size() << std::endl;
  inputs_[my_inport] = inp;
//  std::cerr << "inputs size3: " << inputs_.size() << std::endl;
}

void
pisces_NtoM_queue::set_output(
  sprockit::sim_parameters* port_params,
  int my_outport, int dst_inport,
  event_handler* output)
{
  debug_printf(sprockit::dbg::pisces_config | sprockit::dbg::pisces,
    "On %s setting output %s:%d for local port %d, mapper=(%d,%d,%d) of %d",
    to_string().c_str(),
    output->to_string().c_str(), dst_inport,
    my_outport, port_offset_, port_div_, port_mod_,
    outputs_.size());

  pisces_output out;
  out.dst_inport = dst_inport;
  out.handler = output;

  if (my_outport > outputs_.size()) {
    std::cerr << sprockit::printf("my_outport %i > outputs_.size() %i\n", my_outport, outputs_.size());
    abort();
  }
  outputs_[my_outport] = out;

  int num_credits = port_params->get_byte_length_param("credits");
  int num_credits_per_vc = num_credits / num_vc_;
  for (int i=0; i < num_vc_; ++i) {
    debug_printf(sprockit::dbg::pisces_config,
                 "On %s:%p, initing %d credits on port:%d vc:%d",
                 to_string().c_str(), this,
                 num_credits_per_vc,
                 my_outport, i);
    credit(my_outport, i) = num_credits_per_vc;
  }
}

#if PRINT_FINISH_DETAILS
void
print_msg(const std::string& prefix, switch_id addr, pisces_payload* pkt)
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
pisces_NtoM_queue::start_message(message* msg)
{
  spkt_throw(sprockit::illformed_error,
    "pisces_NtoM_queue:: should never start a flow");
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
            pisces_payload* pkt = *pit;
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


