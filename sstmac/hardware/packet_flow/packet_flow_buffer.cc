#include <sstmac/hardware/packet_flow/packet_flow_buffer.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/common/runtime.h>
#include <sprockit/util.h>

#define PRINT_FINISH_DETAILS 0

namespace sstmac {
namespace hw {

packet_flow_buffer::packet_flow_buffer(
  const timestamp& send_lat,
  const timestamp& credit_lat,
  packet_flow_bandwidth_arbitrator* arb)
  : packet_flow_sender(send_lat, credit_lat),
  bytes_delayed_(0),
  arb_(arb)
{
}

packet_flow_buffer::~packet_flow_buffer()
{
  if (arb_) delete arb_;
}

void
packet_flow_finite_buffer::set_input(int this_inport, int src_outport,
                              event_handler* input)
{
  debug_printf(sprockit::dbg::packet_flow_config,
    "On %s:%d setting input %s:%d",
    to_string().c_str(), this_inport,
    input->to_string().c_str(), src_outport);
  input_.src_outport = src_outport;
  input_.handler = input;
}

void
packet_flow_buffer::set_output(int this_outport, int dst_inport,
                               event_handler* output)
{
  debug_printf(sprockit::dbg::packet_flow_config,
  "On %s:%d setting output %s:%d",
  to_string().c_str(), this_outport,
  output->to_string().c_str(), dst_inport);

  output_.handler = output;
  output_.dst_inport = dst_inport;
}

packet_flow_network_buffer::packet_flow_network_buffer(
  const timestamp& send_lat,
  const timestamp& credit_lat,
  int max_num_bytes,
  int num_vc,
  int packet_size,
  packet_flow_bandwidth_arbitrator* arb)
  : packet_flow_finite_buffer(send_lat, credit_lat, max_num_bytes, arb),
    num_vc_(num_vc),
    queues_(num_vc),
    credits_(num_vc, 0),
    packet_size_(packet_size)
{
}

void
packet_flow_network_buffer::init_credits(int port, int num_credits)
{
  long num_credits_per_vc = num_credits / num_vc_;
  for (int i=0; i < num_vc_; ++i) {
    credits_[i] = num_credits_per_vc;
  }
}

void
packet_flow_network_buffer::handle_credit(packet_flow_credit* msg)
{
  int vc = msg->vc();
#if SSTMAC_SANITY_CHECK
  if (vc >= credits_.size()) {
    spkt_throw_printf(sprockit::value_error,
                     "packet_flow_buffer::handle_credit: on %s, port %d, invalid vc %d",
                     to_string().c_str(),
                     msg->port(), vc);
  }
#endif
  int& num_credits = credits_[vc];
  num_credits += msg->num_credits();
  //we've cleared out some of the delay
  bytes_delayed_ -= msg->num_credits();

  packet_flow_debug(
    "On %s with %d credits, handling {%s} for vc:%d -> byte delay now %d",
     to_string().c_str(),
     num_credits,
     msg->to_string().c_str(),
     vc,
     bytes_delayed_);

  /** while we have sendable payloads, do it */
  packet_flow_payload* payload = queues_[vc].pop(num_credits);
  while (payload) {
    num_credits -= payload->num_bytes();
    //this actually doesn't create any new delay
    //this message was already queued so num_bytes
    //was already added to bytes_delayed
    send(arb_, payload, input_, output_);
    payload = queues_[vc].pop(num_credits);
  }

  delete msg;
}

void
packet_flow_network_buffer::do_handle_payload(packet_flow_payload* pkt)
{
  int dst_vc = pkt->vc();
#if SSTMAC_SANITY_CHECK
  //vc default to uninit instead of zero to make sure routers set VC
  dst_vc = dst_vc == routing_info::uninitialized ? 0 : dst_vc;
#endif

#if SSTMAC_SANITY_CHECK
  if (dst_vc >= credits_.size()) {
    spkt_throw_printf(sprockit::value_error,
                     "packet_flow_buffer::handle_payload: on %s, port %d, invalid vc %d",
                     to_string().c_str(),
                     msg->port(),
                     dst_vc);
  }
#endif
  int& num_credits = credits_[dst_vc];

  packet_flow_debug(
    "On %s with %d credits, handling {%s} for vc:%d",
    to_string().c_str(),
    num_credits,
    pkt->to_string().c_str(),
    dst_vc);

  // it either gets queued or gets sent
  // either way there's a delay accumulating for other messages
  bytes_delayed_ += pkt->num_bytes();
  if (num_credits >= pkt->num_bytes()) {
    num_credits -= pkt->num_bytes();
    send(arb_, pkt, input_, output_);
  }
  else {
    queues_[dst_vc].push_back(pkt);
#if SSTMAC_SANITY_CHECK
    if (queue_depth_reporting_) {
      if(queues_[dst_vc].size() > 0 &&
         !(queues_[dst_vc].size() % queue_depth_delta_)) {
        std::cout << "warning: packet flow output buffer queue has reached a depth of "
                  << queues_[dst_vc].size()
                  << "\n";
      }
    }
#endif
  }
}

void
packet_flow_network_buffer::deadlock_check()
{
  for (int i=0; i < queues_.size(); ++i){
    payload_queue& queue = queues_[i];
    packet_flow_payload* pkt = queue.front();
    if (pkt){
      int vc = pkt->next_vc();
      deadlocked_channels_.insert(vc);
      pkt->set_inport(output_.dst_inport);
      vc = update_vc_ ? pkt->next_vc() : pkt->vc();
      std::cerr << "Starting deadlock check on " << to_string() << " on queue " << i
        << " going to " << output_.handler->to_string()
        << " outport=" << pkt->next_port()
        << " inport=" << pkt->inport()
        << " vc=" << vc
        << std::endl;
      output_.handler->deadlock_check(pkt);
    }
  }
}

void
packet_flow_network_buffer::build_blocked_messages()
{
  //std::cerr << "\tbuild blocked messages on " << to_string() << std::endl;
  for (int i=0; i < queues_.size(); ++i){
    payload_queue& queue = queues_[i];
    packet_flow_payload* pkt = queue.pop(1000000);
    while (pkt){
      blocked_messages_[pkt->vc()].push_back(pkt);
      //std::cerr << "\t\t" << "into port=" << msg->inport() << " vc=" << msg->vc()
      //  << " out on port=" << msg->port() << " vc=" << msg->routable_message::vc() << std::endl;
      pkt = queue.pop(10000000);
    }
  }
}

void
packet_flow_network_buffer::deadlock_check(event* ev)
{
  if (blocked_messages_.empty()){
    build_blocked_messages();
  }

  packet_flow_payload* payload = safe_cast(packet_flow_payload, ev);
  int outport = payload->next_port();
  int inport = payload->inport();
  int vc = update_vc_ ? payload->next_vc() : payload->vc();
  if (deadlocked_channels_.find(vc) != deadlocked_channels_.end()){
    spkt_throw_printf(sprockit::value_error,
      "found deadlock:\n%s", to_string().c_str());
  }

  deadlocked_channels_.insert(vc);

  std::list<packet_flow_payload*>& blocked = blocked_messages_[vc];
  if (blocked.empty()){
    spkt_throw_printf(sprockit::value_error,
      "channel is NOT blocked on deadlock check on outport=%d inport=%d vc=%d",
      outport, inport, vc);
  } else {
    packet_flow_payload* next = blocked.front();
    next->set_inport(output_.dst_inport);
    std::cerr << to_string() << " going to "
      << output_.handler->to_string()
      << " outport=" << next->next_port()
      << " inport=" << next->inport()
      << " vc=" << next->next_vc()
      << " : " << next->to_string()
      << std::endl;
    output_.handler->deadlock_check(next);
  }
}

#if PRINT_FINISH_DETAILS
extern void
print_msg(const std::string& prefix, switch_id addr, packet_flow_payload* pkt);
#endif


#if PRINT_FINISH_DETAILS
    switch_id addr;
    structured_topology* top = safe_cast(structured_topology, sstmac_runtime::current_topology());
    if (event_location_.is_node_id()){
        node_id nid = event_location_.convert_to_node_id();
        addr = switch_id(int(nid)/4);
        coordinates my_coords = top->get_node_coords(nid);
        coutn << "Network Injection Buffer " << my_coords.to_string() << "\n";
    }
    else {
        addr = switch_id(event_location_.location);
        coordinates my_coords = top->get_switch_coords(addr);
        coutn << "Network Switch Buffer " << my_coords.to_string() << "\n";
    }

     coutn << "\tQueue\n";
     for (int i=0; i < queues_.size(); ++i){
        coutn << "\t\tVC " << i << std::endl;
        payload_queue& que = queues_[i];
        payload_queue::iterator pit, pend = que.end();
        for (pit = que.begin(); pit != pend; ++pit){
            packet_flow_payload* pkt = *pit;
            print_msg("\t\t\tPending: ", addr, msg);
        }
    }

    coutn << "\tQueue\n";
    for (int i=0; i < credits_.size(); ++i){
        coutn << "\t\tVC " << i << " credits=" << credits_[i] << "\n";
    }
#endif

int
packet_flow_network_buffer::queue_length() const
{
  long bytes_sending = arb_->bytes_sending(now());
  long total_bytes_pending = bytes_sending + bytes_delayed_;
  long queue_length = total_bytes_pending / packet_size_;
  debug_printf(sprockit::dbg::packet_flow | sprockit::dbg::packet_flow_queue,
    "On %s, %d bytes delayed, %d bytes sending, %d total pending, %d packets in queue",
     to_string().c_str(),
     bytes_delayed_,
     bytes_sending,
     total_bytes_pending, queue_length);
  return std::max(0L, total_bytes_pending);
}

std::string
packet_flow_buffer::buffer_string(const char* name) const
{
  int id;
  if (event_location().is_switch_id()){
    id = event_location().convert_to_switch_id();
  } else {
    id = event_location().convert_to_node_id();
  }
  return sprockit::printf("%s %d", name, id);
}

int
packet_flow_infinite_buffer::num_initial_credits() const
{
  spkt_throw_printf(sprockit::value_error,
    "packet_flow_infinite_buffer::num_initial_credits: should never be called");
}

packet_flow_eject_buffer::packet_flow_eject_buffer(
  const timestamp& send_lat,
  const timestamp& credit_lat,
  int max_num_bytes,
  packet_flow_bandwidth_arbitrator* arb)
  : packet_flow_finite_buffer(send_lat, credit_lat, max_num_bytes, arb)
{
}

void
packet_flow_eject_buffer::return_credit(packet* msg)
{
  send_credit(input_, safe_cast(packet_flow_payload, msg), now());
}

void
packet_flow_eject_buffer::do_handle_payload(packet_flow_payload* pkt)
{
  debug_printf(sprockit::dbg::packet_flow,
    "On %s, handling {%s}",
    to_string().c_str(),
    pkt->to_string().c_str());
  int one_vc = 0;
  int one_outport = 0;
  send(arb_, pkt, input_, output_);
}

void
packet_flow_eject_buffer::handle_credit(packet_flow_credit* pkt)
{
  spkt_throw_printf(sprockit::illformed_error,
                   "packet_flow_eject_buffer::handle_credit: should not handle credits");
}

void
packet_flow_eject_buffer::init_credits(int port, int num_credits)
{
  spkt_throw_printf(sprockit::illformed_error,
                   "packet_flow_eject_buffer::init_credits: should not handle credits");
}

packet_flow_injection_buffer::packet_flow_injection_buffer(
  const timestamp& out_lat,
  packet_flow_bandwidth_arbitrator* arb,
  int packet_size)
  : packet_flow_infinite_buffer(out_lat, arb),
    credits_(0),
    packet_size_(packet_size)
{
}

void
packet_flow_injection_buffer::init_credits(int port, int num_credits)
{
  debug_printf(sprockit::dbg::packet_flow | sprockit::dbg::packet_flow_config,
    "On %s, initializing with %d credits",
     to_string().c_str(), num_credits);
  credits_ = num_credits;
}

void
packet_flow_injection_buffer::handle_credit(packet_flow_credit* msg)
{
  debug_printf(sprockit::dbg::packet_flow,
    "On %s with %d credits, handling {%s} -> byte delay now %d",
     to_string().c_str(),
     credits_,
     msg->to_string().c_str(),
     bytes_delayed_);

  credits_ += msg->num_credits();
  //we've cleared out some of the delay
  bytes_delayed_ -= msg->num_credits();

  delete msg;
  //send_what_you_can();

  //delete msg;
}

void
packet_flow_injection_buffer::do_handle_payload(packet_flow_payload* pkt)
{
  credits_ -= pkt->byte_length();
  //we only get here if we cleared the credits
  send(arb_, pkt, input_, output_);
}

#if PRINT_FINISH_DETAILS
extern void
print_msg(const std::string& prefix, switch_id addr, packet_flow_payload* pkt);
#endif

#if PRINT_FINISH_DETAILS
    switch_id addr;
    structured_topology* top = safe_cast(structured_topology, sstmac_runtime::current_topology());
    if (event_location_.is_node_id()){
        node_id nid = event_location_.convert_to_node_id();
        addr = switch_id(int(nid)/4);
        coordinates my_coords = top->get_node_coords(nid);
        coutn << "Network Injection Buffer " << my_coords.to_string() << "\n";
    }
    else {
        addr = switch_id(event_location_.location);
        coordinates my_coords = top->get_switch_coords(addr);
        coutn << "Network Switch Buffer " << my_coords.to_string() << "\n";
    }

     coutn << "\tQueue\n";
     for (int i=0; i < queues_.size(); ++i){
        coutn << "\t\tVC " << i << std::endl;
        payload_queue& que = queues_[i];
        payload_queue::iterator pit, pend = que.end();
        for (pit = que.begin(); pit != pend; ++pit){
            packet_flow_payload* pkt = *pit;
            print_msg("\t\t\tPending: ", addr, msg);
        }
    }

    coutn << "\tQueue\n";
    for (int i=0; i < credits_.size(); ++i){
        coutn << "\t\tVC " << i << " credits=" << credits_[i] << "\n";
    }
#endif

int
packet_flow_injection_buffer::queue_length() const
{
  long bytes_sending = arb_->bytes_sending(now());
  long total_bytes_pending = bytes_sending + bytes_delayed_;
  long queue_length = total_bytes_pending / packet_size_;
  debug_printf(sprockit::dbg::packet_flow,
    "On %s, %d bytes delayed, %d total pending, %d packets in queue\n",
     to_string().c_str(),
     bytes_delayed_,
     total_bytes_pending, queue_length);
  return std::max(0L, total_bytes_pending);
}



}
}


