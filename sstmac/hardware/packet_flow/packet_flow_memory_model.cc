#include <sstmac/hardware/packet_flow/packet_flow_memory_model.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/libraries/compute/compute_message.h>
#include <sstmac/common/runtime.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

SpktRegister("packet_flow", memory_model, packet_flow_memory_model);

//int packet_flow_memory_model::mtu_;

packet_flow_memory_sender::packet_flow_memory_sender(node* parent) :
  bw_noise_(0),
  interval_noise_(0),
  num_noisy_intervals_(0),
  parent_node_(parent)
{
  int num_channels = 4;
  pending_.resize(num_channels);
  for (int i=0; i < num_channels; ++i){
    channels_available_.push_back(i);
  }
}

packet_flow_memory_model::packet_flow_memory_model()
{
}

void
packet_flow_memory_sender::init_params(sprockit::sim_parameters *params)
{
  max_single_bw_ = params->get_bandwidth_param("max_single_bandwidth");
  max_bw_ = params->get_bandwidth_param("total_bandwidth");
  latency_ = params->get_time_param("latency");
  arb_ = packet_flow_bandwidth_arbitrator_factory::get_optional_param(
        "arbitrator", "cut_through", params);
  endpoint_ = packet_flow_endpoint_factory::get_optional_param(
    "arbitrator", "cut_through", params);
  /**
   sstkeyword {
   docstring=The minimum number of bytes a single packet train can contain.ENDL
   Raising this value increases the coarseness and lowers the accuracy.;
   };
   */
  mtu_ = params->get_optional_int_param("mtu", 1<<30); //Defaults to huge value
}

void
packet_flow_memory_sender::set_event_parent(event_scheduler *m)
{
  endpoint_->set_event_parent(m);
  packet_flow_sender::set_event_parent(m);
}

void
packet_flow_memory_sender::finalize_init()
{
  arb_->set_bw(max_bw_);
  init_noise_model();
  endpoint_->set_exit(parent_node_);
  endpoint_->init_param1(parent_node_->addr());
  init_loc_id(event_loc_id(parent_node_->addr()));
}

void
packet_flow_memory_model::init_factory_params(sprockit::sim_parameters *params)
{
  memory_model::init_factory_params(params);
  max_single_bw_ = params->get_bandwidth_param("max_single_bandwidth");
  sender_ = new packet_flow_memory_sender(parent_node_);
  sender_->init_params(params);
  sender_->finalize_init();
}

void
packet_flow_memory_sender::init_noise_model()
{
  if (bw_noise_){
    arb_->partition(interval_noise_, num_noisy_intervals_);
    arb_->init_noise_model(bw_noise_);
  }
}

packet_flow_memory_model::~packet_flow_memory_model()
{
  //if (bw_noise_) delete bw_noise_;
  //bw_noise_ = 0;
}

void
packet_flow_memory_model::finalize_init()
{
  memory_model::finalize_init();
}

int
packet_flow_memory_sender::num_initial_credits() const
{
  spkt_throw_printf(sprockit::value_error,
    "packet_flow_memory_model::num_initial_credits: should never be called");
}

void
packet_flow_memory_model::set_event_parent(event_scheduler* m)
{
  memory_model::set_event_parent(m);
  sender_->set_event_parent(m);
}

void
packet_flow_memory_model::access(const sst_message::ptr& msg)
{
  sw::compute_message::ptr cmsg = ptr_safe_cast(sw::compute_message, msg);
  cmsg->set_access_id(parent_node_->allocate_unique_id());
  sender_->start(msg);
}

int
packet_flow_memory_sender::allocate_channel()
{
  if (channels_available_.empty()){
    int oldsize = pending_.size();
    //double size of pending
    int newsize = oldsize*2;
    pending_.resize(newsize);
    for (int i=oldsize; i != newsize; ++i){
      channels_available_.push_back(i);
    }
  }
  int channel = channels_available_.front();
  channels_available_.pop_front();
  return channel;
}

void
packet_flow_memory_sender::start(const sst_message::ptr& msg)
{
  sw::compute_message::ptr cmsg = ptr_safe_cast(sw::compute_message, msg);

  packet_flow_payload::ptr payload = next_chunk(0L, cmsg);

  if (!payload->is_tail()){
    int channel = allocate_channel();
    payload->set_inport(channel);
    //not quite done - need to wait for credits to send the rest of this
    pending_msg& p = pending_[channel];
    p.byte_offset = payload->num_bytes();
    p.msg = cmsg;
    debug_printf(sprockit::dbg::packet_flow,
      "memory starting payload %s for compute message of size %d on channel %d at byte offset %d\n",
      payload->to_string().c_str(), cmsg->byte_length(), channel, p.byte_offset);
  }

  handle_payload(payload);
}


void
packet_flow_memory_sender::do_handle_payload(const packet_flow_payload::ptr& msg)
{
  //set the bandwidth to the max single bw
  msg->init_bw(max_single_bw_);
  timestamp packet_head_leaves;
  timestamp packet_tail_leaves;
  arb_->arbitrate(now(), msg, packet_head_leaves, packet_tail_leaves);
  timestamp finish = packet_head_leaves + latency_;

  send_to_endpoint(finish, msg);

  //might need to send some credits back
  if (!msg->is_tail()){
    int ignore_vc = -1;
    packet_flow_credit::ptr credit = new packet_flow_credit(msg->inport(), ignore_vc, msg->num_bytes());
    packet_flow_handler::send_self_message(packet_tail_leaves, credit);
  }
}

void
packet_flow_memory_sender::send_to_endpoint(timestamp finish, const packet_flow_payload::ptr& msg)
{
  SCHEDULE(finish, endpoint_, msg);
}

void
packet_flow_memory_sender::set_input(int my_inport, int dst_outport, event_handler* input)
{

}

void
packet_flow_memory_sender::set_output(int my_outport, int dst_inport, event_handler* output)
{
}

void
packet_flow_memory_sender::init_credits(int port, int num_credits)
{
}

packet_flow_payload::ptr
packet_flow_memory_sender::next_chunk(long byte_offset, const sw::compute_message::ptr& parent)
{
  long bytes_left = parent->byte_length() - byte_offset;
  long bytes_to_send = bytes_left > mtu_ ? mtu_ : bytes_left;

  packet_flow_payload::ptr payload = new packet_flow_payload(
                                         parent,
                                         bytes_to_send, //only a single message
                                         byte_offset);

  if (parent->max_bw() != 0){
    payload->set_bw(parent->max_bw());
  }

  return payload;
}

void
packet_flow_memory_sender::handle_credit(const packet_flow_credit::ptr& msg)
{
  int channel = msg->port();
  pending_msg& p = pending_[channel];

  //printf("Handling credit of size %d for channel %d\n",
  //    msg->num_credits(), channel);

  packet_flow_payload::ptr payload = next_chunk(p.byte_offset, p.msg);
  payload->set_inport(channel);
  p.byte_offset += payload->num_bytes();

  //printf("Handling payload of size %d for message of size %d on channel %d at byte offset %d\n",
  //  payload->num_bytes(), p.msg->byte_length(), channel, p.byte_offset);

  if (p.byte_offset == p.msg->byte_length()){
    //whole message has been sent
    //channel is available
    channels_available_.push_back(channel);
    //and clear the pending message
    p.byte_offset = 0;
    p.msg = 0;
  }


  handle_payload(payload);
}

}
}
